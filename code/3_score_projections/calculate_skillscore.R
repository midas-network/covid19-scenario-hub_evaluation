library(dplyr)
library(data.table)
library(SMHEvaluationUtils)

#### LOAD WIS ------------------------------------------------------------------
source("code/3_score_projections/scoring_functions.R")
# set base location of 
WIS <- SMHEvaluationUtils::load_scores("data-output/WIS")

# add plausibility weights
source("code/0_setup_scenario_plausibility/define_variant_takeover.R")

# a few fixes for model updates in later rounds (add to SMH EVALUATION)
# R13: MOBS_NEU-GLEAM_COVID and MOBS_NEU-GLEAM_COVID-OT get weight = 0.5
# R14: MOBS_NEU-GLEAM_COVID gets weight 0 and MOBS_NEU-GLEAM_COVID_OT gets weight 1
#      USC-SIkJalpha-update gets weight 0 and USC-SIkJalpha gets weight 1
model_exclusions <- data.frame(model_name = c("MOBS_NEU-GLEAM_COVID", 
                                              "MOBS_NEU-GLEAM_COVID_OT", 
                                              "MOBS_NEU-GLEAM_COVID", 
                                              "USC-SIkJalpha-update"), 
                               round = c(13,13,14,14), 
                               weight = c(0.5, 0.5, 0, 0))

# plausible scenarios
WIS <- SMHEvaluationUtils::add_plaus_weight(proj = WIS,
                                             variant_takeover = variant_takeover_date, 
                                             modelname_round_weight = model_exclusions,
                                             keep_flags = TRUE, 
                                             p = "data-raw/data-scenarios/MostPlausibleScenarios.csv")


#### ENSEMBLE SKILL SCORE ------------------------------------------------------
# skill score
# using weighted WIS to calculate skill
ens_skill_byround <- skill_score(WIS %>%
                                   .[, plaus_weight := plaus_week * plaus_weight] %>% 
                                   .[, .(plaus_WIS = weighted.mean(WIS, plaus_weight)), 
                                     by = .(round, target, target_end_date, model_name, 
                                            location)] %>%
                                   .[substr(model_name, 1, 3) %in% c("Ens")] %>%
                                   select(target, target_end_date,
                                          model_name, location, round, plaus_WIS) %>% 
                                   rename(score = plaus_WIS), 
                                 grouping = c("round", "target"))
ens_skill <- skill_score(WIS %>%
                           .[, plaus_weight := plaus_week * plaus_weight] %>% 
                           .[, .(plaus_WIS = weighted.mean(WIS, plaus_weight)), 
                             by = .(round, target, target_end_date, model_name, 
                                    location)] %>%
                           .[substr(model_name, 1, 3) %in% c("Ens")] %>%
                           select(target, target_end_date, 
                                  model_name, location, round, plaus_WIS) %>% 
                           rename(score = plaus_WIS), 
                         grouping = c("target")) %>%
  .[, round:= "overall"]
ens_skill_byround = rbindlist(list(ens_skill_byround, ens_skill), use.names = TRUE)

# save results
write.csv(ens_skill_byround, "data-output/skillscore_ens.csv")


#### INDIVIDUAL MODEL SKILL SCORE ----------------------------------------------
# find teams that have national projection to include in skill score
include_projs <- WIS[location == "US" & 
                       !(model_name %in% c("Ensemble", "Ensemble_LOP_untrimmed", 
                                           "null_gam", "null_fh", "null_naive")), 
                         .(model_name, round)] %>% unique()
include_projs <- paste(include_projs$model_name, include_projs$round, sep = "_")

skill_byround <- skill_score(WIS %>% 
                               .[paste(model_name, round, sep = "_") %in% include_projs &
                                   substr(target,1,3) == "inc"] %>% 
                               .[, model_name := ifelse(model_name == "MOBS_NEU-GLEAM_COVID_OT", "MOBS_NEU-GLEAM_COVID", 
                                                        ifelse(model_name == "USC-SIkJalpha-update", "USC-SIkJalpha", model_name))] %>% 
                               .[, .(plaus_WIS = weighted.mean(WIS, plaus_weight)), 
                                 by = .(round, target, target_end_date, model_name, 
                                        location)] %>%
                               .[, .(target, target_end_date,
                                     model_name, location, round, plaus_WIS)] %>%
                               rename(score = plaus_WIS), 
                             grouping = c("round", "target"))
skill <- skill_score(WIS %>% 
                       .[paste(model_name, round, sep = "_") %in% include_projs &
                           substr(target,1,3) == "inc"] %>% 
                       .[, model_name := ifelse(model_name == "MOBS_NEU-GLEAM_COVID_OT", "MOBS_NEU-GLEAM_COVID", 
                                                ifelse(model_name == "USC-SIkJalpha-update", "USC-SIkJalpha", model_name))] %>% 
                       .[, .(plaus_WIS = weighted.mean(WIS, plaus_weight)), 
                         by = .(round, target, target_end_date, model_name, 
                                location)] %>%
                       .[, .(target, target_end_date,
                             model_name, location, round, plaus_WIS)] %>%
                       rename(score = plaus_WIS),
                     grouping = c("target")) %>%
  .[, round:= "overall"]
skill_byround = rbindlist(list(skill_byround, skill), use.names = TRUE)

# relativize skill score to null model
# skill_byround <- skill_byround[substr(model_name,1,4) == "null"] %>%
#   data.table::dcast(round + target ~ model_name, value.var="skill") %>% 
#   .[skill_byround[substr(model_name,1,4)!= "null" & substr(target,1,3) == "inc"], 
#     on = .(round, target)] %>%
#   data.table::melt(c("round", "target", "model_name", "skill")) %>%
#   setnames("value", "baseline_skill") %>%
#   setnames("variable", "null_model") %>%
#   .[, rel_skill := skill/baseline_skill]


# hack some bootstrapping for now
# leave out one week
for(i in 1:52){
  print(i)
  # calculate skill score with one week out
  skill_byround_tmp <- skill_score(WIS %>%
                                     .[, proj_week := (target_end_date - min(target_end_date))/7 + 1, by = .(round)] %>%
                                     .[paste(model_name, round, sep = "_") %in% include_projs &
                                         substr(target,1,3) == "inc" &
                                         proj_week != i] %>%
                                     .[, model_name := ifelse(model_name == "MOBS_NEU-GLEAM_COVID_OT", "MOBS_NEU-GLEAM_COVID", 
                                                              ifelse(model_name == "USC-SIkJalpha-update", "USC-SIkJalpha", model_name))] %>% 
                                     .[, .(plaus_WIS = weighted.mean(WIS, plaus_weight)), 
                                       by = .(round, target, target_end_date, model_name, 
                                              location)] %>%
                                     .[, .(target, target_end_date,
                                           model_name, location, round, plaus_WIS)] %>%
                                     rename(score = plaus_WIS),
                                   grouping = c("round", "target"))
  skill_tmp <- skill_score(WIS %>%
                             .[, proj_week := (target_end_date - min(target_end_date))/7 + 1, by = .(round)] %>%
                             .[paste(model_name, round, sep = "_") %in% include_projs &
                                 substr(target,1,3) == "inc" &
                                 proj_week != i] %>%
                             .[, model_name := ifelse(model_name == "MOBS_NEU-GLEAM_COVID_OT", "MOBS_NEU-GLEAM_COVID", 
                                                      ifelse(model_name == "USC-SIkJalpha-update", "USC-SIkJalpha", model_name))] %>% 
                             .[, .(plaus_WIS = weighted.mean(WIS, plaus_weight)), 
                               by = .(round, target, target_end_date, model_name, 
                                      location)] %>%
                             .[, .(target, target_end_date,
                                   model_name, location, round, plaus_WIS)] %>%
                             rename(score = plaus_WIS),
                           grouping = c("target")) %>%
    .[, round:= "overall"]
  skill_byround_tmp = rbindlist(list(skill_byround_tmp, skill_tmp), use.names = TRUE)
  # relativize skill score to null model
  # skill_byround_tmp <- skill_byround_tmp[substr(model_name,1,4) == "null"] %>%
  #   data.table::dcast(round + target ~ model_name, value.var="skill") %>%
  #   .[skill_byround_tmp[substr(model_name,1,4)!= "null" & substr(target,1,3) == "inc"],
  #     on = .(round, target)] %>%
  #   data.table::melt(c("round", "target", "model_name", "skill")) %>%
  #   setnames("value", "baseline_skill") %>%
  #   setnames("variable", "null_model") %>%
  #   .[, rel_skill := skill/baseline_skill]
  # save results
  if(i == 1){
    skill_boot = skill_byround_tmp[, wk_out := 1]
  }
  else{
    skill_boot = rbind(skill_boot, skill_byround_tmp[, wk_out := 1])
  }
}

# randomly draw each week n_samp times to generate intervals 
set.seed(3)
n_samp = 1000
h <- proj[plaus_week > 0 &
            model_name == "Ensemble_LOP", 
          .(h = (max(target_end_date) - min(target_end_date))/7 + 1), by = .(round)]

w <- list()
for(i in 1:nrow(h)){
  w[[i]] <- data.frame(samp = 1:n_samp,
                     round = h[i, "round"],
                     wk_out = sample(1:unlist(h[i, "h"]), n_samp, replace = TRUE))
}
w <- rbindlist(w)
w <- w[, round := as.character(round)]

intervals <- setDT(w)[setDT(skill_boot) %>%
                        .[, round := as.character(round)], 
                      on= .(wk_out, round),
                      allow.cartesian = TRUE] %>%
  .[, .(Q5 = quantile(skill, 0.05),
        Q95 = quantile(skill, 0.95)),
    by = .(round, target, model_name)]

# save outputs
skill_byround <- skill_byround %>% 
  .[, round := as.character(round)] %>% 
  .[intervals, on = .(round, target, model_name)]
write.csv(skill_byround, "data-output/skill-score/skillscore_indmods.csv")


#### CACLCULATE SCENARIO SKILL SCORE (ALL MODELS) ------------------------------
# calculate point estimates (for each scenario from all models)
scenario_skill <- skill_score(WIS %>%
                                .[!model_name %in% c("null_fh", "null_gam", "null_naive") &
                                    substr(target, 1,3) == "inc"] %>%
                                select(target, target_end_date, scenario_id,
                                       model_name, location, round, WIS) %>% 
                                rename(score = WIS) %>%
                                rename(model = model_name) %>%
                                rename(model_name = scenario_id),
                              grouping = c("round","model", "target"))
scenario_skill <- scenario_skill %>%
  rename(scenario_id = model_name) %>%
  rename(model_name = model)
write.csv(scenario_skill, "data-output/skill-score/skillscore_scenarios_allmods.csv")

#### CACLCULATE SCENARIO SKILL SCORE (ENS ONLY, RELATIVE TO FH, BOOSTRAP) ------
scenario_skill <- skill_score(WIS %>%
                                .[model_name %in% c("null_fh", "Ensemble_LOP") &
                                    substr(target, 1,3) == "inc"] %>%
                                .[, scenario_id := ifelse(model_name == "null_fh", 
                                                          paste0("N-", round), scenario_id)] %>%
                                select(target, target_end_date, scenario_id,
                                       location, round, WIS) %>% 
                                rename(score = WIS) %>%
                                rename(model_name = scenario_id),
                              grouping = c("round","target"))
scenario_skill <- scenario_skill %>%
  rename(scenario_id = model_name)
# relativize skill score to null model
scenario_skill <- scenario_skill %>%
  .[, scenario_letter := substr(scenario_id,1,1)] %>%
  .[scenario_letter == "N"] %>%
  data.table::dcast(round + target ~ scenario_letter, value.var="skill") %>% 
  .[scenario_skill[substr(scenario_id,1,1)!= "N"], 
    on = .(round, target)] %>%
  .[, rel_skill := skill/N]


# hack some bootstrapping for now
for(i in 1:52){
  print(i)
  scenario_skill_tmp <- skill_score(WIS %>%
                                  .[,proj_week := (target_end_date - min(target_end_date))/7 + 1, by = .(round, target, location, model_name)] %>%
                                  .[model_name %in% c("null_fh", "Ensemble_LOP") &
                                      substr(target, 1,3) == "inc" &
                                      proj_week != i] %>%
                                  .[, scenario_id := ifelse(model_name == "null_fh", 
                                                            paste0("N-", round), scenario_id)] %>%
                                  select(target, target_end_date, scenario_id,
                                         location, round, WIS) %>% 
                                  rename(score = WIS) %>%
                                  rename(model_name = scenario_id),
                                grouping = c("round","target"))
  scenario_skill_tmp <- scenario_skill_tmp %>%
    rename(scenario_id = model_name)
  # relativize skill score to null model
  scenario_skill_tmp <- scenario_skill_tmp %>%
    .[, scenario_letter := substr(scenario_id,1,1)] %>%
    .[scenario_letter == "N"] %>%
    data.table::dcast(round + target ~ scenario_letter, value.var="skill") %>% 
    .[scenario_skill_tmp[substr(scenario_id,1,1)!= "N"], 
      on = .(round, target)] %>%
    .[, rel_skill := skill/N]
  if(i == 1){
    scenario_skill_boot <- scenario_skill_tmp[, wk_out := i]
  }
  else{
    scenario_skill_boot <- rbindlist(list(scenario_skill_boot, scenario_skill_tmp[, wk_out := i]))
  }
}

# randomly draw each week n_samp times to generate intervals
set.seed(102)
n_samp = 1000
h <- WIS[plaus_week > 0 &
            model_name == "Ensemble_LOP", 
          .(h = (max(target_end_date) - min(target_end_date))/7 + 1), by = .(round)]
w <- list()
for(i in 1:nrow(h)){
  w[[i]] <- data.frame(samp = 1:n_samp,
                       round = h[i, "round"],
                       wk_out = sample(1:unlist(h[i, "h"]), n_samp, replace = TRUE))
}
w <- rbindlist(w)
intervals <- setDT(w)[setDT(scenario_skill_boot) %>%
                        .[, round := as.integer(round)], 
                      on= .(wk_out, round),
                      allow.cartesian = TRUE] %>%
  .[, .(Q5 = quantile(skill, 0.05),
        Q95 = quantile(skill, 0.95)),
    by = .(round,  target, scenario_id, scenario_letter)]

# combine point estimates and intervals 
scenario_skill <- scenario_skill[intervals, on = .(round, target, scenario_id, scenario_letter)]
write.csv(scenario_skill, "data-output/skill-score/skillscore_scenarios_ensboot.csv")

