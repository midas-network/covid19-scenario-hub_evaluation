#### LOAD DATA -----------------------------------------------------------------
# load WIS data
WIS <- load_scores("code/evaluation/data/WIS")
# load coverage data
cov <- load_scores("code/evaluation/data/coverage")
# load individual model skill scores
skill_byround <- setDT(read.csv("code/evaluation/data/skillscore_indmods.csv"))
# load increasing/decreasing data
inc_dec <- setDT(read.csv("code/evaluation/data/increasing_decreasing.csv")) %>%
  .[, ":=" (X = NULL,
            target_end_date = as.IDate(target_end_date))]
inc_dec_obs <- setDT(read.csv("code/evaluation/data/increasing_decreasing_obs.csv")) %>%
  .[, ":=" (X = NULL,
            target_end_date = as.IDate(target_end_date))]

# load variant takeover dates
source("code/evaluation/src/0_setup_scenario_plausibility/define_variant_takeover.R")
WIS <- SMHEvaluationUtils::add_plaus_weight(proj = WIS,
                                            variant_takeover = variant_takeover_date, 
                                            modelname_round_weight = model_exclusions,
                                            keep_flags = TRUE)
cov <- SMHEvaluationUtils::add_plaus_weight(proj = cov,
                                            variant_takeover = variant_takeover_date, 
                                            modelname_round_weight = model_exclusions,
                                            keep_flags = TRUE)

#### ABSTRACT ------------------------------------------------------------------
# number of unique projections
# exclude all ensemble but one, and all null models
proj2 <- read_data_files(data_repo_path = data_repo_path, 
                         API = FALSE,
                         truth_data = FALSE, 
                         raw_file = TRUE)
list2round <- sort(as.character(1:length(proj2)))
proj2_nrow <- lapply(proj2, function(x){setDT(x);x[substr(model_name,1,3) !="nul" &
                                                     substr(model_name,1,3) != "Ens" & 
                                                     !grepl("cum", target),
                                                   .(scenario_id, target, target_end_date, location, model_name)] %>% 
    unique() %>% 
    nrow()
})
sum(unlist(proj2_nrow)[which(!(list2round %in% c(8,10)))])
rm(proj2)
rm(proj2_nrow)


# projection horizon
scenario_info <- read.csv("code/evaluation/data/round_dates.csv")
scenario_info <- scenario_info[,-1]
scenario_info[,-1] <- apply(scenario_info[,-1], 1:2, function(i){return(ifelse(unlist(gregexpr("/", i))[1] == 2, paste0("0", i), i))})
scenario_info[,-1] <- lapply(scenario_info[,-1], as.Date, origin="1970-01-01", format = "%m/%d/%y")
setDT(scenario_info)
scenario_info <- scenario_info %>%
  # add variant exclusions
  .[, variant_cutoff_date := as.Date(ifelse(round == 1, variant_takeover_date["alpha"], 
                                            ifelse(round %in% 2:5, variant_takeover_date["delta"],
                                                   ifelse(round %in% 6:10, variant_takeover_date["omicron"], NA))), origin = "1970-01-01")] %>% 
  .[, projection_end_date_new := min(projection_end_date, variant_cutoff_date-7, na.rm = TRUE), by = .(round)] %>% #var_cutoff-7 because variant_cuttoff is excluded
  .[, variant_cutoff_date := NULL] %>%
  .[, projection_start_date_mmwr := MMWRweek2Date(
    MMWRyear = MMWRweek(projection_start_date)$MMWRyear,
    MMWRweek = MMWRweek(projection_start_date)$MMWRweek,
    MMWRday = 7
  )] %>% 
  .[, projection_end_date_mmwr := MMWRweek2Date(
    MMWRyear = MMWRweek(projection_end_date)$MMWRyear,
    MMWRweek = MMWRweek(projection_end_date)$MMWRweek,
    MMWRday = 7
  )] %>% 
  .[, projection_end_date_new_mmwr := MMWRweek2Date(
    MMWRyear = MMWRweek(projection_end_date_new)$MMWRyear,
    MMWRweek = MMWRweek(projection_end_date_new)$MMWRweek,
    MMWRday = 7
  )]
setDT(scenario_info)


scenario_info[, horizon := (projection_end_date_mmwr - projection_start_date_mmwr)/7 + 1] %>% 
  .[, horizon_trunc := (projection_end_date_new_mmwr - projection_start_date_mmwr)/7 + 1] %>%
  .[, .(round, horizon, horizon_trunc)] %>% 
  .[, .(m = mean(horizon_trunc), 
        md = median(horizon_trunc))]


# overall 95% PI coverage
cov[model_name == "Ensemble_LOP" & 
      alpha == 0.95 &
      !is.na(cov)] %>% # for later rounds without obs
  .[, .(cov95 = sum(cov*plaus_weight)/sum(plaus_weight))]

### PLAUSIBLE ------------------------------------------------------------------
# turnaround time (table_details from figure_1.R)
mean(as.integer(gsub(" days", "", table_details$turnaround_time)), na.rm = TRUE)/7
range(as.integer(gsub(" days", "", table_details$turnaround_time)), na.rm = TRUE)/7

# how many total plausible scenario-weeks
proj[model_name == "Ensemble_LOP", .(round, target_end_date)] %>% 
  unique() %>% 
  .[, .(n = .N), by = .(round)] %>% 
  pull(n) %>% sum()

# how many are excluded because of truncation
proj[model_name == "Ensemble_LOP" & plaus_week == 0, .(round, target_end_date)] %>% 
  unique() %>% 
  .[, .(n = .N), by = .(round)] %>% 
  pull(n) %>% sum()

79/400

# how many possible scenario-weeks
proj[model_name == "Ensemble_LOP" & 
       !is.na(obs), # exclude projections that do not have an observation
     .(round, scenario_id, target_end_date)] %>% 
  unique() %>% 
  .[, .(n = .N), by = .(round)] %>% 
  pull(n) %>% sum()

# how many plausible scenario-weeks
proj[model_name == "Ensemble_LOP" & 
       plaus_weight > 0  & # restrict to those that are plausible
       !is.na(obs), # exclude projections that do not have an observation
     .(round, scenario_id, target_end_date)] %>% 
  unique() %>% 
  .[, .(n = .N), by = .(round)] %>% 
  pull(n) %>% sum()

465/1484

# how many scenario-weeks with multiple scenarios in one round
proj[model_name == "Ensemble_LOP" & 
       plaus_weight > 0 & plaus_weight < 1 & # restrict to those that are duplicates
       !is.na(obs), # exclude projections that do not have an observation
     .(round, scenario_id, target_end_date)] %>% 
  unique() %>% 
  .[, .(n = .N), by = .(round)] %>% 
  pull(n) %>% sum()

(346/2)/1484


### ENSEMBLE VS. IND MODELS ----------------------------------------------------
# overall relative WIS across targets
skill_byround %>% 
  .[round == "overall" & model_name == "Ensemble_LOP"]

# best relative WIS
skill_byround %>% 
  .[round != "overall"] %>%
  .[, r := rank(skill), by = .(round, target)] %>%
  .[model_name == "Ensemble_LOP" & r == 1] %>% 
  nrow()

# top two relative WIS
skill_byround %>% 
  .[round != "overall"] %>%
  .[, r := rank(skill), by = .(round, target)] %>%
  .[model_name == "Ensemble_LOP" & r %in% 1:2] %>% 
  nrow()/42

# top three relative WIS
skill_byround %>% 
  .[round != "overall"] %>%
  .[, r := rank(skill), by = .(round, target)] %>%
  .[model_name == "Ensemble_LOP" & r %in% 1:3] %>% 
  nrow()/42

# total
nrow(skill_byround[model_name == "Ensemble_LOP" & round != "overall"])

# coverage across all locations and rounds -- ensemble
cov[model_name == "Ensemble_LOP" & 
      alpha == 0.95 &
      !is.na(cov)] %>% # for later rounds without obs
  .[, .(cov95 = sum(cov*plaus_weight)/sum(plaus_weight)), by = .(target)] %>% 
  .[, cov95 := round(cov95,2)] %>% head()

# ind mods (median & IQR)
# find teams that have national projection to include in this calc
include_projs <- cov[location == "US" & 
                       !(substr(model_name,1,3) %in% c("Ens", "nul")), 
                     .(model_name, round)] %>% unique()
include_projs <- paste(include_projs$model_name, include_projs$round, sep = "_")

cov[paste(model_name, round, sep = "_") %in% include_projs & 
      alpha == 0.95 &
      !is.na(cov)] %>%
  .[, model_name := ifelse(model_name == "MOBS_NEU-GLEAM_COVID_OT", "MOBS_NEU-GLEAM_COVID", 
                           ifelse(model_name == "USC-SIkJalpha-update", "USC-SIkJalpha", model_name))] %>% 
  .[, .(cov95 = sum(cov*plaus_weight)/sum(plaus_weight)), by = .(target, model_name)] %>% 
  .[, .(m = round(median(cov95),2), 
        l = round(quantile(cov95, 0.25),2), 
        u = round(quantile(cov95, 0.75),2)), by = .(target)] %>% head()

### ENSEMBLE VS. NULLS ---------------------------------------------------------

## naive null vs. ensemble_LOP
WIS %>%
  .[model_name %in% c("null_naive", "Ensemble_LOP") & 
      !is.na(WIS)] %>%
  .[, .(plaus_WIS = weighted.mean(WIS, plaus_weight)), by = .(target, model_name)] %>%
  .[, rel_WIS := plaus_WIS[model_name == "Ensemble_LOP"]/plaus_WIS, by = .(target)] %>%
  .[model_name != "Ensemble_LOP"] %>%
  .[, rel_WIS := round(rel_WIS,2)] %>%
  .[, rel_WIS_pct := 1-round(rel_WIS,2)] %>%
  head()


# naive null vs. ensemble_LOP - range across rounds
WIS %>%
  .[model_name %in% c("null_naive", "Ensemble_LOP") & 
      !is.na(WIS)] %>%
  .[, .(plaus_WIS = weighted.mean(WIS, plaus_weight)), by = .(target, round, model_name)] %>%
  .[, rel_WIS := plaus_WIS[model_name == "Ensemble_LOP"]/plaus_WIS, by = .(target, round)] %>%
  .[model_name != "Ensemble_LOP"] %>%
  .[, .(mn = round(min(rel_WIS),2), 
        mx = round(max(rel_WIS), 2)), 
    by = .(target)] %>%
  head()

## FH null vs. ensemble_LOP
WIS %>%
  .[model_name %in% c("null_fh", "Ensemble_LOP") & 
      !is.na(WIS)] %>%
  .[, .(plaus_WIS = weighted.mean(WIS, plaus_weight)), by = .(target, model_name)] %>%
  .[, rel_WIS := plaus_WIS[model_name == "Ensemble_LOP"]/plaus_WIS, by = .(target)] %>%
  .[model_name != "Ensemble_LOP"] %>%
  .[, rel_WIS_pct := round(rel_WIS,2)] %>%
  head()

# FH null vs. ensemble_LOP - range across rounds
WIS %>%
  .[model_name %in% c("null_fh", "Ensemble_LOP") & 
      !is.na(WIS)] %>%
  .[, .(plaus_WIS = weighted.mean(WIS, plaus_weight)), by = .(target, round, model_name)] %>%
  .[, rel_WIS := plaus_WIS[model_name == "Ensemble_LOP"]/plaus_WIS, by = .(target, round)] %>%
  .[model_name != "Ensemble_LOP"] %>%
  .[, .(mn = round(min(rel_WIS),2), 
        mx = round(max(rel_WIS), 2)), 
    by = .(target)] %>%
  head()

### SCENARIO SKILL -------------------------------------------------------------
# see figure_5.R

### INC DEC --------------------------------------------------------------------
plt <- copy(inc_dec)
plt <- plt%>%
  setnames("change_bin", "change_bin_proj", skip_absent = TRUE) %>%
  .[inc_dec_obs[, c("location", "target", "target_end_date", "change_bin")], 
    on = .(location, target, target_end_date)] %>%
  .[!is.na(change_bin) & !is.na(change_bin_proj) & !is.na(obs)] %>%
  .[, correct_flag := ifelse(change_bin_proj == change_bin, 1, 0)]

# pct correct overall
plt[!(quantile %in% c(0.75, 0.975)), 
    .(c = sum(correct_flag*plaus_weight)/sum(plaus_weight)), 
    by = .(model_name)] %>%
  .[, c := round(c, 2)] %>% head()

# precision = TP_i/(all i predicted)
plt %>%
  .[!(quantile %in% c(0.75, 0.975)) & 
      model_name == "Ensemble_LOP"] %>%
  .[, .(n_total = sum(plaus_weight, na.rm = TRUE)), by = .(change_bin_proj, change_bin)] %>%
  .[, .(prec = n_total[change_bin==change_bin_proj]/sum(n_total)), by = .(change_bin_proj)] %>% 
  .[, prec := round(prec,2)] %>% head

# recall = TP_i/(all i observed)
plt %>%
  .[!(quantile %in% c(0.75, 0.975)) & 
      model_name == "Ensemble_LOP"] %>%
  .[, .(n_total = sum(plaus_weight, na.rm = TRUE)), by = .(change_bin_proj, change_bin)] %>%
  .[, .(prec = n_total[change_bin==change_bin_proj]/sum(n_total)), by = .(change_bin)] %>% 
  .[, prec := round(prec,2)] %>% head

# percent classified as inc when dec and vice versa
plt %>% 
  .[!(quantile %in% c(0.75, 0.975)) & 
      model_name == "Ensemble_LOP"] %>%
  .[, .(n_total = sum(plaus_weight, na.rm = TRUE)), by = .(change_bin_proj, change_bin)] %>% 
  .[, n_total_pct := round(n_total/sum(n_total),2), by = .(change_bin)] %>% 
  .[paste(change_bin_proj, change_bin) %in% c("inc dec", "dec inc")] %>% head()

# precision for 95% PI
plt %>%
  .[!(quantile %in% c(0.75, 0.5)) & 
      model_name == "Ensemble_LOP"] %>%
  .[, .(n_total = sum(plaus_weight, na.rm = TRUE)), by = .(change_bin_proj, change_bin)] %>%
  .[, .(prec = n_total[change_bin==change_bin_proj]/sum(n_total)), by = .(change_bin_proj)] %>% 
  .[, prec := round(prec,2)] %>% head

# recall for 95% PI
plt %>%
  .[!(quantile %in% c(0.75, 0.5)) & 
      model_name == "Ensemble_LOP"] %>%
  .[, .(n_total = sum(plaus_weight, na.rm = TRUE)), by = .(change_bin_proj, change_bin)] %>%
  .[, .(prec = n_total[change_bin==change_bin_proj]/sum(n_total)), by = .(change_bin)] %>% 
  .[, prec := round(prec,2)] %>% head

#### METHODS -------------------------------------------------------------------
cov[is.na(cov), .(round, target_end_date)] %>% 
  unique() %>% 
  .[, .(n = .N), by = .(round)]

