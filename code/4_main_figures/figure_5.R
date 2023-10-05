library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)

#### SETUP ---------------------------------------------------------------------
# load objects to assist with plotting
source("code/plot_setup.R")

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

#### WIS BOOTSTRAP INTERVALS ---------------------------------------------------
boot <- list()
for(i in 1:52){
  print(i)
  boot[[i]] <- WIS[model_name %in% c("null_fh", "Ensemble_LOP") & plaus_week == 1] %>%
    .[, proj_week := (target_end_date - min(target_end_date))/7 + 1, by = .(round)] %>%
    .[proj_week != i] %>%
    .[, .(mean_WIS = mean(WIS, na.rm = TRUE), 
          proj_week_excl = i), by = .(target, round, scenario_id, plaus_scenario, model_name)] %>%
    .[, rel_mean_WIS := mean_WIS/mean_WIS[model_name == "null_fh"], by = .(round, target)]
}
boot <- rbindlist(boot)


n_samp <- 1000
set.seed(897)
boot_samp <- WIS[model_name %in% c("null_fh", "Ensemble_LOP") & plaus_week == 1] %>%
  .[, .(n_weeks = (max(target_end_date) - min(target_end_date))/7 + 1), by = .(round)] %>%
  .[, .(proj_week_excl = sample(1:n_weeks, n_samp, replace = TRUE), 
        samp_id = 1:n_samp), by = .(round)]

ints <- boot[boot_samp, on = .(proj_week_excl, round), allow.cartesian = TRUE] %>%
  .[!is.na(target)] %>%
  .[, .(Q5 = quantile(rel_mean_WIS,0.05), 
        mean = mean(rel_mean_WIS), 
        med = median(rel_mean_WIS),
        Q95 = quantile(rel_mean_WIS,0.95)), 
    by = .(round, scenario_id, plaus_scenario, target, model_name)]


#### PLOT AVERAGE WIS RELATIVE TO FH -------------------------------------------
p <- WIS[model_name %in% c("null_fh", "Ensemble_LOP") & plaus_week == 1] %>%
  .[, .(mean_WIS = mean(WIS, na.rm = TRUE)), by = .(target, round, scenario_id, plaus_scenario, model_name)] %>% #na.rm for projections w/out observations
  .[, rel_mean_WIS := mean_WIS/mean_WIS[model_name == "null_fh"], by = .(round, target)] %>%
  .[ints, on = .(round, scenario_id, plaus_scenario, model_name, target)] %>%
  .[, scenario_letter := substr(scenario_id,1,1)] %>%
  .[, plaus_weight_flag := ifelse(plaus_scenario > 0, 1, 0)] %>%
  .[, xpos := as.numeric(as.factor(round)) + as.numeric(as.factor(scenario_letter))/5 - 0.5 ] %>%
  .[ scenario_letter != "N" ] %>%
  .[, best := ifelse(Q5 <= min(Q95), "*", ""), by = .(round, target)] %>%
  .[, target := factor(target, levels = paste("inc", c("case", "hosp", "death")))]

p_allprojs <- WIS[model_name %in% c("null_fh", "Ensemble_LOP")] %>%
  .[, .(mean_WIS = mean(WIS, na.rm = TRUE)), by = .(target, round, scenario_id, plaus_scenario, model_name)] %>%
  .[, rel_mean_WIS := mean_WIS/mean_WIS[model_name == "null_fh"], by = .(round, target)] %>%
  .[ints, on = .(round, scenario_id, plaus_scenario, model_name, target)] %>%
  .[, scenario_letter := substr(scenario_id,1,1)] %>%
  .[, plaus_weight_flag := ifelse(plaus_scenario > 0, 1, 0)] %>%
  .[, xpos := as.numeric(as.factor(round)) + as.numeric(as.factor(scenario_letter))/5 - 0.5 ] %>%
  .[ scenario_letter != "N"] %>%
  .[, best := ifelse(Q5 <= min(Q95), "*", ""), by = .(round, target)] %>%
  .[, target := factor(target, levels = paste("inc", c("case", "hosp", "death")))]


ggplot(data = p, 
       aes(x = xpos, y = rel_mean_WIS, color = as.factor(plaus_weight_flag))) + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(data = p_allprojs, 
             color = "grey", shape = 21) +
  geom_linerange(aes(ymin = Q5, ymax = Q95)) +
  geom_point(size = 1.2) +
  geom_text(aes(label = scenario_letter), color = "white", size = 0.9) +
  geom_text(aes(label = best), hjust = 0, vjust = 0.2, size = 3) +
  geom_text(data = WIS[model_name == "Ensemble_LOP" & plaus_week == 1 & !is.na(WIS)] %>%
              .[, .(n_weeks = paste(length(unique(target_end_date)), "weeks\n")), by = .(round)] %>%
              .[, target := factor("inc death")], 
            aes(x = as.numeric(as.factor(round)), label = n_weeks), y = -Inf, color = "black", size = 1.5, vjust = 0) +
  facet_grid(rows = vars(target), 
             labeller = labeller(target = target_labs), 
             switch = "y") +
  labs(x = "SMH round", y = "WIS relative to 4-week forecast model") +
  scale_color_manual(values = c("black", "darkorange")) +
  scale_x_continuous(breaks = 1:14,
                     expand = c(0,0),
                     labels = round_labs[1:14],
                     limits = c(0.5,14.5)) +
  scale_y_log10() +
  theme_bw(base_size = 7) +
  theme(axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        strip.background = element_blank(), 
        strip.placement = "outside")
ggsave("figures/main_figures/figure5.pdf", width = 5.75, height = 3.75)


## values for text
# how often is most plausible scenario best
p[best == "*" & plaus_weight_flag == 1, .(target, round)] %>% 
    unique() %>% 
    nrow()
# total combinations
p[, .(target, round)] %>% 
     unique() %>% 
     nrow()

# truncated vs. not truncated
p[,.(target, round, scenario_id, rel_mean_WIS)] %>%
  setnames("rel_mean_WIS", "rel_mean_WIS_trunc") %>%
  .[p_allprojs[,.(target, round, scenario_id, rel_mean_WIS)] %>%
      setnames("rel_mean_WIS", "rel_mean_WIS_all"),
    on = .(target, round, scenario_id)] %>%
  .[, ":=" (rel_mean_WIS_all = round(rel_mean_WIS_all,2),
            rel_mean_WIS_trunc = round(rel_mean_WIS_trunc,2))] %>%
  .[, f:= ifelse(rel_mean_WIS_all == rel_mean_WIS_trunc, "equal", ifelse(
    rel_mean_WIS_all > rel_mean_WIS_trunc, "trunc lower", "trunc higher"))] %>% 
  # data.table::dcast(round ~ f) # to check results by round
  # exclude truncated rounds
  .[!(round %in% c(6, 11:16))] %>% 
  .[, .(n = .N), by = "f"]
