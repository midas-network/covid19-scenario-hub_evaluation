library(dplyr)
library(data.table)
library(knitr)
library(cowplot)

# load truth data and projections
source("code/2_classify_trends/setup_incdec_analysis.R")

#### IMPLEMENT CHOSEN CASE -----------------------------------------------------
lag = 2
pct_flat = 1/3

# classify each observation as increasing/decreasing 
pct_change_obs <- SMHEvaluationUtils::calculate_pct_change(
  dat = truth_dat_incdec, 
  lag = lag, 
  byvars = c("target", "location"))

# classify based on quantiles and pct_flat
inc_dec_obs <- SMHEvaluationUtils::full_classif(truth_dat_incdec, 
                            lag = lag, pct_flat = pct_flat, byvars = c("target", "location"))

#### APPLY TO PROJECTIONS ------------------------------------------------------
## calculate inc/dec on projections
# get cutoff thresholds
thresh_obs <- SMHEvaluationUtils::calc_thresh(pct_change_obs, pct_flat)
# classify projections using threshold
inc_dec_proj <- SMHEvaluationUtils::full_classif(dat = proj_addobs, 
                                                 lag = lag, 
                                                 pct_flat = pct_flat,
                                                 byvars = c("location", "target", "model_name", "scenario_id", "round", "quantile"), 
                                                 calc_thresh_flag = FALSE, 
                                                 thresh = thresh_obs)

# add another null: continuation of current trend
alt_null <- inc_dec_obs[, .(target_end_date, change_bin, location, target, obs)] %>%
  .[proj[, .(target_end_date = as.Date(min(target_end_date) - 7)), by = .(round)], 
    on = .(target_end_date)] %>%
  .[, target_end_date := NULL] %>% 
  .[proj[, .(target_end_date, round, location, target)] %>% unique(), on = .(round, location, target)]

# if 1 week back is NA, then look 2 weeks back
alt_null2 <- inc_dec_obs[, .(target_end_date, change_bin, location, target, obs)] %>%
  .[proj[, .(target_end_date = as.Date(min(target_end_date) - 14)), by = .(round)], 
    on = .(target_end_date)] %>%
  .[, target_end_date := NULL] %>% 
  .[proj[, .(target_end_date, round, location, target)] %>% unique(), on = .(round, location, target)]

alt_null <- alt_null[alt_null2 %>% 
                       setnames(c("change_bin", "obs"), 
                                c("change_bin2", "obs2"), 
                                skip_absent = TRUE), 
                     on = .(location, target, round, target_end_date)] %>%
  .[, ":=" (change_bin = ifelse(is.na(change_bin), change_bin2, change_bin), 
             change_bin2 = NULL, 
             obs2 = NULL)]


# bind rows
inc_dec <- rbindlist(list(inc_dec_proj[, target_end_date := as.IDate(target_end_date)] %>%
                            .[, .(round, location, target, model_name, obs, scenario_id, 
                                  target_end_date, quantile, change_bin)], 
                          alt_null[, ":=" (model_name = "null_trend", 
                                           target_end_date = as.IDate(target_end_date), 
                                           quantile = 0.5,
                                           scenario_id = "NULL-MODEL")]), 
                     fill= TRUE, use.names = TRUE) %>%
  .[!is.na(change_bin)]



# add plausibility 
inc_dec <- add_plaus_weight(proj = inc_dec, 
                            p = "data-raw/data-scenarios/MostPlausibleScenarios.csv",
                            variant_takeover = variant_takeover_date, 
                            modelname_round_weight = model_exclusions)

# save
write.csv(inc_dec, "data-output/increase-decrease-classification/increasing_decreasing.csv")

# save obs separately
write.csv(inc_dec_obs[!is.na(change_bin)], "data-output/increase-decrease-classification/increasing_decreasing_obs.csv")

#### some checks ####
# should be the same across models for each round
inc_dec[, .(mn = min(target_end_date)), 
        by = .(round, model_name)] %>%
  dcast(model_name ~ round, value.var = "mn") %>%
  View()

# should be the same across models for each round
inc_dec[, .(mx = max(target_end_date)), 
        by = .(round, model_name)] %>%
  dcast(model_name ~ round, value.var = "mx") %>%
  View()

# should have the same number of projections for all models in each round
# (a few exceptions for data anomalies)
inc_dec[, .(n = .N), by = .(round, model_name, quantile)] %>% 
  .[, n := ifelse(model_name == "Ensemble_LOP", n/4, n)] %>%
  dcast(model_name + quantile  ~ round, value.var = "n")




## values for text
# threshold values
thresh_obs

# pct excluded from distribution
(length(which(pct_change_proj$pct_change > 2.5)) + length(which(pct_change_proj$pct_change < -2.5)))/length(pct_change_proj$pct_change)
range(pct_change_proj$pct_change)


