library(dplyr)
library(data.table)
library(knitr)
library(cowplot)

#### SETUP ---------------------------------------------------------------------
# set start date for data
start_date <- "2020-12-19"
end_date <- "2023-04-20"
excluded_loc <- c("60", "66", "69", "72", "78")

# load projections
#source("code/prepare_projections.R")


#### SETUP TRUTH DATA ----------------------------------------------------------
# fix truth_dat
truth_dat_incdec <- truth_data %>%
  # only calculate for incident outcomes
  .[substr(target, 1,3) == "inc" &
      # exclude territories
      !(location %in% excluded_loc) &
      # filter to only projected dates
      target_end_date >= as.Date(start_date) & 
      target_end_date <= as.Date(end_date)]

#### SETUP PROJECTION DATA -----------------------------------------------------
## add observations for calculating first two weeks for Ensemble_LOP
proj_addobs <- proj %>%
  # only calculate for incident outcomes
  .[substr(target, 1,3) == "inc" &
      quantile %in% c(0.5, 0.75, 0.975) &
      model_name %in% c("Ensemble_LOP", "null_fh")] %>%
  .[, obs := value] %>%
  .[, .(location, location_name, target, model_name, scenario_id, scenario_name, 
        plaus_weight, round, quantile, obs, target_end_date)]

# create new rows with two and one week prior
proj_first_week <- proj[model_name == "Ensemble_LOP" & 
                          quantile %in% c(0.5, 0.75, 0.975), 
                        .(target_end_date = min(target_end_date)), 
                        by = .(round, scenario_id, scenario_name, quantile)] 
proj_first_week <- rbindlist(list(
  proj_first_week %>%
    mutate(target_end_date = target_end_date - 7), 
  proj_first_week %>%
    mutate(target_end_date = target_end_date - 14))) %>%
  setDT()
# pull observations for these dates
truth_to_add <- truth_dat_incdec[, .(target, target_end_date, location, location_name, obs)] %>%
  .[proj_first_week, on = .(target_end_date), allow.cartesian = TRUE]

# do the same for FH data
proj_fh_prior_weeks <- load_null_fh(pp_flag = FALSE, 
                                    p = "data-output/projections-null/") %>%
  .[proj_period_flag == 0 & 
      quantile %in% c(0.5, 0.75, 0.975)] %>%
  .[, obs := value] %>%
  .[, value := NULL]

# add on to proj df
proj_addobs <- rbindlist(list(
  proj_addobs, 
  truth_to_add[, ":=" (model_name = "Ensemble_LOP", 
                       plaus_weight = 0)], 
  proj_fh_prior_weeks[, ":=" (plaus_weight = 0)] %>%
    .[,.(round, target, target_end_date, location_name, location, obs, 
         model_name, scenario_id, scenario_name, quantile, plaus_weight)]), 
  use.names = TRUE
)

