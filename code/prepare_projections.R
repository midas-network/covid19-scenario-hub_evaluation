# THIS SCRIPT LOADS AND FORMATS DATA FOR EVALUATION, IMPLEMENTS EXCLUSIONS 

library(data.table)
library(dplyr)
library(stringr)
library(SMHEvaluationUtils)

#### LOAD TRUTH DATA -----------------------------------------------------------
# input function to read data
source("code/read_data_files.R")
# set path of data repo
data_repo_path <- "data-raw/"

# gold standard data
truth_data <- read_data_files(data_repo_path = data_repo_path, API = FALSE)
names(truth_data) <- c("cum case", "inc case", "cum death", "inc death", "inc hosp")
truth_data <- rbindlist(truth_data, idcol="target") %>%
  setnames(old = c("value", "time_value", "geo_value_fullname", "fips"), 
           new = c("obs", "target_end_date", "location_name", "location"))
# limit truth data to before JHU CSSE stops producing data: March 04, 2023
truth_data <- truth_data[target_end_date <= as.Date("2023-03-04")]


#### LOAD PROJECTION INFO ------------------------------------------------------
# scenario info by round
scenario_info <- read.csv(file.path(paste0(data_repo_path, 
                                           "/data-scenarios/scenario_round_info.csv"))) %>%
  select(-date_round) %>%
  unique()
setDT(scenario_info)[,round:=sub("round","",round)]

# projection period by round 
proj_period <- read.csv(file.path(paste0(data_repo_path, 
                                         "/data-scenarios/all_dates_by_round.csv"))) %>%
  setDT() %>%
  .[, ":=" (target_end_date = as.IDate(target_end_date, format = "%m/%d/%y"))] %>%
  .[proj_period_flag == 1] 

#### LOAD SMH PROJECTIONS ------------------------------------------------------

# SMH projections
proj <- read_data_files(data_repo_path = data_repo_path, 
                        API = FALSE,
                        truth_data = FALSE, 
                        raw_file = TRUE)
list2round <- sort(as.character(1:length(proj)))

proj <- SMHEvaluationUtils::compile_SMH_projections(proj, 
                             list2round, 
                             proj_period, 
                             scenario_info, 
                             rounds_to_include = c(1:7,9,11:16),
                             summarize_exclusions = file.path(paste0(data_repo_path,"../data-output/exclusions")))

# null projections (generated using code in `/code/1_generate_null_projections` )
proj_null <- SMHEvaluationUtils::load_all_null_projs(p = "data-output/projections-null/")

#  merge proj and nulls
proj <- rbindlist(list(proj, proj_null[, ":=" (location_name = NULL)]), use.names = TRUE)
rm(proj_null)

#### ADD TRUTH DATA ------------------------------------------------------------

# Merge the model data and truth data
proj <- merge(proj, 
              truth_data[substr(target,1,3) == "inc"], 
              all.x = TRUE, 
              on = .(target_end_date, location, target))

# remove projections that have negative observations
proj <- proj %>% 
  .[, r := ifelse(is.na(obs), 0, ifelse(obs < 0, 1, 0))] %>%
  # keep NA observations for now, becasue these are projections we don't have observations for
  .[, r := ifelse(is.na(obs), 0, r)] %>% 
  # but there are some rows with NA target, model_name etc. remove those
  .[ r == 0 & !is.na(target)] %>% 
  .[,  r:= NULL]
  
#### ADD LOCATION INFO ---------------------------------------------------------
# location information
locations <- read.csv(file.path(paste0(data_repo_path, "data-locations/locations.csv"))) %>%
  setDT() %>%
  .[!(location %in% c("60", "66", "69", "72", "74", "78"))]
proj <- proj %>% 
  .[, location_name := NULL] %>%
  .[locations, on = .(location)]

#### ADD PLAUS WEIGHT ----------------------------------------------------------
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
proj <- SMHEvaluationUtils::add_plaus_weight(proj = proj,
                                     variant_takeover = variant_takeover_date, 
                                     modelname_round_weight = model_exclusions,
                                     keep_flags = TRUE, 
                                     p = "data-raw/data-scenarios/MostPlausibleScenarios.csv")


