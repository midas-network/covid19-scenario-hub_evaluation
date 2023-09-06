library(uscovid19vacc) # JHU-IDD's package, using Youyang Gu's data up to 2021-03-01
library(data.table)
library(lubridate)
library(ggplot2)
library(scales)
library(cowplot)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(dplyr)
library(ggtext)


#### LOAD OBSERVED VACCINATION DATA --------------------------------------------
# data pulled from CDC vaccination database
# https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc
cdc_path <- "https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD"

# abbreviations and populations of 50 states + DC + US
loc <- fread("data-raw/data-locations/locations.csv")
loc <- loc[abbreviation %in% c(state.abb, "DC", "US")] # remove PR, territories
non_terr_fips <- loc$location

# CDC vaccination database
vacc_raw <- fread(cdc_path) %>% 
  setDT() %>%
  .[, Date := as.Date(Date, format = "%m/%d/%Y")]

vacc_US <- vacc_raw[Location == "US", .(date = Date, distrib = Distributed, admin_dose1 = Administered_Dose1_Recip,
                                        jj = Administered_Janssen, dose1_12p_pct = Administered_Dose1_Recip_12PlusPop_Pct)]

# Replace CDC's "administered dose 1" data with YYG's for 2021-02-28 and earlier
data("vacc_us_states_yyg", package = "uscovid19vacc")
vacc_us_states_yyg <- as.data.table(vacc_us_states_yyg)
yyg_vacc_US <- vacc_us_states_yyg[Date <= as.Date("2021-02-28") & Location == "US", .(date = Date, admin_dose1 = administered_dose1_adj)]
yyg_vacc_US <- yyg_vacc_US[order(date)] # match order of dates in vacc_US and yyg_vacc_US
vacc_US <- vacc_US[order(date)]
vacc_US[date >= min(as.Date(yyg_vacc_US$date)) & date <= as.Date("2021-02-28"), admin_dose1 := yyg_vacc_US$admin_dose1]

# Calculate "Moderna + Pfizer 1st doses" using "1st doses" - "J&J"
vacc_US[, mp_dose1 := admin_dose1 - jj, by = .(date)]

# Also, remove weird discontinuities in CDC's dose1_12p_pct data
vacc_US[dose1_12p_pct <= 0, dose1_12p_pct := NA]

# Used for Rounds 3 and 4 coverage saturation; source: https://www.census.gov/popclock
US_pop <- 332 * 10^6

# define variant takeover dates
source("code/0_setup_scenario_plausibility/define_variant_takeover.R")

# load annual vaccination data for R14 and later
R14_scenarioCD_vacc <- read.csv("data-raw/data-scenarios/R14_vaccination_scenario_C_D.csv") %>%
  setDT %>%
  .[location_name == "United States"] %>% 
  .[, Age := case_when(Age == "18-49 Years" ~ "y_18_49",
                       Age == "50-64 Years" ~ "y_50_64",
                       Age == ">65 Years" ~ "y_65p")]

#### DEFINE SCENARIO PROJECTIONS -----------------------------------------------
# SMH rounds 1-7 info
proj_start_dates <- as.Date(c("2021-01-09", "2021-01-30", "2021-03-13", "2021-04-03", "2021-05-08", "2021-06-05", "2021-07-10")) # rounds 1-7
proj_end_dates <- proj_start_dates + 25*7

# create data.frame with all rounds/dates
proj_US <- setDT(expand.grid(round = 1:4, 
                       date = seq(proj_start_dates[1], proj_end_dates[4], 1)))
# add start/end date for each round
proj_US <- proj_US[, ":=" (start_date = proj_start_dates[round], 
                           end_date = proj_end_dates[round])] %>%
  .[date >= start_date & date <= end_date]
# add columns for optimistic, intermediate, pessimistic, and truth 
proj_US <- proj_US[, ":=" (opt = 0, 
                           int = 0, 
                           pess = 0, 
                           truth = 0, 
                           vacc_type = "tot")]
# set up to have different moderna/pfizer and J&J for round 4
proj_US[round == 4, "vacc_type"] <- "mp"
proj_US <- rbindlist(list(proj_US, 
                     proj_US[round == 4] %>%
                       .[, vacc_type := "jj"]))

## ASSUMPTIONS: 
# 1. per month = per 30 days
# 2. priority group = age 65+
# 3. for scenarios that define uptake until a certain coverage (e.g., no more 
#    than 75% of any population group receives the vaccine - R4), assume this 
#    is distributed evenly in the population (i.e., vaccination will not exceed
#    US_pop*0.75)

## ROUND 1 
# dose type: distributed
# optimistic: actually distributed doses in Dec + 50 million doses/month
# intermediate: actually distributed doses in Dec + 
#               25 million doses/month in January, 50 million doses/month afterward
# pessimistic: no vaccination (0 doses)
starting_date <- proj_start_dates[1]
starting_dose <- vacc_US[date == starting_date, distrib]
proj_US[round == 1] <- proj_US[round == 1] %>%
  .[, ":=" (opt = starting_dose + 50E6/30 * as.integer(date - starting_date), 
            int =  starting_dose + ifelse(date < "2021-02-01", 25E6/30 * as.integer(date - starting_date),
                     25E6/30*as.integer(as.Date("2021-01-31") - starting_date) +  50E6/30 * as.integer(date - as.Date("2021-01-31"))), 
            pess =  0/30 * as.integer(date - starting_date), 
            truth = unlist(vacc_US[date %between% c(proj_start_dates[1], proj_end_dates[1]), .(distrib)]))]

## ROUND 2 
# dose type:   administered first doses (admin_dose1)
#              assuming all scenarios are specifying administered because of 
#              team's implemented it this way (according to Friday discussion notes)
# optimistic:  observed administration rate persists for January, 50 million doses administered/month after
# pessimistic: observed administration rate persists until 50% coverage in any priority group (assume 50+)
starting_date <- proj_start_dates[2]
starting_dose <- vacc_US[date == starting_date, admin_dose1]
# find administration rate in january
jan_rate <- (starting_dose - vacc_US[date == as.Date("2021-01-01"), admin_dose1])/as.integer(starting_date - as.Date("2021-01-01"))
# find when 50+ population reaches 50% prevalence
# note: we do not have age-specific vaccination data this early in pandemic, 
#       so assume all doses go to 50+ only
pop_50p <- 109E6
pess_50p_nosaturation <- (starting_dose + (jan_rate*as.integer(proj_US[round == 2, date] - starting_date)))/pop_50p
max_cov_pess <- as.Date(min(which(pess_50p_nosaturation > 0.5)) + starting_date)
# add in scenario vaccination
proj_US[round == 2] <- proj_US[round == 2] %>%
  .[, ":=" (opt = starting_dose + ifelse(date <= as.Date("2021-01-31"), jan_rate * as.integer(date - starting_date), 
                                         jan_rate*as.integer(as.Date("2021-01-31") - starting_date) + 50E6/30*as.integer(date - as.Date("2021-01-31"))), 
            int = NA,
            pess =  starting_dose + ifelse(date < max_cov_pess, 
                                           jan_rate*as.integer(date - starting_date), 
                                           jan_rate*as.integer(max_cov_pess - starting_date)),
            truth = unlist(vacc_US[date %between% c(proj_start_dates[2], proj_end_dates[2]), .(admin_dose1)]))]

## ROUND 3
# dose type: administered first doses (admin_dose1)
# optimistic: actually administered doses in Dec-Feb, 35 million doses/month
#             until 90% coverage is reached in the population
# pessimistic: actually administered doses in Dec + 
#               20 million doses/month until 50% coverage is reached in the population
starting_date <- proj_start_dates[3]
starting_dose <- vacc_US[date == starting_date, admin_dose1]
max_cov_opt <- US_pop*0.9
max_cov_pess <- US_pop*0.5
proj_US[round == 3] <- proj_US[round == 3] %>%
  .[, ":=" (opt = pmin(starting_dose + 35E6/30*as.integer(date - starting_date), max_cov_opt), 
            int = NA,
            pess =  pmin(starting_dose + 20E6/30*as.integer(date - starting_date), max_cov_pess), 
            truth = unlist(vacc_US[date %between% c(proj_start_dates[3], proj_end_dates[3]), .(admin_dose1)]))]

## ROUND 4
# dose type: administered first doses (admin_dose1)
# optimistic: actually administered doses in Dec-Mar, 50 million doses/month(Moderna/Pfizer)
#             actually administered doses in March, 10 million doses/month in April, 
#             15 million in May, 20 million June - September (J&J)
#             vaccination proceeds until 90% coverage is reached in the population
# pessimistic: actually administered doses in Dec-Mar, 45 million doses/month(Moderna/Pfizer)
#             actually administered doses in March, 5 million doses/month in April - September (J&J)
#             vaccination proceeds until 75% coverage is reached in the population
starting_date <- proj_start_dates[4]
starting_dose <- vacc_US[date == starting_date, mp_dose1]
starting_dose_jj <- vacc_US[date == starting_date, jj]
max_cov_opt <- US_pop*0.9
max_cov_pess <- US_pop*0.75
proj_US[round == 4 & vacc_type == "mp"] <- proj_US[round == 4 & vacc_type == "mp"] %>%
  .[, ":=" (opt = pmin(starting_dose + 50E6/30*as.integer(date - starting_date), max_cov_opt), 
            int = NA,
            pess =  pmin(starting_dose + 45E6/30*as.integer(date - starting_date), max_cov_pess), 
            truth = unlist(vacc_US[date %between% c(proj_start_dates[4], proj_end_dates[4]), .(mp_dose1)]))]
# J&J
proj_US[round == 4 & vacc_type == "jj"] <- proj_US[round == 4 & vacc_type == "jj"] %>%
  .[, ":=" (opt = starting_dose_jj + ifelse(date <= as.Date("2021-04-30"), 10E6/30*as.integer(date - starting_date), 
                                            ifelse(date <= as.Date("2021-05-31"), 
                                                   10E6/30*as.integer(as.Date("2021-04-30") - starting_date) + 
                                                     15E6/30*as.integer(date-as.Date("2021-04-30")), 
                                                   10E6/30*as.integer(as.Date("2021-04-30") - starting_date) + 
                                                     15E6/30*as.integer(as.Date("2021-05-31")-as.Date("2021-04-30")) + 
                                                     20E6/30*as.integer(date - as.Date("2021-05-31")))), 
            int = NA,
            pess =  starting_dose_jj + 5E6/30*as.integer(date - starting_date), 
            truth = unlist(vacc_US[date %between% c(proj_start_dates[4], proj_end_dates[4]), .(jj)]))]

# plot to check
# proj_US[round == 4] %>% 
#   melt(c("date", "round", "start_date", "end_date", "vacc_type")) %>% 
#   ggplot(aes(x = date, y = value, color = variable, group = interaction(variable, vacc_type))) + 
#   geom_line()

## ROUND 9
# dose type:    administered primary series in 5-11 year olds
# child_vax:    vaccination is approved in 5-11 year olds, coverage mirrors 
#               uptake in 12-17 year olds
# no_child_vax: vaccination is not approved in 5-11 year olds
approved_12to17 = as.Date("2021-05-13")
approved_5to11 = as.Date("2021-11-01")
ndays = as.integer(approved_5to11-approved_12to17)

# there is ~4M vaccinations of 12-17 yr olds at the start of the projection period, 
# find out how many (and add what happens if we take them out)
d = vacc_raw[Location == "US" & Date == "2021-05-13", .(Administered_Dose1_Recip_12Plus, 
                                                        Administered_Dose1_Recip_18Plus, 
                                                        Series_Complete_12Plus, 
                                                        Series_Complete_18Plus)] %>%
  .[, .(admin_diff = Administered_Dose1_Recip_12Plus - Administered_Dose1_Recip_18Plus, 
        series_diff = Series_Complete_12Plus - Series_Complete_18Plus)]

R9 <- vacc_raw %>% 
  # filter to only the weeks we need
  .[Date >= as.Date("2021-05-13") & Location == "US"] %>%
  # first find population size of each group
  .[, ":=" (pop_5plus = Administered_Dose1_Recip_5Plus/(Administered_Dose1_Recip_5PlusPop_Pct/100), 
            pop_12plus = Administered_Dose1_Recip_12Plus/(Administered_Dose1_Recip_12PlusPop_Pct/100), 
            pop_18plus = Administered_Dose1_Recip_18Plus/(Administered_Dose1_Recip_18PlusPop_Pct/100)
  )] %>%
  # average away slight fluctuations in population size
  .[, ":=" (pop_5to11_only = mean(pop_5plus - pop_12plus, na.rm = TRUE), 
            pop_12to17_only = mean(pop_12plus - pop_18plus, na.rm = TRUE)), by = .(Location)] %>%
  # calculate administration percent
  .[, ":=" (admin_5to11_only = (Administered_Dose1_Recip_5Plus - Administered_Dose1_Recip_12Plus)/pop_5to11_only, 
            admin_12to17_only = (Administered_Dose1_Recip_12Plus - Administered_Dose1_Recip_18Plus)/pop_12to17_only, 
            series_5to11_only = (Series_Complete_5Plus - Series_Complete_12Plus)/pop_5to11_only, 
            series_12to17_only = (Series_Complete_12Plus - Series_Complete_18Plus)/pop_12to17_only, 
            admin_12to17_only_reduce = (Administered_Dose1_Recip_12Plus - Administered_Dose1_Recip_18Plus - d$admin_diff)/pop_12to17_only, 
            series_12to17_only_reduce = (Series_Complete_12Plus - Series_Complete_18Plus - d$series_diff)/pop_12to17_only)] %>%
  .[, .(Date, Location, admin_5to11_only, admin_12to17_only, series_5to11_only, series_12to17_only, pop_5to11_only, pop_12to17_only, 
        admin_12to17_only_reduce, series_12to17_only_reduce)] %>%
  data.table::melt(c("Date", "Location", "pop_5to11_only", "pop_12to17_only")) %>%
  .[, ":=" (vacc_type = ifelse(substr(variable,1,3) == "adm", "admin_dose1", "series_complete"), 
            age = ifelse(grepl("5to11", variable), "5-11", "12-17"), 
            reduced = ifelse(grepl("reduce", variable), TRUE, FALSE))]
  
  
R9_all <- rbindlist(list(
  R9 %>% 
    .[, scenario := ifelse(grepl("5to11", variable), "truth", ifelse(grepl("reduce", variable), "child_vax_reduce", "child_vax"))] %>% 
    .[, Date := as.Date(ifelse(grepl("12to17",variable), Date + ndays, Date), origin = "1970-01-01")] %>% 
    .[Date <= as.Date("2022-03-12") & Date >= approved_5to11] %>% # restrict to proj period
    .[, .(Date, vacc_type, scenario, value)], 
  R9 %>% 
    .[, .(Date, vacc_type)] %>% 
    unique() %>% 
    .[Date <= as.Date("2022-03-12") & Date >= approved_5to11] %>% # restrict to proj period
    .[, ":=" (scenario = "no_child_vax", 
              value = 0)]), 
  use.names = TRUE)

## ROUND 14 
# dose type:      administered bivalent doses
# age-restricted: bivalent booster for adults 50+ and chronic conditions only; uptake
#                 is 15% lower than uptake of booster 1
# all-ages:       bivalent booster for all adults 18+; uptake is 10% lower than 
#                 2021-2022 flu coverage (see csv file)
R14_CD <- R14_scenarioCD_vacc %>%
  .[, n_doses := Population*Boost.coverage.rd14.sc_C_D/100] %>%
  data.table::dcast(location_name + Week_Ending_Sat ~ Age, value.var = "n_doses") %>%
  .[, ":=" (y_18p = y_18_49 + y_50_64 + y_65p)] %>% 
  .[location_name == "United States"] %>% 
  .[, value := as.integer(y_18p*0.9)] # 10% lower
boost1_approved = as.Date("2021-09-22")
bival_scenarioapproved = as.Date("2022-10-01")
ndays = as.integer(bival_scenarioapproved-boost1_approved)
R14_AB <- vacc_raw[Location == "US",
                   .(Date, Additional_Doses_50Plus)] %>%
  .[, Date := as.Date(Date, format = "%m/%d/%Y")] %>%
  .[, Date := Date + ndays] %>% 
  .[Date <= as.Date("2023-06-03") & Date >= as.Date("2022-06-11")] %>% # restrict to proj period
  .[, value := Additional_Doses_50Plus*0.85] #15% lower
R14_truth <- vacc_raw[Location == "US",
                      .(Date, Bivalent_Booster_18Plus)] %>%
  .[, Date := as.Date(Date, format = "%m/%d/%Y")] %>%
  .[Date <= as.Date("2023-06-03") & Date >= as.Date("2022-06-11")] # restrict to proj period
R14_all <- rbindlist(list(
  R14_AB[, .(Date, value)] %>%
    .[, scenario := "restricted-ages"],  
  R14_truth %>% 
    setnames("Bivalent_Booster_18Plus", "value", skip_absent = TRUE) %>% 
    .[, scenario := "truth"], 
  R14_CD[, .(Week_Ending_Sat, value)] %>% 
    setnames("Week_Ending_Sat", "Date", skip_absent = TRUE) %>% 
    .[, Date := as.Date(Date)] %>% 
    .[, scenario := "all-ages"]
))
  
## ROUND 15: 
# dose type:  administered bivalent
# early:      bivalent booster for all adults 18+; uptake is 10% lower than 
#             2021-2022 flu coverage (see csv file) beginning on Sep 11, 2022
# late:       bivalent booster for all adults 18+; uptake is 10% lower than 
#             2021-2022 flu coverage (see csv file) beginning on Nov 13, 2022
R14_approved = as.Date("2022-10-01")
R15_AB_approved = as.Date("2022-09-11")
R15_CD_approved = as.Date("2022-11-13")
R15_AB <- R14_scenarioCD_vacc %>%
  .[, n_doses := Population*Boost.coverage.rd14.sc_C_D/100] %>%
  data.table::dcast(location_name + Week_Ending_Sat ~ Age, value.var = "n_doses") %>%
  .[, ":=" (y_18p = y_18_49 + y_50_64 + y_65p)] %>% 
  # shift approval date based on scenario
  .[, Week_Ending_Sat := as.Date(Week_Ending_Sat) - (R14_approved-R15_AB_approved)] %>%
  .[location_name == "United States"] %>% 
  .[, value := as.integer(y_18p*0.9)]
R15_CD <- R14_scenarioCD_vacc %>%
  .[, n_doses := Population*Boost.coverage.rd14.sc_C_D/100] %>%
  data.table::dcast(location_name + Week_Ending_Sat ~ Age, value.var = "n_doses") %>%
  .[, ":=" (y_18p = y_18_49 + y_50_64 + y_65p)] %>% 
  # shift approval date based on scenario
  .[, Week_Ending_Sat := as.Date(Week_Ending_Sat) - (R14_approved-R15_CD_approved)] %>%
  .[location_name == "United States"] %>% 
  .[, value := as.integer(y_18p*0.9)]
R15_all <- rbindlist(list(
  R15_AB[, .(Week_Ending_Sat, value)] %>%
    setnames("Week_Ending_Sat", "Date", skip_absent = TRUE) %>% 
    .[, scenario := "early"],  
  # use same truth as R14
  R14_truth %>% 
    setnames("Bivalent_Booster_18Plus", "value", skip_absent = TRUE) %>% 
    .[, scenario := "truth"], 
  R15_CD[, .(Week_Ending_Sat, value)] %>% 
    setnames("Week_Ending_Sat", "Date", skip_absent = TRUE) %>% 
    .[, Date := as.Date(Date)] %>% 
    .[, scenario := "late"]
))


## ROUND 16: 
# dose type:  administered bivalent
# high:       90% of flu vaccination levels by Feb 1, 2022
# low:        booster 1 levels by April 29, 2023 
#             (assume booster 1 levels are from the first 6 months of booster 1)
R16_AB <- R14_scenarioCD_vacc %>%
  .[, n_doses := Population*Boost.coverage.rd14.sc_C_D/100] %>%
  data.table::dcast(location_name + Week_Ending_Sat ~ Age, value.var = "n_doses") %>%
  .[, ":=" (y_18p = y_18_49 + y_50_64 + y_65p)] %>% 
  .[y_18p == max(y_18p)] %>% 
  .[, Date := as.Date("2023-02-01")] # set date to end of projection period following scenario
R16_CD <- vacc_raw[Location == "US",
                   .(Date, Additional_Doses_18Plus)] %>%
  .[, Date := as.Date(Date, format = "%m/%d/%Y")] %>%
  .[Date == boost1_approved + 182] %>% # six months after booster 1 approved
  .[, Date := as.Date("2023-04-29")]   # set date to end of projection period following scenario
R16_all <- rbindlist(list(
  R16_AB[, .(Date, y_18p)] %>%
    setnames("y_18p", "value", skip_absent = TRUE) %>% 
    .[, scenario := "high"],  
  # use same truth as R14
  R14_truth %>% 
    setnames("Bivalent_Booster_18Plus", "value", skip_absent = TRUE) %>% 
    .[, scenario := "truth"], 
  R16_CD[, .(Date, Additional_Doses_18Plus)] %>%
    setnames("Additional_Doses_18Plus", "value", skip_absent = TRUE) %>% 
    .[, scenario := "low"]
))



## cumulative coverage rounds

## ROUND 5: 
# dose type: cumulative coverage
# optimistic: national saturation at 83% of vaccine-eligible population by October 31, 2021
# pessimistic: national saturation at 68% of vaccine-eligible population by October 31, 2021

## ROUND 6: 
# dose type: cumulative coverage
# optimistic: national saturation at 86% of vaccine-eligible population by November 30, 2021
# pessimistic: national saturation at 75% of vaccine-eligible population by November 30, 2021

## ROUND 7: 
# dose type: cumulative coverage
# optimistic: national saturation at 80% of vaccine-eligible population by December 31, 2021
# pessimistic: national saturation at 70% of vaccine-eligible population by December 31, 2021

proj_US_cumcov <- data.frame(round = 5:7, 
                             opt = c(0.83, 0.86, 0.80), 
                             pess = c(0.68, 0.75, 0.70))
proj_US_cumcov$truth_full <- unlist(vacc_US[date %in% as.Date(c("2021-10-31", "2021-11-30", "2021-12-31")), "dose1_12p_pct"])
proj_US_cumcov$truth_trunc <- unlist(vacc_US[date %in% pmin(as.Date(c("2021-10-31", "2021-11-30", "2021-12-31")), 
                                                            variant_takeover_date[c(2,3,3)]), 
                                             "dose1_12p_pct"])


#### SCENARIO PROJECTION DIFFERENCES -------------------------------------------
# add variant takeover dates to proj_US
proj_US <- proj_US %>%
  .[, trunc_date := as.Date(ifelse(round == 1, 
                                   variant_takeover_date[1], 
                                   variant_takeover_date[2]), origin = "1970-01-01")]

# calculate differences between each scenario and truth
diffs <- proj_US %>%
  melt(c("round", "date", "vacc_type", "start_date", "trunc_date", "end_date")) %>%
  setnames("variable", "scenario") %>%
  # filter to only start date, end_date, and trunc_date
  melt(c("round", "vacc_type", "scenario", "date", "value"), value.name = "proj_date") %>%
  .[date == proj_date] %>%
  # reshape with one column per date
  dcast(round + vacc_type + scenario~variable, value.var = "value") %>%
  # calculate differences 
  .[, ":=" (diff_full = end_date - start_date, 
            diff_trunc = trunc_date - start_date)] %>%
  # reshape for one column per scenario
  melt(c("round","vacc_type", "scenario")) %>%
  # reduce to millions
  .[, value_mill := round(value/1E6)] %>%
  data.table::dcast(round + vacc_type + variable ~ scenario, value.var = "value_mill") %>%
  .[variable %in% c("diff_full", "diff_trunc")] %>%
  # chose scenario closest to truth
  .[, chosen := ifelse(round > 1, ifelse(abs(opt - truth) > abs(pess - truth), "pess", "opt"), 
                       ifelse(all(abs(opt - truth) < c(abs(int - truth), abs(pess - truth))), "opt", 
                       ifelse(abs(int - truth) > abs(pess - truth), "pess", "int"))), by = .(round, vacc_type, variable)]

diffs_weekly <- proj_US %>%
  .[MMWRweekday(date) == "Saturday" & vacc_type != "jj"] %>%
  .[, ":=" (bound_flag = ifelse(truth <= opt & truth >= pess, TRUE, FALSE), 
            trunc_flag = ifelse(date <= trunc_date, TRUE, FALSE))] %>%
  .[, .(n_wks_bound = sum(bound_flag), 
        n_wks_total = .N, 
        pct_bound = round(sum(bound_flag)/.N,2)), by = .(round, trunc_flag)]

  
## Round 1
# difference in distributed doses 
diffs[round == 1]
# number of weeks bounding (all)
diffs_weekly[round == 1 , .(n_wks_bound = sum(n_wks_bound), 
                           n_wks_total = sum(n_wks_total), 
                           pct_bound = round(sum(n_wks_bound)/sum(n_wks_total),2))]
# number of weeks bounding (truncated)
diffs_weekly[round == 1 & trunc_flag == TRUE]
# date 50% coverage in priority group is reached
vacc_raw[Location == "US", .(Date, Administered_Dose1_Recip_65PlusPop_Pct)] %>%
  .[Administered_Dose1_Recip_65PlusPop_Pct > 50] %>%
  pull(Date) %>% min()
  

## Round 2
# difference in administered doses 
diffs[round == 2]
# number of weeks bounding (all)
diffs_weekly[round == 2 , .(n_wks_bound = sum(n_wks_bound), 
                            n_wks_total = sum(n_wks_total), 
                            pct_bound = round(sum(n_wks_bound)/sum(n_wks_total),2))]
# number of weeks bounding (truncated)
diffs_weekly[round == 2 & trunc_flag == TRUE]
# find date after which 65+ is > 50% vaccinated
# note: seems like data is missing, this field goes from 0 to 54.8%
vacc_raw[Location == "US", .(Date, Administered_Dose1_Recip_65PlusPop_Pct)] %>%
  .[Administered_Dose1_Recip_65PlusPop_Pct > 50] %>% 
  pull(Date) %>% min()


## Round 3
diffs[round == 3]
# number of weeks bounding (all)
diffs_weekly[round == 3 , .(n_wks_bound = sum(n_wks_bound), 
                            n_wks_total = sum(n_wks_total), 
                            pct_bound = round(sum(n_wks_bound)/sum(n_wks_total),2))]
# number of weeks bounding (truncated)
diffs_weekly[round == 3 & trunc_flag == TRUE]
# find date after which entire population is > 50% vaccinated
# note: seems like data is missing, this field goes from 0 to 54.8%
vacc_raw[Location == "US", .(Date, Administered_Dose1_Recip_12PlusPop_Pct)] %>%
  .[Administered_Dose1_Recip_12PlusPop_Pct > 50] %>% 
  pull(Date) %>% min()

## Round 4
diffs[round == 4]
# number of weeks bounding (all)
diffs_weekly[round == 4 , .(n_wks_bound = sum(n_wks_bound), 
                            n_wks_total = sum(n_wks_total), 
                            pct_bound = round(sum(n_wks_bound)/sum(n_wks_total),2))]
# number of weeks bounding (truncated)
diffs_weekly[round == 4 & trunc_flag == TRUE]
# find date after which entire population is > 75% vaccinated
# note: seems like data is missing, this field goes from 0 to 54.8%
vacc_raw[Location == "US", .(Date, Administered_Dose1_Recip_12PlusPop_Pct)] %>%
  .[Administered_Dose1_Recip_12PlusPop_Pct > 75] %>% 
  pull(Date) %>% min()

## cumulative coverage
diff_cumcov <- setDT(proj_US_cumcov) %>% 
  melt(c("round", "truth_full", "truth_trunc")) %>%
  setnames("variable", "scenario") %>%
  .[, ":=" (diff_full = abs(truth_full/100 - value), 
            diff_trunc = abs(truth_trunc/100 - value))] %>%
  .[, value := NULL] %>%
  melt(c("round", "truth_full", "truth_trunc", "scenario")) %>%
  data.table::dcast(round + truth_full + truth_trunc + variable ~ scenario, value.var = "value") %>%
  .[, realistic := ifelse(opt > pess, "pess", "opt")]

## Round 5
diff_cumcov[round == 5]
# round 5 is truncated very early in the projection period, so the 
# comparison of cumulative coverage at the end of the projection period may not
# be reflective; so let's calculate the rate over the projection period and 
# extend it until the end of the projection period
# NOTE: coverage pct is not available on projection start date (2021-05-01), 
# so we use the first available 2021-05-13
cov_rate_r5 <- (vacc_US[date == variant_takeover_date[2], "dose1_12p_pct"] - 
  vacc_US[date == as.Date("2021-05-13"), "dose1_12p_pct"])/as.integer((variant_takeover_date[2] - as.Date("2021-05-13")))
# extending this rate for the rest of the projection period
vacc_US[date == variant_takeover_date[2], "dose1_12p_pct"] + 
  cov_rate_r5*as.integer((as.Date("2021-10-31") - variant_takeover_date[2]))
  
## Round 6
diff_cumcov[round == 6]

## Round 7
diff_cumcov[round == 7]

## Round 9
# only use full series here for simplicity
# but note: results are consistent for administered doses 
# number of individuals in "optimistic reduced scenario"
d/1E6
# coverage at end of projection period
R9_all[Date == max(Date) & 
         scenario %in% c("child_vax", "child_vax_reduce") & 
         vacc_type == "series_complete"]
# coverage before truncation
R9_all[Date == "2021-12-25" & 
         scenario %in% c("child_vax", "child_vax_reduce") & 
         vacc_type == "series_complete"]
# truth
R9_all[Date %in% c(max(Date), "2021-12-25") & 
         scenario == "truth" & 
         vacc_type == "series_complete" ]
# bounding in all weeks
R9_all %>%
  .[ , trunc_flag := ifelse(Date > variant_takeover_date["omicron"], 1, 0)] %>% 
  .[MMWRweek(Date)$MMWRday == 7] %>%
  data.table::dcast(Date + vacc_type + trunc_flag ~ scenario, value.var = "value") %>% 
  # use 'reduce' category as most restrictive
  # note: expect 100% bounding because lower is 0
  .[, bound_reduce := ifelse(truth < `child_vax_reduce` & truth > `no_child_vax`, 1, 0)] %>%
  .[, .(b_all = sum(bound_reduce), 
        b_trunc = sum(bound_reduce*trunc_flag), 
        n_trunc = sum(trunc_flag)), by = .(vacc_type)]

## Round 14
# evaluated as of March 20, 2023 (for now)
R14_all[Date %in% c(as.Date("2023-03-15"), as.Date("2023-03-11"))] %>% 
  .[, value := value*1E-6] %>%
  head()
# total doses during projection period
R14_all[Date == max(Date)] %>% 
  .[, value := value*1E-6] %>% 
  head()
# % of weeks bounding
bound_R14 <- R14_all %>%
  .[, ":=" (mmwr_year = MMWRweek(Date)$MMWRyear, 
            mmwr_week = MMWRweek(Date)$MMWRweek, 
            mmwr_day = MMWRweek(Date)$MMWRday)] %>%
  .[!(paste0(scenario, mmwr_day) %in% paste0("restricted-ages",1:6))] %>%
  .[!is.na(value)] %>% 
  data.table::dcast(mmwr_year + mmwr_week ~ scenario, value.var = "value") %>% 
  .[, bound := ifelse(truth < `all-ages` & truth > `restricted-ages`, 1, 0)] %>% 
  .[!is.na(bound)] %>% 
  .[, .(b = sum(bound), 
        n = .N)]
bound_R14

## Round 15
R15_all[, m:=max(value), by = .(scenario)] %>%
  .[value == m] %>%
  .[, value := value*1E-6] %>% 
  head()

R15_all[Date %in% c(as.Date("2023-03-15"), as.Date("2023-03-19"))] %>% 
  .[, value := value*1E-6] %>%
  head()

# % of weeks bounding
bound_R15 <- R15_all %>%
  .[, ":=" (mmwr_year = MMWRweek(Date)$MMWRyear, 
            mmwr_week = MMWRweek(Date)$MMWRweek, 
            mmwr_day = MMWRweek(Date)$MMWRday)] %>%
  #.[!(paste0(scenario, mmwr_day) %in% paste0("truth",2:7))] %>%
  .[!is.na(value)] %>% 
  data.table::dcast(mmwr_year + mmwr_week ~ scenario, value.var = "value") %>% 
  .[, bound := ifelse(truth < early & truth > late, 1, 0)] %>% 
  .[!is.na(bound)] %>% 
  .[, .(b = sum(bound), 
        n = .N)]
bound_R15 

## Round 16
R16_all[scenario %in% c("optimistic", "pessimistic")] %>%
  .[, value := value*1E-6] %>% 
  head()
# truth as of Feb 1, 2023
R16_all[scenario == "truth" & Date == "2023-02-01"] %>%
  .[, value := value*1E-6] %>% 
  head()
R16_all[scenario == "truth" & Date == "2023-03-15"] %>%
  .[, value := value*1E-6] %>% 
  head()


## overall weekly bounding
# number of weeks bounding (1-4)
bound_R1.4 <- diffs_weekly[round %in% 1:4 , .(n_wks_bound = sum(n_wks_bound), 
                                n_wks_total = sum(n_wks_total), 
                                pct_bound = sum(n_wks_bound)/sum(n_wks_total))]
# add in R14-15
(bound_R1.4$n_wks_bound + bound_R14$b + bound_R15$b)/(bound_R1.4$n_wks_total + bound_R14$n + bound_R15$n)

# number of weeks bounding (truncated)
diffs_weekly[round %in% 1:4 & trunc_flag == TRUE, .(n_wks_bound = sum(n_wks_bound), 
                                                    n_wks_total = sum(n_wks_total), 
                                                    pct_bound = sum(n_wks_bound)/sum(n_wks_total))]


#### PLOT OUTCOMES -------------------------------------------------------------
source("code/plot_setup.R")

### PLOT OBSERVED VACCINATIONS R1-4
proj_US %>%
  melt(c("round", "date", "vacc_type", "start_date", "end_date", "trunc_date")) %>%
  setnames("variable", "scenario") %>%
  .[, trunc := ifelse(date < trunc_date, 1, 0)] %>%
  .[, scenario:= factor(scenario, levels = c("truth", "opt", "int", "pess"))] %>%
  ggplot() + 
  # add variant takeover date names
  geom_vline(aes(xintercept = trunc_date), 
             linetype = "dotted", color = "black") + 
  geom_line(aes(x = date, 
                y = value, 
                color = scenario, 
                alpha = as.factor(trunc), 
                group = interaction(scenario,vacc_type, trunc)), size = 1) + 
  geom_label(data = data.frame(round = 1:4, 
                               dt = proj_start_dates[1:4],
                               txt = paste0("realistic scenario:\n", c("optimistic", "optimistic", "optimistic", "pessimistic"))), 
             aes(x = dt, y = Inf, label = txt), hjust = 0, vjust = 1, label.size = NA, size = 2.75) +
  guides(alpha = "none") +
  facet_grid(cols = vars(round),
             labeller = labeller(round = round_labs),
             scales = "free") + 
  scale_alpha_manual(values = c(0.4, 1)) +
  scale_color_manual(values = c("black", brewer.pal(3,"Dark2")),
                     labels = c("truth","optimistic", "intermediate","pessimistic")) +
  scale_x_date(date_breaks = "2 months",
               date_labels = "%b\n%Y",  
               expand = c(0,0)) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6), 
                     name = "US vaccine doses") +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave("figures/scenario_plaus/vacc_assump_R1-R4.pdf", width = 9, height = 3.3)


#### PLOT VACCINATIONS R3 ONLY (for process figure)
# vaccination uptake
proj_US %>%
  .[round == 3] %>%
  melt(c("round", "date", "vacc_type", "start_date", "end_date", "trunc_date")) %>%
  setnames("variable", "scenario") %>%
  .[, trunc := ifelse(date < trunc_date, 1, 0)] %>%
  .[, scenario:= factor(scenario, levels = c("truth", "opt", "int", "pess"))] %>%
  ggplot() + 
  # add variant takeover date names
  geom_vline(aes(xintercept = trunc_date), 
             linetype = "dotted", color = "black") + 
  geom_line(aes(x = date, 
                y = value, 
                color = scenario, 
                alpha = as.factor(trunc), 
                group = interaction(scenario,vacc_type, trunc)), size = 1) + 
  geom_richtext(data = data.frame(l = c("optimistic", "pessimistic", "reality"), 
                              scenario = c("opt", "pess", "truth"), 
                              x = as.IDate("2021-05-25"), 
                              y = unlist(t(proj_US[round == 3 & date == "2021-05-25", .(opt, pess, truth)])), 
                              ang = c(45, 30, 32)), 
            aes(x = x, y = y, label = l, angle = ang, color = scenario), hjust = 1, size = 3, label.size = NA, label.padding = unit(0.01, "lines")) +
  geom_text(data = data.frame(round = 3, 
                               dt = proj_start_dates[3],
                               txt = paste0(" realistic scenario:\n", c(" optimistic"))), 
             aes(x = dt, y = Inf, label = txt), hjust = 0, vjust = 1, size = 3) +
  guides(alpha = "none") +
  facet_grid(cols = vars(round),
             labeller = labeller(round = round_labs),
             scales = "free") + 
  scale_alpha_manual(values = c(0.4, 1)) +
  scale_color_manual(values = c("black", brewer.pal(3,"Dark2")),
                     labels = c("truth","optimistic", "intermediate","pessimistic")) +
  scale_x_date(date_breaks = "2 months",
               date_labels = "%b\n%Y",  
               expand = c(0,0), 
               name = "projection horizon") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6), 
                     name = "US vaccine doses") +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "none", 
        legend.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())
ggsave("figures/main_figures/Figure 2/vacc_assump_R2.pdf", width = 2.5, height = 2.5)

#### ROUND 5-7
p1 = proj_US_cumcov %>%
  .[, end_date := as.Date(c("2021-10-31", "2021-11-30", "2021-12-31"))] %>%
  melt(c("round", "truth_full", "truth_trunc", "end_date")) %>%
  .[, variable := factor(variable, levels = c("opt","pess"))] %>%
  ggplot(aes(x = end_date, y = value, color = variable)) +
  geom_point(size = 2) + 
  geom_vline(data = data.frame(round = 5, 
                               dt = as.Date(variant_takeover_date[2])), 
             aes(xintercept = dt), linetype = "dotted", color = "black") +
  geom_line(data = vacc_US[ date > "2021-05-01" & date < "2022-01-15"], 
            aes(x = as.Date(date),y = dose1_12p_pct/100), color = "black", size = 1) +
  geom_line(data = data.frame(round = 5, 
                              x = c(variant_takeover_date[2], as.Date("2021-10-31")), 
                              y = c(unlist(vacc_US[date == variant_takeover_date[2], "dose1_12p_pct"]), 
                                    unlist(vacc_US[date == variant_takeover_date[2], "dose1_12p_pct"] + 
                                      cov_rate_r5*as.integer((as.Date("2021-10-31") - variant_takeover_date[2]))))), 
            aes(x = x, y = y/100), linetype = "longdash", size = 1, color = "black") +
  geom_label(data = data.frame(round = 5:7, 
                               dt = as.Date("2021-05-01"),
                               txt = paste0("realistic scenario:\n", c("optimistic", "optimistic", "optimistic"))), 
             aes(x = dt, y = Inf, label = txt), hjust = 0, vjust = 1, label.size = NA, size = 2.75, color = "black") +
  facet_grid(cols = vars(round),
             labeller = labeller(round = round_labs),
             scales = "free") + 
  scale_color_manual(values = brewer.pal(3,"Dark2")[c(1,3)],
                     labels = c("optimistic", "pessimistic")) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b\n%Y",  
               expand = c(0,0)) +
  scale_y_continuous(labels = percent, 
                     name = "US vaccine coverage") +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

# 2-D bracketing panel
bracketing_fig = proj_US_cumcov %>% 
  filter(round != 5) %>%
  mutate(opt_trans = c(1.2, 1.2),
         pess_trans = c(1.6, 1.6),
         truth_trans = c(1.5, 1.5))
scenario_points = data.frame(x = c(rep(bracketing_fig$opt,2), rep(bracketing_fig$pess,2)), 
                             y = rep(c(bracketing_fig$opt_trans, bracketing_fig$pess_trans),2), 
                             round = rep(c(6,7),4), 
                             letter = c("A", "A", "B", "B", "C", "C", "D", "D"))

p2 = ggplot(data = bracketing_fig) + 
  geom_segment(aes(x = opt, xend = pess, y = opt_trans, yend = opt_trans, color = as.factor(round)), linetype = "dotted") +
  geom_segment(aes(x = opt, xend = pess, y = pess_trans, yend = pess_trans, color = as.factor(round)), linetype = "dotted") +
  geom_segment(aes(x = opt, xend = opt, y = opt_trans, yend = pess_trans, color = as.factor(round)), linetype = "dotted") +
  geom_segment(aes(x = pess, xend = pess, y = opt_trans, yend = pess_trans, color = as.factor(round)), linetype = "dotted") +
  geom_point(data = scenario_points, 
             aes(x = x, y = y, color = as.factor(round)), size = 4) +
  geom_text(data = scenario_points, 
            aes(x = x, y = y, label = letter), color = "white", size = 2) +
  # truth
  geom_point(aes(x = truth_full/100, 
                 y = truth_trans,
                 color = as.factor(round)), shape = 8, size = 2) +
  geom_text(aes(x = truth_full/100, 
                y = truth_trans,
                color = as.factor(round), label = "truth\n"), vjust = 0.2, size = 2.75) +
  labs(x = "scenario axis 1:\nvaccine coverage", 
       y = "scenario axis 2:\ntransmissibility increase", 
       color = "round") +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(labels = percent) +
  theme_bw() + 
  theme(legend.position = 'bottom', 
        panel.grid = element_blank())

# combine into single figure
plot_grid(p1, p2, nrow = 1, 
          labels = LETTERS[1:2],
          rel_widths = c(0.7, 0.3), 
          align = "h", axis = "tb")
ggsave("figures/scenario_plaus/vacc_assump_R5-R7.pdf", width = 9, height = 3.3)


#### ROUND 9
vlabs = c("administered doses", 
          "completed 2-dose series")
names(vlabs) = c("admin_dose1", "series_complete")
# plot outcomes
R9_all %>%
  .[ , trunc_flag := ifelse(Date > variant_takeover_date["omicron"], 1, 0)] %>%
  ggplot(aes(x = Date, y = value, color = scenario, alpha = as.factor(trunc_flag), linetype = scenario)) + #
  geom_vline(xintercept = variant_takeover_date["omicron"], linetype = "dotted", color = "black") +
  geom_line(size = 1) + 
  guides(alpha = "none") +
  facet_wrap(vars(vacc_type),
             labeller = labeller(vacc_type = vlabs)) +
  scale_alpha_manual(values = c(1, 0.4)) +
  scale_color_manual(values = c(brewer.pal(3,"Dark2")[c(1,1,3)], "black"),
                     labels = c("optimistic", "optimistic (reduced)", "pessimistic", "truth")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "solid"),
                        labels = c("optimistic", "optimistic (reduced)", "pessimistic", "truth")) +
  scale_x_date(date_labels = "%b\n%Y",  
               expand = c(0,0)) +
  scale_y_continuous(labels = percent, 
                     name = "US vaccine coverage") +
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.position = "bottom", 
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave("figures/scenario_plaus/vacc_assump_R9.pdf", width = 5.5, height = 3.3)

#### ROUND 14-16
rbindlist(list(R14_all %>% 
                 .[, ":=" (mmwr_year = NULL, 
                           mmwr_week = NULL, 
                           mmwr_day = NULL, 
                           round = 14)], 
               R15_all %>%
               .[, ":=" (mmwr_year = NULL, 
                         mmwr_week = NULL, 
                         mmwr_day = NULL, 
                         m = NULL,
                         round = 15)], 
               R16_all[, round := 16])) %>%
  .[, scenario := ifelse(scenario == "truth", "truth", 
                         ifelse(scenario %in% c("all-ages", "early", "high"), "optimistic", "pessimistic"))] %>%
  .[, f := ifelse(round == 16 & scenario != "truth", 0, 1)] %>%
  .[f == 1] %>% 
  ggplot(aes(x = Date, y = value, color = scenario)) + 
  geom_line(size = 1) +
  geom_point(data = R16_all[scenario != "truth"] %>% 
               .[, scenario := ifelse(scenario == "high", "optimistic", "pessimistic")], 
             show.legend = FALSE) +
  facet_grid(cols = vars(round), 
             labeller = labeller(round = round_labs)) +
  scale_color_manual(values = c(brewer.pal(3,"Dark2")[c(1,3)], "black"),
                     labels = c("optimistic", "pessimistic", "truth")) +
  scale_x_date(date_labels = "%b\n%Y") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), 
                     name = "US bivalent booster doses") +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave("figures/scenario_plaus/vacc_assump_R14-R16.pdf", width = 7, height = 3)

