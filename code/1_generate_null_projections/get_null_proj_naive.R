#### get 4 week ahead Forecast Hub predictions ---------------------------------
library(covidHubUtils)
library(dplyr)
library(reshape2)
library(data.table)

loc <- read.csv("data-raw/data-locations/locations.csv")
r <- read.csv("data-raw/data-scenarios/all_dates_by_round.csv") %>%
  setDT() %>%
  .[, ":=" (#X = NULL, 
    target_end_date = as.IDate(target_end_date, format = "%m/%d/%y"))]

# first pull cases and deaths (weekly forecasts)
inc_targets <- c(paste(4, "wk ahead inc", c("case", "death"))) 
# manipulate first date of projection period to align with forecast_date (4wks before)
first_SMH_target_end_date <- r[, .(target_end_date = min(target_end_date)), by = .(round)]
forcast_dates <- first_SMH_target_end_date %>% 
  mutate(target_end_date = as.Date(target_end_date - 26)) %>% pull(target_end_date) %>% as.character()
# pull from FH usnig zoltar
FH_baseline <- load_forecasts(
  models = c("COVIDhub-baseline"),
  dates = forcast_dates,
  date_window_size = 6,
  locations = loc$location,
  types = c("quantile"),
  targets = inc_targets,
  source = "zoltar",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")
)

# reformatting
null_naive <- setDT(FH_baseline) %>%
  setnames("target_variable", "target", skip_absent = TRUE) %>%
  .[type == "quantile"]

# next pull hospitalizations
# note: hospitalization forecasts are daily
# we pull all 20-26 day ahead forecasts to roll up forecasts and 
# align final day with SMH target_end_date
FH_baseline_hosp <- load_forecasts(
  models = c("COVIDhub-baseline"),
  date_window_size = 6,
  locations = loc$location,
  types = c("quantile"),
  targets = paste(20:26,"day ahead inc hosp"), # 
  source = "zoltar",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")
)

# reformatting
null_naive_hosp <- setDT(FH_baseline_hosp) %>%
  setnames("target_variable", "target") %>%
  .[type == "quantile"]


# roll up daily FH hosp to weekly
# draw 10000 random samples from each daily distribution, sum the samples and 
# find the distribution; this assumes days are independent
f <- function(quantile, value, horizon, u, ret_q){
  d <- data.table(quantile = quantile, 
                  value = value, 
                  horizon = horizon)
  r <- d %>%
    .[, .(v = approx(quantile, value, u, rule = 2)$y,
          s = 1:length(u)), 
      by = .(horizon)] %>%
    .[, .(value = sum(v)), by = s] %>%
    .[, .(quantile = ret_q, 
          value = quantile(value, ret_q))]
  return(r)
}
set.seed(101)
u <- runif(10000)
rq <- unique(null_naive_hosp$quantile)
roll2 <- null_naive_hosp %>%
  # set target end date to saturday of that week (to align with SMH target_end_date)
  .[, target_end_date := as.Date(target_end_date + (26-as.integer(horizon)))] %>%
  .[, f(quantile, value, horizon, u, rq), by = .(location, location_name, target_end_date)] %>%
  # add target
  .[, target := "inc hosp"] %>%
  # select only first week of SMH projections
  .[target_end_date %in% first_SMH_target_end_date$target_end_date]

# bind targets back together
null_naive <- rbindlist(list(
  null_naive %>%
    .[, c("location", "location_name","target", "target_end_date", "quantile", "value")], 
  roll2), use.names = TRUE)

# add round
# use 4-wk forecast hub baseline model for all projection weeks
first_wk <- null_naive[first_SMH_target_end_date,
                       on = .(target_end_date)]
# add round to null_naive
null_naive <- null_naive[first_SMH_target_end_date, on = .(target_end_date)]
null_naive <- null_naive[, target_end_date:=NULL] %>%
  .[r, on = .(round), allow.cartesian = TRUE]
rm(first_wk)

write.csv(null_naive, "data-output/projections-null/naive_proj.csv")

