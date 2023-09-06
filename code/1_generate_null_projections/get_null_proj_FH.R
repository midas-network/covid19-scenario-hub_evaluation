#### get 4 week ahead Forecast Hub predictions ---------------------------------
library(covidHubUtils)
library(dplyr)
library(data.table)

loc <- read.csv("data-raw/data-locations/locations.csv")
r <- read.csv("data-raw/data-scenarios/all_dates_by_round.csv") %>%
  setDT() %>%
  .[, ":=" (#X = NULL, 
            target_end_date = as.IDate(target_end_date, format = "%m/%d/%y"))]

inc_targets <- c(paste(4, "wk ahead inc", c("case", "death")),
                 paste(20:26,"day ahead inc hosp")) # to align final day with SMH target_end_date
FH_data <- load_forecasts(
  models = c("COVIDhub-4_week_ensemble"),
  # dates = unique(proj$target_end_date),
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
null_FH <- setDT(FH_data) %>%
  setnames("target_variable", "target", skip_absent = TRUE) %>%
  .[type == "quantile"] 

# roll up daily FH hosp to weekly
# draw 10000 random samples from each daily distribution, sum the samples and 
# find the distribution; this assumes days are independent (I think?)
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
rq <- unique(null_FH$quantile)
roll2 <- null_FH %>%
  .[target == "inc hosp"] %>%
  # set target end date to final date
  .[, target_end_date := as.Date(target_end_date + (26-as.integer(horizon)))] %>%
  .[, f(quantile, value, horizon, u, rq), by = .(location, location_name, target_end_date)] %>%
  .[, target := "inc hosp"]

# bind targets back together
null_FH <- rbindlist(list(
  null_FH[target != "inc hosp"] %>%
    .[, c("location", "location_name","target", "target_end_date", "quantile", "value")], 
  roll2), use.names = TRUE)

# add round
null_FH <- null_FH %>%
  .[r, on = .(target_end_date), allow.cartesian = TRUE]

write.csv(null_FH, "data-output/projections-null/FH_proj.csv")

