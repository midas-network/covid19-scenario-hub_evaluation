#### load packages -------------------------------------------------------------
library(dplyr)
library(data.table)
library(mgcv)
library(ggplot2)

#### load data -----------------------------------------------------------------
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

# location information
locations <- read.csv("data-raw/data-locations/locations.csv")

# get prediction start/end dates
r <- read.csv("data-raw/data-scenarios/all_dates_by_round.csv") %>%
  setDT() %>%
  .[, ":=" (#X = NULL, 
    target_end_date = as.IDate(target_end_date, format = "%m/%d/%y"))] %>%
  .[proj_period_flag == 1] %>%
  .[, .(projection_start_date = min(target_end_date), 
        projection_end_date = max(target_end_date)), by = .(round)]
  
# vector of quantiles
proj_q <- c(0.010, 0.025, seq(0.050, .950,0.05), 0.975, 0.990)

## FOR NOW: ONLY INCIDENT
#truth_data <- truth_data[target == "inc case"]
truth_data <- truth_data[substr(target, 1, 3) == "inc"]


#### fit GAM model -------------------------------------------------------------
# define function to implement GAM 
prd_gam_norm <- function(f, obs, target_end_date, 
                         projection_start_date, projection_end_date, 
                         ret_q, family = "gaussian", ...){
  projection_start_date <- projection_start_date[1]
  projection_end_date <- projection_end_date[1]
  dat <- data.frame(obs = obs, 
                    target_end_date = target_end_date) 
  sm <- gam(formula(f), 
            family = family,
            data = dat)
  # get fit and forward prediction
  pred_dates <-data.frame(
    target_end_date =as.Date(seq(projection_start_date, projection_end_date, by = 7), 
                             origin = "1970-01-01"))
  ## simulate prediction intervals
  ## adapted from: https://www.mail-archive.com/r-help@r-project.org/msg132608.html
  ## example with normal: https://mikl.dk/post/2019-prediction-intervals-for-gam/
  # get estimates and covariance matrix
  beta <- coef(sm)
  Vb <- vcov(sm)
  # simulate beta vectors 
  reps <- 10000
  nb <- length(beta)
  br <- t(chol(Vb)) %*% matrix(rnorm(reps*nb), nb, reps) + beta
  # replicates to linear predictors
  Xp <- predict(sm, newdata = pred_dates, type = "lpmatrix")
  lp <- Xp%*%br
  invfun <- family(sm)$linkinv
  fv <- invfun(lp)
  if(family == "poisson"){
    yr <- matrix(rpois(fv*0, fv), nrow(fv), ncol(fv))
  }
  else if(family == "gaussian"){
    yr <- matrix(rnorm(fv*0, mean = fv, sd = summary(sm)$scale), 
                 nrow(fv), ncol(fv))
  }
  # reshape samples to get distribution
  ret <- yr %>%
    reshape2::melt(keep.rownames = TRUE) %>%
    rename(wk_num = Var1, sim = Var2) %>%
    setDT() %>%
    # square predictions to be back on nominal scale
    .[, value := ifelse(value < 0, 0, value^2)] %>%
    # calculate quantiles
    .[, .(quantile = ret_q,#c(ret_q, 0),  
          value = quantile(value, ret_q)), #c(quantile(value, ret_q), mean(value))), 
      by = "wk_num"] %>%
    left_join(pred_dates %>% mutate(wk_num = 1:nrow(pred_dates)),
              by = "wk_num") %>%
    select(-wk_num)
  return(ret)
}

set.seed(5)
# apply to all data
gam_noseas_sqrt <- dplyr::inner_join(truth_data, 
                                     data.frame(round = c(1:7,9,11:16)),
                                     by = character()) %>%
  setDT() %>%
  .[!(location %in% c("60", "66", "69", "72", "74", "78"))] %>%
  .[r[, .(round, projection_start_date, projection_end_date)], 
    on = c("round")] %>%
  #.[round == 13] %>%
  # filter to non-negative values
  .[obs >= 0] %>%
  # sort by target_end_date
  .[order(target_end_date)] %>% 
  # calculate 14-day MA
  .[, MA14 := frollmean(obs, 2), by = .(target, location, round)] %>%
  # sqrt transform observations (as 14-day MA)
  .[, MA14_sqrt:=sqrt(MA14)] %>%
  # remove NAs (for first in series - frollmean)
  .[!is.na(MA14_sqrt)] %>%
  # choose only last one year of data
  .[target_end_date >= as.Date(projection_start_date) - 52*7 & target_end_date <= projection_start_date] %>%
  # fit gam and return predicted values
  .[, as.list(prd_gam_norm("obs ~ s(as.numeric(target_end_date), bs = 'cr')",
                           MA14_sqrt ,as.numeric(target_end_date), 
                           projection_start_date, projection_end_date, 
                           proj_q, 
                           family = "gaussian")), 
    by = .(target, location, location_name, round)]

write.csv(gam_noseas_sqrt, "data-output/projections-null/GAM_proj.csv")

#### plot GAM predictions ------------------------------------------------------
# plot outcomes for each state and target
p <- list()
for(i in 1:length(unique(gam_noseas_sqrt$target))){
  targ <- unique(gam_noseas_sqrt$target)[i]
  p[[i]] <- gam_noseas_sqrt %>%
    #.[, value:=ifelse(value <0, 0, value^2)] %>%
    .[quantile %in% c(0.05,0.5,0.95) &
        target == targ] %>%
    .[, quantile := paste0("Q", quantile*100)] %>%
    data.table::dcast(target + location_name + round + 
                        target_end_date ~ quantile, value.var = "value") %>%
    ggplot() +
    geom_point(data = truth_data[target_end_date > "2021-01-01" & target == targ & obs > 0],
               aes(x = target_end_date, y = obs), size = 0.5) +
    geom_line(data = truth_data[target == targ & obs > 0] %>%
                .[, MA14 := frollmean(obs, 2), by = .(target, location)] %>%
                .[target_end_date > "2021-01-01"], 
              aes(x = target_end_date, y = MA14)) + 
    geom_ribbon(aes(x = target_end_date, ymin = Q5, max = Q95, fill = as.factor(round)), alpha = 0.4) + 
    geom_line(aes(x = target_end_date, y = Q50, color = as.factor(round)), size = 0.8) +
    ggtitle("GAM no seasonality, sqrt(obs): predictions") + 
    facet_wrap(vars(location_name), scales = "free") + 
    labs(y = targ) +
    theme_bw() + 
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(), 
          legend.position = "none", 
          panel.grid = element_blank())
}
pdf("figures/null_models/null_gam/GAM_pred_intervals.pdf",
    width = 11, height = 8.5)
p
dev.off()

# plot truncated median 
p <- list()
for(i in 1:length(unique(gam_noseas_sqrt$target))){
  targ <- unique(gam_noseas_sqrt$target)[i]
  p[[i]] <- truth_data[target_end_date > "2021-01-01" & target == targ] %>%
    .[, .(max_obs_thresh = max(obs)*1.2), by = "location_name"] %>%
    .[gam_noseas_sqrt, on = .(location_name)] %>%
    #.[, value:=ifelse(value <0, 0, value^2)] %>%
    .[quantile %in% c(0.05,0.5,0.95) &
        target == targ] %>%
    .[, ":=" (quantile = paste0("Q", quantile*100),
              value_truc = ifelse(value > max_obs_thresh, ifelse(quantile == 0.5, NA, max_obs_thresh), value))] %>%
    data.table::dcast(target + location_name + round + 
                        target_end_date ~ quantile, value.var = "value_truc") %>%
    ggplot() +
    geom_point(data = truth_data[target_end_date > "2021-01-01" & target == targ & obs > 0],
               aes(x = target_end_date, y = obs), size = 0.5) +
    geom_line(data = truth_data[target == targ & obs > 0] %>%
                .[, MA14 := frollmean(obs, 2), by = .(target, location)] %>%
                .[target_end_date > "2021-01-01"], 
              aes(x = target_end_date, y = MA14)) + 
    geom_ribbon(aes(x = target_end_date, ymin = Q5, max = Q95, fill = as.factor(round)), alpha = 0.4) + 
    geom_line(aes(x = target_end_date, y = Q50, color = as.factor(round))) +
    ggtitle("GAM no seasonality, sqrt(obs): predictions") + 
    facet_wrap(vars(location_name), scales = "free") + 
    labs(y = targ) +
    theme_bw() + 
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(), 
          legend.position = "none", 
          panel.grid = element_blank())
}
pdf("figures/null_models/null_gam/GAM_pred_truncmedian.pdf",
    width = 11, height = 8.5)
p
dev.off()


