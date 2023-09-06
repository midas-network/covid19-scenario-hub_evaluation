## THIS SCRIPT CALCULATES PERFORMANCE SCORES FOR SMH ROUNDS 1-12

# preamble
library(data.table)
library(dplyr)
library(stringr)
library(tidytext)


#### LOAD FILES AND FUNCTIONS --------------------------------------------------
#source("code/prepare_projections.R")
source("code/3_score_projections/scoring_functions.R")

#### CALCULATE COVERAGE --------------------------------------------------------
cov <- data.table(alpha = c(seq(0.1, 0.9, 0.1), 0.95, 0.98))
# find upper and lower intervals for all alpha levels
cov$upr <- cov$alpha/2 + 0.5
cov$lwr <- 1-(cov$alpha/2 + 0.5)
cov <- melt(cov, "alpha", value.name = "quantile")
cov$quantile = round(cov$quantile,3)
setDT(cov)

# merge with proj to assign alpha and lwr/upr to each quantile in proj
cov <- cov[proj[, quantile := round(quantile,3)], on = .(quantile)] %>% 
  .[round(quantile,3) != 0.5] %>% 
  # reshape to make lwr and upr columnns
  data.table::dcast(round + target+ scenario_id + target_end_date + location + 
                      model_name + alpha + obs ~ variable, value.var = "value")  %>%
  .[, ":=" (cov = ifelse(obs < upr & obs > lwr, 1, 0))] %>% 
  .[, ":=" (upr = NULL,
            lwr = NULL, 
            obs = NULL)]

# save scores by round and target
for(i in unique(cov$round)){
  for(j in unique(cov$target)){
    if(i %in% 13:14){# split R13 because too big
      write.csv(cov[round == i & 
                      target == j & alpha < 0.5], paste0("data-output/coverage/cov95_R",i,"_", j,"1.csv"))
      write.csv(cov[round == i & 
                      target == j & alpha >= 0.5], paste0("data-output/coverage/cov95_R",i,"_", j,"2.csv"))
    }
    else{
      write.csv(cov[round == i & 
                      target == j], paste0("data-output/coverage/cov95_R",i,"_", j,".csv")) 
    }
  }
}

#### CALCULATE WIS -------------------------------------------------------------
# calculate WIS for all projections
scores <-  proj[, wis(quantile,value,obs,IS_components = TRUE),
                by=.(target,target_end_date, scenario_id, model_name, location, round, obs)]

# save scores by round
for(i in unique(scores$round)){
  write.csv(scores[round == i], paste0("data-output/WIS/WIS_R",i,".csv"))
}

