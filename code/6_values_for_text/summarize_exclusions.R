### summarize the projections that were excluded
library(data.table)

# load projections
# source("code/evaluation/src/prepare_projections.R")

# get excluded projections
p <- "code/evaluation/data/exclusions"
f_exc <- list.files(p)
exc <- list()
for(i in 1:length(f_exc)){
  exc[[i]] <- setDT(read.csv(paste0(p, "/",f_exc[i]))) %>% 
    .[, round := as.integer(gsub(".csv", "", gsub("excluded", "", f_exc[i])))]
}

exc <- rbindlist(exc, use.names = TRUE) %>%
  .[, X:= NULL] %>% 
  .[, n := as.integer(n)] %>% 
  .[substr(model_name,1,3) != "Ens"] # only include one ensemble


n_exc <- exc[variable == "any_exclusion" & n > 0] %>% pull(n) %>% sum() - 
  exc[variable == "e_wrong_round"] %>% pull(n) %>% sum() # remove UVA re-submitting R1 projections in R2
n_proj <- proj[substr(model_name,1,4) != "null" & 
                 substr(model_name,1,3) != "Ens", 
                 .(round, scenario_id, target, target_end_date, location, model_name)] %>% 
  unique() %>% nrow()
n_total <- n_exc + n_proj

# how many exclusions total: 
n_exc/n_total

# find percent of projections excluded for rounds 8 and 10
exc[round %in% c(8, 10)] %>%
  .[, .(n = sum(n),
        p = round(sum(n)/n_total, 3)*100), by = .(round)]


# find percent of projections excluded for territories
exc[variable == "e_territory" & n > 0, 
        .(n = sum(n), 
          pct = round(sum(n)/n_total,3)*100), by = .(location)]


# find range of weeks included in each round
proj[, .(mn = min(target_end_date), 
         mx = max(target_end_date)), by = .(round)]

# find number of projections after 2023-03-04 (when CSSE data is no longer available)
proj[target_end_date > as.Date("2023-03-04") & 
       substr(model_name,1,4) != "null" & 
       substr(model_name,1,3) != "Ens", 
     .(round, scenario_id, target, target_end_date, location, model_name)] %>% 
  unique() %>%
  .[, .(n = .N, 
        pct = round(.N/n_total,3)*100), by = .(round)]


# find individual models excluded
exc[model_name == "IHME-IHME_COVID_model_deaths_unscaled"] %>% 
  .[, .(pct = round(sum(n)/n_total,3)*100), 
    by = .(model_name, variable)] %>%
  data.table::dcast(model_name ~variable, value.var = "pct")

exc[model_name == "OliverWyman-Navigator"] %>%
  .[, .(pct = round(sum(n)/n_total,5)*100), 
    by = .(model_name, variable)] %>%
  data.table::dcast(model_name ~variable, value.var = "pct")

# find models without national projections
include_projs <- proj[location == "US" & 
                       !(model_name %in% c("Ensemble", "Ensemble_LOP_untrimmed", 
                                           "null_gam", "null_fh", "null_naive")), 
                     .(model_name, round)] %>% unique()
include_projs <- paste(include_projs$model_name, include_projs$round, sep = "_")

proj[!(paste(model_name, round, sep = "_") %in% include_projs)] %>% 
  .[, .(round, scenario_id, target, target_end_date, location, model_name)] %>% 
  unique() %>% 
  .[, .(n = .N, 
        pct = round(.N/n_total,5)*100), by = .(model_name)]
  

# find other projection exclusions (that weren't excluded elsewhere)
exc[variable == "e_non_monotonic" & n > 0, 
    .(n = sum(n), 
      pct = round(sum(n)/n_total,3)*100), by = .(model_name, round)]

exc[variable == "e_wrong_date" & n > 0, 
    .(n = sum(n), 
      pct = round(sum(n)/n_total,5)*100), by = .(model_name)]


# find all exclusions that "did not comply with SMH specifications"
exc[variable %in% c("e_non_monotonic", "e_model", "e_wrong_date") & 
      n > 0 & 
      model_name != "Ensemble", 
    .(n = sum(n), 
      pct = round(sum(n)/n_total,3)*100)]



