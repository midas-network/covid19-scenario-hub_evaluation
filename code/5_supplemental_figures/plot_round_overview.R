WIS_bytarg <- WIS %>%
  .[!(substr(model_name,1,4) %in% c("null")) & !(model_name %in% c("Ensemble", "Ensemble_LOP_untrimmed"))] %>%
  # normalize WIS by round, target, location (do not group by scenario or model)
  .[, WIS_SD := sd(WIS, na.rm = TRUE), by = .(target, location, target_end_date, round)] %>%
  .[, WIS_norm := WIS/WIS_SD] %>%
  .[model_name == "Ensemble_LOP"] %>% 
  .[, .(value = sum(WIS_norm*plaus_weight, na.rm = TRUE)/sum(plaus_weight, na.rm = TRUE)), by = .(round, target)] %>% 
  .[, metric := "normalized WIS"] 
WIS_overall <- WIS %>%
  .[!(substr(model_name,1,4) %in% c("null")) & !(model_name %in% c("Ensemble", "Ensemble_LOP_untrimmed"))] %>%
  # normalize WIS by round, target, location (do not group by scenario or model)
  .[, WIS_SD := sd(WIS, na.rm = TRUE), by = .(target, location, target_end_date, round)] %>%
  .[, WIS_norm := WIS/WIS_SD] %>%
  .[model_name == "Ensemble_LOP"] %>% 
  .[, .(value = sum(WIS_norm*plaus_weight, na.rm = TRUE)/sum(plaus_weight, na.rm = TRUE)), by = .(round)] %>% 
  .[, target := "overall"] %>% 
  .[, metric := "normalized WIS"]
WIS_summ = rbindlist(list(WIS_bytarg, 
                          WIS_overall), 
                     use.names = TRUE) %>% 
  # define how to fill cells (for comparison)
  .[, fl := (value - min(value))/(max(value) - min(value))] %>%
  .[, value := round(value,1)]
# rm(WIS_bytarg)
# rm(WIS_overall)

# coverage
cov_bytarg <- cov %>%
  .[!is.na(cov) & 
      alpha == 0.95 & 
      model_name == "Ensemble_LOP"] %>%
  .[, .(value = sum(cov*plaus_weight)/sum(plaus_weight)), 
    by = .(round, target)] %>% 
  .[, metric := "95% coverage"]
cov_overall <- cov %>%
  .[!is.na(cov) & 
      alpha == 0.95 & 
      model_name == "Ensemble_LOP"] %>%
  .[, .(value = sum(cov*plaus_weight)/sum(plaus_weight)), 
    by = .(round)] %>% 
  .[, target := "overall"] %>%
  .[, metric := "95% coverage"]
cov_summ = rbindlist(list(cov_bytarg, 
                          cov_overall), 
                     use.names = TRUE) %>% 
  # define how to fill cells (for comparison)
  .[, d := abs(value - 0.95)] %>% 
  .[, fl := (d - min(d))/(max(d) - min(d))] %>%
  .[, d := NULL] %>%
  .[, value := paste0(round(value,2)*100, "%")]
# rm(cov_bytarg)
# rm(cov_overall)


## INC DEC
# load increasing/decreasing data
inc_dec <- setDT(read.csv("data-output/increase-decrease-classification/increasing_decreasing.csv")) %>%
  .[, ":=" (X = NULL,
            target_end_date = as.IDate(target_end_date))] %>% 
  .[locations, on = .(location)]

inc_dec_obs <- setDT(read.csv("data-output/increase-decrease-classification/increasing_decreasing_obs.csv")) %>%
  .[, ":=" (X = NULL,
            target_end_date = as.IDate(target_end_date))] %>% 
  .[locations, on = .(location)]


plt <- copy(inc_dec)
plt <- plt%>%
  setnames("change_bin", "change_bin_proj", skip_absent = TRUE) %>%
  .[inc_dec_obs[, c("location", "target", "target_end_date", "change_bin")], 
    on = .(location, target, target_end_date)] %>%
  .[!is.na(change_bin) & !is.na(change_bin_proj) & !is.na(obs)] %>%
  .[, correct_flag := ifelse(change_bin_proj == change_bin, 1, 0)]

# precision = TP_i/(all i predicted)
precision_bytarg <- plt %>%
  .[!(quantile %in% c(0.75, 0.975)) & 
      model_name == "Ensemble_LOP"] %>%
  .[, .(n_total = sum(plaus_weight, na.rm = TRUE)), by = .(change_bin_proj, change_bin, round, target)] %>%
  .[, .(value = n_total[change_bin==change_bin_proj]/sum(n_total)), by = .(change_bin_proj, round, target)]%>% 
  .[, metric := paste0("precision - ", change_bin_proj)] %>% 
  .[, change_bin_proj := NULL]
precision_overall <- plt %>%
  .[!(quantile %in% c(0.75, 0.975)) & 
      model_name == "Ensemble_LOP"] %>%
  .[, .(n_total = sum(plaus_weight, na.rm = TRUE)), by = .(change_bin_proj, change_bin, round)] %>%
  .[, .(value = n_total[change_bin==change_bin_proj]/sum(n_total)), by = .(change_bin_proj, round)] %>%
  .[, target := "overall"] %>% 
  .[, metric := paste0("precision - ", change_bin_proj)] %>% 
  .[, change_bin_proj := NULL]
precision_summ = rbindlist(list(precision_bytarg, 
                                precision_overall), 
                           use.names = TRUE) %>% 
  # average across classification bins
  .[, value := mean(value), by = .(round, target)] %>%
  .[, metric := "precision"] %>%
  unique() %>%
  # define how to fill cells (for comparison)
  .[, fl := (max(value) - value)/(max(value) - min(value))] %>%
  .[, value := paste0(round(value,2)*100, "%")]
# rm(precision_bytarg)
# rm(precision_overall)

# recall
recall_bytarg <- plt %>%
  .[!(quantile %in% c(0.75, 0.975)) & 
      model_name == "Ensemble_LOP"] %>%
  .[, .(n_total = sum(plaus_weight, na.rm = TRUE)), by = .(change_bin_proj, change_bin, round, target)] %>%
  .[, .(value = n_total[change_bin==change_bin_proj]/sum(n_total)), by = .(change_bin, round, target)] %>%
  .[, metric := paste0("recall - ", change_bin)] %>% 
  .[, change_bin := NULL]
recall_overall <- plt %>%
  .[!(quantile %in% c(0.75, 0.975)) & 
      model_name == "Ensemble_LOP"] %>%
  .[, .(n_total = sum(plaus_weight, na.rm = TRUE)), by = .(change_bin_proj, change_bin, round)] %>%
  .[, .(value = n_total[change_bin==change_bin_proj]/sum(n_total)), by = .(change_bin, round)] %>%
  .[, target := "overall"] %>% 
  .[, metric := paste0("recall - ", change_bin)] %>% 
  .[, change_bin := NULL]
recall_summ = rbindlist(list(recall_bytarg, 
                             recall_overall), 
                        use.names = TRUE) %>% 
  # average across classification bins
  .[, value := mean(value), by = .(round, target)] %>%
  .[, metric := "recall"] %>%
  unique() %>%
  # define how to fill cells (for comparison)
  .[, fl := (max(value) - value)/(max(value) - min(value))] %>%
  .[, value := paste0(round(value,2)*100, "%")]
# rm(recall_bytarg)
# rm(recall_overall)



plt_all_metrics <- rbindlist(list(WIS_summ, 
                                  cov_summ, 
                                  precision_summ, 
                                  recall_summ), 
                             use.names = TRUE) %>% 
  .[, target := factor(target, levels = rev(c(paste("inc", c("case", "hosp", "death")), "overall")))] %>%
  .[, fl := rank(fl), by = .(metric, target)]

n_wks <- WIS[model_name == "Ensemble_LOP" & plaus_week == 1 & !is.na(WIS)] %>%
  .[, .(n_weeks = paste(length(unique(target_end_date)), "weeks")), by = .(round)] 

round_labs_wks <- paste0("round ", n_wks$round, "\n\n", n_wks$n_weeks, "")
names(round_labs_wks) <- n_wks$round


ggplot(data = plt_all_metrics, aes(y = as.factor(target), x = as.factor(round))) + 
  geom_tile(aes(fill = fl)) + 
  geom_text(aes(label = value)) + 
  geom_hline(aes(yintercept = 1.5)) + 
  facet_wrap(vars(metric), ncol = 1) + 
  scale_fill_distiller(palette = "Blues", direction = 1, 
                       breaks = c(1, 14), 
                       labels = c("best", "worst")) +
  scale_x_discrete(expand = c(0,0), 
                   labels = round_labs_wks) +
  scale_y_discrete(expand = c(0,0), 
                   labels = c(target_labs_wrap, "overall")) + 
  theme(axis.title = element_blank(), 
        axis.ticks.x = element_blank(), 
        legend.title = element_blank(),
        legend.position = "bottom", 
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0))
ggsave("figures/performance_results/round_overview.pdf", width = 10, height = 8)

