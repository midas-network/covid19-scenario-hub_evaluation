#### SETUP ---------------------------------------------------------------------
library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(tidytext)

# load increasing/decreasing data
inc_dec <- setDT(read.csv("data-output/increase-decrease-classification/increasing_decreasing.csv")) %>%
  .[, ":=" (X = NULL,
            target_end_date = as.IDate(target_end_date))] %>% 
  .[locations, on = .(location)]

inc_dec_obs <- setDT(read.csv("data-output/increase-decrease-classification/increasing_decreasing_obs.csv")) %>%
  .[, ":=" (X = NULL,
            target_end_date = as.IDate(target_end_date))] %>% 
  .[locations, on = .(location)]


# load objects to assist with plotting
source("code/plot_setup.R")

#### PLOT ALL INCREASE/DECREASE RESULTS ----------------------------------------

id_proj <- inc_dec %>%
  .[model_name == "Ensemble_LOP" &
      quantile == 0.5 & 
      plaus_weight > 0 &
      target == "inc hosp"]

id_obs <- inc_dec_obs[target == "inc hosp"] %>% 
  .[locations, on = .(location)]

for(i in c(1:7,9,11:16)){
  dates = as.Date(range(unlist(id_proj[round == i, .(target_end_date)])), origin = "1970-01-01")
  ggplot(data = id_proj[round == i], aes(x = target_end_date, y = obs)) +
    geom_line(data = id_obs[target_end_date <= dates[2] & target_end_date >= dates[1]], aes(y = obs)) +
    geom_point(data = id_obs[target_end_date <= dates[2] & target_end_date >= dates[1]], 
               aes(y = obs, fill = change_bin), shape = 21, size = 2) +
    geom_line(aes(group = interaction(round, scenario_id))) + 
    geom_point(aes(fill = change_bin, group = interaction(round, scenario_id)), shape = 22, size = 2) + 
    facet_wrap(vars(location_name), scales = "free") +
    labs(subtitle = paste0("Round ", i)) +
    scale_fill_manual(values = rev(brewer.pal(3,"RdYlBu")),
                      labels = c("decreasing", "flat ","increasing"),
                      na.value = "lightgrey") +
    scale_x_date(date_labels = "%b %Y") +
    scale_y_continuous(name = "incident hospitalizations", 
                       labels = label_number(suffix = " K", scale = 1e-3)) +
    theme_bw() +
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(),
          axis.title.x = element_blank(), 
          legend.position = "none",
          panel.grid = element_blank(), 
          panel.spacing = unit(0.01, "cm"),
          strip.background = element_blank())
  ggsave(paste0("figures/trend_classification/inc_dec/inc_dec_full_R",i,".pdf"),  width = 10, height = 10)
}

rm(id_obs, id_proj)

#### PLOT AGREEMENT BEWTEEN OBSERVED AND PREDICTED BY ROUND --------------------
inc_dec %>%
  .[!is.na(change_bin) & quantile == 0.5] %>%
  setnames("change_bin", "change_bin_proj") %>%
  .[inc_dec_obs[, c("location", "target", "target_end_date", "change_bin")], 
    on = .(location, target, target_end_date)] %>%
  .[!is.na(change_bin)] %>%
  .[, correct_flag := ifelse(change_bin_proj == change_bin, 1, 0)] %>% 
  .[, .(n_correct = sum(plaus_weight*correct_flag),
        expectation = (1/3)*sum(plaus_weight), 
        n_total = sum(plaus_weight)), by = .(change_bin, model_name, round)] %>%
  dcast(change_bin  + round ~ model_name, value.var = c("n_correct", "expectation", "n_total")) %>%
  .[!is.na(round)] %>%
  .[, round := factor(round, levels = 1:16)] %>%
  ggplot(aes(x = change_bin, y = n_correct_Ensemble_LOP/n_total_Ensemble_LOP, fill = change_bin)) + 
  geom_bar(stat = "identity") + 
  facet_grid(cols = vars(round), 
             labeller = labeller(round = round_labs)) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid")) +
  scale_x_discrete() +
  scale_y_continuous(expand = c(0,0), 
                     labels = percent,
                     limits = c(0,1), 
                     name = "correctly classified") +
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none", 
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(), 
        strip.placement = "outside")
ggsave("figures/trend_classification/inc_dec_byround_simp.pdf", width = 10, height = 2)

#### PLOT AGREEMENT BEWTEEN OBSERVED AND PREDICTED BY ROUND AND TARGET ---------

inc_dec %>%
  .[!is.na(change_bin) & quantile == 0.5] %>%
  setnames("change_bin", "change_bin_proj") %>%
  .[inc_dec_obs[, c("location", "target", "target_end_date", "change_bin")], 
    on = .(location, target, target_end_date)] %>%
  .[!is.na(change_bin)] %>%
  .[, correct_flag := ifelse(change_bin_proj == change_bin, 1, 0)] %>% 
  .[, .(n_correct = sum(plaus_weight*correct_flag),
        expectation = (1/3)*sum(plaus_weight), 
        n_total = sum(plaus_weight)), by = .(change_bin, target, model_name, round)] %>%
  dcast(change_bin + target + round ~ model_name, value.var = c("n_correct", "expectation", "n_total")) %>%
  .[!is.na(round)] %>%
  .[, round := factor(round, levels = rev(1:16))] %>%
  .[, target := factor(target, levels = paste("inc", c("case", "hosp", "death")))] %>%
  ggplot(aes(x = n_correct_Ensemble_LOP, y = as.factor(round))) + 
  geom_bar(aes(x = n_total_Ensemble_LOP, fill = "total"), stat = "identity") + 
  geom_bar(aes(fill = "% correct"), stat = "identity") + 
  facet_grid(cols = vars(change_bin), rows = vars(target),
             labeller = labeller(target = target_labs, change_bin = classif_labs), 
             switch = "y") +
  labs(x = "# of round-week-locations") +
  scale_fill_brewer(palette = "Greys", direction = -1) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete() +
  theme_bw()+
  theme(axis.title.y = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank(),
        panel.grid = element_blank(), 
        strip.background = element_blank(), 
        strip.placement = "outside")
ggsave("figures/trend_classification/inc_dec_byround.pdf", width = 8, height = 8)



#### PLOT AGREEMENT BEWTEEN OBSERVED AND PREDICTED BY STATE --------------------
inc_dec %>%
  .[!is.na(change_bin) & quantile == 0.5] %>%
  setnames("change_bin", "change_bin_proj") %>%
  .[inc_dec_obs[, c("location", "target", "target_end_date", "change_bin")], 
    on = .(location, target, target_end_date)] %>%
  .[!is.na(change_bin)] %>%
  .[, correct_flag := ifelse(change_bin_proj == change_bin, 1, 0)] %>% 
  .[, .(n_correct = sum(plaus_weight*correct_flag),
        expectation = (1/3)*sum(plaus_weight), 
        n_total = sum(plaus_weight)), by = .(change_bin, target, model_name, location, population)] %>%
  .[!is.na(round)] %>%
   data.table::dcast(target + change_bin + location + population ~ model_name, value.var = c("n_correct", "expectation", "n_total")) %>%
  .[, target:=factor(target, levels = rev(c(paste("inc", c("case", "hosp", "death")))))] %>%
  .[, location := factor(location)] %>%
  .[, pct_correct := n_correct_Ensemble_LOP/n_total_Ensemble_LOP] %>%
  ggplot(aes(x = n_correct_Ensemble_LOP/n_total_Ensemble_LOP, y = reorder(location, population))) + 
  geom_bar(aes(x = n_total_Ensemble_LOP/n_total_Ensemble_LOP, fill = "total"), stat = "identity") + 
  geom_bar(aes(fill = "% correct"), stat = "identity") + 
  geom_vline(xintercept = 1/3) +
  facet_grid(cols = vars(change_bin), rows = vars(target), 
             labeller = labeller(change_bin = classif_labs, 
                                 target = target_labs),
             scales = "free") +
  labs(x = "percent of round-week-locations", y = "states (sorted by population size)") +
  scale_fill_brewer(palette = "Greys", direction = -1, breaks = "% correct") +
  scale_x_continuous(expand = c(0,0), 
                     labels = scales::percent) +
  scale_y_reordered(expand = c(0,0)) +
  theme_bw()+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        legend.position = "none", 
        legend.title = element_blank(),
        panel.grid = element_blank(), 
        strip.background = element_blank())
ggsave("figures/trend_classification/inc_dec_bystate.pdf", width = 7, height = 7)


#### PLOT AGREEMENT BEWTEEN OBSERVED AND PREDICTED BY PROJ WEEK AND TARGET -----
inc_dec %>%
  .[!is.na(change_bin) & quantile == 0.5] %>%
  setnames("change_bin", "change_bin_proj") %>%
  .[inc_dec_obs[, c("location", "target", "target_end_date", "change_bin")], 
    on = .(location, target, target_end_date)] %>%
  .[!is.na(change_bin)] %>%
  .[, correct_flag := ifelse(change_bin_proj == change_bin, 1, 0)] %>% 
  .[, proj_week := (target_end_date - min(target_end_date))/7 + 1, by = .(round)] %>%
  .[, proj_week_bin := as.factor(floor((proj_week+3)/4))] %>%
  .[, proj_week_bin := factor(proj_week_bin, levels = rev(1:13))] %>%
  .[!is.na(change_bin)] %>%
  .[, .(n_correct = sum(plaus_weight*correct_flag),
        expectation = (1/3)*sum(plaus_weight), 
        n_total = sum(plaus_weight)), by = .(change_bin, target, model_name, proj_week_bin)] %>%
  .[!is.na(model_name)] %>%
  .[model_name == "Ensemble_LOP"] %>%
  .[, target:=factor(target, levels = rev(c(paste("inc", c("case", "hosp", "death")))))] %>%
  ggplot(aes(x = n_correct, y = proj_week_bin)) + 
  geom_bar(aes(x = n_total, fill = "total"), stat = "identity") + 
  geom_bar(aes(fill = "% correct"), stat = "identity") + 
  geom_vline(xintercept = 1/3) +
  facet_grid(cols = vars(change_bin), rows = vars(target), 
             labeller = labeller(change_bin = classif_labs, 
                                 target = target_labs),
             scales = "free") +
  scale_fill_brewer(palette = "Greys", direction = -1, breaks = "% correct") +
  scale_x_continuous(expand = c(0,0), 
                     labels = scales::comma, 
                     name = "# of round-week-locations") +
  scale_y_discrete(expand = c(0,0), 
                   labels = rev(paste0(seq(1,49, 4), "-",seq(4,52,4))),
                   name = "projection weeks") +
  theme_bw()+
  theme(legend.position = "none", 
        legend.title = element_blank(),
        panel.grid = element_blank(), 
        strip.background = element_blank())
ggsave("figures/trend_classification/inc_dec_byweek.pdf", width = 7, height = 7)

#### PLOT AGREEMENT BEWTEEN OBSERVED AND PREDICTED BY PROJ WEEK ----------------
inc_dec %>%
  .[!is.na(change_bin) & quantile == 0.5] %>%
  setnames("change_bin", "change_bin_proj") %>%
  .[inc_dec_obs[, c("location", "target", "target_end_date", "change_bin")], 
    on = .(location, target, target_end_date)] %>%
  .[!is.na(change_bin)] %>%
  .[, correct_flag := ifelse(change_bin_proj == change_bin, 1, 0)] %>% 
  .[, proj_week := (target_end_date - min(target_end_date))/7 + 1, by = .(round)] %>%
  .[, proj_week_bin := as.factor(floor((proj_week+3)/4))] %>%
  .[, proj_week_bin := factor(proj_week_bin, levels = rev(1:13))] %>%
  .[!is.na(change_bin)] %>%
  .[, .(n_correct = sum(plaus_weight*correct_flag),
        expectation = (1/3)*sum(plaus_weight), 
        n_total = sum(plaus_weight)), by = .(change_bin, model_name, proj_week_bin)] %>%
  .[!is.na(model_name)] %>%
  .[model_name == "Ensemble_LOP"] %>%
  ggplot(aes(x = n_correct, y = proj_week_bin)) + 
  geom_bar(aes(x = n_total, fill = "total"), stat = "identity") + 
  geom_bar(aes(fill = "% correct"), stat = "identity") + 
  geom_text(aes(x = ifelse(n_correct/2<120, 120, n_correct/2), label = paste0(" ", round(n_correct/n_total*100), "%")), size = 2) +
  geom_vline(xintercept = 1/3) +
  facet_grid(cols = vars(change_bin), 
             labeller = labeller(change_bin = classif_labs),
             scales = "free") +
  scale_fill_brewer(palette = "Greys", direction = -1, breaks = "% correct") +
  scale_x_continuous(expand = c(0,0), 
                     labels = scales::comma, 
                     name = "# of round-week-locations") +
  scale_y_discrete(expand = c(0,0), 
                   labels = rev(paste0(seq(1,49, 4), "-",seq(4,52,4))),
                   name = "projection weeks") +
  theme_bw()+
  theme(legend.position = "none", 
        legend.title = element_blank(),
        panel.grid = element_blank(), 
        strip.background = element_blank())
ggsave("figures/trend_classification/inc_dec_byweek_simp.pdf", width = 7, height = 3)


#### PLOT AGREEMENT BEWTEEN OBSERVED AND PREDICTED BY VARIANT PERIOD -----------
inc_dec %>%
  .[!is.na(change_bin) & quantile == 0.5] %>%
  setnames("change_bin", "change_bin_proj") %>%
  .[inc_dec_obs[, c("location", "target", "target_end_date", "change_bin")], 
    on = .(location, target, target_end_date)] %>%
  .[!is.na(change_bin)] %>%
  .[, correct_flag := ifelse(change_bin_proj == change_bin, 1, 0)] %>% 
  .[, var_period := ifelse(target_end_date < variant_takeover_date["alpha"], "ancestral", 
                           ifelse(target_end_date < variant_takeover_date["delta"], "alpha", 
                                  ifelse(target_end_date < variant_takeover_date["omicron"], "delta", "omicron")))] %>%
  .[, var_period := factor(var_period, levels = rev(c("ancestral", "alpha", "delta", "omicron")))] %>%
  .[, .(n_correct = sum(plaus_weight*correct_flag),
        expectation = (1/3)*sum(plaus_weight), 
        n_total = sum(plaus_weight)), by = .(change_bin, target, model_name, var_period)] %>%
  .[model_name == "Ensemble_LOP"] %>%
  .[, target:=factor(target, levels = rev(c(paste("inc", c("case", "hosp", "death")))))] %>%
  ggplot(aes(x = n_correct, y = var_period)) + 
  geom_bar(aes(x = n_total, fill = "total"), stat = "identity") + 
  geom_bar(aes(fill = "% correct"), stat = "identity") + 
  geom_vline(xintercept = 1/3) +
  facet_grid(cols = vars(change_bin), rows = vars(target), 
             labeller = labeller(change_bin = classif_labs, 
                                 target = target_labs),
             scales = "free") +
  scale_fill_brewer(palette = "Greys", direction = -1, breaks = "% correct") +
  scale_x_continuous(expand = c(0,0), 
                     labels = scales::comma, 
                     name = "# of round-week-locations") +
  scale_y_discrete(expand = c(0,0),
                   name = "variant period") +
  theme_bw()+
  theme(legend.position = "none", 
        legend.title = element_blank(),
        panel.grid = element_blank(), 
        strip.background = element_blank())
ggsave("figures/trend_classification/inc_dec_byvar.pdf", width = 7, height = 5)


