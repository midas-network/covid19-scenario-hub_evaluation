#### SETUP ---------------------------------------------------------------------
library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(cowplot)

# load projections
#source("code/evaluation/src/data_prep/prepare_projections.R")

# load WIS data
# source("code/evaluation/src/data_prep/load_scores.R")
# # set base location of 
# WIS <- load_scores("code/evaluation/data/WIS")
# # load coverage data
# cov <- load_scores("code/evaluation/data/coverage")

# load objects to assist with plotting
# source("code/evaluation/src/plot_setup.R")


#### PLOT ALL STATES FOR EACH NULL ---------------------------------------------
null_labs <- c("4-week ahead US COVID-19 Forecast Hub Ensemble (4-week forecast)", 
               "Generalized Additive Model fit to past 6 months of data (trend-continuation)", 
               "4-week ahead US COVID-19 Forecast Hub Baseline at week before projection period (naive)", 
               "US COVID-19 Scenario Modeling Hub trimmed-LOP ensemble")
names(null_labs) <- c("null_fh", "null_gam", "null_naive", "Ensemble_LOP")

null_labs_simp <- c("4-week forecast", 
                    "trend-continuation", 
                    "naive", 
                    "US COVID-19 Scenario Modeling Hub trimmed-LOP ensemble")
names(null_labs_simp) <- c("null_fh", "null_gam", "null_naive", "Ensemble_LOP")

for(j in names(null_labs)){ 
  for(i in names(target_labs)){
    p <- proj %>%
      .[!(round %in% c(8,10))] %>%
      .[ target_end_date > "2020-09-01" & 
           target == i &
           model_name == j &
           quantile %in% c(0.025,0.5,0.975)] %>%
      # truncate values at 1.5*max(obs) for each location, target
      .[, value_trunc := ifelse(value > 1.5*max(obs, na.rm = TRUE), ifelse(quantile == 0.5, NA, 1.5*max(obs, na.rm = TRUE)), value),
        by = .(location_name)] %>%
      .[, quantile := paste0("Q", quantile*1000)] %>%
      data.table::dcast(scenario_id + round + target_end_date + model_name + 
                          location_name + obs ~ quantile, value.var = "value_trunc") %>%
      ggplot(aes(x = target_end_date, y = obs)) +
      # add variant takeover dates
      geom_vline(data = data.frame(var = names(variant_takeover_date), 
                                   target_end_date = as.Date(variant_takeover_date, origin = "1970-01-01")), 
                 aes(xintercept = target_end_date), 
                 linetype = "dotted") + 
      # add projections
      geom_ribbon(aes(ymin = Q25, ymax = Q975, fill = as.factor(round), group = paste(round,scenario_id)), alpha = 0.25) +
      geom_line(aes(y = Q500, color = as.factor(round), group = interaction(round, scenario_id)), size = 0.75) +
      # add observations
      geom_line(aes(y = obs)) +
      geom_label(data = setDT(locations)[!(location %in% c("60", "66", "69", "72", "74", "78"))], 
                 aes(x = as.Date("2021-02-01"), label = location_name), y = Inf,
                 hjust = 0, vjust = 1, label.size = NA, size = 2.75, color ="black")  +
      facet_wrap(vars(location_name), scales = "free") +
      labs(subtitle = null_labs[j]) +
      scale_x_date(date_labels = "%B %Y") +
      scale_y_continuous(labels = comma, 
                         name = target_labs[i]) +
      theme_bw() + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank(),
            axis.title.x = element_blank(), 
            legend.position = "none",
            panel.grid = element_blank(), 
            panel.spacing = unit(0.01, "cm"),
            strip.background = element_blank(), 
            strip.text = element_blank())
    ggsave(paste0("figures/null_models/",j,"/proj_", substr(i,5,nchar(i)),".pdf"), p, width = 10, height = 7)
  }
}

#### COVERAGE VS. ALL NULLS ----------------------------------------------------
library(scales)

for(i in names(target_labs)){
  p1 = cov[model_name == "Ensemble_LOP" &
             round <= 5 &
             alpha == 0.95 &
             target == i] %>%
    .[, .(cov95 = sum(cov)/.N), by = .(round, target, target_end_date, model_name, scenario_id, plaus_weight, plaus_week)] %>%
    .[, scenario_letter := substr(scenario_id, 1,1)] %>%
    .[, target := factor(target, levels = c("inc case", "inc hosp", "inc death"))] %>%
    .[, plaus_flag := ifelse((plaus_week * plaus_weight) > 0, 1, 0)] %>% 
    .[, first_date := min(target_end_date), by = .(round, target)] %>%
    .[, first_cov := mean(cov95[target_end_date == first_date & plaus_flag > 0]), by = .(round, target)] %>%
    .[, plaus_flag := as.factor(plaus_flag)] %>%
    ggplot(aes(x = target_end_date, y = cov95)) +
    geom_hline(aes(yintercept = 0.95)) +
    geom_line(data = cov[substr(model_name,1,4) == "null" &
                           target == i &
                           alpha == 0.95 &
                           round <= 5] %>%
                .[, .(cov95 = sum(cov)/.N),
                  by = .(round, target, target_end_date, model_name)] %>%
                .[ ,target := factor(target, levels = c("inc case", "inc hosp", "inc death"))],
              aes(x = target_end_date,y = cov95, linetype = model_name),
              color = "darkgrey", size = 1) +
    geom_line(aes(color = as.factor(round), size = plaus_flag, alpha = plaus_flag, group = paste(round, target, scenario_id))) +
    geom_point(aes(x = first_date, y = first_cov, color = as.factor(round)), size = 3) +
    geom_text(aes(x = first_date, y = first_cov, label = round), color = "white", size = 2) +
    facet_grid(cols = vars(round),
               labeller = labeller(round = round_labs),
               scales = "free_x",
               space = "free_x") +
    guides(alpha = "none", color = "none", size = "none") +
    labs(y = paste("95% coverage\nfor", target_labs[i])) +
    scale_alpha_manual(values = c(0.6,1)) +
    scale_color_manual(values = gg_color_hue(14)[1:5]) +
    scale_linetype_manual(labels = c("4-week forecast", 
                                     "trend-continuation", 
                                     "naive"), 
                          values = c("solid","91","9111")) +
    scale_size_manual(values = c(0.5,1.2)) + 
    scale_x_date(date_breaks = "2 months", 
                 date_labels = "%B\n%Y") +
    scale_y_continuous(breaks = c(0,0.95), labels = percent) +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(0.05, "in"),
          legend.key.width = unit(0.75, "in"),
          legend.position = "none", 
          legend.title = element_blank(),
          strip.background = element_blank())
  p2 <- cov[model_name == "Ensemble_LOP" &
              round %in% 6:12 &
              alpha == 0.95 &
              target == i] %>%
    .[, .(cov95 = sum(cov)/.N), by = .(round, target, target_end_date, model_name, scenario_id, plaus_weight, plaus_week)] %>%
    .[, scenario_letter := substr(scenario_id, 1,1)] %>%
    .[, target := factor(target, levels = c("inc case", "inc hosp", "inc death"))] %>%
    .[, plaus_flag := ifelse((plaus_week * plaus_weight) > 0, 1, 0)] %>% 
    .[, first_date := min(target_end_date), by = .(round, target)] %>%
    .[, first_cov := mean(cov95[target_end_date == first_date & plaus_flag > 0]), by = .(round, target)] %>%
    .[, plaus_flag := as.factor(plaus_flag)] %>%
    ggplot(aes(x = target_end_date, y = cov95)) +
    geom_hline(aes(yintercept = 0.95)) +
    geom_line(data = cov[substr(model_name,1,4) == "null" &
                           target == i &
                           alpha == 0.95 &
                           round %in% 6:12] %>%
                .[, .(cov95 = sum(cov)/.N),
                  by = .(round, target, target_end_date, model_name)] %>%
                .[ ,target := factor(target, levels = c("inc case", "inc hosp", "inc death"))],
              aes(x = target_end_date,y = cov95, linetype = model_name),
              color = "darkgrey", size = 1) +
    geom_line(aes(color = as.factor(round), size = plaus_flag, alpha = plaus_flag, group = paste(round, target, scenario_id))) +
    geom_point(aes(x = first_date, y = first_cov, color = as.factor(round)), size = 3) +
    geom_text(aes(x = first_date, y = first_cov, label = round), color = "white", size = 2) +
    facet_grid(cols = vars(round),
               labeller = labeller(round = round_labs),
               scales = "free_x",
               space = "free_x") +
    guides(alpha = "none", color = "none", size = "none") +
    labs(y = paste("95% coverage\nfor", target_labs[i])) +
    scale_alpha_manual(values = c(0.6,1)) +
    scale_color_manual(values = gg_color_hue(14)[6:10]) +
    scale_linetype_manual(labels = c("4-week forecast", 
                                     "trend-continuation", 
                                     "naive"), 
                          values = c("solid","91","9111")) +
    scale_size_manual(values = c(0.5,1.2)) + 
    scale_x_date(date_breaks = "2 months", 
                 date_labels = "%B\n%Y") +
    scale_y_continuous(breaks = c(0,0.95), labels = percent) +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(0.05, "in"),
          legend.key.width = unit(0.75, "in"),
          legend.position = "bottom", 
          legend.title = element_blank(),
          strip.background = element_blank())
  p3 <- cov[model_name == "Ensemble_LOP" &
              round >= 13 &
              alpha == 0.95 &
              target == i] %>%
    .[, .(cov95 = sum(cov)/.N), by = .(round, target, target_end_date, model_name, scenario_id, plaus_weight, plaus_week)] %>%
    .[, scenario_letter := substr(scenario_id, 1,1)] %>%
    .[, target := factor(target, levels = c("inc case", "inc hosp", "inc death"))] %>%
    .[, plaus_flag := ifelse((plaus_week * plaus_weight) > 0, 1, 0)] %>% 
    .[, first_date := min(target_end_date), by = .(round, target)] %>%
    .[, first_cov := mean(cov95[target_end_date == first_date & plaus_flag > 0]), by = .(round, target)] %>%
    .[, plaus_flag := as.factor(plaus_flag)] %>%
    ggplot(aes(x = target_end_date, y = cov95)) +
    geom_hline(aes(yintercept = 0.95)) +
    geom_line(data = cov[substr(model_name,1,4) == "null" &
                           target == i &
                           alpha == 0.95 &
                           round >= 13] %>%
                .[, .(cov95 = sum(cov)/.N),
                  by = .(round, target, target_end_date, model_name)] %>%
                .[ ,target := factor(target, levels = c("inc case", "inc hosp", "inc death"))],
              aes(x = target_end_date,y = cov95, linetype = model_name),
              color = "darkgrey", size = 1) +
    geom_line(aes(color = as.factor(round), size = plaus_flag, alpha = plaus_flag, group = paste(round, target, scenario_id))) +
    geom_point(aes(x = first_date, y = first_cov, color = as.factor(round)), size = 3) +
    geom_text(aes(x = first_date, y = first_cov, label = round), color = "white", size = 2) +
    facet_grid(cols = vars(round),
               labeller = labeller(round = round_labs),
               scales = "free_x",
               space = "free_x") +
    guides(alpha = "none", color = "none", size = "none") +
    labs(y = paste("95% coverage\nfor", target_labs[i])) +
    scale_alpha_manual(values = c(0.6,1)) +
    scale_color_manual(values = gg_color_hue(14)[11:14]) +
    scale_linetype_manual(labels = c("4-week forecast", 
                                     "trend-continuation", 
                                     "naive"), 
                          values = c("solid","91","9111")) +
    scale_size_manual(values = c(0.5,1.2)) + 
    scale_x_date(date_breaks = "2 months", 
                 date_labels = "%B\n%Y") +
    scale_y_continuous(breaks = c(0,0.95), labels = percent) +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(0.05, "in"),
          legend.key.width = unit(0.75, "in"),
          legend.position = "none", 
          legend.title = element_blank(),
          strip.background = element_blank())
  l <- get_legend(p2)
  plot_grid(p1, 
            p2 + theme(legend.position = "none"),
            p3 + theme(legend.position = "none"),
            l, 
            ncol = 1, 
            rel_heights = c(0.3, 0.3, 0.3, 0.1))
  ggsave(paste0("figures/null_models/cov_vs_all_nulls_",substr(i, 5, nchar(i)),".pdf"), width = 12, height = 9)
}

#### WIS VS. ALL NULLS ---------------------------------------------------------

for(i in names(target_labs)){
  p1 <- WIS %>%
    .[, target_end_date:=as.Date(target_end_date)] %>%
    .[!(model_name %in% c("Ensemble_LOP_untrimmed", "Ensemble", "null_gam"))] %>%
    .[target == i & round %in% 1:5] %>%
    # normalize WIS by round, target, location (do not group by scenario or model)
    .[, WIS_SD := sd(WIS, na.rm = TRUE), by = .(target, location, target_end_date, round)] %>%
    .[, WIS_norm := WIS/WIS_SD] %>%
    .[, .(WIS_norm = mean(WIS_norm, na.rm = TRUE)), by = .(round, target, target_end_date, model_name, scenario_id, plaus_weight, plaus_week)] %>%
    .[, plaus_flag := ifelse(plaus_weight*plaus_week > 0, 1, 0)] %>%
    .[model_name == "Ensemble_LOP"] %>%
    .[, first_date := min(target_end_date), by = .(round, target)] %>%
    .[, first_cov := max(WIS_norm[target_end_date == first_date & plaus_weight > 0 & plaus_week > 0]), by = .(round, target)] %>%
    ggplot(aes(x = target_end_date, y  = WIS_norm)) +
    geom_line(data = WIS %>%
                .[, target_end_date:=as.Date(target_end_date)] %>%
                .[!(model_name %in% c("Ensemble_LOP_untrimmed", "Ensemble"))] %>%
                .[target == i & round %in% 1:5] %>%
                # normalize WIS by round, target, location (do not group by scenario or model)
                .[, WIS_SD := sd(WIS, na.rm = TRUE), by = .(target, location, target_end_date, round)] %>%
                .[, WIS_norm := WIS/WIS_SD] %>%
                .[, .(WIS_norm = mean(WIS_norm, na.rm = TRUE)), by = .(round, target, target_end_date, model_name, 
                                                                       scenario_id, plaus_weight)] %>%
                .[model_name %in% paste0("null_", c("fh", "gam", "naive"))],
              aes(linetype = model_name), color = "darkgrey", size = 1.2) +
    geom_line(aes(size = as.factor(plaus_flag), 
                  alpha = as.factor(plaus_flag),
                  group = interaction(round,scenario_id), 
                  color = as.factor(round))) +
    geom_point(aes(x = first_date, y = first_cov, color = as.factor(round)), size = 3) +
    geom_text(aes(x = first_date, y = first_cov, label = round), color = "white", size = 2) +
    guides(alpha = "none", color = "none", size = "none") +
    facet_grid(cols = vars(round),
               labeller = labeller(round = round_labs),
               scales = "free_x",
               space = "free_x") +
    labs(y = paste0("average normalized WIS for\n",target_labs[i])) +
    coord_cartesian(ylim = c(0,5)) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_color_manual(values = gg_color_hue(14)[1:5]) +
    scale_linetype_manual(labels = c("4-week forecast", 
                                     "trend-continuation", 
                                     "naive"), 
                          values = c("solid","91","9111")) +
    scale_size_manual(values = c(0.5, 1.2)) + 
    scale_x_date(date_breaks = "2 months", 
                 date_labels = "%B\n%Y") +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(0.05, "in"),
          legend.key.width = unit(0.75, "in"),
          legend.position = "none", 
          legend.title = element_blank(),
          strip.background = element_blank())
  p2 <- WIS %>%
    .[, target_end_date:=as.Date(target_end_date)] %>%
    .[!(model_name %in% c("Ensemble_LOP_untrimmed", "Ensemble", "null_gam"))] %>%
    .[target == i & round %in% 6:12] %>%
    # normalize WIS by round, target, location (do not group by scenario or model)
    .[, WIS_SD := sd(WIS, na.rm = TRUE), by = .(target, location, target_end_date, round)] %>%
    .[, WIS_norm := WIS/WIS_SD] %>%
    .[, .(WIS_norm = mean(WIS_norm, na.rm = TRUE)), by = .(round, target, target_end_date, model_name, scenario_id, plaus_weight, plaus_week)] %>%
    .[, plaus_flag := ifelse(plaus_weight*plaus_week > 0, 1, 0)] %>%
    .[model_name == "Ensemble_LOP"] %>%
    .[, first_date := min(target_end_date), by = .(round, target)] %>%
    .[, first_cov := max(WIS_norm[target_end_date == first_date & plaus_weight > 0 & plaus_week > 0]), by = .(round, target)] %>%
    ggplot(aes(x = target_end_date, y  = WIS_norm)) +
    geom_line(data = WIS %>%
                .[, target_end_date:=as.Date(target_end_date)] %>%
                .[!(model_name %in% c("Ensemble_LOP_untrimmed", "Ensemble"))] %>%
                .[target == i & round %in% 6:12] %>%
                # normalize WIS by round, target, location (do not group by scenario or model)
                .[, WIS_SD := sd(WIS, na.rm = TRUE), by = .(target, location, target_end_date, round)] %>%
                .[, WIS_norm := WIS/WIS_SD] %>%
                .[, .(WIS_norm = mean(WIS_norm, na.rm = TRUE)), by = .(round, target, target_end_date, model_name, scenario_id, plaus_weight, plaus_week)] %>%
                .[model_name %in% paste0("null_", c("fh", "gam", "naive"))],
              aes(linetype = model_name), color = "darkgrey", size = 1.2) +
    geom_line(aes(size = as.factor(plaus_flag), 
                  alpha = as.factor(plaus_flag),
                  group = interaction(round,scenario_id), 
                  color = as.factor(round))) +
    geom_point(aes(x = first_date, y = first_cov, color = as.factor(round)), size = 3) +
    geom_text(aes(x = first_date, y = first_cov, label = round), color = "white", size = 2) +
    guides(alpha = "none", color = "none", size = "none") +
    facet_grid(cols = vars(round),
               labeller = labeller(round = round_labs),
               scales = "free_x",
               space = "free_x") +
    labs(y = paste0("average normalized WIS for\n",target_labs[i])) +
    coord_cartesian(ylim = c(0,5)) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_color_manual(values = gg_color_hue(14)[6:10]) +
    scale_linetype_manual(labels = c("4-week forecast", 
                                     "trend-continuation", 
                                     "naive"), 
                          values = c("solid","91","9111")) +
    scale_size_manual(values = c(0.5, 1.2)) + 
    scale_x_date(date_breaks = "2 months", 
                 date_labels = "%B\n%Y") +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(0.05, "in"),
          legend.key.width = unit(0.75, "in"),
          legend.position = "bottom", 
          legend.title = element_blank(),
          strip.background = element_blank())
  p3 <- WIS %>%
    .[, target_end_date:=as.Date(target_end_date)] %>%
    .[!(model_name %in% c("Ensemble_LOP_untrimmed", "Ensemble", "null_gam"))] %>%
    .[target == i & round %in% 13:16] %>%
    # normalize WIS by round, target, location (do not group by scenario or model)
    .[, WIS_SD := sd(WIS, na.rm = TRUE), by = .(target, location, target_end_date, round)] %>%
    .[, WIS_norm := WIS/WIS_SD] %>%
    .[, .(WIS_norm = mean(WIS_norm, na.rm = TRUE)), by = .(round, target, target_end_date, model_name, scenario_id, plaus_weight, plaus_week)] %>%
    .[, plaus_flag := ifelse(plaus_weight*plaus_week > 0, 1, 0)] %>%
    .[model_name == "Ensemble_LOP"] %>%
    .[, first_date := min(target_end_date), by = .(round, target)] %>%
    .[, first_cov := max(WIS_norm[target_end_date == first_date & plaus_weight > 0 & plaus_week > 0]), by = .(round, target)] %>%
    ggplot(aes(x = target_end_date, y  = WIS_norm)) +
    geom_line(data = WIS %>%
                .[, target_end_date:=as.Date(target_end_date)] %>%
                .[!(model_name %in% c("Ensemble_LOP_untrimmed", "Ensemble"))] %>%
                .[target == i & round %in% 13:16] %>%
                # normalize WIS by round, target, location (do not group by scenario or model)
                .[, WIS_SD := sd(WIS, na.rm = TRUE), by = .(target, location, target_end_date, round)] %>%
                .[, WIS_norm := WIS/WIS_SD] %>%
                .[, .(WIS_norm = mean(WIS_norm, na.rm = TRUE)), by = .(round, target, target_end_date, model_name, scenario_id, plaus_weight)] %>%
                .[model_name %in% paste0("null_", c("fh", "gam", "naive"))],
              aes(linetype = model_name), color = "darkgrey", size = 1.2) +
    geom_line(aes(size = as.factor(plaus_flag), 
                  alpha = as.factor(plaus_flag),
                  group = interaction(round,scenario_id), 
                  color = as.factor(round))) +
    geom_point(aes(x = first_date, y = first_cov, color = as.factor(round)), size = 3) +
    geom_text(aes(x = first_date, y = first_cov, label = round), color = "white", size = 2) +
    guides(alpha = "none", color = "none", size = "none") +
    facet_grid(cols = vars(round),
               labeller = labeller(round = round_labs),
               scales = "free_x",
               space = "free_x") +
    labs(y = paste0("average normalized WIS for\n",target_labs[i])) +
    coord_cartesian(ylim = c(0,5)) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_color_manual(values = gg_color_hue(14)[11:14]) +
    scale_linetype_manual(labels = c("4-week forecast", 
                                     "trend-continuation", 
                                     "naive"), 
                          values = c("solid","91","9111")) +
    scale_size_manual(values = c(0.5, 1.2)) + 
    scale_x_date(date_breaks = "2 months", 
                 date_labels = "%B\n%Y") +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(0.05, "in"),
          legend.key.width = unit(0.75, "in"),
          legend.position = "bottom", 
          legend.title = element_blank(),
          strip.background = element_blank())
  l <- get_legend(p2)
  plot_grid(p1, 
            p2 + theme(legend.position = "none"),
            p3 + theme(legend.position = "none"),
            l, 
            ncol = 1, 
            rel_heights = c(0.3, 0.3, 0.3, 0.1))
  ggsave(paste0("figures/null_models/WIS_vs_all_nulls_",substr(i, 5, nchar(i)),".pdf"), width = 10, height = 9)
}

#### OVERALL WIS VS ENSEMBLE ---------------------------------------------------
WIS %>%
  .[model_name %in% c("null_fh", "null_gam", "null_naive", "Ensemble_LOP")] %>%
  .[, .(mean_WIS = mean(WIS)), by = .(target, round, scenario_id, plaus_weight, plaus_week, model_name)]
  
# check same number of proj for each model
WIS[model_name %in% c("null_fh", "null_gam", "null_naive", "Ensemble_LOP")] %>% 
  .[, .(n = .N, 
        mn = min(target_end_date),  
        y = max(target_end_date)), 
    by = .(model_name, round, scenario_id)] %>%
  .[, .(n = mean(n)), by = .(model_name, round)] %>%
  dcast(round ~ model_name, value.var = "n")


# plot
rbindlist(list(
  # average WIS by round
  WIS %>%
    #.[ round <= 12] %>%
    .[model_name %in% c("null_fh", "null_gam", "null_naive", "Ensemble_LOP")] %>%
    .[, .(plaus_WIS = weighted.mean(WIS, plaus_weight*plaus_week)), by = .(target, target_end_date, location, round, model_name)] %>%
    .[, .(mean_WIS = mean(plaus_WIS, na.rm = TRUE)), by = .(target, round, model_name)],
  # average WIS across all rounds
  WIS %>%
    #.[ round <= 12] %>%
    .[model_name %in% c("null_fh", "null_gam", "null_naive", "Ensemble_LOP")] %>%
    .[, .(plaus_WIS = weighted.mean(WIS, plaus_weight*plaus_week)), by = .(target, target_end_date, location, round, model_name)] %>%
    .[, .(mean_WIS = mean(plaus_WIS, na.rm = TRUE)), by = .(target, model_name)] %>%
    .[, round := "overall"]
), use.names = TRUE) %>%
  # calculate mean WIS of ensemble relative to each null
  .[, rel_WIS := mean_WIS[model_name == "Ensemble_LOP"]/mean_WIS, by = .(round, target)] %>%
  .[model_name != "Ensemble_LOP"] %>%
  # clean up for plotting
  .[, round := factor(round, levels = c(as.character(c(1:7,9,11:16)), "overall"))] %>%
  .[, xpos := as.numeric(round)] %>%
  .[, target := factor(target, levels = paste("inc", c("case", "hosp", "death")))] %>%
  .[, model_name := factor(model_name, levels = c("null_fh", "null_naive", "null_gam"))] %>%
  ggplot(aes(x = xpos, y = rel_WIS)) + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() + 
  facet_grid(cols = vars(target), rows = vars(model_name), 
             labeller = labeller(target = target_labs, 
                                 model_name = null_labs_simp), 
             switch = "y") + 
  scale_x_continuous(breaks = 1:15,
                     expand = c(0,0),
                     labels = c(round_labs[1:14], "overall"),
                     limits = c(0.5,15.5)) +
  scale_y_log10(name = "WIS of ensemble relative comparator") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.spacing.x = unit(0,"cm"),
        strip.background = element_blank(), 
        strip.placement = "outside")
ggsave("figures/null_models/WIS_vs_all_nulls_summary.pdf", width = 12, height = 6)




