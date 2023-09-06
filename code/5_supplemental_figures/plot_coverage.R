#### SETUP ---------------------------------------------------------------------
library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(mgcv)

# set base location of 
#cov <- load_scores("data-output/coverage")

# load objects to assist with plotting
#source("code/plot_setup.R")

#### PLOT COVERAGE OVER TIME BY ROUND ------------------------------------------
# plot 95% coverage over time 
cov[model_name == "Ensemble_LOP" & 
      substr(target,1,3) == "inc" & 
      alpha == 0.95] %>%
  .[!is.na(cov)] %>% #for later rounds with obs -- NOTE BELOWS DOESN"T MATCH VALUES_FROM_TEXT CALC
  .[, .(cov95 = sum(cov)/.N), by = .(round, target, target_end_date, model_name, scenario_id, plaus_weight)] %>%
  .[, W := weighted.mean(cov95, plaus_weight), 
    by = .(round, target, target_end_date, model_name)] %>%
  .[, scenario_letter := substr(scenario_id, 1,1)] %>%
  .[, scenario_id := NULL] %>%
  .[, plaus_weight := NULL] %>%
  data.table::dcast(round + target + target_end_date + model_name + 
                      W ~ scenario_letter, value.var = "cov95") %>%
  data.table::melt(c("round", "target", "target_end_date", "model_name")) %>%
  .[, target := factor(target, levels = c("inc case", "inc hosp", "inc death"))] %>%
  .[, first_date := min(target_end_date), by = .(round, target)] %>%
  .[, first_cov := value[target_end_date == first_date & variable == "W"], by = .(round, target)] %>%
  ggplot(aes(x = target_end_date, y = value)) +
  geom_hline(aes(yintercept = 0.95)) +
  # add variant takeover dates
  geom_vline(data = data.frame(var = names(variant_takeover_date), 
                               target_end_date = as.Date(variant_takeover_date, origin = "1970-01-01")), 
             aes(xintercept = target_end_date), 
             linetype = "dotted") + 
  # add variant takeover date names
  geom_text(data = data.frame(var = names(variant_takeover_date), 
                              target_end_date = as.Date(variant_takeover_date, origin = "1970-01-01")), 
            aes(x = target_end_date - 4, y = 1.1, label = var), 
            hjust = 1, vjust = 1) +
  geom_line(data = cov[model_name == "null_fh" & 
                         substr(target, 1,3) == "inc" & 
                         alpha == 0.95] %>%
              .[, .(cov95 = sum(cov)/.N),
                by = .(round, target, target_end_date)] %>%
              .[ ,target := factor(target, levels = c("inc case", "inc hosp", "inc death"))],
            aes(x = target_end_date,y = cov95),
            color = "darkgrey", size = 1.2) +
  geom_line(aes(color = as.factor(round), size = variable, alpha = variable, group = paste(round, target, variable))) +
  geom_point(aes(x = first_date, y = first_cov, color = as.factor(round)), size = 3) +
  geom_text(aes(x = first_date, y = first_cov, label = round), color = "white", size = 2) +
  facet_wrap(vars(target), nrow = 3, scales = "free", labeller = labeller(target = target_labs), strip.position = "left") + 
  labs(y = "95% coverage") +
  scale_alpha_manual(values = c(1, rep(0.35,4))) +
  scale_size_manual(values = c(1.3, rep(0.4, 4))) + 
  scale_x_date(date_labels = "%B %Y") +
  scale_y_continuous(breaks = c(0,0.5,0.95), 
                     expand = c(0,0.01),
                     labels = percent, 
                     limits = c(0,1.1)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        legend.position = "none", 
        strip.background = element_blank(), 
        strip.placement = "outside")
ggsave("figures/performance_results/cov_overtime.pdf", width = 8, height = 8)

# plot 50% coverage over time 
cov[model_name == "Ensemble_LOP" & 
      substr(target,1,3) == "inc" & 
      alpha == 0.5] %>%
  .[!is.na(cov)] %>% #for later rounds with obs
  .[, .(cov50 = sum(cov)/.N), by = .(round, target, target_end_date, model_name, scenario_id, plaus_weight)] %>%
  .[, W := weighted.mean(cov50, plaus_weight), 
    by = .(round, target, target_end_date, model_name)] %>%
  .[, scenario_letter := substr(scenario_id, 1,1)] %>%
  .[, scenario_id := NULL] %>%
  .[, plaus_weight := NULL] %>%
  data.table::dcast(round + target + target_end_date + model_name + 
                      W ~ scenario_letter, value.var = "cov50") %>%
  data.table::melt(c("round", "target", "target_end_date", "model_name")) %>%
  .[, target := factor(target, levels = c("inc case", "inc hosp", "inc death"))] %>%
  .[, first_date := min(target_end_date), by = .(round, target)] %>%
  .[, first_cov := value[target_end_date == first_date & variable == "W"], by = .(round, target)] %>%
  ggplot(aes(x = target_end_date, y = value)) +
  geom_hline(aes(yintercept = 0.5)) +
  # add variant takeover dates
  geom_vline(data = data.frame(var = names(variant_takeover_date), 
                               target_end_date = as.Date(variant_takeover_date, origin = "1970-01-01")), 
             aes(xintercept = target_end_date), 
             linetype = "dotted") + 
  # add variant takeover date names
  geom_text(data = data.frame(var = names(variant_takeover_date), 
                              target_end_date = as.Date(variant_takeover_date, origin = "1970-01-01")), 
            aes(x = target_end_date - 4, y = 1.1, label = var), 
            hjust = 1, vjust = 1) +
  geom_line(data = cov[model_name == "null_fh" & 
                         substr(target, 1,3) == "inc" & 
                         alpha == 0.5] %>%
              .[, .(cov50 = sum(cov)/.N),
                by = .(round, target, target_end_date)] %>%
              .[ ,target := factor(target, levels = c("inc case", "inc hosp", "inc death"))],
            aes(x = target_end_date,y = cov50),
            color = "darkgrey", size = 1.2) +
  geom_line(aes(color = as.factor(round), size = variable, alpha = variable, group = paste(round, target, variable))) +
  geom_point(aes(x = first_date, y = first_cov, color = as.factor(round)), size = 3) +
  geom_text(aes(x = first_date, y = first_cov, label = round), color = "white", size = 2) +
  facet_wrap(vars(target), nrow = 3, scales = "free", labeller = labeller(target = target_labs), strip.position = "left") + 
  labs(y = "50% coverage") +
  scale_alpha_manual(values = c(1, rep(0.35,4))) +
  scale_size_manual(values = c(1.3, rep(0.4, 4))) + 
  scale_x_date(date_labels = "%B %Y") +
  scale_y_continuous(breaks = c(0,0.5,0.95), 
                     expand = c(0,0.01),
                     labels = percent, 
                     limits = c(0,1.1)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        panel.grid = element_blank(), 
        legend.position = "none", 
        strip.background = element_blank(), 
        strip.placement = "outside")
ggsave("figures/performance_results/cov50_overtime.pdf", width = 8, height = 8)

#### COVERAGE BY PROJECTION WEEK - ENSEMBLE ONLY -------------------------------
d_plaus <- cov %>%
  .[!is.na(cov)] %>% #for later rounds with obs
  .[model_name == "Ensemble_LOP" & 
      alpha == 0.95] %>%
  .[, .(cov95 = sum(cov*plaus_weight/sum(plaus_weight))), by = .(round, target, target_end_date, model_name)]

d_all <- cov %>%
  .[!is.na(cov)] %>% #for later rounds with obs
  .[model_name == "Ensemble_LOP" & 
      alpha == 0.95] %>%
  .[, .(cov95 = sum(cov)/.N), by = .(round, target, target_end_date, model_name, scenario_id)] %>%
  .[, scenario_id := NULL]
  
d <- rbindlist(list(d_plaus %>% 
                      .[, plaus_flag := "plausible projections"], 
                    d_all %>% 
                      .[, plaus_flag := "all projections"])) %>%
  .[, proj_wk := (target_end_date - min(target_end_date))/7+1, by = "round"] %>%
  .[, round := as.factor(round)] %>%
  .[, target := factor(target, levels = paste("inc", c("case", "hosp", "death")))] %>%
  .[, plaus_flag := factor(plaus_flag, levels = rev(paste(c("all", "plausible"), "projections")))]

rm(d_plaus, d_all)

ggplot(data = d,
       aes(x = proj_wk, y = cov95)) +
  geom_hline(aes(yintercept = 0.95)) +
  geom_point(aes(color = as.factor(round)), alpha = 0.4) + 
  geom_smooth(formula = y ~ splines::bs(x), method = "gam", color = "darkgrey") +
  facet_grid(rows = vars(plaus_flag), #cols = vars(scenario), 
             cols = vars(target),
             labeller = labeller(target = target_labs)) +
  labs(x = "projection horizon (weeks)", y = "SMH ensemble 95% coverage") +
  coord_cartesian(xlim = c(0,26), 
                  ylim = c(0,1)) +
  scale_y_continuous(breaks = c(0,0.5,0.95), labels = percent) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank())
ggsave("figures/performance_results/cov95_projwk_26wks.pdf", width = 8, height = 4)

ggplot(data = d,
       aes(x = proj_wk, y = abs(cov95-0.95))) +
  geom_point(aes(color = as.factor(round)), alpha = 0.4) + 
  geom_smooth(formula = y ~ splines::bs(x), method = "gam", color = "darkgrey") +
  facet_grid(rows = vars(plaus_flag), 
             cols = vars(target),
             labeller = labeller(target = target_labs)) +
  labs(x = "projection horizon (weeks)", y = "|SMH ensemble 95% coverage - 95%|") +
  coord_cartesian(xlim = c(0,26), 
                  ylim = c(0,1)) +
  scale_y_continuous(breaks = c(0,0.5,0.95), labels = percent) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank())
ggsave("figures/performance_results/cov95_diff_projwk_26wks.pdf", width = 8, height = 4)


d_plaus <- cov %>%
  .[!is.na(cov)] %>% #for later rounds with obs
  .[model_name == "Ensemble_LOP" & 
      alpha == 0.5] %>%
  .[, .(cov50 = sum(cov*plaus_weight/sum(plaus_weight))), by = .(round, target, target_end_date, model_name)]

d_all <- cov %>%
  .[!is.na(cov)] %>% #for later rounds with obs
  .[model_name == "Ensemble_LOP" & 
      alpha == 0.5] %>%
  .[, .(cov50 = sum(cov)/.N), by = .(round, target, target_end_date, model_name, scenario_id)] %>%
  .[, scenario_id := NULL]

d2 <- rbindlist(list(d_plaus %>% 
                      .[, plaus_flag := "plausible projections"], 
                    d_all %>% 
                      .[, plaus_flag := "all projections"])) %>%
  .[, proj_wk := (target_end_date - min(target_end_date))/7+1, by = "round"] %>%
  .[, round := as.factor(round)] %>%
  .[, target := factor(target, levels = paste("inc", c("case", "hosp", "death")))] %>%
  .[, plaus_flag := factor(plaus_flag, levels = rev(paste(c("all", "plausible"), "projections")))]

rm(d_plaus, d_all)

ggplot(data = d2,
       aes(x = proj_wk, y = cov50)) +
  geom_hline(aes(yintercept = 0.5)) +
  geom_point(aes(color = as.factor(round)), alpha = 0.4) + 
  geom_smooth(formula = y ~ splines::bs(x), method = "gam", color = "darkgrey") +
  facet_grid(rows = vars(plaus_flag), #cols = vars(scenario), 
             cols = vars(target),
             labeller = labeller(target = target_labs)) +
  labs(x = "projection horizon (weeks)", y = "SMH ensemble 50% coverage") +
  coord_cartesian(xlim = c(0,26), 
                  ylim = c(0,1)) +
  scale_y_continuous(breaks = c(0,0.5,0.95), labels = percent) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank())
ggsave("figures/performance_results/cov50_projwk_26 wks.pdf", width = 8, height = 4)

ggplot(data = d2,
       aes(x = proj_wk, y = abs(cov50-0.5))) +
  geom_point(aes(color = as.factor(round)), alpha = 0.4) + 
  geom_smooth(formula = y ~ splines::bs(x), method = "gam", color = "darkgrey") +
  facet_grid(rows = vars(plaus_flag), #cols = vars(scenario), 
             cols = vars(target),
             labeller = labeller(target = target_labs)) +
  labs(x = "projection horizon (weeks)", y = "|SMH ensemble 50% coverage - 50%|") +
  coord_cartesian(xlim = c(0,26), 
                  ylim = c(0,0.5)) +
  scale_y_continuous(breaks = c(0,0.25,0.5), labels = percent) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank())
ggsave("figures/performance_results/cov50_diff_projwk_26wks.pdf", width = 8, height = 4)

#### Q-Q PLOTS -----------------------------------------------------------------
cov %>%
  .[!is.na(cov)] %>%
  .[!(model_name %in% c("Ensemble_LOP_untrimmed", "Ensemble")) & 
      substr(model_name,1,4) != "null"] %>%
  .[, model_name := ifelse(model_name == "MOBS_NEU-GLEAM_COVID_OT", "MOBS_NEU-GLEAM_COVID", 
                           ifelse(model_name == "USC-SIkJalpha-update", "USC-SIkJalpha", model_name))] %>% 
  .[, .(cov = sum(cov*plaus_weight)/sum(plaus_weight)), 
    by = .(round, model_name, alpha)] %>% 
  .[model_name_key_nat, on = .(model_name)] %>%
  .[, key := factor(key, levels = c("Ens", LETTERS[1:10]))] %>%
  ggplot(aes(x = alpha, y = cov, color = as.factor(round))) + 
  facet_wrap(vars(key)) +
  geom_abline(color = "black", size = 1.2) +
  geom_line(size = 0.7) + 
  scale_x_continuous(expand = c(0,0),
                     labels = percent,
                     limits = c(0,1), 
                     name = "expected coverage") +
  scale_y_continuous(expand = c(0,0),
                     labels = percent,
                     limits = c(0,1), 
                     name = "actual coverage") +
  theme_bw() + 
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(),
        strip.background = element_blank()
        )
ggsave("figures/performance_results/QQplot_allmodels.pdf", width = 8, height = 6)

#### PLOT COVERAGE FOR INDIVIDUAL MODELS OVER LOCATIONS/WEEKS ----------------
include_projs <- WIS[location == "US" & 
                       !(model_name %in% c("Ensemble", "Ensemble_LOP_untrimmed", 
                                           "null_gam", "null_fh", "null_naive")), 
                     .(model_name, round)] %>% unique()
include_projs <- paste(include_projs$model_name, include_projs$round, sep = "_")

for(i in paste("inc", c("case", "hosp", "death"))){
  cov %>% 
    .[!is.na(cov) & 
        alpha == 0.95 & 
        target == i] %>%
    .[paste(model_name, round, sep = "_") %in% include_projs] %>%
    .[, model_name := ifelse(model_name == "MOBS_NEU-GLEAM_COVID_OT", "MOBS_NEU-GLEAM_COVID", 
                             ifelse(model_name == "USC-SIkJalpha-update", "USC-SIkJalpha", model_name))] %>% 
    .[, .(cov95 = sum(cov*plaus_mod)/sum(plaus_mod)), by = .(round, target, model_name, scenario_id, plaus_scenario)] %>%
    .[model_name_key_nat, on = .(model_name)] %>%
    .[, scenario_letter := substr(scenario_id, 1,1)] %>%
    .[, key := factor(key, levels = c("Ens", LETTERS[1:10]))] %>%
    .[, xpos := as.numeric(as.factor(round)) + as.numeric(as.factor(scenario_letter))/5 - 0.5 ] %>%
    .[, plaus_flag := ifelse(plaus_scenario >0, 1, 0)] %>%
    ggplot(aes(x = xpos, y = cov95, color = as.factor(plaus_flag))) + 
    geom_hline(yintercept = 0.95) +
    geom_point(size = 2) + 
    geom_text(aes(label = scenario_letter), color = "white", size = 1.5) +
    facet_grid(rows = vars(key), 
               labeller = labeller(target = target_labs), 
               switch = "y") +
    labs(y = paste("95% coverage for", target_labs[i])) +
    scale_color_manual(values = c("black", "darkorange")) +
    scale_x_continuous(breaks = 1:14,
                       expand = c(0,0),
                       labels = round_labs[1:14],
                       limits = c(0.5,14.5)) +
    scale_y_continuous(breaks = c(0,0.5,0.95), labels = percent) + 
    theme_bw() + 
    theme(axis.ticks.x = element_blank(), 
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing.x = unit(0,"cm"), 
          strip.background = element_blank(),
          strip.placement = "outside")
  ggsave(paste0("figures/performance_results/cov_indmods_",substr(i, 5, nchar(i)),".pdf"), width = 8, height = 9)
}


#### BRACKETING ----------------------------------------------------------------
scenario_cov <- proj[quantile %in% c(0.025, 0.975)] %>%
  .[, quantile:=paste0("Q", quantile*1000)] %>%
  data.table::dcast(round + scenario_name + target + target_end_date + 
                      location + model_name + location_name + obs ~ quantile,
                    value.var = "value") %>%
  # find min lower and max upper across scenarios
  .[, .(Q25 = min(Q25), 
        Q975 = max(Q975)), 
    by = .(round, target, target_end_date, location, model_name, location_name, obs)] %>%
  # calculate "coverage"
  .[, cov_flag := ifelse(obs <= Q975 & obs >= Q25, 1, 0)]

# first plot the "bracketing intervals"
p1 <- proj %>% 
  .[location == "US" & 
      model_name == "Ensemble_LOP" & 
      target == "inc hosp" & 
      quantile %in% c(0.025, 0.975)] %>%
  .[, quantile:=paste0("Q", quantile*1000)] %>%
  data.table::dcast(round + scenario_name + scenario_id + target + target_end_date + 
                      location + model_name + location_name + obs ~ quantile,
                    value.var = "value") %>%
  .[, mn := ifelse(Q25 == min(Q25), 1, 0), by = .(target_end_date, round)] %>%
  .[, mx := ifelse(Q975 == max(Q975), 1, 0), by = .(target_end_date, round)] %>%
  ggplot(aes(x = target_end_date)) + 
  geom_ribbon(aes(ymin = Q25, ymax = Q975, 
                  group = interaction(round, scenario_id), 
                  fill = as.factor(round)), alpha = 0.1) +
  # add observations
  geom_line(aes(y = obs)) +
  coord_cartesian(ylim = c(-0.1,5E5)) +
  #scale_size_manual(values = c(0,0.8)) +
  scale_x_date(date_labels = "%B %Y") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3),
                     expand = c(0,0),
                     name = "US incident hospitalizations\n(ensemble projection intervals)") +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        legend.position = "none")
p2 <- scenario_cov %>% 
  .[location == "US" & model_name == "Ensemble_LOP" & target == "inc hosp"] %>%
  ggplot(aes(x = target_end_date)) + 
  # add coverage ribbons
  geom_ribbon(aes(ymin = Q25, ymax = Q975, fill = as.factor(round)), alpha = 0.3) +
  geom_line(aes(y = Q25, color = as.factor(round)), size = 0.8) +
  geom_line(aes(y = Q975, color = as.factor(round)), size = 0.8) +
  # add observations
  geom_line(aes(y = obs)) +
  coord_cartesian(ylim = c(0,5E5)) +
  scale_x_date(date_labels = "%B %Y") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3),
                     expand = c(0,0),
                     name = "US incident hospitalizations\n(bracketing intervals)") +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        legend.position = "none")



p3 <- scenario_cov %>% 
  .[, .(cov = sum(cov_flag)/.N), 
    by = .(round, model_name, target_end_date, target)] %>%
  .[model_name == "Ensemble_LOP" & substr(target,1,3) == "inc"] %>%
  .[, target := factor(target, levels = c("inc case", "inc hosp", "inc death"))] %>%
  .[, first_date := min(target_end_date), by = .(round, target)] %>%
  .[, first_cov := cov[target_end_date == first_date], by = .(round, target)] %>%
  ggplot(aes(x = target_end_date, y = cov, color = as.factor(round))) + 
  geom_line(aes(color = as.factor(round), group = paste(round, target)), size = 1.2) +
  geom_point(aes(x = first_date, y = first_cov, color = as.factor(round)), size = 3) +
  geom_text(aes(x = first_date, y = first_cov, label = round), color = "white", size = 2) +
  facet_grid(rows = vars(target), 
             labeller = labeller(target = target_labs), 
             switch = "y") +
  scale_x_date(date_labels = "%B %Y") +
  scale_y_continuous(labels = percent,
                     limits = c(0,1),
                     name = "scenario coverage (% bracketing observations)") +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        legend.position = "none", 
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside" 
  )

p <- plot_grid(
  plot_grid(p1,p2, ncol = 1, labels = LETTERS[1:2]), 
  p3, 
  labels = c(NA,LETTERS[3]),
  nrow = 1)
ggsave("figures/performance_results/supp_bracketing.pdf", p, width = 10, height = 6)

