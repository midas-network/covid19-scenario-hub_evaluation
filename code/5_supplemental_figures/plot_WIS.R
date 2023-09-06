#### SETUP ---------------------------------------------------------------------
library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(scales) 
library(SMHEvaluationUtils)
library(usmap)
library(ggpmisc)

# load WIS data
WIS <- load_scores("data-output/WIS")

# load objects to assist with plotting
source("code/plot_setup.R")

# add plaus weight
source("code/0_setup_scenario_plausibility/define_variant_takeover.R")
# a few fixes for model updates in later rounds (add to SMH EVALUATION)
# R13: MOBS_NEU-GLEAM_COVID and MOBS_NEU-GLEAM_COVID-OT get weight = 0.5
# R14: MOBS_NEU-GLEAM_COVID gets weight 0 and MOBS_NEU-GLEAM_COVID_OT gets weight 1
#      USC-SIkJalpha-update gets weight 0 and USC-SIkJalpha gets weight 1
model_exclusions <- data.frame(model_name = c("MOBS_NEU-GLEAM_COVID", 
                                              "MOBS_NEU-GLEAM_COVID_OT", 
                                              "MOBS_NEU-GLEAM_COVID", 
                                              "USC-SIkJalpha-update"), 
                               round = c(13,13,14,14), 
                               weight = c(0.5, 0.5, 0, 0))
WIS <- SMHEvaluationUtils::add_plaus_weight(proj = WIS,
                                            variant_takeover = variant_takeover_date, 
                                            modelname_round_weight = model_exclusions,
                                            keep_flags = TRUE)

# load locations
locations <- read.csv("data-raw/data-locations/locations.csv") %>%
  setDT() %>%
  .[!(location %in% c("60", "66", "69", "72", "74", "78"))]

#### PLOT ENSEMBLE WIS COMPARISONS ---------------------------------------------
rbindlist(list(
  # average WIS by round
  WIS %>%
    .[ substr(model_name,1,3) == "Ens"] %>%
    .[, .(plaus_WIS = weighted.mean(WIS, plaus_weight)), by = .(target, target_end_date, location, round, model_name)] %>%
    .[, .(mean_WIS = mean(plaus_WIS, na.rm = TRUE)), by = .(target, round, model_name)],
  # average WIS across all rounds
  WIS %>%
    .[ substr(model_name,1,3) == "Ens"] %>%
    .[, .(plaus_WIS = weighted.mean(WIS, plaus_weight)), by = .(target, target_end_date, location, round, model_name)] %>%
    .[, .(mean_WIS = mean(plaus_WIS, na.rm = TRUE)), by = .(target, model_name)] %>%
    .[, round := "overall"]
), use.names = TRUE) %>%
  # calculate mean WIS of ensemble relative to each null
  .[, rel_WIS := mean_WIS/mean_WIS[model_name == "Ensemble_LOP"], by = .(round, target)] %>%
  .[model_name != "Ensemble_LOP"] %>%
  # clean up for plotting
  .[, round := factor(round, levels = c(as.character(c(1:7,9,11:16)), "overall"))] %>%
  .[, xpos := as.numeric(round) + as.numeric(as.factor(model_name))/5-0.3] %>%
  .[, target := factor(target, levels = paste("inc", c("case", "hosp", "death")))] %>%
  ggplot(aes(x = xpos, y = rel_WIS, shape = model_name)) + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(size = 2) + 
  facet_grid(rows = vars(target),
             labeller = labeller(target = target_labs), 
             switch = "y") + 
  scale_shape_manual(values = c(16,17), 
                     labels = c("median-vincent", "untrimmed-LOP")) +
  scale_x_continuous(breaks = 1:15,
                     expand = c(0,0),
                     labels = c(round_labs[1:14], "overall"),
                     limits = c(0.5,15.5)) +
  scale_y_log10(name = "WIS of alternate ensembles relative to trimmed-LOP") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.spacing.x = unit(0,"cm"),
        strip.background = element_blank(), 
        strip.placement = "outside")
ggsave("figures/performance_results/WIS_all_ensembles.pdf", width = 8, height = 6)


#### PLOT NORMALIZED WIS FOR INDIVIDUAL MODELS ---------------------------------
include_projs <- WIS[location == "US" &
                       !(model_name %in% c("Ensemble", "Ensemble_LOP_untrimmed",
                                           "null_gam", "null_fh", "null_naive")),
                     .(model_name, round)] %>% unique()
include_projs <- paste(include_projs$model_name, include_projs$round, sep = "_")

for(i in paste("inc", c("case", "hosp", "death"))){
  WIS %>%
    .[target == i] %>%
    .[paste(model_name, round, sep = "_") %in% include_projs] %>%
    .[, model_name := ifelse(model_name == "MOBS_NEU-GLEAM_COVID_OT", "MOBS_NEU-GLEAM_COVID", 
                             ifelse(model_name == "USC-SIkJalpha-update", "USC-SIkJalpha", model_name))] %>% 
    # normalize WIS by round, target, location (do not group by scenario or model)
    .[, WIS_SD := sd(WIS, na.rm = TRUE), by = .(target, location, target_end_date, round)] %>%
    .[, WIS_norm := WIS/WIS_SD] %>%
    .[, .(WIS_norm = mean(WIS_norm, na.rm = TRUE)), by = .(round, target, model_name, scenario_id, plaus_scenario)] %>%
    .[model_name_key_nat, on = .(model_name)] %>%
    .[, scenario_letter := substr(scenario_id, 1,1)] %>%
    .[, key := factor(key, levels = c("Ens", LETTERS[1:9]))] %>%
    .[, xpos := as.numeric(as.factor(round)) + as.numeric(as.factor(scenario_letter))/5 - 0.5 ] %>%
    .[, plaus_flag := ifelse(plaus_scenario > 0, 1, 0)] %>% 
    ggplot(aes(x = xpos, y = WIS_norm, color = as.factor(plaus_flag))) + 
    geom_point(size = 2) + 
    geom_text(aes(label = scenario_letter), color = "white", size = 1.5) +
    facet_grid(rows = vars(key),
               switch = "y") +
    labs(y = paste("average normalized WIS for", target_labs[i])) +
    scale_color_manual(values = c("black", "darkorange")) +
    scale_x_continuous(breaks = 1:14,
                       expand = c(0,0),
                       labels = round_labs[1:14],
                       limits = c(0.5,14.5), 
                       name = "round") +
    #scale_y_continuous(breaks = c(0,0.5,0.95), labels = percent) + 
    theme_bw() + 
    theme(axis.ticks.x = element_blank(), 
          axis.title.x = element_blank(),
          legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing.x = unit(0,"cm"), 
          strip.background = element_blank(),
          strip.placement = "outside")
  ggsave(paste0("figures/performance_results/WIS_indmods_",substr(i, 5, nchar(i)),".pdf"), width = 8, height = 9)
}

#### PLOT NORMALIZED WIS BY STATE ----------------------------------------------
census_regions <- data.frame(abbreviation = c(.west_region, 
                                              .midwest_region, 
                                              .northeast_region, 
                                              .south_region, 
                                              "US"), 
                             census_region = c(rep("west", length(.west_region)), 
                                               rep("midwest", length(.midwest_region)),
                                               rep("northeast", length(.northeast_region)),
                                               rep("south", length(.south_region)), 
                                               "US")
                             )

WIS %>%
        .[, target_end_date:=as.Date(target_end_date)] %>%
        .[model_name == "Ensemble_LOP"] %>%
        # normalize WIS by round, target, location, target_end_date (do not group by scenario or model)
        .[, WIS_SD := sd(WIS, na.rm = TRUE), by = .(target, location, target_end_date, round)] %>%
        .[, WIS_norm := WIS/WIS_SD] %>%
        .[, .(plaus_WIS = weighted.mean(WIS_norm, plaus_weight, na.rm = TRUE)), by = .(target, target_end_date, location, round, model_name)] %>%
        .[, .(mean_WIS = mean(plaus_WIS, na.rm = TRUE)), by = .(target, location)] %>%
        .[locations, on = .(location)] %>%
        .[location != "US" & !is.na(target)] %>%
        .[, target := factor(target, levels = paste("inc", c("case", "hosp", "death")))] %>%
  # add census regions
  .[census_regions, on = .(abbreviation)] %>%
  .[!is.na(target)] %>%
  ggplot(aes(x = population, y = mean_WIS)) + 
  stat_poly_line(color = "black") +
  stat_poly_eq(label.x = "right") +
  geom_text(aes(label = abbreviation, color = as.factor(census_region)), size = 2) + 
  facet_grid(rows = vars(target), 
             labeller = labeller(target = target_labs), 
             #scales = "free", 
             switch = "y") + 
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(labels = label_number(suffix = "M", scale = 1e-6), 
                     name = "state population") +
  scale_y_continuous(name = "average normalized WIS") +
  theme_bw() + 
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        strip.background = element_blank(), 
        strip.placement = "outside")

ggsave("figures/performance_results/normwis_by_pop.pdf", width = 6, height = 6)

#### PLOT WIS COMPONENTS -------------------------------------------------------
# so we compare log(Ensemble_LOP/null_fh)
# so d_IS_disp < 0 when null > ens
# and > 0 when null < ens
WIS %>% 
  .[model_name %in% c("Ensemble_LOP", "null_fh")] %>%
  .[, .(plaus_IS_disp = weighted.mean(IS_disp, plaus_weight)),  
    by = .(target, target_end_date, location, round, model_name)] %>%
  data.table::dcast(target + target_end_date + location + round ~ model_name, value.var = "plaus_IS_disp") %>%
  .[, d_IS_disp := Ensemble_LOP/null_fh] %>%
  .[, .(m = mean(d_IS_disp, na.rm = TRUE), 
        lwr = quantile(d_IS_disp, 0.25, na.rm = TRUE), 
        upr = quantile(d_IS_disp, 0.75, na.rm = TRUE)), 
    by = .(target, target_end_date, round)] %>%
  .[, target := factor(target, levels =paste("inc", c("case", "hosp", "death")))] %>%
  ggplot(aes(x = target_end_date, y = m)) + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = as.factor(round)), alpha = 0.2) +
  geom_line(aes(color = as.factor(round))) +
  geom_point(aes(color = as.factor(round))) +
  facet_grid(rows = vars(target), 
             labeller = labeller(target = target_labs), 
             switch = "y") +
  scale_x_date(date_labels = "%B %Y") +
  scale_y_log10(name = "dispersion of ensemble vs. comparative model") +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        legend.position = "none", 
        strip.background = element_blank(), 
        strip.placement = "outside")
ggsave("figures/performance_results/wis_disp.pdf", width = 6, height = 6)
