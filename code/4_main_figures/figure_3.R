#### SETUP ---------------------------------------------------------------------
library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(scales)

# load WIS
WIS <- SMHEvaluationUtils::load_scores("data-output/WIS")
# load coverage 
cov <- SMHEvaluationUtils::load_scores("data-output/coverage")

# load individual model skill scores
skill_byround <- setDT(read.csv("data-output/skill-score/skillscore_indmods.csv")) #%>%
  # filter to FH null model and only rounds before 12
  #.[!(round %in% as.character(13:15))]


# load objects to assist with plotting
source("code/plot_setup.R")

# load variant takeover dates
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

# plausible scenarios
WIS <- SMHEvaluationUtils::add_plaus_weight(proj = WIS,
                                            variant_takeover = variant_takeover_date, 
                                            modelname_round_weight = model_exclusions,
                                            keep_flags = TRUE, 
                                            p = "data-raw/data-scenarios/MostPlausibleScenarios.csv")
cov <- SMHEvaluationUtils::add_plaus_weight(proj = cov,
                                            variant_takeover = variant_takeover_date, 
                                            modelname_round_weight = model_exclusions,
                                            keep_flags = TRUE, 
                                            p = "data-raw/data-scenarios/MostPlausibleScenarios.csv")

bs <- 7

#### PANEL A: COVERAGE OVER TIME -----------------------------------------------
cov_plot <- cov[model_name == "Ensemble_LOP" &
                  alpha == 0.95 &
      target == "inc hosp"] %>%
  .[, .(cov95 = sum(cov)/.N), by = .(round, target, target_end_date, model_name, scenario_id, plaus_weight, plaus_week)] %>%
  .[, scenario_letter := substr(scenario_id, 1,1)] %>%
  .[, target := factor(target, levels = c("inc case", "inc hosp", "inc death"))] %>%
  .[, plaus_flag := ifelse((plaus_week * plaus_weight) > 0, 1, 0)] %>% 
  .[, first_date := min(target_end_date), by = .(round, target)] %>%
  .[, first_cov := mean(cov95[target_end_date == first_date & plaus_flag > 0]), by = .(round, target)] %>%
  .[!is.na(cov95)] 

pA <- cov[model_name == "Ensemble_LOP" &
            alpha == 0.95 &
            target == "inc hosp"] %>%
  .[, .(cov95 = sum(cov)/.N), by = .(round, target, target_end_date, model_name, scenario_id, plaus_weight, plaus_week)] %>%
  .[, scenario_letter := substr(scenario_id, 1,1)] %>%
  .[, target := factor(target, levels = c("inc case", "inc hosp", "inc death"))] %>%
  .[, plaus_flag := ifelse((plaus_week * plaus_weight) > 0, 1, 0)] %>% 
  .[, first_date := min(target_end_date), by = .(round, target)] %>%
  .[, first_cov := mean(cov95[target_end_date == first_date & plaus_flag > 0]), by = .(round, target)] %>%
  .[!is.na(cov95)] %>%
  ggplot(aes(x = target_end_date, y = cov95)) +
  # add variant takeover dates
  geom_vline(data = data.frame(var = names(variant_takeover_date), 
                               target_end_date = as.Date(variant_takeover_date, origin = "1970-01-01")), 
             aes(xintercept = target_end_date), 
             linetype = "dotted", size = 0.3) + 
  # add variant takeover date names
  geom_text(data = data.frame(var = names(variant_takeover_date), 
                              target_end_date = as.Date(variant_takeover_date, origin = "1970-01-01")), 
            aes(x = target_end_date - 4, y = 1.1, label = var), 
            hjust = 1, vjust = 1, size = 1.9) +
  # add ideal coverage
  geom_hline(aes(yintercept = 0.95), size = 0.3) +
  # add actual coverage for FH
  geom_line(data = cov[model_name == "null_fh" &
                         alpha == 0.95 &
                         target == "inc hosp"] %>%
              .[, .(cov95 = sum(cov)/.N),
                by = .(round, target, target_end_date)] %>%
              .[ ,target := factor(target, levels = c("inc case", "inc hosp", "inc death"))],
            aes(x = target_end_date,y = cov95), 
            color = "darkgrey", size = 0.75) +
  # add actual coverage for SMH
  geom_line(aes(color = as.factor(round), size = as.factor(plaus_flag), alpha = as.factor(plaus_flag), 
                group = paste(round, target, scenario_letter))) +
  # add round indicators
  geom_point(data = cov_plot[, .(round, first_date, first_cov)] %>% unique(), 
             aes(x = first_date, y = first_cov, color = as.factor(round)), size = 1.9) +
  geom_text(data = cov_plot[, .(round, first_date, first_cov)] %>% unique(), 
            aes(x = first_date, y = first_cov, label = round), color = "white", size = 1.25) +
  labs(y = "95% coverage for\nincident hospitalizations") +
  scale_alpha_manual(values = c(0.35,1)) +
  scale_x_date(date_labels = "%B %Y", 
               breaks = as.Date(c("2021-01-01", "2021-07-01", "2022-01-01", "2022-07-01", "2023-01-01"))) +
  scale_y_continuous(breaks = c(0,0.5,0.95), 
                     expand = c(0,0.01),
                     labels = percent, 
                     limits = c(0,1.1)) +
  scale_size_manual(values = c(0.25,0.8)) + 
  theme_bw(base_size = bs) +
  theme(axis.title.x = element_blank(), 
        panel.grid = element_blank(), 
        legend.position = "none")
# make a legend for panel A
l = ggplot(data= data.frame(x = 1:3, y = 1:3)) + 
  geom_line(aes(x=x, y = y, color = "4-week forecast"), size = 0.75) + 
  scale_color_manual(values =  "darkgrey") + 
  theme_bw() +
  theme(legend.title = element_blank(), 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0))
l <- get_legend(l)
pA <- pA + 
  annotation_custom(l, 
                    xmin = as.Date("2022-12-01"), 
                    xmax = Inf, 
                    ymin = 0.05, 
                    ymax = 0.075)


#### PANEL A: WIS OVER TIME ----------------------------------------------------
WIS_plot <-WIS %>%
  .[, target_end_date:=as.Date(target_end_date)] %>%
  .[!(substr(model_name,1,4) %in% c("null")) & !(model_name %in% c("Ensemble", "Ensemble_LOP_untrimmed"))] %>% #round <= 12 & 
  .[target == "inc hosp"] %>%
  # normalize WIS by round, target, location (do not group by scenario or model)
  .[, WIS_SD := sd(WIS, na.rm = TRUE), by = .(target, location, target_end_date, round)] %>%
  .[, WIS_norm := WIS/WIS_SD] %>%
  .[, .(WIS_norm = mean(WIS_norm, na.rm = TRUE)), by = .(round, target, target_end_date, model_name, scenario_id, plaus_weight, plaus_week)] %>%
  .[, plaus_flag := ifelse((plaus_week*plaus_weight) > 0, 1, 0)] %>% 
  .[model_name == "Ensemble_LOP"] %>%
  .[, first_date := min(target_end_date), by = .(round, target)] %>%
  .[, first_cov := max(WIS_norm[target_end_date == first_date & plaus_flag > 0]), by = .(round, target)] %>%
  .[!is.na(WIS_norm)] 

pB <- WIS_plot %>%
  ggplot(aes(x = target_end_date, y  = WIS_norm)) +
  # add variant takeover dates
  geom_vline(data = data.frame(var = names(variant_takeover_date), 
                               target_end_date = as.Date(variant_takeover_date, origin = "1970-01-01")), 
             aes(xintercept = target_end_date), 
             linetype = "dotted", size = 0.3) + 
  # add relative WIS values
  geom_line(data = WIS %>%
              .[, target_end_date:=as.Date(target_end_date)] %>%
              .[!(model_name %in% c("Ensemble_LOP_untrimmed", "Ensemble", "null_gam"))] %>%
              .[target == "inc hosp"] %>%
              # normalize WIS by round, target, location (do not group by scenario or model)
              .[, WIS_SD := sd(WIS, na.rm = TRUE), by = .(target, location, target_end_date, round)] %>%
              .[, WIS_norm := WIS/WIS_SD] %>%
              .[, .(WIS_norm = mean(WIS_norm, na.rm = TRUE)), by = .(round, target, target_end_date, model_name, scenario_id)] %>%
              # plot smallest WIS_norm when there are multiple rounds at one week
              .[, .(WIS_norm = min(WIS_norm)), by = .(target, target_end_date, model_name, scenario_id)] %>%
              .[model_name == "null_fh"],
            color = "darkgrey", size = 0.75) +
  geom_line(aes(size = as.factor(plaus_flag), 
                alpha = as.factor(plaus_flag),
                group = interaction(round,scenario_id), 
                color = as.factor(round))) +
  geom_point(data = WIS_plot[, .(round, first_date, first_cov)] %>% unique(), 
             aes(x = first_date, y = first_cov, color = as.factor(round)), size = 1.9) +
  geom_text(data = WIS_plot[, .(round, first_date, first_cov)] %>% unique(), 
            aes(x = first_date, y = first_cov, label = round), color = "white", size = 1.25) +
  labs(y = "average normalized WIS for\nincident hospitalizations") +
  #coord_cartesian(ylim = c(0,2)) +
  scale_alpha_manual(values = c(0.35, 1)) +
  scale_size_manual(values = c(0.25,0.8)) +  
  scale_x_date(date_labels = "%B %Y", 
               breaks = as.Date(c("2021-01-01", "2021-07-01", "2022-01-01", "2022-07-01", "2023-01-01"))) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw(base_size = bs) +
  theme(axis.title.x = element_blank(), 
        panel.grid = element_blank(), 
        legend.position = "none")


#### PANEL C: INDIVIDUAL MODEL SKILL SCORE -------------------------------------
color_lims <- c(-1,1)*2*sd(unlist(log(skill_byround[, .(skill)])), na.rm = TRUE)

pC <- skill_byround[setDT(model_name_key_nat), on = "model_name" ] %>%
  .[, cutoff := 2*sd(log(skill), na.rm = TRUE)] %>%
  .[, ":=" (round = factor(round, levels = rev(c(1:7, 9, 11:16, "overall"))),
            target = factor(target, levels = c("inc case", "inc hosp", "inc death")),
            key = factor(key, levels = c("Ens", LETTERS[1:(nrow(model_name_key)-1)])),
            fill_val = ifelse(log(skill) > cutoff, cutoff, 
                              ifelse(log(skill) < -cutoff, -cutoff,
                                     log(skill))))] %>%
  .[!is.na(target) ] %>% #& !is.na(null_model)
  ggplot(aes(x = key, y = round)) + 
  geom_tile(aes(fill = fill_val), size = 0.3) + 
  geom_hline(aes(yintercept = 1.5), size = 0.3) +
  geom_text(aes(label = round(skill, 1)), size = 1.6) + 
  facet_wrap(vars(target), nrow = 1,
             scales = "free", labeller = labeller(target = target_labs)) +
  labs(y = "round") +
  scale_fill_gradient2(low = brewer.pal(3, "BrBG")[3],
                       mid = brewer.pal(3, "BrBG")[2], 
                       high = brewer.pal(3, "BrBG")[1], 
                       midpoint = 0, 
                       breaks = c(-1,0,1), 
                       labels = c("better than average", "average","worse than average")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme_bw(base_size = bs) +
  theme(axis.title.x = element_blank(), 
        legend.key.width = unit(0.95, "cm"),
        legend.key.height = unit(0.32, "cm"),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid = element_blank(), 
        strip.background = element_blank())

#### COMBINE INTO MULTI-PANEL FIGURE -------------------------------------------
plot_grid(pA, pB, pC, ncol = 1, 
          align = "v", axis = "l", 
          labels = letters[1:3], 
          label_size = 7,
          rel_heights = c(0.3,0.3,0.45))
# plot_grid(pAB, pC, labels = c(NA, "C"), 
#           rel_widths = c(0.7, 0.3))
ggsave("figures/main_figures/figure3.pdf", width = 6.3, height = 5.1)




