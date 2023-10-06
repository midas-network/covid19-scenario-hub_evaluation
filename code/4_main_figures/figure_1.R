#### PREAMBLE ------------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(tidytext)
library(RColorBrewer)
library(cowplot)
library(scales)

# load data
#source("code/prepare_projections.R")

source("code/0_setup_scenario_plausibility/define_variant_takeover.R")

scenario_info <- read.csv("data-raw/data-scenarios/round_dates.csv")
scenario_info <- scenario_info[,-1]
scenario_info[,-1] <- lapply(scenario_info[,-1], as.Date, origin="1970-01-01")
setDT(scenario_info)
scenario_info <- scenario_info %>%
  # add variant exclusions
  .[, variant_cutoff_date := as.Date(ifelse(round == 1, variant_takeover_date["alpha"], 
                                            ifelse(round %in% 2:5, variant_takeover_date["delta"],
                                                   ifelse(round %in% 6:10, variant_takeover_date["omicron"], NA))), origin = "1970-01-01")] %>% 
  .[, projection_end_date_new := min(projection_end_date, variant_cutoff_date, na.rm = TRUE), by = .(round)] %>%
  .[, variant_cutoff_date := NULL] %>%
  .[, projection_start_date_mmwr := MMWRweek2Date(
    MMWRyear = MMWRweek(projection_start_date)$MMWRyear,
    MMWRweek = MMWRweek(projection_start_date)$MMWRweek,
    MMWRday = 7
  )] %>% 
  .[, projection_end_date_mmwr := MMWRweek2Date(
    MMWRyear = MMWRweek(projection_end_date)$MMWRyear,
    MMWRweek = MMWRweek(projection_end_date)$MMWRweek,
    MMWRday = 7
  )] %>% 
  .[, projection_end_date_new_mmwr := MMWRweek2Date(
    MMWRyear = MMWRweek(projection_end_date_new)$MMWRyear,
    MMWRweek = MMWRweek(projection_end_date_new)$MMWRweek,
    MMWRday = 7
  )]
setDT(scenario_info)

# define information for each round
table_details = setDT(data.frame(round = 1:16, 
                                 round_name = paste("round", 1:16), 
                                 number_models = c(5,5,4,6,8,9,9,6,9,6,8,8,8,6,5,6), 
                                 projection_start_date = scenario_info$projection_start_date[1:16],
                                 projection_end_date = scenario_info$projection_end_date[1:16], 
                                 projection_end_date_trunc = scenario_info$projection_end_date_new[1:16], 
                                 turnaround_time = with(scenario_info[1:16,], ensemble_published-last_updated), 
                                 NPIs = c(rep("compliance", 5), rep(NA, 11)), 
                                 vacc = c(rep("supply", 4), rep("uptake", 3), NA, "childhood\nvaccination",NA, NA, NA, NA, rep("booster\nuptake",3)), 
                                 variant = c(NA, "emergence\n(alpha)", rep(NA, 3), 
                                             "transmissibility\n(delta)","transmissibility\n(delta)",NA, 
                                             "emergence\n(hypothetical)",NA, rep("severity/escape\n(omicron)",2), 
                                             rep("escape\n(hypothetical)",3),"escape\n(variant mix)"),
                                 waning = c(rep(NA, 12), "waning",rep(NA,3))))
table_details$round_name = with(table_details, ifelse(round %in% c(8,10), paste0(round_name,": internal only"), round_name))

table_details2 = setDT(data.frame(round = 1:16, 
                                 round_name = paste("round", 1:16), 
                                 number_models = c(5,5,4,6,8,9,9,6,9,6,8,8,8,6,5,6), 
                                 projection_start_date = scenario_info$projection_start_date[1:16],
                                 projection_end_date = scenario_info$projection_end_date[1:16], 
                                 projection_end_date_trunc = scenario_info$projection_end_date_new[1:16], 
                                 turnaround_time = with(scenario_info[1:16,], ensemble_published-last_updated), 
                                 NPIs = c(rep("compliance", 5), rep(NA, 11)), 
                                 vacc = c(rep("supply", 4), rep("uptake", 3), NA, "childhood\nvaccination",NA, NA, NA, NA, rep("uptake",3)), 
                                 variant = c(NA, "alpha", rep(NA, 3), 
                                             "delta","delta",NA, 
                                             "hypothetical",NA, rep("omicron",2), 
                                             rep("hypothetical",3),"variant mix"),
                                 waning = c(rep(NA, 12), "waning",rep(NA,3))))
table_details2$round_name = with(table_details, ifelse(round %in% c(8,10), paste0(round_name,": internal only"), round_name))

date_lims = with(table_details[c(1:7,9,11:16),], as.Date(c(min(projection_start_date), max(projection_end_date))))

#### PANEL A: ALL PROJECTIONS + OBSERVATIONS -----------------------------------
nb.cols <- 16
pal_ <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)
bs <- 7

pA = proj %>%
  .[!(round %in% c(8,10))] %>%
  #.[, proj_week := (target_end_date - min(target_end_date))/7 +1, by = .(scenario_id, round)] %>%
  .[target == "inc hosp" & 
      location == "US" & 
      target_end_date > "2020-09-01" & 
      #proj_week <= 12 &
      model_name == "Ensemble_LOP" &
      quantile %in% c(0.025,0.5,0.975)] %>%
  .[, quantile := paste0("Q", quantile*1000)] %>%
  data.table::dcast(scenario_id + round + target_end_date + obs + plaus_week + plaus_weight ~ quantile, value.var = "value") %>%
  ggplot(aes(x = target_end_date, y = obs)) +
  # add variant takeover dates
  geom_vline(data = data.frame(var = names(variant_takeover_date), 
                               target_end_date = as.Date(variant_takeover_date, origin = "1970-01-01")), 
             aes(xintercept = target_end_date), 
             linetype = "dotted", size = 0.3) + 
  # add projections
  geom_ribbon(aes(ymin = Q25, ymax = Q975, fill = as.factor(round), group = paste(round,scenario_id)), alpha = 0.03) +
  geom_line(aes(y = Q500, color = as.factor(round), group = interaction(round, scenario_id), alpha = as.factor(plaus_week)), size = 0.5) +
  # add observations
  geom_line(data = truth_data[target == "inc hosp" & location == "US"],
            aes(y = obs), size = 0.3) +
  # add variant takeover date names
  geom_text(data = data.frame(var = names(variant_takeover_date), 
                              target_end_date = as.Date(variant_takeover_date, origin = "1970-01-01")), 
            aes(x = target_end_date - 4, y = Inf, label = var), 
            hjust = 1, vjust = 1, size = 1.9) +
  coord_cartesian(ylim = c(0,3E5)) +
  scale_alpha_manual(values = c(0.2, 1)) +
  # scale_colour_smoothrainbow() +
  # scale_fill_smoothrainbow() +
  #scale_fill_manual(values = pal_2) +
  #scale_color_manual(values = pal_2) +
  scale_x_date(date_labels = "%B %Y",
               #expand = c(0,0),
               limits = date_lims) +
  scale_y_continuous(labels = comma, 
                     name = "US incident hospitalizations") +
  theme_bw(base_size = bs) + 
  theme(axis.title.x = element_blank(), 
        legend.position = "none",
        panel.grid = element_blank())


#### PANEL B: PROJECTION TIMELINES ---------------------------------------------
pB = table_details %>%
  .[!(round %in% c(8,10))] %>%
  ggplot() + 
  geom_vline(data = data.frame(var = names(variant_takeover_date), 
                               target_end_date = as.Date(variant_takeover_date, origin = "1970-01-01")), 
             aes(xintercept = target_end_date), 
             linetype = "dotted", size = 0.3) + 
  # add round dates
  geom_segment(aes(x = projection_start_date, xend = projection_end_date_trunc,  
                   y = round, yend = round, color = as.factor(round)), size = 2.4)+
  geom_segment(aes(x = projection_end_date_trunc, xend = projection_end_date,  
                   y = round, yend = round, color = as.factor(round)), size = 2.4, alpha = 0.3)+
  geom_text(aes(x = projection_start_date + 7, y = round, label = round_name), 
            color = 'white', hjust = 0, size = 1.9) +
  # add internal rounds
  geom_label(data = table_details[round %in% c(8,10)], 
            aes(x = projection_start_date + 7, y = round, label = round_name), 
            color = 'black', fill = "white", hjust = 0, label.size = NA, size = 1.7) +
  scale_x_date(date_labels = "%B %Y",
               #expand = c(0,0),
               limits = date_lims) +
  scale_y_reverse() +
  theme_bw(base_size = bs) + 
  panel_border(color = "white") +
  theme(axis.line.x = element_line(color = "black"), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none", 
        panel.grid = element_blank())


#### PANEL C: TABLE WITH IMPLEMENTATION DETAILS --------------------------------
pC = table_details2 %>% 
  .[!(round %in% c(8,10))] %>%
  data.table::melt(c("round", "projection_start_date", "projection_end_date")) %>%
  .[ variable %in% c("round_name","number_models", "turnaround_time", "NPIs", "vacc", "variant")] %>%
  .[, variable := factor(variable, levels = rev(c("round_name","number_models", "turnaround_time", "NPIs", "vacc", "variant")))] %>%
  .[, rownum := .N:1, by = .(round)] %>%
  .[, fill := ifelse(variable == "round_name", value, NA)] %>%
  .[, fill := factor(fill, levels = paste("round", c(1:7,9,11:16)))] %>%
  .[, col := ifelse(is.na(fill), 0, 1)] %>%
  ggplot(aes(x = as.factor(round), y = rownum)) + 
  geom_tile(aes(x = as.factor(round), y = rownum, fill = as.factor(fill)), size = 0.25) +
  geom_vline(xintercept = seq(1.5,15.5, 1), color = "black", size = 0.3) + 
  geom_hline(yintercept = seq(1.5, 6.5,1), size = 0.3) +
  geom_text(aes(label = value, size = as.factor(col), color = as.factor(col))) +
  panel_border(color = "black") +
  scale_color_manual(values = c("black", "white")) + 
  scale_fill_discrete(na.value = "white") +
  scale_size_manual(values = c(1.75, 2.25)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(breaks = 1:6,
                     expand = c(0,0),
                     labels = rev(c("","number of\nmodels", "turnaround\ntime (days)", "NPI\nscenarios", "vaccination\nscenarios", "variant\nscenarios")) )+
  theme_bw(base_size = bs) + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(hjust = 0.5), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        legend.position = "none", 
        panel.grid = element_blank())

#### COMBINE INTO MULTI-PANEL PLOT ---------------------------------------------
plot_grid(pA, pB, pC,
          axis = "l", align = "v", 
          ncol = 1, 
          rel_heights = c(0.4,0.4,0.3), 
          labels = letters[1:3], 
          label_size = 7)
ggsave("figures/main_figures/figure1.pdf", width = 6.3, height = 4.45)


