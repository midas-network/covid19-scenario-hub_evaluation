library(ggplot2) 
library(RColorBrewer)
library(ggtext)

lims = range(proj[location == "US" &
                    round == 12 & 
                    !(substr(model_name,1,3) %in% c("Ens", "nul")) & 
                    target == "inc hosp" & 
                    quantile %in% c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)] %>% 
               pull(value))

proj[location == "US" &
       round == 12 & 
       !(substr(model_name,1,3) %in% c("Ens", "nul")) & 
       target == "inc hosp" & 
       quantile %in% c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)] %>%
  .[, quantile := paste0("Q", quantile*100)] %>%
  data.table::dcast(scenario_id + target_end_date + model_name ~ quantile, value.var = "value") %>%
  .[, scenario_letter := substr(scenario_id, 1,1)] %>%
  ggplot(aes(x = target_end_date)) + 
  geom_ribbon(aes(ymin = Q1, ymax = Q99, fill = model_name), alpha = 0.1) + 
  geom_ribbon(aes(ymin = Q5, ymax = Q95, fill = model_name), alpha = 0.2) + 
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = model_name), alpha = 0.3) + 
  geom_line(aes(y = Q50, color = model_name)) + 
  geom_text(data = data.frame(scenario_letter = LETTERS[1:4]), 
                       aes(x = Inf, y = Inf, label = paste0("scenario ", scenario_letter, " ")), size = 3, hjust = 1, vjust = 1) + 
  facet_wrap(vars(scenario_letter)) + 
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(expand = c(0,0), 
                     name = "projection horizon") +
  scale_y_continuous(expand = c(0,0), 
                     limits = lims,
                     name = "individual model projections") +
  theme_bw() + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.grid = element_blank(), 
        strip.background = element_blank(), 
        strip.text = element_blank())
ggsave("figures/process_indmods.pdf", width = 2.5, height = 2.5)


proj[location == "US" &
       round == 12 & 
       model_name == "Ensemble_LOP_untrimmed" & 
       target == "inc hosp" & 
       quantile %in% c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)] %>%
  .[, quantile := paste0("Q", quantile*100)] %>%
  data.table::dcast(scenario_id + target_end_date + model_name ~ quantile, value.var = "value") %>%
  .[, scenario_letter := substr(scenario_id, 1,1)] %>%
  ggplot(aes(x = target_end_date)) + 
  geom_ribbon(aes(ymin = Q1, ymax = Q99), alpha = 0.1) + 
  geom_ribbon(aes(ymin = Q5, ymax = Q95), alpha = 0.2) + 
  geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.3) + 
  geom_line(aes(y = Q50)) + 
  geom_text(data = data.frame(scenario_letter = LETTERS[1:4]), 
            aes(x = Inf, y = Inf, label = paste0("scenario ", scenario_letter, " ")), size = 3, hjust = 1, vjust = 1) + 
  facet_wrap(vars(scenario_letter)) + 
  scale_x_continuous(expand = c(0,0), 
                     name = "projection horizon") +
  scale_y_continuous(expand = c(0,0), 
                     limits = lims,
                     name = "ensemble projection") +
  theme_bw() + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.grid = element_blank(), 
        strip.background = element_blank(), 
        strip.text = element_blank())
ggsave("figures/process_ensmod.pdf", width = 2.5, height = 2.5)

date_lims = unlist(proj[round == 2] %>% pull(target_end_date) %>% range())
proj %>%
  .[target == "inc hosp" & 
      round == 2 &
      location == "US" & 
      target_end_date > "2020-09-01" & 
      #proj_week <= 12 &
      model_name == "Ensemble_LOP" &
      quantile %in% c(0.025,0.5,0.975)] %>%
  .[, quantile := paste0("Q", quantile*1000)] %>%
  .[, plaus_flag := ifelse(plaus_weight > 0, 1, 0)] %>%
  data.table::dcast(scenario_id + round + target_end_date + obs + plaus_flag ~ quantile, value.var = "value") %>%
  ggplot(aes(x = target_end_date, y = obs)) +
  # add variant takeover dates
  geom_vline(data = data.frame(var = names(variant_takeover_date[2]), 
                               target_end_date = as.Date(variant_takeover_date[2], origin = "1970-01-01")), 
             aes(xintercept = target_end_date), 
             linetype = "dotted") + 
  # add projections
  geom_ribbon(aes(ymin = Q25, ymax = Q975, 
                  fill = "projection", 
                  group = scenario_id), alpha = 0.05) +
  geom_line(aes(y = Q500, 
                color = "projection",
                group = scenario_id, 
                alpha = as.factor(plaus_flag)), size = 0.75) +
  geom_richtext(data = data.frame(x = as.IDate("2021-06-19"), 
                                  y = proj[target == "inc hosp" & 
                                             round == 2 &
                                             location == "US" & 
                                             substr(scenario_id,1,1) == "B" &
                                             target_end_date == "2021-06-19" & 
                                             model_name == "Ensemble_LOP" &
                                             quantile == 0.5] %>% pull(value), 
                                  lab = "realistic"), 
                aes(x = x, y = y, label = lab), 
                angle = -30, color = "grey50", hjust = 0.75, label.size = NA, 
                label.padding = unit(0.01, "lines"),  size = 3) +
  # add observations
  geom_line(data = truth_data[target == "inc hosp" & location == "US"],
            aes(y = obs, color = "observation"), size = 0.25) +
  geom_point(data = truth_data[target == "inc hosp" & location == "US"],
            aes(y = obs, color = "observation")) +
  # add variant takeover date names
  coord_cartesian(ylim = c(0,3E5)) +
  guides(alpha = "none", fill = "none", linetype = "none") +
  scale_alpha_manual(values = c(0.3, 1)) +
  scale_color_manual(values = c("black", "grey50")) +
  scale_fill_manual(values = c("grey50")) +
  scale_x_date(expand = c(0,0), 
               name = "projection horizon",
               limits = date_lims) +
  scale_y_continuous(expand = c(0,0), 
                     name = "US incident hospitalizations") +
  theme_bw() + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0),
        legend.position = c(0.125,0.85),
        legend.title = element_blank(),
        panel.grid = element_blank())
ggsave("figures/process_eval.pdf", width = 5, height = 2.5)


