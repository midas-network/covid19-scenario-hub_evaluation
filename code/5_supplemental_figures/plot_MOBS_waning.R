library(ggplot2)
library(RColorBrewer)
library(scales)

proj[round == 13 &
       substr(model_name,1,3) == "MOB" &
       target == "inc hosp" & 
       location == "US" & 
       quantile %in% c(0.025, 0.5, 0.975)] %>% 
  .[, quantile := paste0("Q", quantile*1000)] %>%
  dcast(target_end_date + model_name + scenario_id ~ quantile) %>%
  .[, scenario_letter := paste("scenario", substr(scenario_id,1,1))] %>%
  ggplot(aes(x = target_end_date)) + 
  geom_ribbon(aes(ymin = Q25, ymax = Q975, fill = model_name), alpha = 0.2) + 
  geom_line(aes(y = Q500, color = model_name)) + 
  facet_grid(cols = vars(scenario_letter)) + 
  scale_color_manual(values = c("#D81B60", "#1E88E5")) +
  scale_fill_manual(values = c("#D81B60", "#1E88E5")) +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3), 
                     name = "US incident hospitalizations") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        legend.position = "none", 
        panel.grid.minor = element_blank(), 
        strip.background = element_blank())
ggsave("figures/performance_results/MOBS_waning_R13.pdf", width = 10, height = 3)
  
