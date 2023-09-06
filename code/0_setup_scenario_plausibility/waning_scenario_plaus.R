#### meta-analysis estimates from Bobrovitz et al. 
#    The Lancet Infectious Diseases https://doi.org/10.1016/S1473-3099(22)00801-5.

library(data.table)
library(ggplot2)
library(scales)
library(cowplot)
library(RColorBrewer)
library(dplyr)

# estimates of protection against re-infection (to match SMH scenarios in R13)
waning_est <- data.table(month_since_exp = rep(c(1,2,3,4,6,9,12),2), 
                         mean_protection = c(NA, 0.695, 0.652, 0.607, 0.512, 0.370, 0.247, 
                                             0.741, 0.716, 0.690, 0.662, 0.604, 0.511, 0.418),
                         lwr_protection = c(NA, 0.576, 0.529, .480, 0.386, 0.260, 0.164, 
                                            0.648, 0.619, 0.589, 0.558, 0.496, 0.402, 0.315), 
                         upr_protection = c(NA, 0.792, 0.759, 0.721, 0.637, 0.496, 0.355,
                                            0.816, 0.796, 0.775, 0.753, 0.703, 0.619, 0.528), 
                         exp_type = c(rep("infection-only", 7), rep("hybrid",7)))


# interpolate protection at t0
# use slope from closest 2 months of available data
t0_infectiononly <- 2*(waning_est[exp_type == "infection-only" & month_since_exp == 2, "mean_protection"] - 
                       waning_est[exp_type == "infection-only" & month_since_exp == 4, "mean_protection"])/2 + 
  waning_est[exp_type == "infection-only" & month_since_exp == 2, "mean_protection"]
t0_hybrid <- (waning_est[exp_type == "hybrid" & month_since_exp == 1, "mean_protection"] - 
  waning_est[exp_type == "hybrid" & month_since_exp == 3, "mean_protection"])/2 + 
  waning_est[exp_type == "hybrid" & month_since_exp == 1, "mean_protection"]

interp_month_0 = data.table(month_since_exp = c(0,2,0,1), 
                            mean_protection = unlist(c(t0_infectiononly, 
                                                       waning_est[exp_type == "infection-only" & month_since_exp == 2, "mean_protection"],
                                                       t0_hybrid, 
                                                       waning_est[exp_type == "hybrid" & month_since_exp == 1, "mean_protection"])), 
                            exp_type = c(rep("infection-only",2), rep("hybrid",2)))

# scenario specified waning immunity
# optimistic:  40% reduction in protection from baseline levels after 10 months
# pessimistic: 60% reduction in protection from baseline levels after 4 months
waning_scenarios <- expand.grid(scenario = c("optimistic", "pessimistic"), 
                         exp_type = c("infection-only", "hybrid"))
waning_scenarios$month_since_exp <- ifelse(waning_scenarios$scenario == "optimistic", 10, 4)
waning_scenarios$protection_reduc <- ifelse(waning_scenarios$scenario == "optimistic", 0.4, 0.6)
waning_scenarios$mean_protection <- unlist(ifelse(waning_scenarios$exp_type == "hybrid", 
                                    interp_month_0[month_since_exp == 0 & 
                                                     exp_type == "hybrid", "mean_protection"], 
                                    interp_month_0[month_since_exp == 0 & 
                                                     exp_type == "infection-only", "mean_protection"]))
waning_scenarios$mean_protection = waning_scenarios$mean_protection * (1-waning_scenarios$protection_reduc)

# find exact values for text
waning_est %>% 
  .[, .(month_since_exp = c(4,10), 
        mean_protection_interp = approx(month_since_exp, mean_protection, c(4, 10))$y), by = .(exp_type)] %>% 
  .[interp_month_0[month_since_exp == 0, .(mean_protection, exp_type)], on = .(exp_type)] %>% 
  .[, e := (mean_protection_interp - mean_protection)/ mean_protection] %>% 
  .[, e := round(e,2)] %>% head()


#### PLOT OUTCOMES -------------------------------------------------------------
source("code/plot_setup.R")

protection_labs <- c("hybrid immunity (primary series)", 
                     "previous infection")
names(protection_labs) <- c("hybrid", "infection-only")


ggplot(data = waning_est, aes(x = month_since_exp, y = mean_protection)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lwr_protection, ymax = upr_protection), alpha = 0.1) + 
  geom_line(data = interp_month_0, linetype = "dashed") +
  geom_point(data = waning_scenarios, aes(color = scenario)) + 
  facet_grid(cols = vars(exp_type), labeller = labeller(exp_type = protection_labs)) +
  scale_color_manual(values = brewer.pal(3,"Dark2")[c(1,3)]) +
  scale_x_continuous(breaks = seq(0,12,2), 
                     name = "time since last vaccination or infection") +
  scale_y_continuous(labels = percent, 
                     limits = c(0,1), 
                     name = "protection") +
  theme_bw() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.margin = margin(t = 0, unit='cm'),
        panel.grid.minor = element_blank(), 
        strip.background = element_blank())
ggsave("figures/scenario_plaus/waning_assump_R13.pdf", width = 5, height = 3)

