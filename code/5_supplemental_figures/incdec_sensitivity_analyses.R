library(dplyr)
library(data.table)
library(knitr)
library(cowplot)
library(SMHEvaluationUtils)

# load truth data and projections
source("code/2_classify_trends/setup_incdec_analysis.R")

# load plotting objects
source("code/plot_setup.R")


### TEST MUTLIPLE ASSUMPTIONS FOR CLASSIFYING OBSERVATIONS ---------------------
# run multiple parameter assumptions  
test = expand.grid(lag = c(1,2), 
                   pct_flat = c(1/3, 1/5))
test$sim = 1:nrow(test)
# run across all test vals
all_out <- list()
all_vals <- list()
for(i in 1:nrow(test)){
  # # classify each observation as increasing/decreasing 
  inc_dec_obs <- full_classif(
    dat = truth_dat_incdec, 
    lag = test[i,"lag"], 
    pct_flat = test[i, "pct_flat"], 
    byvars = c("target", "location")
  )
  all_out[[i]] <- inc_dec_obs
  all_vals[[i]] <- inc_dec_obs %>% 
    select(lwr_quantile, upr_quantile) %>% #target, 
    unique() %>% 
    mutate(sim = i) 
}

all_vals <- do.call(rbind, all_vals) %>% 
  merge(test) %>%
  mutate(pct_flat = round(pct_flat, 2))

# kable(all_vals)

all_out <- rbindlist(all_out, idcol = TRUE) %>%
  setnames(".id", "sim") %>%
  merge(test)

# plot outcomes under each scenario
all_out %>%
  .[, target := factor(target, levels = paste("inc", c("case", "hosp", "death")))] %>%
  .[, facet_lab :=  paste0(lag, " week lag\n",  round(pct_flat, 2)*100, "% flat")] %>%
  ggplot(data = , aes(x = target_end_date, y = location)) + 
  geom_tile(aes(fill = change_bin)) +
  geom_vline(xintercept = as.Date(variant_takeover_date), linetype = "dashed", size = 0.3) +
  facet_grid(cols = vars(target), 
             rows = vars(facet_lab), 
             labeller = labeller(target = target_labs), 
             switch = "y") + 
  scale_fill_brewer(palette = "RdYlBu", direction = -1, 
                    labels = c("decreasing", "flat", "increasing", "NA")) +
  scale_x_date(date_labels = "%b\n%Y",
               expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        legend.position = 'bottom', 
        legend.title = element_blank(),
        panel.spacing.x = unit(0.1, "lines"),
        strip.background = element_blank()) +
  panel_border(color = "black")
ggsave("figures/incdec_altparms.pdf", width = 6, height = 5)


#### IMPLEMENT CHOSEN CASE -----------------------------------------------------
lag = 2
pct_flat = 1/3

# classify each observation as increasing/decreasing 
pct_change_obs <- SMHEvaluationUtils::calculate_pct_change(
  dat = truth_dat_incdec, 
  lag = lag, 
  byvars = c("target", "location"))

# plot distribution of % change values
t <- calc_thresh(pct_change_obs, pct_flat)
dens <- pct_change_obs[!is.na(pct_change)] %>%
  .[, .(x = density(pct_change)$x, 
        y = density(pct_change)$y), by = .(target)] %>%
  .[t, on = .(target)] %>%
  .[, type := ifelse(x <= lwr_quantile, "decreasing", 
                     ifelse(x >= upr_quantile, "increasing", "flat"))]
t <- melt(t, "target")
setnames(t, "value", "x")
t$y = NA
for(i in 1:nrow(t)){
  t$y[i] = 
    approx(dens[target == t$target[i]]$x, dens[target == t$target[i]]$y, t$x[i])$y
}
t <- t %>% .[, target := factor(target, levels = paste("inc", c("case", "hosp", "death")))]
dens <- dens %>% .[, target := factor(target, levels = paste("inc", c("case", "hosp", "death")))]

ggplot(data = dens,
       aes(x = x, y = y)) +
  geom_line() +
  geom_segment(data = t, aes(x = x, xend = x, y = 0, yend = y, group = variable),
               linetype = "dotted") +
  geom_ribbon(aes(ymin = 0, ymax  = y, fill = type), alpha = 0.6)+
  geom_text(data = t %>% 
              dcast(target ~ variable, value.var = "x") %>%
              .[, ":=" (decreasing = -0.45 + lwr_quantile, 
                        flat = lwr_quantile + upr_quantile, 
                        increasing = 0.45 + upr_quantile)] %>%
              melt("target") %>%
              .[!(variable %in% c("lwr_quantile", "upr_quantile"))],
            aes(x = value, y = 0.015, label = variable), vjust = 0, size = 2.2, hjust = 0.5)+
  geom_text(data = t %>% 
              .[, hjust := ifelse(variable == "lwr_quantile", 1, 0)], 
            aes(x = x, y = y, label = paste0(round(x*100), "%"), hjust = hjust), size = 2.2) +
  facet_grid(cols = vars(target), labeller = labeller(target = target_labs)) +
  labs(x = "% change with 2-week lag", y = "density of observations") + 
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_continuous(expand = c(0,0), 
                     labels = function(x) paste0(x*100,"%"),
                     limits = 2.5*c(-1,1)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid = element_blank(), 
        strip.background = element_blank())
ggsave("figures/trend_classification/incdec_finaldist.pdf", width = 10, height = 4)

# for caption
# what % outside x axis range
nrow(pct_change_obs[pct_change > 2.5 | pct_change < -2.5])/nrow(pct_change_obs)

# range of % change values
range(pct_change_obs$pct_change, na.rm = TRUE)

#### PLOT DIFFERENCES BETWEEN OBSERVED AND PROJECTED % CHANGE ------------------
# classify projections using threshold
inc_dec_proj <- SMHEvaluationUtils::full_classif(dat = proj_addobs, 
                                                 lag = lag, 
                                                 pct_flat = pct_flat,
                                                 byvars = c("location", "target", "model_name", "scenario_id", "round", "quantile"), 
                                                 calc_thresh_flag = TRUE)

inc_dec_proj <- add_plaus_weight(proj = inc_dec_proj, 
                            variant_takeover = variant_takeover_date, 
                            modelname_round_weight = model_exclusions)

plt <- copy(inc_dec_proj)
plt <- plt %>%
  setnames("pct_change", "pct_change_proj", skip_absent = TRUE) %>%
  setnames("change_bin", "change_bin_proj", skip_absent = TRUE) %>%
  .[inc_dec_obs[, c("location", "target", "target_end_date", "pct_change", "change_bin")], 
    on = .(location, target, target_end_date)] %>%
  .[!is.na(change_bin) & !is.na(change_bin_proj) & !is.na(obs)] %>%
  .[, col := paste0(change_bin, change_bin_proj)] %>%
  .[, col := ifelse(col == "flatdec", "decflat", ifelse(col == "flatinc", "incflat", col))] %>%
  .[, col2 := abs(pct_change - pct_change_proj)]

t <- plt[target == "inc hosp", .(target, lwr_quantile, upr_quantile)] %>%
  unique()

ggplot(data = plt[model_name == "Ensemble_LOP" & 
                    quantile == 0.5 &
                    target == "inc hosp" & 
                    plaus_weight > 0 & 
                    pct_change_proj < 2 & 
                    pct_change_proj > -2 & 
                    pct_change < 2 & 
                    pct_change > -2], 
       aes(x = pct_change_proj, y = pct_change, color = col2)) + 
  # geom_abline(intercept = 1, color = "grey") +
  # geom_abline(intercept = -1, color = "grey") +
  # geom_abline(intercept = 2, color = "grey") +
  # geom_abline(intercept = -2, color = "grey") +
  # geom_abline(intercept = 3, color = "grey") +
  # geom_abline(intercept = -3, color = "grey") +
  geom_point(alpha = 0.6, shape = 21) +
  geom_abline(color = "grey") +
  geom_hline(yintercept = t$lwr_quantile, linetype = "dashed") + 
  geom_hline(yintercept = t$upr_quantile, linetype = "dashed") + 
  geom_vline(xintercept = t$lwr_quantile, linetype = "dashed") + 
  geom_vline(xintercept = t$upr_quantile, linetype = "dashed") + 
  geom_text(data = data.frame(x = c(rep(-2.05, 3), c(-(2.05-t$lwr_quantile)/2, (t$lwr_quantile + t$upr_quantile)/2, (2.05-t$upr_quantile)/2)), 
                              y = c(c(-(2.05-t$lwr_quantile)/2, (t$lwr_quantile + t$upr_quantile)/2, (2.05-t$upr_quantile)/2), rep(2.05, 3)), 
                              lab = rep(c("decrease", "flat", "increase"), 2), 
                              angle = c(rep(90, 3), rep(0, 3))), 
            aes(x = x, y = y, label = lab, angle = angle), color = "black", size = 3) + 
  #facet_wrap(vars(round), labeller = label_both) +
  scale_color_distiller(direction = 1,
                        labels = percent,
                        limits = c(0,3), 
                        name = "difference",
                        palette = "YlGnBu") +
  scale_x_continuous(#breaks = , 
    expand = c(0,0), 
    labels = percent,
    #labels = c("decrease", "flat", "increase"), 
    limits = 2.1*c(-1,1), 
    name = "projected % change") + 
  scale_y_continuous(#breaks = c(-(1-t$lwr_quantile)/2, (t$lwr_quantile + t$upr_quantile)/2, (1-t$upr_quantile)/2), 
    expand = c(0,0), 
    labels = percent,
    #labels = c("decrease", "flat", "increase"), 
    limits = 2.1*c(-1,1), 
    name = "observed % change") + 
  theme_bw() +
  theme(legend.position = c(0.91, 0.16), 
        panel.grid = element_blank())
ggsave("figures/trend_classification/inc_dec_pctchangediff.pdf", width = 6, height = 6)
