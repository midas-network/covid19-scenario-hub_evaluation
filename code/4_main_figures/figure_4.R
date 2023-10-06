#### SETUP ---------------------------------------------------------------------
library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(cowplot)

# load increasing/decreasing data
inc_dec <- setDT(read.csv("data-output/increase-decrease-classification/increasing_decreasing.csv")) %>%
  .[, ":=" (X = NULL,
            target_end_date = as.IDate(target_end_date))]

inc_dec_obs <- setDT(read.csv("data-output/increase-decrease-classification/increasing_decreasing_obs.csv")) %>%
  .[, ":=" (X = NULL,
            target_end_date = as.IDate(target_end_date))]


# load objects to assist with plotting
source("code/plot_setup.R")

# load projections (if necessary)
# source("code/prepare_projections.R")

bs = 7

### FUNCTIONS ------------------------------------------------------------------
plot_correct <- function(plt){
  p <- plt %>%
    .[, .(n_correct = sum(correct_flag*plaus_weight),
          expectation = (1/3)*sum(plaus_weight),
          n_total = sum(plaus_weight)), by = .(target, change_bin, model_name)] %>%
    data.table::dcast(target + change_bin ~ model_name, value.var = c("n_correct", "expectation", "n_total")) %>%
    .[, target:=factor(target, levels = rev(c(paste("inc", c("case", "hosp", "death")))))] %>%
    ggplot(aes(x = n_correct_Ensemble_LOP/n_total_Ensemble_LOP, y = target)) +
    geom_bar(aes(x = n_total_Ensemble_LOP/n_total_Ensemble_LOP, fill = "total"), stat = "identity") +
    geom_bar(aes(fill = "SMH"), stat = "identity") +
    geom_segment(aes(x = expectation_Ensemble_LOP/n_total_Ensemble_LOP,xend = expectation_Ensemble_LOP/n_total_Ensemble_LOP,
                     y = as.numeric(target)-0.45, yend = as.numeric(target)+0.45,
                     linetype = "random expectation"), size = 0.3) +
    geom_vline(xintercept = 1/3, size = 0.3) +
    geom_segment(aes(x = n_correct_null_fh/n_total_null_fh,xend = n_correct_null_fh/n_total_null_fh,
                     y = as.numeric(target)-0.45, yend = as.numeric(target)+0.45,
                     linetype = "4-week forecast model"), size = 0.3) +
    geom_segment(aes(x = n_correct_null_trend/n_total_null_trend,xend = n_correct_null_trend/n_total_null_trend,
                     y = as.numeric(target)-0.45, yend = as.numeric(target)+0.45,
                     linetype = "continue current trend"), size = 0.3) +
    facet_grid(cols = vars(change_bin),
               labeller = labeller(change_bin = classif_labs)) +
    labs(x = "% of round-week-locations correct") +
    scale_fill_brewer(palette = "Greys", direction = -1) +
    scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
    scale_x_continuous(expand = c(0,0), labels = scales::percent) +
    scale_y_discrete(expand = c(0,0),
                     labels = rev(paste0("incident\n",c("cases", "hosp.", "deaths")))) +
    theme_bw(base_size = bs)+
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0.7, "line"),
          strip.background = element_blank())
  return(p)
}

plot_confusion <- function(plt){
  p <- plt %>%
    .[, .(n_total = sum(plaus_weight, na.rm = TRUE)), by = .(change_bin_proj, change_bin)] %>%
    .[, correct_flag  := ifelse(change_bin_proj == change_bin, 1, 0)] %>%
    .[, correct_flag := ifelse(paste(change_bin_proj, change_bin) %in% c("inc dec", "dec inc"), -1, correct_flag)] %>%
    .[, pct_correct := n_total/sum(n_total)] %>%
    .[, n_total_char := as.character(round(n_total))] %>%
    .[, pct_correct_txt := paste0(ifelse(nchar(n_total_char) > 6, "CHECK THIS",
                                         ifelse(nchar(n_total_char)>3,
                                         paste0(substr(n_total_char,1,nchar(n_total_char)-3), ",",
                                                substr(n_total_char,nchar(n_total_char)-2,nchar(n_total_char))), n_total_char)), "\n (",
                                  round(pct_correct,2)*100, "%)")] %>%
    .[, ":=" (change_bin = factor(change_bin, levels = c("dec", "flat", "inc")),
              change_bin_proj = factor(change_bin_proj, levels = rev(c("dec", "flat", "inc"))))] %>%
    ggplot(aes(x = change_bin, y = change_bin_proj)) +
    geom_tile(color = "black", fill = "white", size = 0.25) +  #aes(fill = pct_correct), alpha = 0.4,
    geom_text(aes(label = pct_correct_txt), size = 2) +
    # add text for recall (by projected classification)
    geom_text(data = plt %>%
                .[, .(n_total = sum(plaus_weight, na.rm = TRUE)), by = .(change_bin_proj, change_bin)] %>%
                .[, pct_correct := paste0(round(n_total/sum(n_total),2)*100, "%"), by = .(change_bin_proj)] %>%
                .[change_bin_proj == change_bin] %>% .[, change_bin := NULL],
              aes(x = 3.75, label = pct_correct), size = 2) +
    # add text for precision (by observed classification)
    geom_text(data = plt %>%
                .[, .(n_total = sum(plaus_weight, na.rm = TRUE)), by = .(change_bin_proj, change_bin)] %>%
                .[, pct_correct := paste0(round(n_total/sum(n_total),2)*100, "%"), by = .(change_bin)] %>%
                .[change_bin_proj == change_bin] %>% .[, change_bin_proj := NULL],
              aes(y = 0.4, label = pct_correct), size = 2) +
    # add text for all correct
    geom_text(data = plt %>%
                .[, .(n_total = sum(plaus_weight[change_bin == change_bin_proj], na.rm = TRUE)/sum(plaus_weight, na.rm = TRUE))] %>%
                .[, pct_correct := paste0(round(n_total,2)*100, "%")],
              aes(x = 3.75, y = 0.4, label = pct_correct), size = 2) +
    coord_cartesian(xlim = c(0.5, 3.75),
                  ylim = c(0.4, 3.5),
                  clip = 'off') +
    scale_fill_distiller(palette = "Greens",
                         direction = 0,
                         na.value="white") +
    scale_x_discrete(expand = c(0,0),
                     labels = c("decreasing", "flat", "increasing"),
                     name = "observed",
                     position = "top") +
    scale_y_discrete(expand = c(0,0),
                     labels = c("increasing", "flat", "decreasing"),
                     name = "projected") +
    theme_bw(base_size = bs) +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.title.x = element_text(hjust = 0.47),
          axis.title.y = element_text(hjust = 0.51),
          legend.position = "none",
          panel.border = element_blank(),
          panel.grid = element_blank())
  return(p)
}

### PANEL A/B: EXAMPLE ---------------------------------------------------------
dates <- as.IDate(unlist(proj[round == 11 &
                location == "US" &
                quantile == 0.5 &
                plaus_weight == 1 &
                target == "inc hosp"] %>%
  .[, .(target_end_date)]) %>% range())

lm <- c(0,18E4)
pA <- inc_dec_obs[target_end_date >= dates[1] &
                    target_end_date <= dates[2] &
                    location == "US" &
                    target == "inc hosp"] %>%
  ggplot(aes(x = target_end_date, y = obs)) +
  geom_line(size = 0.3) +
  geom_point(aes(fill = change_bin), size = 1.5, shape = 21) +
  scale_fill_manual(values = rev(brewer.pal(3,"RdYlBu")),
                     labels = c("decreasing", "flat ","increasing"),
                     na.value = "lightgrey") +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(limits = lm,
                     name = "observed",
                     labels = label_number(suffix = " K", scale = 1e-3)) +
  theme_bw(base_size = bs) +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())
l <- get_legend(pA)
pA <- pA + theme(legend.position = "none")

pB <- inc_dec[round == 11 &
                model_name == "Ensemble_LOP" &
                location == "US" &
                quantile == 0.5 &
                plaus_weight == 1 &
                target == "inc hosp"] %>%
  ggplot(aes(x = target_end_date, y = obs)) +
  geom_line(size = 0.3) +
  geom_point(aes(fill = change_bin), size = 1.5, shape = 21) +
  scale_fill_manual(values = brewer.pal(3,"RdYlBu")[c(3,1)],
                     labels = c("decreasing", "increasing"),
                     na.value = "lightgrey") +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(limits = lm,
                     name = "projected",
                     labels = label_number(suffix = " K", scale = 1e-3)) +
  theme_bw(base_size = bs) +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor = element_blank())

#### PANEL C/D: PERCENT CORRECT AND CONFUSION MATRIX----------------------------
# merge projected inc/dec with observed inc/dec
plt <- copy(inc_dec)
plt <- plt%>%
  setnames("change_bin", "change_bin_proj", skip_absent = TRUE) %>%
  .[inc_dec_obs[, c("location", "target", "target_end_date", "change_bin")], 
    on = .(location, target, target_end_date)] %>%
  .[!is.na(change_bin) & !is.na(change_bin_proj) & !is.na(obs)] %>%
  .[, correct_flag := ifelse(change_bin_proj == change_bin, 1, 0)]


pC <- plot_correct(plt = plt %>%
                     .[!(quantile %in% c(0.75, 0.975))])

pD <- plot_confusion(plt = plt %>%
                       .[model_name == "Ensemble_LOP" &
                           !(quantile %in% c(0.75, 0.975))])

#### COMBINE INTO ONE FIGURE ---------------------------------------------------
plot_grid(
  plot_grid(pA, pB, l,
            labels = letters[1:2],
            label_size = 7,
            ncol = 1,
            rel_heights = c(0.45,0.45,0.05)),
  plot_grid(pC,pD,
            labels = letters[3:4],
            label_size = 7,
            align = "h", axis = "tb",
            nrow = 1, rel_widths = c(0.7, 0.3)),
  nrow = 1, rel_widths = c(0.2,0.8))
ggsave("figures/main_figures/figure4.pdf", width = 6.3, height = 2)


# #### ALL PROJECTIONS -----------------------------------------------------------

pC <- plot_correct(plt = plt %>%
                     .[!(quantile %in% c(0.75, 0.975))] %>%
                     .[, plaus_weight := 1])

pD <- plot_confusion(plt = plt %>%
                       .[model_name == "Ensemble_LOP" &
                           !(quantile %in% c(0.75, 0.975))] %>%
                       .[, plaus_weight := 1])

plot_grid(pC,pD,
          labels = letters[1:2],
          label_size = 7,
          align = "h", axis = "tb",
          nrow = 1, rel_widths = c(0.7, 0.3))
ggsave("figures/trend_classification/figure4_allproj.pdf", width = 6.3, height = 2)

#### Q75 -----------------------------------------------------------------------
pC <- plot_correct(plt = rbindlist(list(plt %>%
                     .[!(quantile %in% c(0.5, 0.975))],
                     plt %>%
                       .[model_name == "null_trend"])))

pD <- plot_confusion(plt = plt %>%
                       .[model_name == "Ensemble_LOP" &
                           !(quantile %in% c(0.5, 0.975))])

plot_grid(pC,pD,
          labels = letters[1:2],
          label_size = 7,
          align = "h", axis = "tb",
          nrow = 1, rel_widths = c(0.7, 0.3))
ggsave("figures/trend_classification/figure4_Q75.pdf", width = 6.3, height = 2)

#### Q97.5 ---------------------------------------------------------------------

pC <- plot_correct(plt = rbindlist(list(plt %>%
                                         .[!(quantile %in% c(0.5, 0.75))],
                                       plt %>%
                                         .[model_name == "null_trend"])))

pD <- plot_confusion(plt = plt %>%
                       .[model_name == "Ensemble_LOP" &
                           !(quantile %in% c(0.5, 0.75))])

plot_grid(pC,pD,
          labels = letters[1:2],
          label_size = 7,
          align = "h", axis = "tb",
          nrow = 1, rel_widths = c(0.7, 0.3))
ggsave("figures/trend_classification/figure4_Q975.pdf", width = 6.3, height = 2)

#### ACROSS ALL TARGETS --------------------------------------------------------
plt %>%
  .[!(quantile %in% c(0.75, 0.975))] %>%
  .[, .(n_correct = sum(correct_flag*plaus_weight),
        expectation = (1/3)*sum(plaus_weight),
        n_total = sum(plaus_weight)), by = .(change_bin, model_name)] %>%
  dcast(change_bin ~ model_name, value.var = c("n_correct", "expectation", "n_total")) %>%
  .[, change_bin := factor(change_bin, levels = c("dec", "flat", "inc"))] %>%
  ggplot(aes(x = n_correct_Ensemble_LOP/n_total_Ensemble_LOP, y = change_bin)) +
  geom_bar(aes(x = n_total_Ensemble_LOP/n_total_Ensemble_LOP, fill = "total"), stat = "identity") +
  geom_bar(aes(fill = "% correct"), stat = "identity") +
  geom_segment(aes(x = expectation_Ensemble_LOP/n_total_Ensemble_LOP,xend = expectation_Ensemble_LOP/n_total_Ensemble_LOP,
                   y = as.numeric(change_bin)-0.45, yend = as.numeric(change_bin)+0.45,
                   linetype = "random expectation")) +
  geom_vline(xintercept = 1/3) +
  geom_segment(aes(x = n_correct_null_fh/n_total_null_fh,xend = n_correct_null_fh/n_total_null_fh,
                   y = as.numeric(change_bin)-0.45, yend = as.numeric(change_bin)+0.45,
                   linetype = "null model")) +
  geom_segment(aes(x = n_correct_null_trend/n_total_null_trend,xend = n_correct_null_trend/n_total_null_trend,
                   y = as.numeric(change_bin)-0.45, yend = as.numeric(change_bin)+0.45,
                   linetype = "continue current trend")) +
  labs(x = "% of round-week-locations correct") +
  scale_fill_brewer(palette = "Greys", direction = -1) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid")) +
  scale_x_continuous(expand = c(0,0), labels = scales::percent) +
  scale_y_discrete(expand = c(0,0),
                   labels = classif_labs) +
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(1.1, "line"),
        strip.background = element_blank())
ggsave("figures/trend_classification/classif_simp.pdf", width = 5, height = 5)


