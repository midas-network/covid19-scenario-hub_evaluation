# labels
target_labs <- c("incident cases", "incident hospitalizations", "incident deaths")
names(target_labs) <- c("inc case", "inc hosp", "inc death")
target_labs_wrap <-  c("incident\ncases", "incident\nhospitalizations", "incident\ndeaths")
names(target_labs_wrap) <- c("inc case", "inc hosp", "inc death")
round_labs <- c(paste("round", c(1:7,9,11:16)),"observations")
names(round_labs) <- c(c(1:7,9,11:16),0)
scenario_labs <- paste("scenario", LETTERS[1:4])
names(scenario_labs) <- LETTERS[1:4]
classif_labs <- c("decreasing", "increasing", "flat")
names(classif_labs) <- c("dec", "inc", "flat")
plaus_labs <- c("realistic", "not realistic")
names(plaus_labs) <- c(1,0)
null_labs <- c("highly-informed", "trend-continuation","naive")
names(null_labs) <-  c("null_fh", "null_gam", "null_naive")

# give each model a random letter identifier
model_name_key <- data.frame(model_name  = c("USC-SIkJalpha",
                                             "UVA-adaptive",
                                             "NotreDame-FRED",
                                             "JHU_IDD-CovidSP",
                                             "JHUAPL-Bucky",
                                             "NCSU-COVSIM",
                                             "UVA-EpiHiper",
                                             "UTA-ImmunoSEIRS",
                                             "UF-ABM",
                                             "Karlen-pypm",
                                             "MOBS_NEU-GLEAM_COVID",
                                             "CU-AGE-ST",           
                                             "UNCC-hierbin",
                                             "MOBS_NEU-GLEAM_COVID_OT",
                                             "USC-SIkJalpha-update",
                                             "Ensemble_LOP"),
                             key = c(LETTERS[1:15], "Ens"))

model_name_key_nat <- data.frame(model_name  = c("USC-SIkJalpha",
                                                 "UVA-adaptive",
                                                 "JHU_IDD-CovidSP",
                                                 "JHUAPL-Bucky",
                                                 "UVA-EpiHiper",
                                                 "UTA-ImmunoSEIRS",
                                                 "Karlen-pypm",
                                                 "MOBS_NEU-GLEAM_COVID",
                                                 "CU-AGE-ST",           
                                                 "UNCC-hierbin",
                                                 "Ensemble_LOP"),
                                 key = c(LETTERS[1:10], "Ens"))


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


