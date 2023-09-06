# Informing pandemic response in the face of uncertainty: An evaluation of the U.S. COVID-19 Scenario Modeling Hub

This repository contains data and code to reproduce results in 
>Howerton E, Contamin L, Mullany LC, Qin M, Reich NG, Bents S, et al. Informing pandemic response in the face of uncertainty. An evaluation of the U.S. COVID-19 Scenario Modeling Hub. medRxiv; 2023. p. 2023.06.28.23291998. doi:10.1101/2023.06.28.23291998

## Requirements
Some functions used in this analysis are included in the package `SMHEvaluationUtils`, which can be installed from GitHub using

``
remotes::install_github("eahowerton/SMHEvaluationUtils")
``

## Repository structure
This repository contains four main folders:
1. `\code`: all code to perform analyses and generate figures
2. `\data-raw`: all data used for analysis, including scenarios modeled, individual model and ensemble projections, ground truth data, and information about US states projected. 
3. `\data-output`: all data generated through this analysis, including null projections, projection coverage and WIS scores, etc.
4. `\figures`: all figures created for main text and supplementary material

## Analysis
Evaluation of SMH COVID-19 projections can be executed in the following steps, corresponding to the subfolders within `\code`. There are also three utility files stored in `\code` that are used throughout all steps, including `read_data_files.R`, which contains a function to load data; `prepare_projections.R`, which contains code to load truth data and load/format projections; and `plot_setup.R`, which contains objects used across all plots (e.g., labels, color schemes, etc.). 
 1. **setup scenario plausibility:** assess the plausibility of SMH scenarios for Rounds 1-16. This includes: 

      - `define_variant_takeover.R`: find the dates when each variant (Alpha, Delta, and Omicron) reached 50% nationally.
      -  `round_specific_timing.R`: define the timeline for each scenario (including date scenarios were released, data cutoff date, start date of projections, etc.)
      -  `vacc_scenario_plaus.R`: download vaccine uptake data from US CDC, and use this to compare realized vaccine uptake to scenario specified vaccine uptake
      -  `waning_scenario_plaus.R`: use published estimates of rates of waning immunity to assess the validity of waning scenario specifications

 2. **generate null projections:** generate three alternative comparator (or null) models. We compare performance of SMH projections and these models. 
     - `get_null_proj_FH.R`: generate *4-week ahead* comparator model by downloading forecasts from 4-week ahead ensemble model from the U.S. COVID-19 Forecast Hub
      -  `get_null_proj_GAM.R`: generate *trend continuation* comparator model by fitting a generalized additive model to historical data
      -  `get_null_proj_naive.R`: generate *naive* comparator model by downloading forecasts from baseline model from the U.S. COVID-19 Forecast Hub
 3. **classify trends:** classify trends in projected and observed incidence (i.e., as increasing, flat, or decreasing) 
      - `setup_incdec_analysis.R`: load projections and observations for trend classification analysis
      -  `classify_incdec.R`: classify observations and projections as increasing, decreasing or flat
  
 4. **score projections:** calculate coverage and weighted interval score (WIS) for projections from individual models, ensemble and comparator models
      - `calculate_scores.R`: calculate coverage and WIS for all projections; saves results in `data` folder
      - `calculate_skillscore.R`: calculate relative WIS (or "skill score") for individual models, ensembles, and scenarios (plus bootstrapping)
      - `scoring_functions.R`: functions to implement WIS and skill score calculations

 5. **generate main figures:** create 5 figures in main text
      - `figure_1.R`
      - `figure_2.R`
      - `figure_3.R`
      - `figure_4.R`
      - `figure_5.R`

 6. **generate supplemental figures:** create supplementary figures
      -  `plot_nulls.R`: plot null projections
      -  `plot_coverage.R`: plot additional coverage results
      -  `plot_WIS.R`: plot additional WIS results
      -  `plot_incdec.R`: plot additional results from trend classification analysis (classifying trends as increasing, flat, or decreasing)
      -  `incdec_sensitivity_analyses.R`: perform sensitivity analyses on assumptions in trend classification analysis and plot results
      -  `plot_round_overview.R`: summarize results across scores for each round and target
      -  `plot_MOBS_waning.R`: plot example projections with two different waning assumptions (Figure S60)
      
 7. **calculate values used in main text:**
      - `summarize_exclusions.R`: summarize the projections that were excluded from analyses
      -  `values_for_text.R`: calculate values that are included in main text


## Data license and reuse

We are grateful to the teams who have generated these scenarios. The groups have made their public data available under different terms and licenses. You will find the licenses (when provided) within the model-specific folders in the data-processed directory. Please consult these licenses before using these data to ensure that you follow the terms under which these data were released.

All source code that is specific to the overall project is available under an open-source MIT license. We note that this license does NOT cover model code from the various teams or model scenario data (available under specified licenses as described above).

## Additional information
For more information about the U.S. Scenario Modeling Hub, please visit https://github.com/midas-network/covid19-scenario-modeling-hub and https://covid19scenariomodelinghub.org. 