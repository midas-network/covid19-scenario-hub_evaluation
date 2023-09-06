library(MMWRweek)
library(MetBrewer)
library(outbreakinfo)

## use outbreakinfo package to retrieve for each variant
# # alpha
# alpha_lineages_string = lookupSublineages("Alpha", returnQueryString = TRUE)
# alpha_label = c("Alpha")
# names(alpha_label) = alpha_lineages_string
# alpha_US = getPrevalence(pangolin_lineage = alpha_lineages_string,
#                          location = "United States")
# # delta
# delta_lineages_string = lookupSublineages("Delta", returnQueryString = TRUE)
# delta_label = c("Delta")
# names(delta_label) = delta_lineages_string
# delta_US = getPrevalence(pangolin_lineage = delta_lineages_string,
#                              location = "United States")
# # omicron
# omicron_lineages_string = lookupSublineages("Omicron", returnQueryString = TRUE)
# omicron_label = c("Omicron")
# names(omicron_label) = omicron_lineages_string
# omicron_US = getPrevalence(pangolin_lineage = omicron_lineages_string,
#                          location = "United States")
# 
# var_US = bind_rows(alpha_US %>%
#                      mutate(lineage_short = "alpha"),
#                    delta_US %>%
#                      mutate(lineage_short = "delta"),
#                    omicron_US %>%
#                      mutate(lineage_short = "omicron")) %>%
#   setDT()
# 
# # set variant takeover date manually using outbreak.info estimates of prevalence
# variant_takeover_date = unlist(var_US %>%
#                                  .[proportion > 0.5] %>%
#                                  .[, .(date = min(date)), by = "lineage_short"] %>%
#                                  pull(date))
# # change to saturday of same MMWR week (to align with projection target_end_date)
# variant_takeover_date = MMWRweek2Date(
#   MMWRyear = MMWRweek(variant_takeover_date)$MMWRyear,
#   MMWRweek = MMWRweek(variant_takeover_date)$MMWRweek,
#   MMWRday = 7
# )
# names(variant_takeover_date) = c("alpha", "delta", "omicron")

# note: hard code dates generated using above code for speed
variant_takeover_date <- as.IDate(c("2021-04-03", "2021-06-26", "2021-12-15"))
names(variant_takeover_date) <- c("alpha", "delta", "omicron")

