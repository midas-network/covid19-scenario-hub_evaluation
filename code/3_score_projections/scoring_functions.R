#### FUNCTIONS -----------------------------------------------------------------
# q must include median and all values of a and 1-a
wis <- function(q,v,o, 
                a = 2*c(0.010, 0.025, 0.050, 0.100, 0.150, 0.200, 0.250, 0.300, 0.350, 0.400, 0.450, 0.5), 
                IS_components = FALSE){
  q <- round(q,3)
  # check a
  a <- sapply(a/2, function(i){ifelse(i %in% q, 2*i, NA)})
  a <- sapply(a/2, function(i){ifelse((1-i) %in% q, 2*i, NA)})
  a <- a[!is.na(a)]
  # define weight s.t. WIS approximates CRPS (see Bracher et al.)
  w <- a # this assumes w0 = 1/2
  w[length(w)] <- w[length(w)]/2 # do not double count median
  # prepare inputs
  o <- o[1]
  q <- round(q, 4)
  a <- round(a, 4)
  # lower and upper interval bounds
  l <- sapply(a, function(i){v[q == i/2]})
  u <- sapply(a, function(i){v[q == (1-i/2)]})
  # IS components
  IS <- list(
    disp = u-l,
    underpred = 2/a*(o - u)*ifelse(o > u, 1, 0),
    overpred = 2/a*(l - o)*ifelse(o<l, 1, 0)
  )
  # weight
  IS <- lapply(IS, function(i){w*i})
  # sum
  if(IS_components){
    return(list(IS_disp = (1/length(q)) * sum(IS$disp), 
                IS_underpred = (1/length(q)) * sum(IS$underpred), 
                IS_overpred = (1/length(q)) * sum(IS$overpred),
                WIS = 1/(length(q)) * do.call(sum,IS)))
  }
  else{
    return(1/(length(q)) * do.call(sum,IS))
  }
}


# then implement this function by group that you care about (I think)
# this could be more efficient (remove grouping) and do externally with DT
skill_score <- function(score_df, grouping = NA){
  # find all pairs of models to compute skill score
  model_pairs <- expand.grid(model_name = unique(score_df$model_name), 
                             pair_model = unique(score_df$model_name))
  model_pairs <- model_pairs %>% filter(model_name != pair_model)
  setDT(model_pairs)
  # define groupings
  colgrp <- colnames(score_df)[!(colnames(score_df) %in% c("model_name", "score"))]
  grp1 <- c("model_name", "pair_model")
  grp2 <- c("model_name")
  if(!any(is.na(grouping))){
    grp1 <- c(grp1, grouping)
    grp2 <- c(grp2, grouping)
  }  
  # calculate skill score
  # get scores for model and each paired model
  model_pairs <- model_pairs[score_df, 
                             on = "model_name", 
                             allow.cartesian = TRUE] %>%
    .[score_df, 
      on = c(pair_model = "model_name", colgrp), 
      allow.cartesian = TRUE] %>%
    setnames("i.score", "pair_score") %>%
    # keep only rows for which both models have a WIS
    .[!is.na(score*pair_score)] %>%
    # calculate relativeWIS = mean(WIS_model1)/mean(WIS_model2)
    .[, .(relWIS = mean(score)/mean(pair_score)), 
      by = grp1] %>%  
    # calculate geometric mean across all possible pair_models
    .[, .(skill = prod(relWIS)^(1/.N)), by = grp2]
}

