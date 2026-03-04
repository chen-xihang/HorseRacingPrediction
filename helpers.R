shrinkage_rating <- function(data,
                             id_col,
                             rpr_col = "obs_racing_post_rating",
                             date_col = "date",
                             time_col = "race_time",
                             race_id_col = "race_id",
                             half_life_days = 180, # number of days to reach "half-life"
                             k = 10,
                             out_col = "rating_td") {
  lambda <- log(2) / half_life_days
  
  # Ensure date is Date
  data <- data %>%
    mutate(
      "{date_col}" := as.Date(.data[[date_col]])
    ) %>%
    arrange(.data[[date_col]], .data[[time_col]], .data[[race_id_col]])
  
  global_mean <- mean(data[[rpr_col]], na.rm = TRUE)
  
  # Per-entity recursive update:
  # Maintain decayed sufficient statistics (S, W) up to *previous* run.
  data %>%
    group_by(.data[[id_col]]) %>%
    arrange(.data[[date_col]], .data[[time_col]], .data[[race_id_col]], .by_group = TRUE) %>%
    group_modify(~{
      g <- .x
      n <- nrow(g)
      
      S <- numeric(n) # decayed sum of RPRs
      W <- numeric(n) # decayed weight ("effective runs")
      
      # Start: no prior evidence for first row
      S[1] <- 0
      W[1] <- 0
      
      if (n >= 2) {
        for (i in 2:n) {
          # time gap from previous row to current row
          dt <- as.numeric(difftime(g[[date_col]][i], g[[date_col]][i - 1], units = "days"))
          dt <- ifelse(is.na(dt) | dt < 0, 0, dt)
          decay <- exp(-lambda * dt)
          
          # decay previous sufficient statistics forward
          S[i] <- decay * S[i - 1]
          W[i] <- decay * W[i - 1]
          
          # add the previous run's RPR as new evidence (only if observed)
          rpr_prev <- g[[rpr_col]][i - 1]
          if (!is.na(rpr_prev)) {
            S[i] <- S[i] + rpr_prev
            W[i] <- W[i] + 1
          }
        }
      }
      
      g$S_eff <- S
      g$W_eff <- W
      g
    }) %>%
    ungroup() %>%
    mutate(
      decayed_mean = if_else(W_eff > 0, S_eff / W_eff, NA_real_),
      trust_weight = W_eff / (W_eff + k),
      "{out_col}" := if_else(
        W_eff == 0,
        global_mean,
        trust_weight * decayed_mean + (1 - trust_weight) * global_mean
      )
    ) %>%
    select(-S_eff, -W_eff, -decayed_mean, -trust_weight)
}