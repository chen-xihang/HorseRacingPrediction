shrinkage_rating <- function(data,
                             id_col,
                             half_life_days = 180, # "half-life length"
                             k = 10,
                             out_col = "rating") {
  
  lambda <- log(2) / half_life_days
  global_mean <- mean(data$obs_racing_post_rating, na.rm = TRUE)
  
  data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::arrange(date, race_time, race_id) |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::arrange(date, race_time, race_id, .by_group = TRUE) |>
    dplyr::group_modify(~{
      g <- .x
      n <- nrow(g)
      
      S <- numeric(n)
      W <- numeric(n)
      
      S[1] <- 0
      W[1] <- 0
      
      if (n >= 2) {
        for (i in 2:n) {
          
          dt <- as.numeric(difftime(g$date[i], g$date[i - 1], units = "days"))
          dt <- ifelse(is.na(dt) | dt < 0, 0, dt)
          
          decay <- exp(-lambda * dt)
          
          # decay previous evidence
          S[i] <- decay * S[i - 1]
          W[i] <- decay * W[i - 1]
          
          # add previous run's RPR
          rpr_prev <- g$obs_racing_post_rating[i - 1]
          if (!is.na(rpr_prev)) {
            S[i] <- S[i] + rpr_prev
            W[i] <- W[i] + 1
          }
        }
      }
      
      g$S_eff <- S
      g$W_eff <- W
      g
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      decayed_mean = if_else(W_eff > 0, S_eff / W_eff, NA_real_),
      trust_weight = W_eff / (W_eff + k),
      "{out_col}" := if_else(
        W_eff == 0,
        global_mean,
        trust_weight * decayed_mean + (1 - trust_weight) * global_mean
      )
    ) |>
    dplyr::select(-S_eff, -W_eff, -decayed_mean, -trust_weight)
}

brier <- function(p, y){ 
  mean((p - y)^2)
}

log_loss <- function(p, y) {
  -mean(y * log(p) + (1 - y) * log(1 - p))
}
