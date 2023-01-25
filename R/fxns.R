generate_sims <- function(n, timesteps, prob) {
  
  sims <- untb::untb(start = rep(1, times = n), prob = prob, gens = timesteps, keep = T)
  
  return(sims)
  
}

summarize_sad <- function(a_row) {
  
  richness = length(unique(a_row))
  abundance = length(a_row)
  
  rt <- table(a_row)
  
  hill1 = hillR::hill_taxa(rt, q = 1)
  hill2 = hillR::hill_taxa(rt, q = 2)
  
  return(data.frame(richness = richness,
                    abundance = abundance,
                    fs_hill1 = hill1,
                    fs_hill2 = hill2))
  
}

summarize_sims <- function(all_sims) {
  
  sads <- apply(all_sims, 1, FUN = summarize_sad)
  sads <- dplyr::bind_rows(sads, .id = "timestep")
}

generate_p_table <- function(all_sims) {
  
  richness <- apply(all_sims, MARGIN = 1, FUN = function(x) {(length(unique(x)))})
  
  max_s = max(richness) + 1
  max_n = ncol(all_sims)
    
  p_table <- feasiblesads::fill_ps(max_s, max_n, storeyn = F)
  
  return(p_table)
}

fs_hills <- function(s, n, p_table, ndraws) {
  
  fs <- feasiblesads::sample_fs(s = s, n = n, nsamples = ndraws, p_table = p_table, storeyn = FALSE)
  
  fs_hill1 <- hillR::hill_taxa(fs, q = 1)
  
  fs_hill2 <- hillR::hill_taxa(fs, q = 2)

  return(data.frame(
    s = s,
    n = n,
    fs_hill1 = fs_hill1,
    fs_hill2 = fs_hill2
  ))
  
}

mete_hills <- function(s, n, ndraws) {
  mete_esf <- meteR::meteESF(S0 = s, N0 = n)
  
  mete_sad <- meteR::sad(mete_esf)
  
  mete_sad_draws <- replicate(n = ndraws, sort(mete_sad$r(s)), simplify = T) %>%
    t() 
  
  
  mete_hill1 <- hillR::hill_taxa(mete_sad_draws, q = 1)
  
  mete_hill2 <- hillR::hill_taxa(mete_sad_draws, q = 2)
  
  
  return(data.frame(
    s = s,
    n = n,
    mete_hill1 = mete_hill1,
    mete_hill2 = mete_hill2
  ))
  
}


compare_to_baselines <- function(a_row, p_table, ndraws = 100) {
  
  a_row <- as.numeric(a_row)
  
  # this stresses me out but will do for now
  timestep = a_row[1]
  s = a_row[2]
  n = a_row[3]
  hill1 = a_row[4]
  hill2 = a_row[5]
  
  if(s == 1) {
    
    out <- data.frame(
      timestep = timestep,
      s = s,
      n = n,
      hill1 = hill1,
      hill2 = hill2,
      fs_hill1_percentile = NA,
      fs_hill2_percentile = NA,
      mete_hill1_percentile = NA,
      mete_hill2_percentile = NA
    )
    
    
  } else {
  
  fs_hill_values <- fs_hills(s, n, p_table, ndraws)
  mete_hill_values <- mete_hills(s, n, ndraws)
  
  fs_hill1_percentile <- calc_percentile(hill1, fs_hill_values$fs_hill1)
  fs_hill2_percentile <- calc_percentile(hill2, fs_hill_values$fs_hill2)
  
  mete_hill1_percentile <- calc_percentile(hill1, mete_hill_values$mete_hill1)
  mete_hill2_percentile <- calc_percentile(hill2, mete_hill_values$mete_hill2)
  
  fs_hill1_envelope <- calc_envelope(fs_hill_values$fs_hill1) 
  colnames(fs_hill1_envelope) <- paste0("fs_hill1_", colnames(fs_hill1_envelope))
  
  mete_hill1_envelope <- calc_envelope(mete_hill_values$mete_hill1)
  colnames(mete_hill1_envelope) <- paste0("mete_hill1_", colnames(mete_hill1_envelope))
  
  
  out <- data.frame(
    timestep = timestep,
    s = s,
    n = n,
    hill1 = hill1,
    hill2 = hill2,
    fs_hill1_percentile = fs_hill1_percentile,
    fs_hill2_percentile = fs_hill2_percentile,
    mete_hill1_percentile = mete_hill1_percentile,
    mete_hill2_percentile = mete_hill2_percentile
  )
  
  out <- cbind(out, fs_hill1_envelope)
  
  out <- cbind(out, mete_hill1_envelope)
  }
  
  
  return(out)
  
}

calc_percentile <- function(obs, compare) {
  
  return(mean(compare <= obs))
  
}

calc_envelope <- function(vals) {
  
  lower_2p5 = quantile(vals, probs = .025)
  lower_5 = quantile(vals, probs = .05)
  upper_95 = quantile(vals, probs = .95)
  upper_97p5 = quantile(vals, probs = .975)
  
  data.frame(
    lower_2p5 = lower_2p5,
    lower_5 = lower_5,
    upper_95 = upper_95,
    upper_97p5 = upper_97p5
  )
  
}

#' plan
#' 1. run untb sim x
#' 2. calculate max s and n, use this to create the p table x. just as easy to calc hills here. x
#' 3. for each timestep x
#'    1. draw from fs and calc  hill1 x
#'    2. draw from mete and calchill1 x
#'    3. summarize hill percentiles x 
#' 4. aggregate over timesteps
#' 
#' to do this for repeated sims, you'll probably eventually want to condense the p table step. the more you can condense and reuse the p tables the faster the overall pipeline gets. 