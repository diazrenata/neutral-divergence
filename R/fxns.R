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
                    hill1 = hill1,
                    hill2 = hill2))
  
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
    fs_hill1 = fs_hill1,
    fs_hill2 = fs_hill2
  ))
  
}

mete_hills <- function(s, n, ndraws) {
  mete_esf <- meteR::meteESF(S0 = s, N0 = n)
  
  mete_sad <- meteR::sad(mete_esf)
  
  mete_sad_draws <- replicate(n = ndraws, sort(mete_sad$r(s)), simplify = T) %>%
    as.data.frame()
}

#' plan
#' 1. run untb sim x
#' 2. calculate max s and n, use this to create the p table x. just as easy to calc hills here. x
#' 3. for each timestep x
#'    1. draw from fs and calc  hill1 x
#'    2. draw from mete and calchill1 IN PROGRESS
#'    3. summarize hill percentiles
#' 4. aggregate over timesteps
#' 
#' to do this for repeated sims, you'll probably eventually want to condense the p table step. the more you can condense and reuse the p tables the faster the overall pipeline gets. 