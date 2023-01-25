library(untb)
library(dplyr)
source(here::here("R", "fxns.R"))

all_sims <- generate_sims(100, 10000, .05)
p_table <- generate_p_table(all_sims)
sim_summary <- summarize_sims(all_sims)
percentile_summaries <- apply(sim_summary, MARGIN = 1, FUN = compare_to_baselines, p_table = p_table, ndraws = 1000)
percentile_summaries_df <- bind_rows(percentile_summaries)


library(ggplot2)

long_percentiles <- percentile_summaries_df %>%
  tidyr::pivot_longer(-timestep, names_to = "vars", values_to = "vals") %>%
  mutate(is_percentile = grepl("percentile", vars))

ggplot(filter(long_percentiles, is_percentile), aes(timestep, vals, color = vars)) +
  geom_line() +
  facet_wrap(vars(vars)) +
  geom_hline(yintercept = .05)


ggplot(filter(long_percentiles, !is_percentile), aes(timestep, vals, color = vars)) +
  geom_line() +
  facet_wrap(vars(vars), scales = "free_y")



ggplot(filter(long_percentiles), aes(timestep, vals, color = vars)) +
  geom_line() +
  facet_wrap(vars(vars), scales = "free_y")
