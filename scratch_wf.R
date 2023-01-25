library(untb)
library(dplyr)
source(here::here("R", "fxns.R"))

all_sims <- generate_sims(100, 1000, .5)
p_table <- generate_p_table(all_sims)
sim_summary <- summarize_sims(all_sims)
percentile_summaries <- apply(sim_summary, MARGIN = 1, FUN = compare_to_baselines, p_table = p_table, ndraws = 100)
percentile_summaries_df <- bind_rows(percentile_summaries)


library(ggplot2)
theme_set(theme_bw())

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


ggplot(percentile_summaries_df, aes(timestep, hill1)) + 
  geom_line() +
  geom_ribbon(aes(ymin = fs_hill1_lower_2p5, ymax = fs_hill1_upper_97p5), alpha = .5, fill = "purple")+
  geom_ribbon(aes(ymin = mete_hill1_lower_2p5, ymax = mete_hill1_upper_97p5), alpha = .5, fill= "pink")


ggplot(percentile_summaries_df, aes(timestep, hill1)) + 
  geom_line() +
  geom_ribbon(aes(ymin = mete_hill1_lower_2p5, ymax = mete_hill1_upper_97p5), alpha = .2)
