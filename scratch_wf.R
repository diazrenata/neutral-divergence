library(untb)
source(here::here("R", "fxns.R"))

all_sims <- generate_sims(100, 5000, .05)
p_table <- generate_p_table(all_sims)
sim_summary <- summarize_sims(all_sims)

