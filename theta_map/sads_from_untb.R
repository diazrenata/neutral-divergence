#### Generating SADs from untb ####

library(untb)
library(dplyr)
library(ggplot2)
source(here::here("theta_map", "map_fxns.R"))

# Some toy starting pars might be v = .2, J = 250, theta = 100

v = .01
J = 250
theta = get_theta(J, v)
S = round(get_S(J, theta))

# Generate SAD

# ex_sad <- untb::expected.abundance(J, theta) Fails

# Generating things from a Fisher logseries

go_fishing <- function(S, J) {
  
  fishdraw <- fisher.ecosystem(J, S, 10 * S)
  
   as.data.frame(fishdraw)
  
}


fishing <- replicate(1000, go_fishing(S, J), simplify = F)

fishes <- bind_rows(fishing, .id = "Sim") %>%
  group_by(Sim) %>%
  mutate(Rank = dplyr::row_number()) %>%
  ungroup()

ggplot(fishes, aes(Rank, Freq, group = Sim)) +
  geom_line()

fishes_summary <- fishes %>%
  group_by(Sim) %>%
  summarize(S = dplyr::n(),
            N = sum(Freq),
            even = vegan::diversity(Freq, "simpson"))

ggplot(fishes_summary, aes(S)) +
  geom_histogram() +
  geom_vline(xintercept = S)

ggplot(fishes_summary, aes(N)) +
  geom_histogram() +
  geom_vline(xintercept = J)


ggplot(fishes_summary, aes(even)) +
  geom_histogram()

go_meteing <- function(S, J) {
  mete_ESF <- meteR::meteESF(S0 = S, N0 = J)
  mete_SAD <- meteR::sad(mete_ESF)
  
  SAD_draw <- mete_SAD$r(S)
  
  return(data.frame(
    Freq = sort(SAD_draw)
  ))
}


meteing <- replicate(1000, go_meteing(S, J), simplify = F)

metees <- bind_rows(meteing, .id = "Sim") %>%
  group_by(Sim) %>%
  mutate(Rank = dplyr::row_number()) %>%
  ungroup()

ggplot(metees, aes(Rank, Freq, group = Sim)) +
  geom_line()

metees_summary <- metees %>%
  group_by(Sim) %>%
  summarize(S = dplyr::n(),
            N = sum(Freq),
            even = vegan::diversity(Freq, "simpson"))

ggplot(metees_summary, aes(S)) +
  geom_histogram() +
  geom_vline(xintercept = S)

ggplot(metees_summary, aes(N)) +
  geom_histogram() +
  geom_vline(xintercept = J)



ggplot(metees_summary, aes(even)) +
  geom_histogram()



both_summaries <- metees_summary %>%
  mutate(model = "METE") %>%
  bind_rows(fishes_summary %>% mutate(model = "fish"))


ggplot(both_summaries, aes(even)) +
  geom_histogram() +
  facet_wrap(vars(model), ncol = 1)

