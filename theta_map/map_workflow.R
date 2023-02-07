library(dplyr)
library(ggplot2)

vs <- seq(.001, .9, by = .01)

js <- seq(50, 40000, by = 100) 

vs_and_js <- expand.grid(vs, js) %>%
  rename(v = Var1,
         J = Var2) %>%
  mutate(S = get_S_from_pars(J = J, v= v)) 





ggplot(vs_and_js, aes(J, v, color = S)) + 
  geom_point(alpha = .02) +
  #scale_x_log10() +
  scale_color_viridis_c(option = "plasma", end = .8, trans = "log10") +
  geom_point(data= filter(vs_and_js, S < 300))
