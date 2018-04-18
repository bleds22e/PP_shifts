# plotting RMark results

rmark_results <- read.csv("data/MARKdata/MARKoutput_PP_all_S.stratum.p.dot.Psi.s.csv", stringsAsFactors = FALSE)

rmark_results$time = c("pre", "post", "pre", "post", "pre", "post", NA, 
                       "pre", "post", "pre", "post", "pre", "post",
                       "pre", "post", "pre", "post", "pre", "post")

rmark_results$metric = rep("S", nrow(rmark_results))
rmark_results$metric[7] = "p"
rmark_results$metric[8:19] = "Psi"

rmark_results$stratum = c("A", "A", "B", "B", "C", "C", NA, 
                          "AB", "AB",  "AC", "AC", "BA", "BA",
                          "BC", "BC", "CA", "CA", "CB", "CB")

library(tidyverse)

plot_rmark <- rmark_results %>% 
  filter(metric != "p", stratum == "A" | stratum == "B" | stratum == "AB" | stratum == "BA")

ggplot(plot_rmark[(plot_rmark$metric == "S"),]) +
  geom_pointrange(aes(x = time, y = estimate, 
                      ymin = (estimate - se), ymax = (estimate + se), 
                      color = stratum), 
                  position = position_dodge(.1), size = .75, width = .2) +
  theme_classic()
ggsave("figures/Survival.png")

ggplot(plot_rmark[(plot_rmark$metric == "Psi"),]) +
  geom_pointrange(aes(x = time, y = estimate, 
                      ymin = (estimate - se), ymax = (estimate + se), 
                      color = stratum), 
                  position = position_dodge(.1), size = .75, width = .2) +
  theme_classic()
ggsave("figures/Psi.png")
