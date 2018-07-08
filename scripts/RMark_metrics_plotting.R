# plotting RMark results
# July 2018
# EKB

# LIBRARIES and DATA #
library(tidyverse)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

rmark_results <- read.csv("data/top_model_summary.csv", stringsAsFactors = FALSE)

# DATA PREP #
rmark_results$time = c("pre", "post", "pre", "post", "pre", "post", NA, 
                       "pre", "post", "pre", "post", "pre", "post",
                       "pre", "post", "pre", "post", "pre", "post")

rmark_results$metric = rep("S", nrow(rmark_results))
rmark_results$metric[7] = "p"
rmark_results$metric[8:19] = "Psi"

rmark_results$stratum = c("A", "A", "B", "B", "C", "C", NA, 
                          "AB", "AB",  "AC", "AC", "BA", "BA",
                          "BC", "BC", "CA", "CA", "CB", "CB")
rmark_results$Treatment = c("Control", "Control", "KR Exclosure", "KR Exclosure", "Removal", "Removal", NA,
                            "Control to KR Exclosure", "Control to KR Exclosure", "Control to Removal", "Control to Removal",
                            "KR Exclosure to Control", "KR Exclosure to Control", "KR Exclosure to Removal", "KR Exclosure to Removal",
                            "Removal to Control", "Removal to Control", "Removal to KR Exclosure", "Removal to KR Exclosure")

plot_rmark <- rmark_results %>% 
  filter(metric != "p", stratum == "A" | stratum == "B" | stratum == "AB" | stratum == "BA")

# PLOTTING #

plot_rmark$time <- factor(plot_rmark$time, levels = c("pre", "post"))
ggplot(plot_rmark[(plot_rmark$metric == "S"),], color = treatment) +
  geom_pointrange(aes(x = time, y = estimate, 
                      ymin = (estimate - se), ymax = (estimate + se), 
                      color = Treatment), 
                  position = position_dodge(.1), size = .75, width = .2) +
  scale_colour_manual(values = cbPalette) + 
  ggtitle("Estimated Survival") + # put title in the middle
  theme_classic()

ggsave("figures/Survival.png")

ggplot(plot_rmark[(plot_rmark$metric == "Psi"),]) +
  geom_pointrange(aes(x = time, y = estimate, 
                      ymin = (estimate - se), ymax = (estimate + se), 
                      color = stratum), 
                  position = position_dodge(.1), size = .75, width = .2) +
  ggtitle("Transition Probability") + 
  theme_classic()
ggsave("figures/Psi.png")
