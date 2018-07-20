# plotting RMark results
# July 2018
# EKB

# LIBRARIES and DATA #
library(tidyverse)
library(patchwork)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

rmark_results <- read.csv("data/MARKdata/MARK_SerenityRun/top_model_summary_20180712.csv", stringsAsFactors = FALSE)

# DATA PREP #
rmark_results$time = c("Before", "After", "Before", "After", "Before", "After", NA, 
                       "Before", "After", "Before", "After", "Before", "After",
                       "Before", "After", "Before", "After", "Before", "After")

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
plot_rmark$time <- factor(plot_rmark$time, levels = c("Before", "After"))

# PLOTTING #

x_axis_title <- expression(paste(bolditalic("C. baileyi"), bold(" Infiltration Status")))

plot1 <- ggplot(plot_rmark[(plot_rmark$metric == "S"),], color = Treatment) +
  geom_pointrange(aes(x = time, y = estimate, 
                      ymin = (estimate - se), ymax = (estimate + se), 
                      color = Treatment, shape = Treatment), 
                      position = position_dodge(.1), size = 1, width = .2) +
  scale_colour_manual(values = cbPalette) + 
  xlab(x_axis_title) +
  ylab("Estimated Survival") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12))

#ggsave("figures/Survival.png")

(plot2 <- ggplot(plot_rmark[(plot_rmark$metric == "Psi"),], aes(x = time, y = estimate)) +
  geom_pointrange(aes(ymin = (estimate - se), ymax = (estimate + se), 
                      color = Treatment, shape = Treatment), 
                      position = position_dodge(.1), size = 1) +
  scale_colour_manual(values = cbPalette) + 
  xlab(x_axis_title) +
  ylab("Transition Probability") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12)))

#ggsave("figures/Psi.png")

plot3 <- plot1 + plot2
#ggsave("figures/S_and_Psi.png", plot3)
