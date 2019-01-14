# Making new figures with multipanelfigure
#   - also according the Ecology guidelines
# Jan 2019

library(multipanelfigure)

# Figure 1

x_axis_title <- expression(paste("Mean ", italic("C. baileyi"), " per plot"))
y_axis_title <- expression(paste(italic("C. penicillatus"), " difference from equal"))

# plot 1c
plot1c <- ggplot(data = PP_PB_join, aes(x = PB_avg_indiv, y = PP_residuals)) +
  geom_hline(aes(yintercept = 0), color = 'dark gray') +
  geom_smooth(aes(y = fitted(PP_PB_model_linear_AR1)),  size = 1, color = "black") +
  geom_point(size = 2) +
  xlab(x_axis_title) +
  ylab(y_axis_title) +
  labs(subtitle = 'c') +
  theme_classic()+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
        plot.subtitle = element_text(size = 10, hjust = -.075),
        axis.line = element_line(size = .25),
        axis.title.x = element_text(size = 10, margin = margin(t = 10)),
        axis.title.y = element_text(size = 10, margin = margin(r = 5)),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.margin = margin(l = 25))

# plot 1b
y_axis_title <- expression(paste(italic("C. pen."), " diff. from equal"))

plot1b <- ggplot(PP_and_PB_fulljoin, aes(x = year, y = PP_residuals)) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 1995, xmax = 1998,
             ymin = -Inf, ymax = Inf) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 2008, xmax = 2010,
             ymin = -Inf, ymax = Inf) +
    geom_hline(aes(yintercept = 0), color = 'black') +
    geom_point(size = 2) +
    geom_line()+
    xlab("Year") +
    ylab(y_axis_title) +
    labs(subtitle = 'b') +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 10, hjust = -.05),
          axis.line = element_line(size = .25),
          axis.title.x = element_text(size = 10, margin = margin(t = 10)),
          axis.title.y = element_text(size = 8, margin = margin(r = 5)),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          plot.margin = margin(r = 15, l = 5))

# plot 1a
y_axis_title <- expression(paste("Mean ", italic("C. baileyi"), " per plot"))

plot1a <- ggplot(PP_and_PB_fulljoin, aes(x = year, y = PB_avg_indiv)) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 1995, xmax = 1998,
             ymin = -Inf, ymax = Inf) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 2008, xmax = 2010,
             ymin = -Inf, ymax = Inf) +
    geom_point(size = 2) +
    geom_line() +
    xlab("Year") +
    ylab(y_axis_title) +
    labs(subtitle = 'a') +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 10, hjust = -.05),
          axis.line = element_line(size = .25),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8, margin = margin(r = 5)),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          plot.margin = margin(r = 15, l = 5))

# create figure 1

####### this is where things get wonky
# figure1 <- multi_panel_figure(width = c(75, 75), height = c(35, 35), 
#                               panel_label_type = "lower-alpha")
# figure1
# 
# figure1 %<>% fill_panel(plot1a, row = 1, column = 1)
# figure1 %<>% fill_panel(plot1b, row = 2, column = 1)
# figure1 %<>% fill_panel(plot1c, row = 1:2, column = 2)
# figure1
# 
# ggsave("figures/ms_figures/fig1_test.png", plot1, width = 7, height = 3.5, dpi = 600)

plot1 <- (plot1a/plot1b) | plot1c
plot1

ggsave("figures/ms_figures/fig1_test.png", plot1, width = 7, height = 3.5, dpi = 600)

# Figure 2

x_axis_title <- expression(paste(italic("C. baileyi"), " establishment"))
y_axis_title <- expression(paste("Estimated Survival, ", italic("S")))

ggplot(plot_rmark[(plot_rmark$metric == "S"),], color = Treatment) +
    geom_pointrange(aes(x = time, y = estimate, 
                        ymin = (estimate - se), ymax = (estimate + se), 
                        color = Treatment), 
                    position = position_dodge(.1), size = .75) +
    scale_colour_manual(values = cbbPalette) + 
    xlab(x_axis_title) +
    ylab("Estimated survival") + # how to you insert greek letters into titles?
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          axis.line = element_line(size = .25),
          axis.title.x = element_text(size = 14, margin = margin(t = 10)),
          axis.title.y = element_text(size = 14, margin = margin(r = 10)),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.position = "top",
          legend.title = element_blank(),
          plot.margin = margin(l = 5, t = 20))

ggplot(plot_rmark[(plot_rmark$metric == "Psi"),]) +
    geom_pointrange(aes(x = time, y = estimate,
                        ymin = (estimate - se), ymax = (estimate + se), 
                        color = Treatment), 
                    position = position_dodge(.1), size = .75) +
    scale_colour_manual(values = cbbPalette) + 
    xlab(x_axis_title) +
    ylab("Transition probability") +
    guides(color = guide_legend(nrow = 2)) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          axis.line = element_line(size = .25),
          axis.title.x = element_text(size = 14, margin = margin(t = 10)),
          axis.title.y = element_text(size = 14, margin = margin(r = 10)),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.position = "top", 
          legend.title = element_blank(),
          plot.margin = margin(l = 5))
