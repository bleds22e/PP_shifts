# Code for making figures for ESA talk
# EKB, July 2018


# plot the regression (using original model)
ggplot(data = PP_and_PB_innerjoin, aes(x = PB_avg_indiv, y = PP_residuals)) +
  geom_hline(aes(yintercept = 0), color = 'black')+
  geom_point(size = 3) +
  xlab("Average PB per Plot by Year") +
  ylab("PP Residuals for 1:1 Line") +
  theme_classic()+
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.margin = margin(10, 15, 10, 10)))
#ggsave("figures/ESA_talk_figures/PP_residuals_PB_abund_notitle.png", plot1, width = 5, height = 4.5)


# PP residuals through time
ggplot(PP_and_PB_fulljoin, aes(x = year, y = PP_residuals)) +
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 1995, xmax = 1998,
           ymin = -Inf, ymax = Inf) +
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 2008, xmax = 2010,
           ymin = -Inf, ymax = Inf) +
  geom_hline(aes(yintercept = 0), color = 'black') +
  geom_point(size = 3) +
  geom_line()+
  xlab("Year") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        plot.title = element_text(face = "bold", size = 18, hjust = -.125),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12), 
        plot.margin = margin(10, 15, 10, 10))
#ggsave("figures/ESA_talk_figures/PP_resid_1to1.png")

# Average PB individuals through time
ggplot(PP_and_PB_fulljoin, aes(x = year, y = PB_avg_indiv)) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 1995, xmax = 1998,
             ymin = -Inf, ymax = Inf) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 2008, xmax = 2010,
             ymin = -Inf, ymax = Inf) +
    geom_point(size = 3) +
    geom_line() +
    xlab("Year") +
    ylab("Average PB per Plot") +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          plot.title = element_text(face = "bold", size = 14, hjust = -.125),
          axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
          axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
          axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          plot.margin = margin(10, 15, 10, 10))
#ggsave("figures/ESA_talk_figures/avg_PB_per_plot_per_year.png")

# New PPs
ggplot(new_PP_per_plot, aes(x = year,
                            y = avg_plot_sum_by_year,
                            color = plot_type,
                            group = plot_type)) +
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 1995, xmax = 1998,
           ymin = -Inf, ymax = Inf) +
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 2008, xmax = 2010,
           ymin = -Inf, ymax = Inf) +
  scale_color_manual(values = cbbPalette, name = "Plot Type") +
  geom_point(size = 2.5) +
  geom_line() +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .5) +
  ylab("Avg. New PP Individuals per Plot") +
  xlab("Year") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        legend.position = "bottom")
#ggsave("figures/ms_figures/new_PP_per_year.png", plot6, width = 6, height = 4.5)
