# Code for making figures for ESA talk
# EKB, July 2018

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# plot the regression (using original model)

x_axis_title <- expression(paste(bold("Mean "), bolditalic("C. baileyi"), bold(" per Plot")))
y_axis_title <- expression(paste(bolditalic("C. penicillatus"), bold(" Difference from Equal")))

ggplot(data = PP_PB_join, aes(x = PB_avg_indiv, y = PP_residuals)) +
  geom_hline(aes(yintercept = 0), color = 'black')+
  geom_point(size = 3) +
  xlab(x_axis_title) +
  ylab(y_axis_title) +
  theme_classic()+
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.margin = margin(10, 15, 10, 10))

x_axis_title <- expression(paste(bolditalic("C. baileyi"), bold(" individuals (mean per plot)")))
y_axis_title <- expression(paste(bolditalic("C. penicillatus"), bold(" residual abundance")))

# plot 1c
ggplot(PP_PB_join, aes(x = PB_avg_indiv, y = PP_residuals)) +
  geom_hline(aes(yintercept = 0), color = 'dark gray') +
  geom_smooth(aes(y = fitted(model)),  size = 1, color = "black") +
  geom_point(size = 3) +
  xlab(x_axis_title) +
  ylab(y_axis_title) +
  theme_classic()+
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.margin = margin(10, 15, 10, 10))

ggsave("preliminary_work/figures/ESA_talk_figures/2010/PP_PB_regression.png", width = 5, height = 4.5, dpi = 600)


# PP residuals through time
ggplot(PP_and_PB_fulljoin, aes(x = year, y = PP_residuals)) +
  # annotate(geom = "rect", fill = "grey", alpha = 0.4,
  #          xmin = 1995, xmax = 1998,
  #          ymin = -Inf, ymax = Inf) +
  # annotate(geom = "rect", fill = "grey", alpha = 0.4,
  #          xmin = 2008, xmax = 2010,
  #          ymin = -Inf, ymax = Inf) +
  geom_point(size = 1) +
  geom_hline(aes(yintercept = 0), color = 'black') +
  #geom_line()+
  xlab("Year") +
  ylab(y_axis_title) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        plot.title = element_text(face = "bold", size = 18, hjust = -.125),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12), 
        plot.margin = margin(10, 15, 10, 10))
ggsave("preliminary_work/figures/ESA_talk_figures/2010/PP_resid_1to1_axes.png", width = 6, height = 4, dpi = 600)

# Average PB individuals through time

y_axis_title <- expression(paste(bolditalic("C. baileyi"), bold(" individuals (mean per plot)")))

ggplot(PP_and_PB_fulljoin, aes(x = year, y = PB_avg_indiv)) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 1995, xmax = 1998,
             ymin = -Inf, ymax = Inf) +
    # annotate(geom = "rect", fill = "grey", alpha = 0.4,
    #          xmin = 2008, xmax = 2010,
    #          ymin = -Inf, ymax = Inf) +
    geom_point(size = 3) +
    geom_line() +
    xlab("Year") +
    ylab(y_axis_title) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          plot.title = element_text(face = "bold", size = 14, hjust = -.125),
          axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
          axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
          axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          plot.margin = margin(10, 15, 10, 10))
ggsave("preliminary_work/figures/ESA_talk_figures/2010/avg_PB_per_plot_1bar.png", width = 6, height = 4)


y_axis_title <- expression(atop(paste(bolditalic("C. baileyi"), bold(" individuals")),
                                bold("(mean per plot)")))

ggplot(PB_only, aes(x = year, y = avg_ind_per_prd, color = plot_type, group = plot_type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .5) +
  scale_color_manual(values = cbbPalette) +
  xlab("Year") +
  ylab(y_axis_title) +
  labs(color = "Plot type") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        plot.title = element_text(face = "bold", size = 14, hjust = -.125),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.margin = margin(10, 15, 10, 10))
ggsave("preliminary_work/figures/ESA_talk_figures/2010/FigureS1.png", height = 3, width = 6, dpi = 600)

# New PPs

y_axis_title <- expression(paste(bold("New "), bolditalic("C. penicillatus"), bold(" Individuals")))

ggplot(new_PP_per_plot_summary, aes(x = year,
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
  ylab(y_axis_title) +
  xlab("Year") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        legend.position = "top",
        legend.title = element_blank())
ggsave("preliminary_work/figures/ESA_talk_figures/2010/PP_PB_reg.png", width = 6, height = 4.5, dpi = 600)


# RMark plots

x_axis_title <- expression(paste(bolditalic("C. baileyi"), bold(" establishment")))

ggplot(plot_rmark[(plot_rmark$metric == "S"),], color = Treatment) +
    geom_pointrange(aes(x = time, y = estimate,
                        ymin = (estimate - se), ymax = (estimate + se),
                        color = Treatment),
                        position = position_dodge(.1), size = .75) +
    scale_colour_manual(values = cbbPalette) +
    xlab(x_axis_title) +
    ylab("Residency") +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.title = element_text(face = "bold", size = 18, hjust = -.355),
          axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
          axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
          axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "top",
          legend.title = element_blank())
ggsave("preliminary_work/figures/ESA_talk_figures/2010/PP_PB_residency.png", width = 4, height = 4, dpi = 600)


ggplot(plot_rmark[(plot_rmark$metric == "Psi"),]) +
    geom_pointrange(aes(x = time, y = estimate,
                        ymin = (estimate - se), ymax = (estimate + se), 
                        color = Treatment), 
                    position = position_dodge(.1), size = .75) +
    scale_colour_manual(values = cbbPalette) + 
    xlab(x_axis_title) +
    ylab("Transition Probability") +
    guides(color = guide_legend(nrow = 2)) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.title = element_text(face = "bold", size = 18, hjust = -.35),
          axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
          axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
          axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "top", 
          legend.title = element_blank(),
          plot.margin = margin(r = 10, l = 5))
ggsave("preliminary_work/figures/ESA_talk_figures/2010/PP_PB_trans_probs.png", width = 4, height = 4.5, dpi = 600)


# Biomass Ratios

ggplot(biomass_ratio, aes(year, EX_to_CO_ratio, group = 1))+
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 1995, xmax = 1998,
             ymin = -Inf, ymax = Inf) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 2008, xmax = 2010, # 2008 is the last time PBs were on 8 krat exclosure plots (366); 2010 first time not caught in a census
             ymin = -Inf, ymax = Inf) +
    geom_point(size = 3) +
    geom_line()+
    ylab("KR Exlcosure:Control Biomass") +
    xlab("Year") +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
          axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
          axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          plot.margin = margin(10, 15, 10, 10))

ggsave("figures/ESA_talk_figures/biomass.png", width = 6, height = 4.5)
