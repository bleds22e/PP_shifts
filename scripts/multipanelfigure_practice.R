# Making new figures with multipanelfigure
# Jan 2019

library(multipanelfigure)

# Figure 1

x_axis_title <- expression(paste("Mean ", italic("C. baileyi"), " per plot"))
y_axis_title <- expression(paste(italic("C. penicillatus"), " difference from equal"))

# plot 1c
plot1c <- ggplot(data = PP_PB_join, aes(x = PB_avg_indiv, y = PP_residuals)) +
  geom_hline(aes(yintercept = 0), color = 'dark gray') +
  geom_smooth(aes(y = fitted(PP_PB_model_linear_AR1)),  size = 1, color = "black") +
  geom_point(size = 3) +
  xlab(x_axis_title) +
  ylab(y_axis_title) +
  theme_classic()+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
        axis.line = element_line(size = .25),
        axis.title.x = element_text(size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(size = 14, margin = margin(r = 5)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

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
    geom_point(size = 3) +
    geom_line()+
    xlab("Year") +
    ylab(y_axis_title) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          axis.line = element_line(size = .25),
          axis.title.x = element_text(size = 14, margin = margin(t = 10)),
          axis.title.y = element_text(size = 14, margin = margin(r = 5)),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
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
    geom_point(size = 3) +
    geom_line() +
    xlab("Year") +
    ylab(y_axis_title) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          axis.line = element_line(size = .25),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 14, margin = margin(r = 5)),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.margin = margin(r = 15, l = 5))


