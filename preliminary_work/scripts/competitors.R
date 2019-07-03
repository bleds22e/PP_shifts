PB_Dipos <- all %>% 
  filter(species %in% c('PB', 'DM', 'DO', 'DS'))

# total number new PPs (avg plot sum by year)
PB_Dipo_per_plot <- PB_Dipos %>%
  filter(plot_type != "Removal") %>% 
  group_by(plot, month, year, plot_type) %>%
  summarise(count = n(species)) %>%
  ungroup()
PB_Dipo_per_plot <- PB_Dipo_per_plot %>%
  group_by(year, plot_type, plot) %>% 
  summarise(count = sum(count)) %>% 
  ungroup()

# put zeros where they belong
PB_Dipo_per_plot <- full_join(all_plots_all_years, PB_Dipo_per_plot) %>%
  filter(plot_type != "Removal")
PB_Dipo_per_plot$count[is.na(PB_Dipo_per_plot$count)] <- 0

PB_Dipo_per_plot$time_point <- NA

for (i in 1:nrow(PB_Dipo_per_plot)) {
  if (PB_Dipo_per_plot$year[i] <= 1997) {
    PB_Dipo_per_plot$time_point[i] = "Before"
  } else {
    PB_Dipo_per_plot$time_point[i] = "After"
  }
}

competitor_summary <- PB_Dipo_per_plot %>% 
  group_by(year, plot_type) %>% 
  summarise(avg_plot_sum_by_year = mean(count), se = plotrix::std.error(count))

competitor_summary <- competitor_summary %>%
  mutate(ymin = avg_plot_sum_by_year - se,
         ymax = avg_plot_sum_by_year + se) 

competitor_summary_noPB <- PB_Dipo_per_plot %>% 
  filter(species != 'PB') %>% 
  group_by(year, plot_type) %>% 
  summarise(avg_plot_sum_by_year = mean(count), se = plotrix::std.error(count))

competitor_summary_noPB <- competitor_summary_noPB %>%
  mutate(ymin = avg_plot_sum_by_year - se,
         ymax = avg_plot_sum_by_year + se)


test3 <- PB_Dipo_per_plot %>% 
  filter(species != 'PB', year == 1990, plot_type == "Control")

plot_avg_competitors <- function(data){
  
  # rename plot_treatments for plotting
  data$plot_type <- plyr::revalue(data$plot_type, c("Krat_Exclosure" = "KR Exclosure"))
  
  y_axis_title <- expression(atop("Mean competitors"),
                             phantom('W'))
  
  plot <- ggplot(data, aes(x = year,
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
    labs(subtitle = 'c') +
    #guides(color = guide_legend(override.aes = list(size = 3))) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 14, hjust = -.14, vjust = -.5), 
          axis.title.x = element_text(size = 12, margin = margin(t = 10)),
          axis.title.y = element_text(size = 12, margin = margin(r = -10)),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "top", 
          legend.title = element_blank(),
          plot.margin = margin(r = 10, t = 15))
  
  return(plot)
  
}

(total_competitors <- plot_avg_competitors(competitor_summary))

ggsave("figures/1989-2010/Total_Competitors_2.png", total_competitors)

after_PB <- PB_Dipo_per_plot %>% filter(year > 1997)

results.lme <- lme(count ~ plot_type, random = list(~ 1 | year, ~ 1 | plot), data = after_PB)
anova(results.lme)
