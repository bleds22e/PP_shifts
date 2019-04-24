# Testing for significance of time and treatment on new PPs
# EKB
# April 2019

source("scripts/PP_shifts.R")

### Test 2-way ANOVAs ###

# Things to deal with:
#   - which way to summarize the data
#   - what about after 2010? seems to be washing out treatment and interaction

# 2-way ANOVA by monthly count

test1 <- new_PP_per_plot
test1$time_point <- NA

for (i in 1:nrow(test1)) {
  if (test1$year[i] < 1997) {
    test1$time_point[i] = "Before"
  } else if (test1$year[i] > 1997){
    test1$time_point[i] = "After"
  } else {
    if (test1$month[i] < 7){
      test1$time_point[i] = "Before"
    } else {
      test1$time_point[i] = "After"
    }
  }
}

test1 <- filter(test1, year <= 2010)
anova2w <- aov(count ~ plot_type * time_point, data = test1)
summary(anova2w)

interaction.plot(test1$year, test1$plot_type, test1$count)

# 2-way ANOVA by year

test2 <- new_PP_per_plot %>%
  group_by(year, plot_type, plot) %>% 
  summarise(count = sum(count))

all_plots_all_years <- all %>% tidyr::expand(nesting(plot, plot_type), year)

test2<- full_join(all_plots_all_years, test2) %>% 
  filter(plot_type != "Removal")
test2$count[is.na(test2$count)] <- 0


test2$time_point <- NA

for (i in 1:nrow(test2)) {
  if (test2$year[i] <= 1997) {
    test2$time_point[i] = "Before"
  } else {
    test2$time_point[i] = "After"
  }
  
  # if (test2$plot_type[i] == 'Control'){
  #   test2$count[i] = test2$count[i]/10
  # } else {
  #   test2$count[i] = test2$count[i]/8
  # }
}

test2 <- filter(test2, year <= 2010)
anova2w <- aov(count ~ plot_type * time_point, data = test2)
summary(anova2w)

interaction.plot(test2$year, test2$plot_type, test2$count)
#
# ggplot(data = new_PP_per_plot, aes(x = year, y = count, color = plot_type)) +
#   geom_point() +
#   geom_line() +
#   theme_bw()

# 2-way ANOVA by avg per plot per year

new_PP_per_plot_summary$time_point <- NA

for (i in 1:nrow(new_PP_per_plot_summary)) {
  if (new_PP_per_plot_summary$year[i] <= 1997) {
    new_PP_per_plot_summary$time_point[i] = "Before"
  } else {
    new_PP_per_plot_summary$time_point[i] = "After"
  }
}

new_PP_per_plot_summary <- filter(new_PP_per_plot_summary, year <= 2010)
anova2w <- aov(avg_plot_sum_by_year ~ plot_type * time_point, data = new_PP_per_plot_summary)
summary(anova2w)

interaction.plot(new_PP_per_plot_summary$year, new_PP_per_plot_summary$plot_type, new_PP_per_plot_summary$avg_plot_sum_by_year)


### Test repeated-measures ANOVAs ###

# using test2 data
# actually more accurate data because divided by total number of plots
results.lme <- nlme::lme(count ~ plot_type*time_point, random = ~ 1 | year, data = test2)
anova(results.lme)

results.lme <- lme4::lmer(count ~ plot_type*time_point + (1 | year) + (1 | plot), data = test2)
anova(results.lme)

model1 <- aov(count ~ plot_type*time_point + Error(year), data = test2)
summary(model1)



# try with avg by plot
model2 <- aov(avg_plot_sum_by_year ~ plot_type * time_point + Error(year/plot_type*time_point), data = new_PP_per_plot_summary)
summary(model1)
results.lme2 <- lme(avg_plot_sum_by_year ~ plot_type*time_point, random = ~1|year, data = new_PP_per_plot_summary)
anova(results.lme2)
