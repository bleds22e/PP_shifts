# Movement of PPs based on krats and PBs
# EKB 3/2017

### LIBRARIES

library(dplyr)
library(ggplot2)
library(RCurl)
source('scripts/season_function.R')

#insert location here
rodents <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv") 

### DATA

rdat <- read.csv(text = rodents, header = TRUE, na.strings = c(""))
rdat <- select(rdat, period, year, month, plot, species) %>% 
  filter(period > 0,
         year > 1987,
         plot > 0,
         species %in% c('PP', 'PB', 'DO', 'DM', 'DS'))

### PREP DATA

rdat$sp_type = NA
for (i in 1:length(rdat$period)){
  if (rdat$species[i] %in% c('DO', 'DM', 'DS')){
    rdat$sp_type[i] = 'Dipo'
  } else if (rdat$species[i] == 'PP'){
    rdat$sp_type[i] = 'PP'
  } else {
    rdat$sp_type[i] = 'PB'
  }
}

rdat$plot_type = NA
for (i in 1:length(rdat$period)){
  if (rdat$plot[i] %in% c(1, 2, 4, 8, 9, 11, 12, 14, 17, 22)){
    rdat$plot_type[i] = 'Control'
  } else if (rdat$plot[i] %in% c(3, 6, 13, 15, 18, 19, 20, 21 )){
    rdat$plot_type[i] = 'Dipo_Exclosure'
  } else {
    rdat$plot_type[i] = 'Full_Exclosure' # 5, 7, 10, 16, 23, 24
  }
}

rdat <- rdat %>% filter(plot_type != 'Full_Exclosure') 

rdat <- add_seasons(rdat, 2)

#============================================================#

### Relative Abundances

# by year
count <- select(rdat, year, sp_type, plot_type) %>% 
  group_by(year, plot_type, sp_type) %>% 
  summarise(count = n())
total <- select(count, year, sp_type, plot_type, count) %>% 
  ungroup() %>% 
  group_by(year, plot_type) %>% 
  summarise(total = sum(count))
joined <- inner_join(count, total, by = c("year", "plot_type")) %>% 
  mutate(rel_abund = count/total) %>% 
  rename(Year = year, Species = sp_type, Plot = plot_type, Relative_Abundance = rel_abund)

# season & year
count_s <- select(rdat, season2_yr, sp_type, plot_type, season2) %>% 
  group_by(season2_yr, season2, plot_type, sp_type) %>% 
  summarise(count = n())
total_s <- select(count_s, season2_yr, sp_type, plot_type, season2, count) %>% 
  ungroup() %>% 
  group_by(season2_yr, plot_type, season2) %>% 
  summarise(total = sum(count))
joined_s <- inner_join(count_s, total_s, by = c("season2_yr", "season2", "plot_type")) %>% 
  mutate(rel_abund = count/total) %>% 
  rename(Year = season2_yr, Season = season2, Species = sp_type, Plot = plot_type, Relative_Abundance = rel_abund)

### PLOT

# year
ggplot(joined, aes(x = Year, y = Relative_Abundance, fill = Species)) +
  geom_bar(position = position_stack(), stat = "identity") +
  facet_wrap( ~ Plot, nrow = 2) 

#ggsave('figures/PP_movement_year.png')

# season and year
ggplot(joined_s, aes(x = Year, y = Relative_Abundance, fill = Species)) +
  geom_bar(position = position_stack(), stat = "identity") +
  facet_grid(Plot ~ Season)

#ggsave("figures/PP_movement_season.png")

#========================================================#

### NUMBER OF INDIVIDUALS

# avg. number of individual per plot, by year
avg_by_year <- rdat %>% 
  group_by(plot, year, sp_type, plot_type) %>% 
  summarise(count = n()) %>% 
  ungroup()
avg_by_year <- avg_by_year %>% 
  group_by(year, sp_type, plot_type) %>%  
  summarise(avg_indiv = mean(count))

# avg. number of individual per plot, by season & year
avg_by_season_yr <- rdat %>% 
  group_by(plot, season2_yr, season2, sp_type, plot_type) %>% 
  summarise(count = n()) %>% 
  ungroup()
avg_by_season_yr <- avg_by_season_yr %>% 
  group_by(season2, season2_yr, sp_type, plot_type) %>% 
  summarise(avg_indiv = mean(count))

### PLOTS

ggplot(avg_by_year, aes(x = year, y = avg_indiv, color = sp_type)) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ plot_type, nrow = 2) +
  theme_bw()

#ggsave("figures/avg_PP_indv_year.png")

ggplot(avg_by_season_yr, aes(x = season2_yr, y = avg_indiv, color = sp_type)) +
  geom_point() +
  geom_line() +
  facet_grid(plot_type ~ season2) +
  theme_bw()

#ggsave("figures/avg_indv_season_yr.png")

# PPs on Controls vs PPs on dipo-exclosures
controls <- avg_by_year %>% 
  filter(plot_type == 'Control', sp_type == 'PP') %>% 
  rename(Controls = avg_indiv) %>% 
  select(-plot_type)
dipo_ex <- avg_by_year %>% 
  filter(plot_type == 'Dipo_Exclosure', sp_type == 'PP') %>% 
  rename(Dipo_Exclosures = avg_indiv) %>% 
  select(-plot_type)
joined_PP <- full_join(controls, dipo_ex, by = c("year", "sp_type")) %>% 
  filter(year < 2017)

ggplot(joined_PP, aes(x = Controls, y = Dipo_Exclosures))+
  geom_point(size = 2) +
  stat_smooth(method = "lm") +
  theme_bw()
#ggsave("figures/PP_by_plot_type.png")

model <- lm(joined_PP$Dipo_Exclosures ~ joined_PP$Controls)
summary(model)

# is this significantly different from 1:1 line?
model_1 <- lm(joined_PP$Dipo_Exclosures ~ 1 + offset(joined_PP$Controls))
summary(model_1)

anova(model, model_1) # not significant
car::linearHypothesis(model, 'joined_PP$Controls = 1') # not significant

joined_PP$predicted <- predict(model)
joined_PP$residuals <- residuals(model)

# residuals over time
ggplot(joined_PP, aes(x = year, y = residuals)) +
  geom_hline(aes(yintercept = 0), color = 'red')+
  geom_point(size = 2)+
  theme_bw()

#ggsave('figures/PP_residuals_over_time.png')

# along a 1:1 line
x = joined_PP$Controls
y = joined_PP$Dipo_Exclosures
resid_1to1 <- resid(lm(y-x ~ 0))

plot(resid_1to1)
abline(h = 0)
dev.copy(png, "figures/resid_1_to_1.png")
dev.off()

# two regressions by season
controls_season <- avg_by_season_yr %>% 
  filter(plot_type == 'Control', sp_type == 'PP') %>% 
  rename(Controls = avg_indiv) %>% 
  select(-plot_type)
dipo_ex_season <- avg_by_season_yr %>% 
  filter(plot_type == 'Dipo_Exclosure', sp_type == 'PP') %>% 
  rename(Dipo_Exclosures = avg_indiv) %>% 
  select(-plot_type)
joined_PP_season <- full_join(controls_season, dipo_ex_season, by = c("season2_yr", "sp_type", "season2")) %>% 
  filter(season2_yr < 2017)

ggplot(joined_PP_season, aes(x = Controls, y = Dipo_Exclosures))+
  geom_point(size = 2) +
  stat_smooth(method = "lm", aes(group = season2, color = season2), se = FALSE) +
  theme_bw()
#ggsave('figures/PP_reg_by_season.png')

### Plot residuals against number PBs
PB_avg_year <- select(avg_by_year, year, sp_type, avg_indiv) %>%
  group_by(year) %>% 
  filter(sp_type == 'PB') %>% 
  select(-sp_type) %>% 
  summarise(PB_avg_indiv = sum(avg_indiv))
PP_and_PB <- inner_join(PB_avg_year, joined_PP, by = "year")

ggplot(data = PP_and_PB, aes(x = PB_avg_indiv, y = residuals)) +
  geom_hline(aes(yintercept = 0), color = 'red')+
  geom_point(size = 2) + 
  stat_smooth(method = 'lm', formula = y ~ poly(x,2)) +
  theme_bw()
#ggsave('figures/PP_resid_by_PB_abund.png')

# residuals and PB abund over time
dat <- select(PP_and_PB, year, PB_avg_indiv)
dat1 <- unique(joined_PP$year)
dat1 <- as.data.frame(dat1)
colnames(dat1) <- 'year'
full_PB_years <- full_join(dat1, dat, by = 'year')
full_PB_years[is.na(full_PB_years)] <- 0

library(gridExtra)
plot1 <- ggplot(joined_PP, aes(x = year, y = residuals)) +
  geom_hline(aes(yintercept = 0), color = 'red')+
  geom_point(size = 2)+
  ggtitle("PP Residuals") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
plot2 <- ggplot(full_PB_years, aes(x = year, y = PB_avg_indiv)) +
  geom_point(size = 2) +
  geom_line() +
  ggtitle("PB Avg. Individuals per Plot by Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
plot3 <- grid.arrange(plot2, plot1, nrow = 2)
#ggsave("figures/PP_resid_PB_avg.png", plot = plot3)