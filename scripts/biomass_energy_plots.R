# Biomass and Energy Plots for PP/PB Project
# Feb 2018
# EKB

# LIBRARIES
library(portalr)
library(ggplot2)
library(dplyr)

# BIOMASS

# download biomass data by plot from portalr
biomass_data <- portalr:::get_rodent_data(path = "repo", level = "Plot", output = "biomass")

# select only controls and krat exclosures
biomass_dat <- biomass_data %>% 
  filter(treatment == "control" | treatment == "exclosure")
biomass_dat_rowSums <- as.data.frame(rowSums(biomass_dat[,4:24])) # sum across rows
colnames(biomass_dat_rowSums) <- c("rowSums") # rename to something useful

# sum biomass to get total by period and plot type
biomass_total <- cbind(biomass_dat, biomass_dat_rowSums) %>% 
  group_by(period, treatment) %>% 
  summarise(totals = sum(rowSums))

# plot total biomass through time by plot types
ggplot(biomass_total, aes(x = period, y = totals)) +
  geom_line(aes(color = treatment)) +
  xlab("Period") +
  ylab("Total Rodent biomass") +
  theme_bw()
#ggsave("~bleds22e/Dropbox (UFL)/Grad_School/Projects/Other/figures/total_biomass_by_plot.png")

# ENERGY

# download energy data by plot from portalr
energy_data <- get_rodent_data(path = "repo", level = "Plot", output = "energy")

# select only controls and krat exclosures
energy_dat <- energy_data %>% 
  filter(treatment == "control" | treatment == "exclosure")
energy_dat_rowSums <- as.data.frame(rowSums(energy_dat[,4:24])) # sum across rows
colnames(energy_dat_rowSums) <- c("rowSums") # rename to something useful

# sum energy to get total by period and plot type
energy_total <- cbind(energy_dat, energy_dat_rowSums) %>% 
  group_by(period, treatment) %>% 
  summarise(totals = sum(rowSums))

# plot total energy through time by plot types
ggplot(energy_total, aes(x = period, y = totals)) +
  geom_line(aes(color = treatment)) +
  xlab("Period") +
  ylab("Total Rodent energy") +
  theme_bw()
#ggsave("~bleds22e/Dropbox (UFL)/Grad_School/Projects/Other/figures/total_energy_by_plot.png")

# LINEAR MODELS

# Biomass
biomass_spread <- tidyr::spread(biomass_total, treatment, totals)
biomass_spread <- biomass_spread[-(1:3),]

model <- lm(biomass_spread$exclosure ~ biomass_spread$control, data = biomass_spread)
plot(model)
summary(model)
str(model)

biomass_spread$predicted <- predict(model)
biomass_spread$residuals <- residuals(model)

plot(x = biomass_spread$control, y = biomass_spread$exclosure)
abline(0,1, col = "black")
abline(model, col = "red")
#dev.copy(png, "figures/biomass_control_vs_exc_.png")
#dev.off()

# test 1:1 line just for funsies
x = biomass_spread$control
y = biomass_spread$exclosure
model_1to1 <- lm(y-x ~ 0)
biomass_spread$residuals_1to1 <- residuals(model_1to1)
plot(x = biomass_spread$period, y = biomass_spread$residuals_1to1)
abline(h = 0, col = "red")
abline(v = 216)
dev.copy(png, "figures/biomass_resids_1to1_through_time.png")
dev.off()

# residuals over time
ggplot(biomass_spread, aes(x = period, y = residuals)) +
  geom_hline(aes(yintercept = 0), color = 'red')+
  geom_vline(aes(xintercept = 216), color = 'black') +
  geom_point(size = 2)+
  ylab("Residuals of biomass LM") +
  theme_bw()
ggsave("figures/biomass_resid_through_time.png")

# Biomass
energy_spread <- tidyr::spread(energy_total, treatment, totals)
energy_spread <- energy_spread[-(1:3),]

model <- lm(energy_spread$exclosure ~ energy_spread$control, data = energy_spread)
plot(model)
summary(model)
str(model)

energy_spread$predicted <- predict(model)
energy_spread$residuals <- residuals(model)

plot(x = energy_spread$control, y = energy_spread$exclosure)
abline(0,1, col = "black")
abline(model, col = "red")
dev.copy(png, "figures/energy_control_vs_exc_.png")
dev.off()

# test 1:1 line just for funsies
x = energy_spread$control
y = energy_spread$exclosure
model_1to1 <- lm(y-x ~ 0)
energy_spread$residuals_1to1 <- residuals(model_1to1)
plot(x = energy_spread$period, y = energy_spread$residuals_1to1)
abline(h = 0, col = "red")
abline(v = 216)
dev.copy(png, "figures/energy_resids_1to1_through_time.png")
dev.off()

# residuals through time
ggplot(energy_spread, aes(x = period, y = residuals)) +
  geom_hline(aes(yintercept = 0), color = 'red')+
  geom_vline(aes(xintercept = 216), color = "red") +
  geom_point(size = 2)+
  ylab("Residuals of Energy LM") +
  theme_bw()
ggsave("figures/energy_resid_through_time.png")

# ENERGY CONSUMPTION 

# ratio
energy_ratio <- energy_spread %>% mutate(EX_to_CO_ratio = exclosure/control)

ggplot(energy_ratio, aes(period, EX_to_CO_ratio))+
  geom_line()+
  geom_vline(xintercept = 216, color = "red") +
  xlab("Period") +
  ylab("Exlcosure:Control energy Use") +
  theme_bw()
ggsave("figures/energy_use_ratio.png")  
