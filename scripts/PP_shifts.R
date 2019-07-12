# PP Shifts Paper
# Ellen K. Bledsoe
# May 2019

# LIBRARIES and SOURCE CODE #

library(tidyverse)
library(portalr)
library(RCurl)
library(plotrix)
library(RMark)
library(forecast)
library(nlme)
library(patchwork) # devtools::install_github("thomasp85/patchwork")
library(rapportools)

source("scripts/functions.r")

# DATA FILES

# rodent file from repo
rodents <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv")
rdat <- read.csv(text = rodents, header = TRUE, na.strings = c(""), stringsAsFactors = FALSE)

# species file
species <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_species.csv")
sdat <- read.csv(text = species, header = TRUE, na.strings = c(""), stringsAsFactors = FALSE)

# trapping file from repo
trapping <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_trapping.csv")
tdat <- read.csv(text = trapping, header = TRUE, stringsAsFactors = FALSE)


##########################################################
# DATA PREP
##########################################################

#---------------------------------------------------------
# Clean the Data
#---------------------------------------------------------

# make it match Sarah Supp's data structure to use her code
all <- repo_data_to_Supp_data(rdat, sdat) %>% 
  filter(year <= 2010)

# make table of plots and treatment types
plots_and_treatments <- unique(all[c("plot", "plot_type")]) %>% arrange(plot)
all_plots_all_years <- all %>% tidyr::expand(nesting(plot, plot_type), year)

# Find and Remove Periods with One Day of Trapping

# summarize trapping
trap_count <- tdat %>%
  group_by(period) %>%
  summarise(count = sum(sampled))
bad_periods <- filter(trap_count, count < 20) # periods that weren't fully trapped
bad_periods <- as.list(bad_periods$period)

# don't use periods with only one day of trapping
all_no_incomplete = all[-which(all$period %in% bad_periods),] 

#---------------------------------------------------------
# Figure out PB "burn in" period
#---------------------------------------------------------

# get PB per plot per period
PB <- all %>% filter(species == 'PB')
PB_plot_count <- PB %>%
  select(period, Treatment_Number, plot) %>%
  group_by(period, Treatment_Number) %>%
  summarise(count = n_distinct(plot))

PB_min <- min(PB$period) # when PB first show up
PB_max <- min(PB_plot_count$period[PB_plot_count$count == 8]) 
  #first time PBs are found in all 8 krat exclosures

# PB decline
# 2008 (prd 366) = last time caught in all 8 krat exclosures
# 2010 (prd 388) = first time not caught during a survey since PB_max


##################################################################################
# Patch Preference of C. penicillatus in Response to C. baileyi Abundance
##################################################################################

no_removals <- all_no_incomplete %>% filter(Treatment_Number != 3)

# find number of periods per year
prd_per_year <- no_removals %>% 
  group_by(year) %>% 
  summarise(prd_per_yr = n_distinct(period))

#----------------------------------------------------------
# Average Number of PP Individual per Plot per Year
#----------------------------------------------------------

avg_by_year <- # count of individuals in each plot
  no_removals %>% 
  filter(species == 'PP' | species == 'PB') %>%
  group_by(plot, year, species, plot_type) %>%
  summarise(count = n(species)) %>%
  ungroup()

all_plots_all_years_PP_PB <- all_plots_all_years %>%
  tidyr::expand(nesting(plot, plot_type), species = c("PP", "PB"), year)
avg_by_year <- full_join(all_plots_all_years_PP_PB, avg_by_year) %>%
  filter(plot_type != "Removal")
avg_by_year$count[is.na(avg_by_year$count)] <- 0

avg_by_year <- # average by treatment type per year
  avg_by_year %>% 
  group_by(year, species, plot_type) %>%
  summarize(avg_indiv = mean(count), se = plotrix::std.error(count)) 

avg_by_year <- # adjust for number of periods per year
  right_join(avg_by_year, prd_per_year, by = "year") %>% 
  mutate("avg_ind_per_prd" = avg_indiv/prd_per_yr, "se_by_prd" = se/prd_per_yr)

# fix plot types for plotting
avg_by_year_plotting <- avg_by_year %>% 
  mutate(ymin = avg_ind_per_prd - se_by_prd,
         ymax = avg_ind_per_prd + se_by_prd) %>%
  replace_na(list(avg_ind_per_prd = 0, se_by_prd = 0, ymin = 0, ymax = 0))
avg_by_year_plotting$plot_type <- plyr::revalue(avg_by_year$plot_type, c("Krat_Exclosure" = "KR Exclosure")) 

# Plot PB by treatment
PB_only <- filter(avg_by_year_plotting, species == "PB", year > 1994)

(plot_supp1 <- plot_PB_timeseries_by_treament(PB_only))

# ggsave("figures/1989-2010/FigureS1.png", plot_supp1, height = 3, width = 6, dpi = 600)
# ggsave("figures/1989-2010/FigureS1.tiff", plot_supp1,
#        height = 3, width = 6, dpi = 600, compression = "lzw")

#-----------------------------------------------------------
# Find Deviation from Equal PP Populations (1:1 line)
#-----------------------------------------------------------

# get data ready to plot
avg_by_year_spread <- avg_by_year %>% 
  select(year, species, plot_type, avg_ind_per_prd) %>% 
  spread(plot_type, avg_ind_per_prd)

avg_by_year_spread <- avg_by_year_spread[which(complete.cases(avg_by_year_spread)),]

# get only the PPs
PP_only <- avg_by_year_spread[avg_by_year_spread$species == 'PP',]

# linear model along a 1:1 line
equal_PP_line = lm(PP_only$Krat_Exclosure - PP_only$Control ~ 0)

# make new data frame with predicted and residuals
year <- as.data.frame(unique(avg_by_year_spread$year))
PP_predicted <- as.data.frame(predict(equal_PP_line))
PP_residuals <- as.data.frame(residuals(equal_PP_line))

PP_linear_model <- bind_cols(year, PP_predicted, PP_residuals)
colnames(PP_linear_model) <- c("year", "PP_predicted", "PP_residuals")

#----------------------------------------------------------
# Run regression on PB abundance and PP residual abundance
#----------------------------------------------------------

# remove years where no PBs are present
PB_only <- PB_only %>% 
  select(year, avg_ind_per_prd) %>% 
  summarise(PB_avg_indiv = sum(avg_ind_per_prd))
PP_PB_join <- inner_join(PB_only[, c(-2)], PP_linear_model, by = "year")

# define x and y for the linear model
x = PP_PB_join$PB_avg_indiv
y = PP_PB_join$PP_residuals

# check structure
acf(x)
acf(y)

# build and compare models
PP_PB_model_linear <- gls(y ~ x)
PP_PB_model_linear_AR1 <- gls(y ~ x, correlation = corAR1(form = ~1))
anova(PP_PB_model_linear, PP_PB_model_linear_AR1)

#-----------------------------------------------------------
# Create Figure 1
#-----------------------------------------------------------

# get all years in which PPs are present
PP_and_PB_fulljoin <- full_join(PP_linear_model, PB_only[,c(1,3)], by = "year")
PP_and_PB_fulljoin[is.na(PP_and_PB_fulljoin)] <- 0 # to line up PB abundance for plotting

### make the plots

# average PB individuals through time
(plot1a <- plot_PB_timeseries(PP_and_PB_fulljoin))

# PP residuals through time
(plot1b <- plot_PP_residuals_timeseries(PP_and_PB_fulljoin))

# plot the regression (using original model)
(plot1c <- plot_PP_regression(PP_PB_join, PP_PB_model_linear_AR1))

# use `patchwork` to put them together into one figure
(plot1 <- (plot1a/plot1b) | plot1c)

# ggsave("figures/1989-2010/Figure1.png", plot1, width = 7, height = 3.5, dpi = 600)
# ggsave("figures/1989-2010/Figure1.tiff", plot1,
#        width = 7, height = 3.5, dpi = 600, compression = 'lzw')


############################################################
# C. penicillatus Population-level Metrics and RMARK
############################################################

# select only PPs from the data and use Sarah's code to clean
all_clean <- clean_data_for_capture_histories(all)
PP_only <- filter(all_clean, species == 'PP')

#-----------------------------------------------------------
# Run MARK analyses on all PPs
#-----------------------------------------------------------

# Create a set of capture histories by treatment and by plot if needed
tags_all = unique(PP_only$tag)
periods_all = seq(min(PP_only$period), max(PP_only$period))

#################################################################################
### Warning: this will take a long time to run and maybe crash your computer! ###
#################################################################################
#                                                                               #
#       The `create_trmt_hist` function to create mark_trmt_all will            #
#       take a long time to run. If you don't want to run it, you can           #
#       read in the results from the GitHub repo (line 233-4) and then          #
#       run the RMark code -OR- skip to line 286 for all RMark results          #
#                                                                               #
#################################################################################

# mark_trmt_all = create_trmt_hist(PP_only, tags_all, periods_all)

# load in capture histories if already created
all_hist <- getURL("https://raw.githubusercontent.com/bleds22e/PP_shifts/master/data/PP_capture_history_all_20180711.csv")
mark_trmt_all <- read.csv(text = all_hist, header = TRUE, stringsAsFactors = FALSE)

# prep data for RMark
all_ms <- select(mark_trmt_all, captures) %>% dplyr::rename("ch" = "captures")
first_PP <- min(PP_only$period)

# # Process data
# ms.pr = process.data(all_ms, begin.time = first_PP, model = "Multistrata")
# 
# # Create default design data
# ms.ddl = make.design.data(ms.pr)
# 
# # add design covariates for PB era
# PB_time_after = as.factor(seq(PB_max, 433))
# 
# ms.ddl$S$PB_time = 0
# ms.ddl$S$PB_time[ms.ddl$S$time %in% PB_time_after] = 1
# 
# ms.ddl$p$PB_time = 0
# ms.ddl$p$PB_time[ms.ddl$p$time %in% PB_time_after] = 1
# 
# ms.ddl$Psi$PB_time = 0
# ms.ddl$Psi$PB_time[ms.ddl$Psi$time %in% PB_time_after] = 1

# Run the models and examine the output

#################################################################################
### Warning: this will take a long time to run and maybe crash your computer! ###
#################################################################################
#                                                                               #
#       Running the `run.ms` function to get ms.results will take a             #
#       long time to run. If you don't want to run this yourself, you           #
#       can read in the RMark results from GitHub instead (line 286)            #
#                                                                               #
#################################################################################

# MarkViewer="open -a TextEdit" # edit to make results pop up on a Mac

# ms.results = run.ms(S_dot = NULL,
#                     S_stratum = list(formula = ~ -1 + stratum * PB_time),
#                     p_dot = list(formula = ~ 1),
#                     p_stratum = NULL,
#                     Psi_s = list(formula =  ~ -1 + stratum:tostratum * PB_time, link = "logit"))
# ms.results
# names(ms.results)
# 
# ms.summary = ms.results$S.stratum.p.dot.Psi.s
# ms.summary
# rmark_results <- ms.summary$results$real
# write.csv(ms.summary$results$real, "data/MARKdatatop_model_summary_[DATE].csv")

# read in Mark results if skipping that section
rmark_results <- read.csv("data/top_model_summary_20190416.csv", stringsAsFactors = FALSE)

# plot RMark results

plot_rmark <- prep_RMark_data_for_plotting(rmark_results)

(plot2a <- plot_estimated_survival(plot_rmark))

(plot2b <- plot_transition_probability(plot_rmark))

#------------------------------------------------------------
# Number of New PP Individuals Showing Up on Plots
#------------------------------------------------------------

# make empty dataframe
first_period <- setNames(data.frame(matrix(ncol = 21, nrow = 0)), names(PP_only))

# create dataframe of only first period each tag is present
for (i in 1:length(tags_all)){
  tmp <- PP_only[PP_only$tag == tags_all[i],] # rows for a given tag
  tmp2 <- tmp[tmp$period == min(tmp$period),] # row with earliest period
  first_period <- rbind(first_period, tmp2)   # add to new dataframe
}

# total number new PPs (avg plot sum by year)
new_PP_per_plot <- first_period %>%
  filter(plot_type != "Removal") %>% 
  group_by(plot, month, year, plot_type) %>%
  summarise(count = n(species)) %>%
  ungroup()
new_PP_per_plot <- new_PP_per_plot %>%
  group_by(year, plot_type, plot) %>% 
  summarise(count = sum(count))

# put zeros where they belong
new_PP_per_plot <- full_join(all_plots_all_years, new_PP_per_plot) %>%
  filter(plot_type != "Removal")
new_PP_per_plot$count[is.na(new_PP_per_plot$count)] <- 0

new_PP_per_plot$time_point <- NA

for (i in 1:nrow(new_PP_per_plot)) {
  if (new_PP_per_plot$year[i] <= 1997) {
    new_PP_per_plot$time_point[i] = "Before"
  } else {
    new_PP_per_plot$time_point[i] = "After"
  }
}

results.lme <- lme(count ~ plot_type*time_point, random = list(~ 1 | year, ~ 1 | plot), data = new_PP_per_plot)
anova(results.lme)

# calculate mean and standard error for plotting

new_PP_per_plot_summary <- new_PP_per_plot %>% 
  group_by(year, plot_type) %>% 
  summarise(avg_plot_sum_by_year = mean(count), se = plotrix::std.error(count))

new_PP_per_plot_summary <- new_PP_per_plot_summary %>%
  mutate(ymin = avg_plot_sum_by_year - se,
         ymax = avg_plot_sum_by_year + se) 

# plot new PP individuals
(plot2c <- plot_new_PP_individuals(new_PP_per_plot_summary))

# Make Figure 2
(plot2 <- plot2a + plot2b - plot2c + plot_layout(ncol = 1))

# ggsave("figures/1989-2010/Figure2.png", plot2, width = 6, height = 7, dpi = 600)
# ggsave("figures/1989-2010/Figure2.tiff", plot2,
#        width = 6, height = 7, dpi = 600, compression = "lzw")

#############################################################
# System-level Aspects of Patch Preference
#############################################################

# download biomass data by plot from portalr
energy_data <- portalr::energy(path = "repo", level = "Plot")

# select certain treatments and filter by time
energy_dat <- energy_data %>%
  filter(treatment == "control" | treatment == "exclosure", 
         period >= 118 & period <= 433) 

# get periods and associated years
years_and_periods <- unique(all[,c("year", "period")])
energy_dat <- left_join(energy_dat, years_and_periods)

# sum across rows and rename column
energy_dat_rowSums <- as.data.frame(rowSums(energy_dat[,4:24]))
colnames(energy_dat_rowSums) <- c("rowSums")

# summarise energy to get total by period and plot type
energy_total <- cbind(energy_dat, energy_dat_rowSums) %>%
  group_by(year, treatment) %>%
  summarise(totals = sum(rowSums))

# change the data structure to run the linear model
energy_spread <- tidyr::spread(energy_total, treatment, totals) %>% 
  filter(year <= 2010)

# ratio
energy_ratio <- energy_spread %>% mutate(EX_to_CO_ratio = exclosure/control)

(plot3_energy <- plot_energy_ratio(energy_ratio))
# ggsave("figures/1989-2010/Figure3_energy.png", plot3_energy, width = 3.5, height = 3, dpi = 600)

#############################################################
# Check for density-dependent habitat seleciton 
#############################################################

PB_Dipos <- all %>% 
  filter(species %in% c('PB', 'DM', 'DO', 'DS'))

# total number competitors (avg plot sum by year)
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

# add PB time point
for (i in 1:nrow(PB_Dipo_per_plot)) {
  if (PB_Dipo_per_plot$year[i] <= 1997) {
    PB_Dipo_per_plot$time_point[i] = "Before"
  } else {
    PB_Dipo_per_plot$time_point[i] = "After"
  }
}

# get means and SE
competitor_summary <- PB_Dipo_per_plot %>% 
  group_by(year, plot_type) %>% 
  summarise(avg_plot_sum_by_year = mean(count), se = plotrix::std.error(count))

competitor_summary <- competitor_summary %>%
  mutate(ymin = avg_plot_sum_by_year - se,
         ymax = avg_plot_sum_by_year + se) 

# plot results
(total_competitors <- plot_avg_competitors(competitor_summary))
# ggsave("figures/1989-2010/FigureS5.png", total_competitors, width = 6, height = 3, dpi = 600)

# run mixed model on just the "after PB" data
after_PB <- PB_Dipo_per_plot %>% filter(year > 1997)

results.lme <- lme(count ~ plot_type, random = list(~ 1 | year, ~ 1 | plot), data = after_PB)
anova(results.lme)
