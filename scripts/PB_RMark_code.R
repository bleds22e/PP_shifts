# Get Population Level PP Data
# EKB, code modified from S. Supp and RMark examples
# March 7, 2018

# LIBRARIES and SOURCE CODE

# install portalr if not already done
#devtools::install_github("weecology/portalr")

# libraries
library(portalr)
library(RCurl)
library(RMark)
library(tidyverse)
library(forecast)
library(nlme)
library(patchwork)

source("scripts/functions_SRS.r")
source("scripts/functions_EKB.r")

# colorblind palette for plotting
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73","#CC79A7")

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
all <- repo_data_to_Supp_data(rdat, sdat)
 
# Find and Remove Periods with One Day of Trapping
#   should I actually do this or just leave them in?

# summarize trapping
trap_count <- tdat %>%
  group_by(period) %>%
  summarise(count = sum(sampled))
bad_periods <- filter(trap_count, count < 20) # periods that weren't fully trapped
bad_periods <- as.list(bad_periods$period)

# don't use periods with only one day of trapping
all_no_incomplete = all[-which(all$period %in% bad_periods),] # need to change this;

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
PB_max <- min(PB_plot_count$period[PB_plot_count$count == 8]) #first time PBs are found in all 8 krat exclosures

# PB decline?
# 2008 (prd 366) = last time caught in all 8 krat exclosures
# 2010 (prd 388) = first time not caught during a survey since PB_max

############################################################
# PPs IN THE CONTEXT OF PBs
############################################################

no_removals <- all_no_incomplete %>% filter(Treatment_Number != 3) 

#----------------------------------------------------------
# Average Number of PP Individual per Plot per Year
#----------------------------------------------------------

avg_by_year <- no_removals %>% # count of individuals in each plot
  filter(species == 'PP' | species == 'PB') %>% 
  group_by(plot, year, species, plot_type) %>% 
  summarise(count = n()) %>% 
  ungroup()
avg_by_year <- avg_by_year %>% # average by treatment type per year
  group_by(year, species, plot_type) %>%  
  summarize(avg_indiv = mean(count))      

# fix plot types for plotting
avg_by_year_plotting <- avg_by_year
avg_by_year_plotting$plot_type <- plyr::revalue(avg_by_year$plot_type, c("Krat_Exclosure" = "Kangaroo Rat Exclosure"))

ggplot(avg_by_year_plotting, aes(x = year, y = avg_indiv, color = species, group = species)) + 
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 1995, xmax = 1998,
           ymin = -Inf, ymax = Inf) +
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 2008, xmax = 2010,
           ymin = -Inf, ymax = Inf) +
  scale_color_manual(values = cbbPalette) +
  geom_line() +
  geom_point() +
  facet_wrap( ~ plot_type, nrow = 2) + 
  xlab("Year") +
  ylab("Avg. Inidividuals per Plot") +
  labs(color = "Species") +
  theme_classic()

#-----------------------------------------------------------
# Residuals Against the 1:1 line
#-----------------------------------------------------------

# get data ready to plot
avg_by_year_spread <- spread(avg_by_year, plot_type, avg_indiv)
avg_by_year_spread <- avg_by_year_spread[which(complete.cases(avg_by_year_spread)),] 

# get only the PPs
PP_only <- avg_by_year_spread[avg_by_year_spread$species == 'PP',]

# linear model along a 1:1 line
x = PP_only$Control
y = PP_only$Krat_Exclosure
against_1_to_1 = lm(y-x ~ 0)

# make new data frame with predicted and residuals

year <- as.data.frame(unique(avg_by_year_spread$year))
PP_predicted <- as.data.frame(predict(against_1_to_1))
PP_residuals <- as.data.frame(residuals(against_1_to_1))

PP_linear_model <- bind_cols(year, PP_predicted, PP_residuals)
colnames(PP_linear_model) <- c("year", "PP_predicted", "PP_residuals")

#----------------------------------------------------------
# Plot PP Residuals Against PB Abundance
#----------------------------------------------------------

# get PB abundance
PB_avg_year <- select(avg_by_year, year, species, avg_indiv) %>%
  group_by(year) %>% 
  filter(species == 'PB') %>% 
  select(-species) %>% 
  summarise(PB_avg_indiv = sum(avg_indiv))
# remove years where no PBs are present
PP_and_PB_innerjoin <- inner_join(PB_avg_year, PP_linear_model, by = "year")

# define x and y for the polynomial model
x2 = PP_and_PB_innerjoin$PB_avg_indiv
y2 = PP_and_PB_innerjoin$PP_residuals

# quadratic model 
PP_PB_model_original <- gls(y2 ~ x2 + I(x2^2))
summary(PP_PB_model_original)

# test for autoregressive structure
auto.arima(PP_linear_model$PP_residuals)

# run with autoregression = 1
PP_PB_model_AR1 <- gls(y2 ~ x2 + I(x2^2),
                 correlation = corAR1(form = ~1))
summary(PP_PB_model_AR1)

# compare models
anova(PP_PB_model_original, PP_PB_model_AR1)

# plot the regression (using original model)
plot1 <- ggplot(data = PP_and_PB_innerjoin, aes(x = PB_avg_indiv, y = PP_residuals)) +
  geom_hline(aes(yintercept = 0), color = 'black')+
  stat_smooth(method = 'lm', formula = y ~ x + I(x^2), size = 2) + # quadratic smoothing
  geom_point(size = 2) + 
  xlab("Average PB per Plot by Year") +
  ylab("PP Residuals for 1:1 Line") +
  ggtitle("C") +
  theme_classic()+
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        plot.title = element_text(face = "bold", size = 18, hjust = -.1),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.margin = margin(l = 25))
#ggsave("figures/ms_figures/PP_residuals_PB_abund_notitle.png", plot1, width = 5, height = 4.5)

#-----------------------------------------------------------
# Plot PP Residuals and PP Abundance Through Time
#-----------------------------------------------------------

# get all years in which PPs are present
PP_and_PB_fulljoin <- full_join(PP_linear_model, PB_avg_year, by = "year")
PP_and_PB_fulljoin[is.na(PP_and_PB_fulljoin)] <- 0 # to line up PB abundance for plotting

# PP residuals through time
plot2 <- ggplot(PP_and_PB_fulljoin, aes(x = year, y = PP_residuals)) +
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 1995, xmax = 1998,
           ymin = -Inf, ymax = Inf) +
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 2008, xmax = 2010,
           ymin = -Inf, ymax = Inf) +
  geom_point(size = 3) +
  geom_hline(aes(yintercept = 0), color = 'red')+
  xlab("Year") +
  ylab("PP Resid. for 1:1 Line") +
  ggtitle("B") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        plot.title = element_text(face = "bold", size = 18, hjust = -.125),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12))

# Average PB individuals through time
plot3 <- ggplot(PP_and_PB_fulljoin, aes(x = year, y = PB_avg_indiv)) +
  annotate(geom = "rect", fill = "grey", alpha = 0.4, 
           xmin = 1995, xmax = 1998, 
           ymin = -Inf, ymax = Inf) +
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 2008, xmax = 2010,
           ymin = -Inf, ymax = Inf) +
  geom_point(size = 3) +
  geom_line() +
  ggtitle("A") +
  xlab("Year") +
  ylab("Average PB per Plot") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        plot.title = element_text(face = "bold", size = 18, hjust = -.125),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12))


plot4 <- (plot3/plot2) | plot1
#ggsave("figures/ms_figures/PB_patchwork.png", plot4, width = 12.5, height = 6)


############################################################
# PP POPULATION-LEVEL RATES and RMARK
############################################################

# select on PPs from the data and use Sarah's code to clean
all_clean <- clean_data_for_capture_histories(all)
PP_only <- filter(all_clean, species == 'PP')

#-----------------------------------------------------------
# Create Capture Histories
#-----------------------------------------------------------

# make two different capture histories based on PB_max
pre_PB_max <- PP_only[(PP_only$period < PB_max),]
post_PB_max <- PP_only[(PP_only$period >= PB_max),]

### Create a set of capture histories by treatment and by plot
tags_pre = unique(pre_PB_max$tag) 
tags_post = unique(post_PB_max$tag)
tags_all = unique(PP_only$tag)

periods_pre = seq(min(PP_only$period),(PB_max-1)) # include all periods, even those with no PPs
periods_post = seq(PB_max, max(PP_only$period))
periods_all = seq(min(PP_only$period), max(PP_only$period))

mark_trmt_pre = create_trmt_hist(pre_PB_max, tags_pre, periods_pre) # create capture history before PB_max
mark_trmt_post = create_trmt_hist(post_PB_max, tags_post, periods_post) # create capture history after PB_max
mark_trmt_all = create_trmt_hist(PP_only, tags_all, periods_all) # create one giant capture history

# for future use:
# write.csv(mark_trmt_pre, "data/PP_capture_history_prePBmax.csv")
# write.csv(mark_trmt_post, "data/PP_capture_history_postPBmax.csv")
# write.csv(mark_trmt_all, "data/PP_capture_history_all.csv")

#---------------------------------------------------------------
# Run MARK analyses on all PPs
#---------------------------------------------------------------

# load in capture histories 
all <- getURL("https://raw.githubusercontent.com/bleds22e/PP_shifts/master/data/MARKdata/PP_capture_history_all.csv")
mark_trmt_all <- read.csv(text = all, header = TRUE, stringsAsFactors = FALSE)

# prep data for RMark
all_ms <- select(mark_trmt_all, captures) %>% rename(ch = captures)
first_PP <- min(pre_PB_max$period)

# Process data
ms.pr = process.data(all_ms, begin.time = first_PP, model = "Multistrata")

# Create default design data
ms.ddl = make.design.data(ms.pr)

# add design covariates for PB era
PB_time_after = as.factor(seq(PB_max, 435))

ms.ddl$S$PB_time = 0
ms.ddl$S$PB_time[ms.ddl$S$time %in% PB_time_after] = 1

ms.ddl$p$PB_time = 0
ms.ddl$p$PB_time[ms.ddl$p$time %in% PB_time_after] = 1

ms.ddl$Psi$PB_time = 0
ms.ddl$Psi$PB_time[ms.ddl$Psi$time %in% PB_time_after] = 1

# Run the models and examine the output

MarkViewer="open -a TextEdit" # edit to make results pop up on a Mac
ms.results = run.ms(S_dot = NULL, 
                    S_stratum = list(formula = ~ -1 + stratum + PB_time),
                    p_dot = list(formula = ~ 1),
                    p_stratum = NULL,
                    Psi_s = list(formula =  ~ -1 + stratum:tostratum + PB_time, link = "logit"))
ms.results
names(ms.results)

ms.summary = ms.results$S.stratum.p.dot.Psi.s
ms.summary
#write.csv(ms.summary$results$real, "data/MARKdata/MARKoutput_PP_all_real.csv")

#------------------------------------------------------------
# Number of New PP Individuals Showing Up on Plots
#------------------------------------------------------------

# Sarah's code _should_ go through everything, so each should be a unique tag
#   - should ultimately check that Sarah's code does what we hope it does
#   - `testthat` ? lol
# I think every new/different animal gets a different tag
# so just need to find the first period each tag is caught

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
  group_by(plot, year, plot_type) %>% 
  summarise(count = n()) %>% 
  ungroup() 
new_PP_per_plot <- new_PP_per_plot %>% 
  group_by(year, plot_type) %>% 
  summarise(avg_plot_sum_by_year = mean(count), se = plotrix::std.error(count))

# plot new PPs
new_PP_per_plot <- new_PP_per_plot %>% 
  mutate(ymin = avg_plot_sum_by_year - se, 
         ymax = avg_plot_sum_by_year + se) %>% 
  replace_na(list(avg_plot_sum_by_year = 0, se = 0, ymin = 0, ymax = 0))

new_PP_per_plot$plot_type <- plyr::revalue(new_PP_per_plot$plot_type, c("Krat_Exclosure" = "Kangaroo Rat Exclosure"))

plot6 <- ggplot(new_PP_per_plot, aes(x = year, 
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
  #scale_y_log10() +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        legend.position = "bottom")
#ggsave("figures/ms_figures/new_PP_per_year.png", plot6, width = 6, height = 4.5)  


#############################################################
# PP BIOMASS CALCULATIONS
#############################################################

# BIOMASS

# download biomass data by plot from portalr
biomass_data <- portalr:::get_rodent_data(path = "repo", level = "Plot", output = "biomass")

# select certain treatments and filter by time
biomass_dat <- biomass_data %>% 
  filter(treatment == "control" | treatment == "exclosure", period >= 118 & period <= 433) # get the right time periods

# add a year column for later summarization
year_prd_pairs <- unique(tdat[,c("year", "period")]) # get associated years and periods
biomass_dat$year = NA

for (i in 1:nrow(biomass_dat)){
  prd <- biomass_dat$period[i]
  biomass_dat$year[i] = year_prd_pairs$year[year_prd_pairs$period == prd]
}

#------------------------------------------------------------
# Biomass Against the 1:1 line
#------------------------------------------------------------

# sum across rows and rename column
biomass_dat_rowSums <- as.data.frame(rowSums(biomass_dat[,4:24])) 
colnames(biomass_dat_rowSums) <- c("rowSums") 

# summarise biomass to get total by period and plot type
biomass_total <- cbind(biomass_dat, biomass_dat_rowSums) %>% 
  group_by(year, treatment) %>% 
  summarise(totals = sum(rowSums))

# change the data structure to run the linear model
biomass_spread <- tidyr::spread(biomass_total, treatment, totals)

# run linear model against the 1:1 line
model_1to1 <- lm((biomass_spread$exclosure - biomass_spread$control) ~ 0)
biomass_spread$residuals <- residuals(model_1to1)
plot(x = biomass_spread$year, y = biomass_spread$residuals)

ggplot(biomass_spread, aes(x = year, y = residuals)) +
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 1995, xmax = 1998,
           ymin = -Inf, ymax = Inf) +
  geom_point() +
  stat_smooth(method = 'lm', formula = y ~ x + I(x^2)) +
  theme_bw()

#------------------------------------------------------------
# Biomass Ratios
#------------------------------------------------------------

# ratio
biomass_ratio <- biomass_spread %>% mutate(EX_to_CO_ratio = exclosure/control)

plot5 <- ggplot(biomass_ratio, aes(year, EX_to_CO_ratio, group = 1))+
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 1995, xmax = 1998,
           ymin = -Inf, ymax = Inf) +
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 2008, xmax = 2010, # 2008 is the last time PBs were on 8 krat exclosure plots (366); 2010 first time not caught in a census
           ymin = -Inf, ymax = Inf) +
  geom_point(size = 3) +
  geom_line()+
  xlab("Year") +
  ylab("Kangaroo Rat Exlcosure:Control Biomass") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.margin = margin(10, 15, 10, 10))
#ggsave("figures/ms_figures/biomass_ratio.png", plot5, width = 5.5, height = 4.5)  
