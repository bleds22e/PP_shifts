# Get Population Level PP Data
# EKB, code modified from S. Supp and RMark examples
# March 7, 2018

# LIBRARIES and SOURCE CODE
library(RCurl)
library(RMark)
library(tidyverse)
library(gridExtra)
source("scripts/movement_fxns.r")
source("scripts/additional_movement_fxns.r")
source("scripts/additional_fxns_EKB.r")
# colorblind palette for plotting
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73","#CC79A7")

# DATA FILES

# rodent file from repo
rodents <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv") 
rdat <- read.csv(text = rodents, header = TRUE, na.strings = c(""))

# species file
species <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_species.csv")
sdat <- read.csv(text = species, header = TRUE, na.strings = c(""))

# trapping file from repo
trapping <- getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_trapping.csv")
tdat <- read.csv(text = trapping, header = TRUE, stringsAsFactors = FALSE)

##########################################################
# DATA PREP
##########################################################

#---------------------------------------------------------
# Clean the Data 
#---------------------------------------------------------

# mostly taken from Sarah's rodent_data.r

# Clean the Rodent Data

# make it match Sarah Supp's data to use her code
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
all = all[-which(all$period %in% bad_periods),]

#---------------------------------------------------------
# Figure out PB "burn in" period
#---------------------------------------------------------

# get PB per plot per period
PB <- all %>% filter(species == 'PB') 
PB_plot_count <- PB %>% 
  select(period, Treatment_Number, plot) %>% 
  group_by(period, Treatment_Number) %>% 
  summarise(count = n())

PB_min <- min(PB$period) # when PB first show up
PB_max <- min(PB_plot_count$period[PB_plot_count$count == 8]) #first time PBs are found in all 8 krat exclosures


############################################################
# PPs IN THE CONTEXT OF PBs
############################################################

no_removals <- all %>% filter(Treatment_Number != 3) 

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

ggplot(avg_by_year_plotting, aes(x = year, y = avg_indiv, color = species)) + 
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 1995, xmax = 1998,
           ymin = -Inf, ymax = Inf) +
  scale_color_manual(values = cbbPalette) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ plot_type, nrow = 2) + 
  xlab("Year") +
  ylab("Avg. Inidividuals per Plot") +
  labs(color = "Species") +
  theme_bw()

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

# check that the residuals look correct
plot(PP_only$residuals)
abline(h = 0)

#----------------------------------------------------------
# Plot PP Residuals Against PB Abundance
#----------------------------------------------------------

# get PB abundance
PB_avg_year <- select(avg_by_year, year, species, avg_indiv) %>%
  group_by(year) %>% 
  filter(species == 'PB') %>% 
  select(-species) %>% 
  summarise(PB_avg_indiv = sum(avg_indiv))
PP_and_PB_innerjoin <- inner_join(PB_avg_year, PP_linear_model, by = "year")

x = PP_and_PB_innerjoin$PB_avg_indiv
y = PP_and_PB_innerjoin$PP_residuals
PP_PB_model <- lm(formula = y ~ x + I(x^2))
summary(PP_PB_model)

ggplot(data = PP_and_PB_innerjoin, aes(x = PB_avg_indiv, y = PP_residuals)) +
  geom_hline(aes(yintercept = 0), color = 'black')+
  stat_smooth(method = 'lm', formula = y ~ x + I(x^2), size = 2) +
  geom_point(size = 3) + 
  xlab("Average PB Individuals per Plot per Year") +
  ylab("Residuals Against the 1:1 Line for PP") +
  labs(title = "y = 0.0008x^2 - 0.3026x + 10.3314, Adj. R2 = 0.696, n = 21, p = 0.8.686e-06") +
  theme_bw()+
  theme(plot.title = element_text(face = "italic", colour = "dark grey", size = 14, hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14))
#ggsave("figures/PP_residuals_PB_abund.png", width = 7.5, height = 7)

#-----------------------------------------------------------
# Plot PP Residuals and PP Abundance Through Time
#-----------------------------------------------------------



############################################################
# PP POPULATION-LEVEL RATES and RMARK
############################################################

# Clean Data

# remove bad or unclear data
all_clean <- clean_data_for_capture_histories(all)
# select on PPs from the data and use Sarah's code to clean
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

periods_pre = seq(min(PP_only$period),(PB_max-1)) # include all periods, even those with no PPs
periods_post = seq(PB_max, max(PP_only$period))

mark_trmt_pre = create_trmt_hist(pre_PB_max, tags_pre, periods_pre) # create capture history before PB_max
mark_trmt_post = create_trmt_hist(post_PB_max, tags_post, periods_post) # create capture history after PB_max

# for future use
# write.csv(mark_trmt_pre, "data/PP_capture_history_prePBmax.csv")
# write.csv(mark_trmt_post, "data/PP_capture_history_postPBmax.csv")

#---------------------------------------------------------------
# Run MARK analyses on both data sets
#---------------------------------------------------------------

# load in capture histories if already obtained
#mark_trmt_pre <- read.csv("data/MARKdata/PP_capture_history_prePBmax.csv", header = TRUE, stringsAsFactors = FALSE)
#mark_trmt_post <- read.csv("data/MARKdata/PP_capture_history_postPBmax.csv", header = TRUE, stringsAsFactors = FALSE)

# prep data for RMark
pre_ms <- select(mark_trmt_pre, captures) %>% rename(ch = captures)
post_ms <- select(mark_trmt_post, captures) %>% rename(ch = captures)
first_PP <- min(pre_PB_max$period)

### Before PBs Infiltrated

# Process data
ms.pr = process.data(pre_ms, begin.time = first_PP, model = "Multistrata")
  
# Create default design data
ms.ddl = make.design.data(ms.pr)

# Run the models and examine the output

ms.results = run.ms()
ms.summary = ms.results$S.stratum.p.dot.Psi.s
write.csv(ms.summary$results$real, "data/MARKdata/MARKoutput_PP_prePBmax_real.csv")

### After PBs Infiltrated

# Process data
ms.pr = process.data(post_ms, begin.time = PB_max, model = "Multistrata")

# Create default design data
ms.ddl = make.design.data(ms.pr)

# Run the models and examine the output

ms.results = run.ms()
ms.summary = ms.results$S.stratum.p.dot.Psi.s
write.csv(ms.summary$results$real, "data/MARKdata/MARKoutput_PP_postPBmax_real.csv")

#------------------------------------------------------------
# Number of New PP Individuals Showing Up on Plots
#------------------------------------------------------------



#############################################################
# PP BIOMASS CALCULATIONS
#############################################################