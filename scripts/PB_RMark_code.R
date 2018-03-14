# Get Population Level PP Data
# EKB, code modified from S. Supp and RMark examples
# March 7, 2018

# LIBRARIES and SOURCE CODE
library(dplyr)
library(RCurl)
library(RMark)
source("scripts/movement_fxns.r")
source("scripts/additional_movement_fxns.r")
source("scripts/additional_fxns_EKB.r")

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

#---------------------------------------------------------
# Clean the Data 
#---------------------------------------------------------

# mostly taken from Sarah's rodent_data.r

# Clean the Rodent Data

# make it match Sarah Supp's data to use her code
all <- repo_data_to_Supp_data(rdat, sdat)

# remove bad or unclear data
all_clean <- clean_data_for_capture_histories(all)

# Find and Remove Periods with One Day of Trapping

# summarize trapping
trap_count <- tdat %>% 
  group_by(period) %>% 
  summarise(count = sum(sampled))
bad_periods <- filter(trap_count, count < 20) # periods that weren't fully trapped
bad_periods <- as.list(bad_periods$period)

# don't use periods with only one day of trapping
all_clean = all_clean[-which(all_clean$period %in% bad_periods),]

# select on PPs from the data and use Sarah's code to clean
PP_only <- filter(all_clean, species == 'PP')

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


############################################################
# PPs IN THE CONTEXT OF PBs
############################################################



############################################################
# PP POPULATION-LEVEL RATES and RMARK
############################################################

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