# PP Shifts Paper
# Ellen K. Bledsoe, with code from S. Supp
# March 7, 2018

# LIBRARIES and SOURCE CODE

# install portalr if not already done
#devtools::install_github("weecology/portalr")

library(portalr)
library(RCurl)
library(RMark)
library(tidyverse)
library(forecast)
library(nlme)
library(patchwork)
library(rapportools)
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

source("scripts/functions_SRS.r")
source("scripts/functions_EKB.r")

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

# summarize trapping
trap_count <- tdat %>%
  group_by(period) %>%
  summarise(count = sum(sampled))
bad_periods <- filter(trap_count, count < 20) # periods that weren't fully trapped
bad_periods <- as.list(bad_periods$period)

# don't use periods with only one day of trapping
all_no_incomplete = all[-which(all$period %in% bad_periods),] # need to change this?

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


############################################################
# PPs IN THE CONTEXT OF PBs
############################################################

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

PB_only <- filter(avg_by_year_plotting, species == "PB")
y_axis_title <- expression(paste(bold("Avg. "), bolditalic("C. baileyi"), bold(" Individuals")))

(plot_supp1 <- ggplot(PB_only, aes(x = year, y = avg_ind_per_prd, color = plot_type, group = plot_type)) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .5) +
  scale_color_manual(values = cbbPalette) +
  xlab("Year") +
  ylab(y_axis_title) +
  labs(color = "Plot Type") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        legend.position = "top", 
        legend.title = element_blank(),
        plot.margin = margin(r = 15, l = 10)))
#ggsave("figures/ms_figures/PB_by_plottype_toplegend.png")

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
# Plot PP Residuals Against PB Abundance
#----------------------------------------------------------

# remove years where no PBs are present
PB_only <- PB_only %>% 
  select(year, avg_ind_per_prd) %>% 
  summarise(PB_avg_indiv = sum(avg_ind_per_prd))
PP_PB_join <- inner_join(PB_only[, c(-2)], PP_linear_model, by = "year")

# define x and y for the linear model
x = PP_PB_join$PB_avg_indiv
y = PP_PB_join$PP_residuals

# test for autoregressive structure
auto.arima(PP_linear_model$PP_residuals)

# build and compare models
PP_PB_model_linear <- gls(y ~ x)
PP_PB_model_linear_AR1 <- gls(y ~ x, correlation = corAR1(form = ~1))
anova(PP_PB_model_linear, PP_PB_model_linear_AR1)

# plot the regression (using original model)

x_axis_title <- expression(paste(bold("Avg. "), bolditalic("C. baileyi"), bold(" per Plot")))
y_axis_title <- expression(paste(bolditalic("C. penicillatus"), bold(" Difference from Equal")))

(plot1c <-
  ggplot(data = PP_PB_join, aes(x = PB_avg_indiv, y = PP_residuals)) +
  geom_hline(aes(yintercept = 0), color = 'black', linetype = 2)+
  geom_smooth(aes(y = fitted(PP_PB_model_linear_AR1)),  size = 1, color = "black") +
  #stat_smooth(method = 'lm', size = 2, color = "black") + #### WHY IS THIS NOT WORKING? ####
  geom_point(size = 3) +
  xlab(x_axis_title) +
  ylab(y_axis_title) +
  ggtitle("C") +
  theme_classic()+
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
        plot.title = element_text(face = "bold", size = 18, hjust = -.1),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 5)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.margin = margin(l = 25)))
#ggsave("figures/ms_figures/PP_residuals_PB_abund_geomsmooth.png", plot1c, width = 5, height = 4.5)

#-----------------------------------------------------------
# Plot PP Residuals and PP Abundance Through Time
#-----------------------------------------------------------

# get all years in which PPs are present
PP_and_PB_fulljoin <- full_join(PP_linear_model, PB_only[,c(1,3)], by = "year")
PP_and_PB_fulljoin[is.na(PP_and_PB_fulljoin)] <- 0 # to line up PB abundance for plotting

# PP residuals through time

y_axis_title <- expression(paste(bolditalic("C. pen."), bold(" Diff. from Equal")))

(plot1b <- ggplot(PP_and_PB_fulljoin, aes(x = year, y = PP_residuals)) +
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
  ggtitle("B") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
        plot.title = element_text(face = "bold", size = 18, hjust = -.115),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 5)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12)))

# Average PB individuals through time

y_axis_title <- expression(paste(bold("Avg. "), bolditalic("C. baileyi"), bold(" per Plot")))

(plot1a <- ggplot(PP_and_PB_fulljoin, aes(x = year, y = PB_avg_indiv)) +
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
  ylab(y_axis_title) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
        plot.title = element_text(face = "bold", size = 18, hjust = -.115),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 5)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12)))


plot1 <- (plot1a/plot1b) | plot1c
#ggsave("figures/ms_figures/PB_patchwork_blackline.png", plot1, width = 12.5, height = 6.5)


############################################################
# PP POPULATION-LEVEL RATES and RMARK
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
mark_trmt_all = create_trmt_hist(PP_only, tags_all, periods_all)

# for future use:
# write.csv(mark_trmt_pre, "data/PP_capture_history_prePBmax.csv")
# write.csv(mark_trmt_post, "data/PP_capture_history_postPBmax.csv")
# write.csv(mark_trmt_all, "data/PP_capture_history_all_20180711.csv")

# load in capture histories if already created
all <- getURL("https://raw.githubusercontent.com/bleds22e/PP_shifts/master/data/PP_capture_history_all_20180711.csv")
mark_trmt_all <- read.csv(text = all, header = TRUE, stringsAsFactors = FALSE)

# prep data for RMark
all_ms <- select(mark_trmt_all, captures) %>% dplyr::rename("ch" = "captures")
first_PP <- 123 # define in reference to other dataframe #123

# Process data
ms.pr = process.data(all_ms, begin.time = first_PP, model = "Multistrata")

# Create default design data
ms.ddl = make.design.data(ms.pr)

# add design covariates for PB era
PB_time_after = as.factor(seq(PB_max, 433))

ms.ddl$S$PB_time = 0
ms.ddl$S$PB_time[ms.ddl$S$time %in% PB_time_after] = 1

ms.ddl$p$PB_time = 0
ms.ddl$p$PB_time[ms.ddl$p$time %in% PB_time_after] = 1

ms.ddl$Psi$PB_time = 0
ms.ddl$Psi$PB_time[ms.ddl$Psi$time %in% PB_time_after] = 1

# Run the models and examine the output

MarkViewer="open -a TextEdit" # edit to make results pop up on a Mac

  ### Warning: this will take a long time to run and maybe crash your computer! ###
ms.results = run.ms(S_dot = NULL,
                    S_stratum = list(formula = ~ -1 + stratum * PB_time),
                    p_dot = list(formula = ~ 1),
                    p_stratum = NULL,
                    Psi_s = list(formula =  ~ -1 + stratum:tostratum * PB_time, link = "logit"))
ms.results
names(ms.results)

ms.summary = ms.results$S.stratum.p.dot.Psi.s
ms.summary
#write.csv(ms.summary$results$real, "data/MARKdata/MARK_SerenityRun/top_model_summary_[DATE].csv")

# read in Mark results if skipping that section
rmark_results <- read.csv("data/MARKdata/MARK_SerenityRun/top_model_summary_20180712.csv", stringsAsFactors = FALSE)

# prep RMark results for plotting
rmark_results$time = c("Absent", "Present", "Absent", "Present", "Absent", "Present", NA, 
                       "Absent", "Present", "Absent", "Present", "Absent", "Present",
                       "Absent", "Present", "Absent", "Present", "Absent", "Present")

# add descriptive columns
rmark_results$metric = rep("S", nrow(rmark_results))
rmark_results$metric[7] = "p"
rmark_results$metric[8:19] = "Psi"

rmark_results$stratum = c("A", "A", "B", "B", "C", "C", NA, 
                          "AB", "AB",  "AC", "AC", "BA", "BA",
                          "BC", "BC", "CA", "CA", "CB", "CB")
rmark_results$Treatment = c("Control", "Control", "KR Exclosure", "KR Exclosure", "Removal", "Removal", NA,
                            "Control to KR Exclosure", "Control to KR Exclosure", "Control to Removal", "Control to Removal",
                            "KR Exclosure to Control", "KR Exclosure to Control", "KR Exclosure to Removal", "KR Exclosure to Removal",
                            "Removal to Control", "Removal to Control", "Removal to KR Exclosure", "Removal to KR Exclosure")

plot_rmark <- rmark_results %>% 
  filter(metric != "p", stratum == "A" | stratum == "B" | stratum == "AB" | stratum == "BA")
plot_rmark$time <- factor(plot_rmark$time, levels = c("Absent", "Present"))

# plot RMark results

x_axis_title <- expression(paste(bolditalic("C. baileyi"), bold(" Presence")))

(plot2a <- ggplot(plot_rmark[(plot_rmark$metric == "S"),], color = Treatment) +
    geom_pointrange(aes(x = time, y = estimate, 
                        ymin = (estimate - se), ymax = (estimate + se), 
                        color = Treatment), 
                        position = position_dodge(.1), size = .75) +
    scale_colour_manual(values = cbbPalette) + 
    xlab(x_axis_title) +
    ylab("Estimated Survival") +
    ggtitle("A") +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.title = element_text(face = "bold", size = 18, hjust = -.355),
          axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
          axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
          axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12),
          legend.position = "top",
          legend.title = element_blank(),
          plot.margin = margin(t = 20)))
ggsave("figures/ms_figures/Survival.png", width = 4, height = 4)

(plot2b <- ggplot(plot_rmark[(plot_rmark$metric == "Psi"),]) +
    geom_pointrange(aes(x = time, y = estimate,
                        ymin = (estimate - se), ymax = (estimate + se), 
                        color = Treatment), 
                        position = position_dodge(.1), size = .75) +
    scale_colour_manual(values = cbbPalette) + 
    xlab(x_axis_title) +
    ylab("Transition Probability") +
    ggtitle("B") +
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
          plot.margin = margin(l = 25)))
ggsave("figures/ms_figures/TransitionProbability.png", width = 4, height = 4)

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
  group_by(plot, year, plot_type) %>%
  summarise(count = n(species)) %>%
  ungroup()
new_PP_per_plot <- new_PP_per_plot %>%
  group_by(year, plot_type) %>%
  summarise(avg_plot_sum_by_year = mean(count), se = plotrix::std.error(count))

# plot new PPs
new_PP_per_plot <- new_PP_per_plot %>%
  mutate(ymin = avg_plot_sum_by_year - se,
         ymax = avg_plot_sum_by_year + se) %>%
  replace_na(list(avg_plot_sum_by_year = 0, se = 0, ymin = 0, ymax = 0))

# rename plot_treatments for plotting
new_PP_per_plot$plot_type <- plyr::revalue(new_PP_per_plot$plot_type, c("Krat_Exclosure" = "KR Exclosure"))
y_axis_title <- expression(paste(bold("Avg. "), bolditalic("C. penicillatus"), bold(" Individuals")))

(plot2c <- ggplot(new_PP_per_plot, aes(x = year,
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
  ylab("Avg. Individuals") +
  xlab("Year") +
  ggtitle("C") +
  #guides(color = guide_legend(override.aes = list(size = 3))) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.75),
        plot.title = element_text(face = "bold", size = 18, hjust = -.137, vjust = -5),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        legend.position = "top", 
        legend.title = element_blank(),
        plot.margin = margin(r = 10, t = 15)))
ggsave("figures/ms_figures/new_PP_per_year.png", plot2c, width = 6, height = 4.5)

# make figure for ms
plot2 <- plot2a + plot2b - plot2c + plot_layout(ncol = 1)
ggsave("figures/ms_figures/PP_metrics.png", plot2, width = 6, height = 7)

#############################################################
# PP BIOMASS CALCULATIONS
#############################################################

# download biomass data by plot from portalr
biomass_data <- portalr::get_rodent_data(path = "repo", level = "Plot", output = "biomass")

# select certain treatments and filter by time
biomass_dat <- biomass_data %>%
  filter(treatment == "control" | treatment == "exclosure", 
         period >= 118 & period <= 433) # get the right time periods

# add a year column for later summarization
year_prd_pairs <- unique(tdat[,c("year", "period")]) # get associated years and periods
biomass_dat$year = NA

for (i in 1:nrow(biomass_dat)){
  prd <- biomass_dat$period[i]
  biomass_dat$year[i] = year_prd_pairs$year[year_prd_pairs$period == prd]
}

#------------------------------------------------------------
# Biomass Ratios
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

# ratio
biomass_ratio <- biomass_spread %>% mutate(EX_to_CO_ratio = exclosure/control)

(plot6 <- ggplot(biomass_ratio, aes(year, EX_to_CO_ratio, group = 1))+
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 1995, xmax = 1998,
           ymin = -Inf, ymax = Inf) +
  annotate(geom = "rect", fill = "grey", alpha = 0.4,
           xmin = 2008, xmax = 2010, # 2008 is the last time PBs were on 8 krat exclosure plots (366); 2010 first time not caught in a census
           ymin = -Inf, ymax = Inf) +
  geom_point(size = 3) +
  geom_line()+
  ylab("Kangaroo Rat Exlcosure:Control Biomass") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.margin = margin(10, 15, 10, 10)))
#ggsave("figures/ms_figures/biomass_ratio.png", plot6, width = 5.5, height = 4.5)
