# Looks at rodent movement
# EKB modified
# Jan 2018

library(dplyr)
library(ggplot2)
library(stringr)

source("movement_fxns.r")
source("additional_movement_fxns.r")

clean_rodents <- read.csv("rawdata/cleaned_1989-2009.csv", stringsAsFactors = FALSE)
small_rodents = filter(clean_rodents, species == "PB" | species == "PP") # check 0021PP

# treatment types have already been included:
#   treatment type 1 = controls: 1, 2, 4, 8, 9, 11, 12, 14, 17, 22
#   treatment type 2 = krat exclosures: 3, 6, 13, 15, 18, 19, 20, 21
#   treatment type 3 = full exclosures: 5, 7, 10, 16, 23, 24

# make sure text is text and not atomic
small_rodents$tag = as.character(small_rodents$tag) 
small_rodents$sex = as.character(small_rodents$sex)
small_rodents$species = as.character(small_rodents$species)

# give untagged indivs unique tag numbers (7 digits)
# small_rodents = id_unknowns(small_rodents)        

# get list of unique tags
tags = unique(small_rodents$tag)   

# output list of flagged data
flags = find_bad_spp_data(small_rodents, tags, 9)

# get list of unique "bad tags"
badtags=unique(flags$tag)

# delete bad tags from dataset for analysis
for (i in 1:length(badtags)) {
  small_rodents = subset(small_rodents, tag != badtags[i])
  }

# don't use periods with negative period numbers and periods with only one day of trapping
small_rodents = subset(small_rodents, period != 267 & period != 277 & period != 278 &
                                      period != 283 & period != 284 & period != 300 &
                                      period != 311 & period != 313 & period != 314 &
                                      period != 318 & period != 321 & period != 323 &
                                      period != 337 & period != 339 & period != 344 &
                                      period != 351 & period > 0)
  

### Create a set of capture histories by treatment and by plot
tags = unique(sort(small_rodents$tag))
periods = unique(sort(small_rodents$period))

mark_trmt = create_trmt_hist(small_rodents, tags, periods) 
mark_plot = create_plot_hist(small_rodents, tags, periods) 

# get list of indivs that moved plots or treatment, species is included
moving_rats = find_rats_that_move(small_rodents, tags)

# get rodents that move plots
moving_tags_plot = unique(moving_rats$tag)
outcount = 0
MARK_movers_plot = data.frame("ch"=1, "censored"=1, "tag"=1, "spp"=1, "sex"=1, "mass"=1)
for (i in 1:length(moving_tags_plot)) {
  mover_plot = subset(mark_plot, tags == moving_tags_plot[i])
  if (nrow(mover_plot) > 0) {
  outcount = outcount + 1
  MARK_movers_plot[outcount,] <- mover_plot
  }}   

# get rodents that move treatments
moving_tags_trmt = unique(moving_rats$tag)
outcount = 0
MARK_movers_trmt = data.frame("ch"=1, "censored"=1, "tag"=1, "spp"=1, "sex"=1, "mass"=1)
for (i in 1:length(moving_tags_trmt)) {
  mover_trmt = subset(mark_trmt, tags == moving_tags_trmt[i])
  if (nrow(mover_trmt) > 0) {
    outcount = outcount + 1
    MARK_movers_trmt[outcount,] <- mover_trmt
  }}  

#count non-movers
moving_tags_plot = unique(mark_plot$tag)
nonmover_plot = mark_plot
for (i in 1:length(moving_tags_plot)) {
  nonmover_plot = subset(nonmover_plot, tags != moving_tags_plot[i])
}

moving_tags_trmt = unique(mark_trmt$tag)
nonmover_trmt = mark_trmt
for (i in 1:length(moving_tags_trmt)) {
  nonmover_trmt = subset(nonmover_trmt, tags != moving_tags_trmt[i])
}

# find num captures/rat
rat_catches=num_captures(small_rodents, tags)

# how often are movers moving trmt?
trmt_moves = examine_trmt_moves(small_rodents, moving_tags_trmt) # fixed
  trmt_moves = trmt_moves[which(trmt_moves$num_moves > 0),]
  trmt_moves$num_moves = as.numeric(trmt_moves$num_moves)
  trmt_moves$c2r = as.numeric(trmt_moves$c2r)
  trmt_moves$r2c = as.numeric(trmt_moves$r2c)
  trmt_moves$c2e = as.numeric(trmt_moves$c2e)
  trmt_moves$r2e = as.numeric(trmt_moves$r2e)
  
plot_moves = examine_plot_moves(small_rodents, moving_tags_plot)

### PLOTTING options

library(ggplot2)
library(gridExtra)

# plot total times moved for each move type and PP/PB
movement_freq_data <- trmt_moves %>% select(sp, c2r, r2c) %>% 
  filter(sp == "PB" | sp == "PP")
movement_freq_data <- tidyr::gather(movement_freq_data, "type", "count", 2:3)

ggplot(movement_freq_data, aes(x = count)) +
  facet_grid(sp ~ type) +
  geom_bar() + 
  xlab("Movement Frequency") + 
  ylab("Total Count") +
  theme_bw() 
#ggsave("output/movement_frequency.png")

# add first year captured

trmt_moves$first_year <- trmt_moves$tags

for (t in 1:length(trmt_moves$tags)){
  tmp <- which(small_rodents$tag==trmt_moves$tag[t])
  trmt_moves$first_year[t] = min(small_rodents$yr[tmp])
}

# get percent of time spent in each plot type

trmt_moves$time_in_control <- trmt_moves$num_moves
trmt_moves$time_in_krat_exclosure <- trmt_moves$num_moves
trmt_moves$time_in_exile <- trmt_moves$num_moves

for (t in 1:length(trmt_moves$num_moves)){
  string = trmt_moves$move_list[t]
  trmt_moves$time_in_control[t] = str_count(string, "A")/str_length(string)
  trmt_moves$time_in_krat_exclosure[t] = str_count(string, "B")/str_length(string)
  trmt_moves$time_in_exile[t] = str_count(string, "C")/str_length(string)
}

percentsAB <- trmt_moves %>% 
  select(sp, first_year, time_in_control, time_in_krat_exclosure, time_in_exile) %>% 
  group_by(sp, first_year) %>% 
  summarise_all(mean)
percentsAB <- tidyr::gather(percentsAB, time_type, mean, 3:5)

ggplot(percentsAB, aes(x = first_year, y = mean, fill = time_type)) +
  geom_bar(position = position_stack(), stat = "identity") +
  facet_wrap(~ sp, nrow = 2) +
  xlab("First Year Caught") +
  ylab("Mean Time Spent per Treatment")
#ggsave("output/movers_mean_treatment_time.png")  


# if moved once, which way?

# starting treatment?
