# Code for working with individual-level rodent data
# movement with stakes

library(reshape2)
library(ggplot2)
library(calibrate)
library(fields)
library(stringr)
library(plyr)
library(gridExtra)
library(ggbiplot)
library(ape)
library(geiger)
library(ggplot2)
library(picante)
library(PhyloOrchard)

#---------------------------------------------------------------------------------
#          setup - select wd, import data, source code,  file to collect results
#---------------------------------------------------------------------------------
#set working directory

source("scripts/movement_fxns.R")

#import all data
all = read.table('data/rawdata/all_1980-2009.txt', sep = ',', header = TRUE)

#import species information
species_table = read.csv('rawdata/species_table.csv', sep = ',', header = TRUE)

#import cleaned data, if next step (clean up the data) has previously been run
allclean = read.csv('data/rawdata/cleaned_1989-2009.csv', sep = ',', header = TRUE)
  # NOTE: all7 == allclean, if have allclean, can skip the datacleaning steps
  all7=allclean

#---------------------------------------------------------------------------------
#          clean up the data -- skip this section if allclean exists
#---------------------------------------------------------------------------------

# change some cols from factor to character class
all$tag = as.character(all$tag)
all$species = as.character(all$species)
all$sex = as.character(all$sex)

allmonths = count_months(all, c(1980:2009))

#subset data where species are known (e.g., no "unidentified rodents" or genus-only)
all2 = subset(all, species!="DX" & species!="UR" & species!="RX" & species!="SX" & species!="PX" & species != "OX")

# give untagged individuals a unique 7-number code
all2 = id_unknowns(all2, 16)

# make sure when note2 == "*" it is counted as a new tag
# necessary if using ALL data (ear and toe tags)
# returns the dataset with new IDs for checking for duplicate tags that occur over a suspiciously long time period
tags = unique(all2$tag)
all3 = starred_tags(all2, tags, 9, 16)

#check for dead individuals, give all with same tag after marked dead a new unique ID
tags = unique(all3$tag)
all4 = is_dead(all3, tags, 9, 16)

# check for individuals with same tag, but captured over long timespan (may be able to separate them) 
# necessary if using ALL data (ear and toe tags)
# returns the dataset with new IDs for those that can easily be sorted out based on sex and year
tags = unique(all4$tag)
dups = is_duplicate_tag(all4, tags, 10, 9, 16) #check to see if can separate tags based

#eliminate bad data based on tags identified in dups$bad
duptags = unique(dups$bad$tag)
all5 = dups$data[-which(dups$data$tag %in% duptags),] #delete rows flagged as duplicates without clear resolution

tags = unique(all5$tag)
same = same_period(all5, tags)

#eliminate tags that appear more than once in the same period - questionable data
sametags = unique(same$tag)
all6 = all5[-which(all5$tag %in% sametags),]

# get rid of 'bad data'; deletes data where species is inconsistent. 
all7 = subsetDat(all6)

#subset data for years of analysis
all7 = subset(all7, yr > 1988)


#---------------------------------------------------------------------------------
#          calculate life-history details - temporal persistence, reproduction
#---------------------------------------------------------------------------------
spplist = unique(all7$species)
totalyears = c(1989:2009) #will need to be adjusted based on time frame want to use
controlplots = c(1,2,4,8,9,11,12,14,17,22)
months = count_months(all7, totalyears)

persistence = data.frame(species=NA, propyrs=NA, propmos=NA, meanabun=NA, maxabun=NA, reprod=NA, bodysize=NA)
  outcount = 1
yearly_control_abundance = data.frame(year = totalyears)
reprod_mo_yr = data.frame(year=NA, month=NA, proprepro=NA, numfemales=NA, species=NA)
avg_mo_reprod = data.frame(species=NA, month=NA, proprepro=NA)

for (i in 1:length(spplist)){
  #subset species data
  spdata = subset(all7, species == spplist[i])
  
    # proportion of years they were seen in CONTROL plots
    conspdata = subset(spdata, plot %in% controlplots)
    conpropyrs = length(unique(conspdata$yr))/length(totalyears)
    # average proportion of months they were seen in during years in which they were present on CONTROLS
    conavgmos = mean_win_yr_occ(conspdata, totalyears, months)
    # number of unique individuals in each year on control plots only
    conabun = allyrs_abun(conspdata, totalyears)
    # mean number of unique individuals on control plots only, includes ZEROES
    meanconabun = mean(conabun)
    # max number of unique individuals on control plots only
    maxconabun = max(conabun)
  
  #record persistence and abundance data from control plots
  persistence[outcount,1] = as.character(spplist[i])
  persistence[outcount,2:5] = c(round(conpropyrs,4), round(conavgmos,4), round(meanconabun,4), maxconabun)
  yearly_control_abundance = cbind(yearly_control_abundance, conabun)
  
  #subset females for each species
  spdataF = subset(spdata, sex == "F")
  
    #average proportion of reproductive females by month across all years
    reprd = mean_mo_repro(spdataF, totalyears) #vector with 12 items (one for each month)
      avg_mo_reprod = rbind(avg_mo_reprod, reprd)    
  
  # proportion of reproductive females by month and year
    reprdyr = mo_repro(spdataF) #matrix with 120 x 5
      reprod_mo_yr = rbind(reprod_mo_yr, reprdyr)
  
    # track the number of times individual females uniquely reproduce within each year
    irep = indiv_repro(spdataF) #matix with cols "tag", "year" and "num_reprod"
      assign(paste0(spplist[i], 'irep'), irep)
      persistence[outcount,6] = round(mean(irep$num_reprod),2)
  
  # record mean body size for individuals captured for the species (excluding juveniles and pregnant individuals)
  massdata = spdata[which(spdata$pregnant != "P" & spdata$reprod != "J"),]
  persistence[outcount,7] = round(mean(massdata$wgt, na.rm=TRUE),2)
  
  outcount = outcount + 1
}

#add species names to the dataframe of abundance vectors
names(yearly_control_abundance) = c("year", as.character(spplist))
#make sure cols are numeric
persistence$propyrs = as.numeric(persistence$propyrs)
persistence$propmos = as.numeric(persistence$propmos)
persistence$meanabun = as.numeric(persistence$meanabun)
persistence$maxabun = as.numeric(persistence$maxabun)
avg_mo_reprod$proprepro = as.numeric(avg_mo_reprod$proprepro)
avg_mo_reprod$month = as.numeric(avg_mo_reprod$month)
#delete Null rows
avg_mo_reprod = avg_mo_reprod[-1,]
reprod_mo_yr = reprod_mo_yr[-1,]


#melt control abundance data frame for later plotting
yrcontrolabuns = melt(yearly_control_abundance, id.vars=c("year"))
names(yrcontrolabuns) = c("year", "species", "abun")

# TEMPORAL PERSISTENCE CAN BE BASED ON TWO METHODS:
# (1) Identify Core species based on annual persistence, following Coyle et al. 2013 (American Naturalist)
corespecies = persistence[which(persistence$propyrs >= 0.666),1]
intermediatespecies = persistence[which(persistence$propyrs > 0.333 & persistence$propyrs < 0.666),1]
transientspecies = persistence[which(persistence$propyrs <= 0.333),1]

# (2) Identify Core species based on annual AND seasonal persistence
corespecies2 = persistence[which(persistence$propyrs >= 0.666 & persistence$propmos >= 0.666),1]
transientspecies2 = persistence[which(persistence$propyrs <= 0.333 & persistence$propmos <= 0.333),1]
intermediatespecies2 = persistence[-which(persistence$species %in% c(corespecies2, transientspecies2)),1]

#Categorize species by feeding guild
granivores = c("DO", "DM", "DS", "PB", "PP", "PF", "PH", "PI",
               "PE", "PM", "PL", "RM", "RF", "RO", "BA")
folivores = c("SH", "SF", "SO", "NAO")
carnivores = c("OT", "OL")

#add columns to persistence (for later plotting) for status and guild
persistence$guild = rep(NA, nrow(persistence))
persistence$status = rep(NA, nrow(persistence))
persistence$status2 = rep(NA, nrow(persistence))

for (row in 1:nrow(persistence)){
  if (persistence[row,]$species %in% granivores){ persistence[row,]$guild = "granivore" }
  else if (persistence[row,]$species %in% folivores) { persistence[row,]$guild = "folivore" }
  else { persistence[row,]$guild = "carnivore" }

  if (persistence[row,]$species %in% corespecies){ persistence[row,]$status = "core" }
  else if (persistence[row,]$species %in% intermediatespecies) { persistence[row,]$status = "intermediate" }
  else { persistence[row,]$status = "transient" }
  
  if (persistence[row,]$species %in% corespecies2){ persistence[row,]$status2 = "core" }
  else if (persistence[row,]$species %in% intermediatespecies2) { persistence[row,]$status2 = "intermediate" }
  else { persistence[row,]$status2 = "transient" }
}

#add a single value for proportion of years * average proportion of months
persistence$oneval = persistence$propyrs * persistence$propmos

#add status and guild to yrcontrolabuns
yrcontrolabuns$guild = rep(NA, nrow(yrcontrolabuns))
yrcontrolabuns$status = rep(NA, nrow(yrcontrolabuns))
yrcontrolabuns$status2 = rep(NA, nrow(yrcontrolabuns))

for (row in 1:nrow(yrcontrolabuns)){
  if (yrcontrolabuns[row,]$species %in% granivores){ yrcontrolabuns[row,]$guild = "granivore" }
  else if (yrcontrolabuns[row,]$species %in% folivores) { yrcontrolabuns[row,]$guild = "folivore" }
  else { yrcontrolabuns[row,]$guild = "carnivore" }
  
  if (yrcontrolabuns[row,]$species %in% corespecies){ yrcontrolabuns[row,]$status = "core" }
  else if (yrcontrolabuns[row,]$species %in% intermediatespecies) { yrcontrolabuns[row,]$status = "intermediate" }
  else { yrcontrolabuns[row,]$status = "transient" }
  
  if (yrcontrolabuns[row,]$species %in% corespecies2){ yrcontrolabuns[row,]$status2 = "core" }
  else if (yrcontrolabuns[row,]$species %in% intermediatespecies2) { yrcontrolabuns[row,]$status2 = "intermediate" }
  else { yrcontrolabuns[row,]$status2 = "transient" }
}


#---------------------------------------------------------------------------------
#          calculate movement distances, multi-state capture histories
#---------------------------------------------------------------------------------

#set names for the distance list
spnames = as.character(spplist)

#create lists with sublist names reflecting each species
meterlist = sapply(spnames,function(x) NULL)
taglist = sapply(spnames,function(x) NULL)


for (i in 1:length(spplist)){
  #subset species data
  spdata = subset(all7, species == spplist[i])
  
    # get a vector unique tags, then get a vector of distances moved for all recaptured individuals, by SPECIES
    tags = unique(spdata$tag)
    print (paste(spplist[i], "has", length(tags), "individuals, and avg mass is:", 
                 round(mean(spdata$wgt, na.rm=T),2), sep = " "))
    print (paste(spplist[i], "avg mass is:", round(mean(spdata$wgt, na.rm=T),2), sep = " "))
    mtrs = distance_moved(spdata, tags)
    meterlist[i] = list(mtrs)
    taglist[i] = list(tags)
}

#Identify breakpoint, modal, mean and max distance for all species in meterlist
breakpoint = sapply(meterlist,function(x){
  expm1(mean(log1p(x)) + sd(log1p(x)))
})

modal_distance = sapply(meterlist,function(x){
  as.numeric(names(sort(-table(x)))[1])
})

mean_distance <-sapply(meterlist,function(x){
  as.numeric(mean(x))
})

max_distance <-sapply(meterlist,function(x){
  as.numeric(max(x))
})

persistence = cbind(persistence, breakpoint, modal_distance, mean_distance, max_distance)


#---------------------------------- From this point on, the analysis will be separated by foraging guild
#------------------------ Granivores, Folivores, and Carnivores (on METHOD 1)

#check for differences when all the guilds are lumped
coreall = unlist(meterlist[which(names(meterlist) %in% corespecies)], use.names=F)
intermedall = unlist(meterlist[which(names(meterlist) %in% intermediatespecies)], use.names=F)
transall = unlist(meterlist[which(names(meterlist) %in% transientspecies)], use.names=F)

# concatenate core guild data - used to ask if these species behave differently from others
coremeters = meterlist[which(names(meterlist) %in% corespecies)]
coregran = unlist(coremeters[which(names(coremeters) %in% granivores)], use.names=F) 
corefoli = unlist(coremeters[which(names(coremeters) %in% folivores)], use.names=F)
corecarn = unlist(coremeters[which(names(coremeters) %in% carnivores)], use.names=F)
coreallsp = unlist(coremeters, use.names=F)

  # find breakpoints to use in MARK data structure for future analyses
  # data reasonably well fits a lognormal distribution (eyeball and J. Powell)
  # breakpoint = mean(logdata) + sd(logdata) of all the distances traveled by recaptured individuals    
  # using log1p, and back transforming using expm1 should solve the problem of lots of zeros 
  coregran_brkpt = expm1(mean(log1p(coregran)) + sd(log1p(coregran)))
  corefoli_brkpt = expm1(mean(log1p(corefoli)) + sd(log1p(corefoli)))
  corecarn_brkpt = expm1(mean(log1p(corecarn)) + sd(log1p(corecarn)))
  coreallsp_brkpt = expm1(mean(log1p(coreallsp)) + sd(log1p(coreallsp)))


#------------------------ Granivores, Folivores, and Carnivores (on METHOD 2)

# concatenate core guild data - used to ask if these species behave differently from others
# Check that the definition of "core" is still the same, need to change this chunk of code by hand, if necessary
# coremeters2 = meterlist[which(names(meterlist) %in% corespecies2)]
# coregran2 = unlist(coremeters2[which(names(coremeters2) %in% granivores)], use.names=F) 
# corefoli2 = unlist(coremeters2[which(names(coremeters2) %in% folivores)], use.names=F)
# corecarn2 = unlist(coremeters2[which(names(coremeters2) %in% carnivores)], use.names=F)
# 
# # find breakpoints to use in MARK data structure for future analyses
# coregran_brkpt2 = expm1(mean(log1p(coregran2)) + sd(log1p(coregran2)))
# corefoli_brkpt2 = expm1(mean(log1p(corefoli2)) + sd(log1p(corefoli2)))
# corecarn_brkpt2 = expm1(mean(log1p(corecarn2)) + sd(log1p(corecarn2)))


#concatenate data for granivores only, and 
#add in the transition, modal, mean, and max distances traveled by each species for later plotting
graniv_persist = persistence[which(persistence$species %in% granivores),] 


#-------------------------------------------------------------------------------------------
#      Get MARK capture histories for all species - Uses METHOD 1 (year-based temporal persistence)
#-------------------------------------------------------------------------------------------
spplist = c(granivores, folivores, carnivores)
periods = c(130:380) # all sampling periods 1989-2009
all_excl = c(5, 7, 10, 16, 23, 24) 
krat_excl = c(5, 7, 10, 16, 23, 24, 3, 6, 13, 15, 18, 19, 20, 21)

for (i in 1:length(spplist)){
  
  #subset species data, for each species in turn
  spdata = subset(all7, species == spplist[i])
  
  if (spplist[i] %in% list("DM", "DS", "DO")) { exclosures = krat_excl} 
  else { exclosures = all_excl} 
  
  #use different benchmarks based on feeding guild (since home range size differs based on food needs)
  if (spplist[i] %in% list(granivores)){ benchmark = coregran_brkpt }
  else if(spplist[i] %in% list(folivores)){ benchmark = corefoli_brkpt }
  else { benchmark = corecarn_brkpt }
  
  #the first species begins the new data matrix for MARK
  if (i == 1) {
  MARK = noplacelikehome(spdata, periods, exclosures, benchmark)
  }
  
  #all subsequent species are appended onto the end of the existing MARK data matrix
  else {
  nextMARK = noplacelikehome(spdata, periods, exclosures, benchmark)
  MARK = rbind(MARK, nextMARK)
  }
}

MARK = as.data.frame(MARK[,1:3])
names(MARK) = c("ch", "freq", "species")

#separate into files based on species
for (s in spplist){
  data = MARK[which(MARK[,3] == spplist[s]),]
  if(nrow(data) > 10){
    write.table(data, file = paste("mark_datafiles//", spplist[s], "_mark.txt", sep=""), sep=" ", row.names=F)
  }
}


#-----------------------------------------------------------------------------------
#         Run R MARK analysis
#           This should run through all the Mark tables and generate output as 
#           saved .csv files
#-----------------------------------------------------------------------------------
source("MARK_analyses.r")
runRMARK("C://Users//sarah//Documents//GitHub//portal-rodent-dispersal//mark_datafiles//") #if run on server: "~/portal-rodent-dispersal/"


#-----------------------------------------------------------------------------------
#        If MARK_analyses.r has already been run, and results saved as .csv files
#          analyze the data from the Program Mark analysis
#-----------------------------------------------------------------------------------
#---------- concatenate results
#grab all the .csv files to loop over and summarize results
rfiles = list.files(paste(getwd(), "/mark_output", sep=""), pattern = "real", full.name=T)

# loop thru the files to make a new dataframe with the estimated parameters
estimates = data.frame(species=NA, S=1, S_se=1, p=1, p_se=1, Psi=1, Psi_se = 1)
outcount = 1

for (f in 1:length(rfiles)){
  dat = read.csv(rfiles[f], header=T, sep=",")
  spname = str_sub(rfiles[f],-6,-5)
  estimates[outcount,1] = spname
  estimates[outcount,2:7] = c(dat[1,2], dat[1,3], dat[2,2], dat[2,3], dat[3,2], dat[3,3])
  outcount = outcount + 1
}

#change "AO" to "NAO" to match naming schema - was shortened in MARK_analyses.r
estimates[which(estimates$species == "AO"),1] = "NAO" 

#---------------------------------------------------------------------------------
#       compare medians among groups and PCA plot results
#---------------------------------------------------------------------------------

# merge ALL data for analysis and pca plots
mall = merge(persistence, estimates, by = c("species", "species"))
mall = merge(mall, species_table, by="species")
names(mall)[22] = "sciname"

keepcols = c("species", "propyrs", "meanabun", "reprod", 
             "breakpoint", "S", "p", "Psi")
keepdesc = c("species", "family", "guild", "status")

pcadat = mall[,names(mall) %in% keepcols]
catdat = mall[,names(mall) %in% keepdesc]
rownames(pcadat) = pcadat$species
pcadat = pcadat[,-1]

#save mall for later analyses
row.names(mall) = mall$sciname
write.csv(mall, "traits.csv")

#---------------------------------------------------------------------------------
#                                 summarize results
#---------------------------------------------------------------------------------
#------------------------ compare means/medians for all the species

c = mall[which(mall$status == "core"),]
i = mall[which(mall$status == "intermediate"),]
t = mall[which(mall$status == "transient"),]
nc = mall[which(mall$status %in% c("intermediate", "transient")),]

print (paste("median breakpoint for core:", round(median(c$breakpoint),2), "range", range(c$breakpoint)[1],"to",range(c$breakpoint)[2]))
print (paste("median breakpoint for non-core:", round(median(nc$breakpoint),2), "range",  range(nc$breakpoint)[1], "to", range(nc$breakpoint)[2]))
print (paste("median breakpoint for intermediate:", round(median(i$breakpoint),2)))
print (paste("median breakpoint for transient:", round(median(t$breakpoint),2)))

print (paste("mean Psi for core:", round(mean(c$Psi),2), "range", range(c$Psi)[1],"to",range(c$Psi)[2]))
print (paste("mean Psi for non-core:", round(mean(nc$Psi),2), "range",  range(nc$Psi)[1], "to", range(nc$Psi)[2]))
print (paste("mean Psi for intermediate:", round(mean(i$Psi),2)))
print (paste("mean Psi for transient:", round(mean(t$Psi),2)))

print (paste("mean p for core:", round(mean(c$p),2),  "range", range(c$p)[1], "to",range(c$p)[2]))
print (paste("mean p for non-core:", round(mean(nc$p),2),  "range", range(nc$p)[1], "to", range(nc$p)[2]))
print (paste("mean p for intermediate:", round(mean(i$p),2)))
print (paste("mean p for transient:", round(mean(t$p),2)))

print (paste("mean S for core:", round(mean(c$S),2),  "range", range(c$S)[1], "to",range(c$S)[2]))
print (paste("mean S for non-core:", round(mean(nc$S),2),  "range", range(nc$S)[1],"to",range(nc$S)[2]))
print (paste("mean S for intermediate:", round(mean(i$S),2)))
print (paste("mean S for transient:", round(mean(t$S),2)))


#------------------------ PCA biplot for all the species
# Standardize the matrix to correct for different units by subtracting the
# means and dividing by sd
zscore <- apply(pcadat, 2, function(x) {
  y <- (x - mean(x))/sd(x)
  return(y)
  })
rownames(zscore) <- rownames(pcadat)
#make prettier labels, change colnames of zscore
colnames(zscore) = c("persistence", "mean N", "fecundity", "benchmark (m)", "Phi", "p", "Psi")

#run pca analysis
trait_pc<-prcomp(zscore)

#Use dev libary to ggplot PCA, color by clades
#Try the ggplot biplot to color by clades (or later, behavioral roles)
toCol = catdat[catdat$species %in% rownames(trait_pc$x),"status"]
toColGuild = catdat[catdat$species %in% rownames(trait_pc$x),"guild"]
toColFam = catdat[catdat$species %in% rownames(trait_pc$x),"family"]


#---------------------------- FIGURE 4
  ggbiplot(trait_pc, groups=toCol, labels=rownames(trait_pc$x), 
            ellipse=TRUE, label.size = 3, varname.size = 4) + 
  theme_classic() + guides(color=guide_legend(title="")) +
  theme(text = element_text(size=14), legend.direction = "horizontal", 
        legend.position = "bottom", legend.box = "vertical") + 
  scale_y_continuous(limits=c(-2.5,3)) + scale_x_continuous(limits=c(-3,3))

ggsave("Fig4-PCA_status.png", dpi=600, height=5.4, width=7, units="in")


#---------------------------- FIGURE A3
#Label species names and clades, circles cover normal distribuiton of families
  ggbiplot(trait_pc, groups=toColFam, labels=rownames(trait_pc$x), 
            ellipse=TRUE, label.size = 3, varname.size = 4) + 
  theme_classic() + guides(color=guide_legend(title="")) +
  theme(text = element_text(size=14), legend.direction = "horizontal", 
          legend.position = "bottom", legend.box = "vertical") + 
    scale_y_continuous(limits=c(-3,3)) + scale_x_continuous(limits=c(-3,3))

ggsave("FigA3-PCA_family.png", dpi=600, height=5.4, width=7, units="in")


#------------------------ FIGURE 2 - persistence, ala core v. transient literature
status_plot = ggplot(persistence, aes(propyrs, propmos)) + geom_point(aes(shape=guild), size = 3) + 
  theme_classic() + xlab("proportion years present") +
  ylab("proportion months present") + xlim(0,1) + ylim(0,1) + 
  geom_vline(xintercept=0.66, linetype="dashed", col = "grey50", size = 0.5) + 
  geom_vline(xintercept=0.33, linetype="dashed", col = "grey50", size = 0.5) + # ggtitle("Rodents 1989 - 2009") + 
  geom_text(aes(label = species), hjust=0, vjust=0) +
  theme(text = element_text(size=14), legend.direction = "horizontal", 
        legend.position = "bottom", legend.box = "vertical")
ggsave("Fig1_persistence.png", dpi=600, height=5.5, width=7.3, units="in")


# ----------------------------- FIGURE 3 - plot the movement histograms for status groups
png("Fig3-histograms.png", res=600, width=11.4, height=3.7, units="in")
coregrp = ggplot(data.frame(coreall), aes(coreall)) + 
  geom_histogram(binwidth=6, aes(y = ..count../sum(..count..))) + 
  scale_y_continuous(labels = percent_format(), limits=c(0,0.30)) +
  theme_classic() + xlab("distance between recaptures") + ggtitle("core") + ylab("percent") +
  scale_x_continuous(breaks = seq(0,550, by=100), limits = c(0,550)) + theme(text = element_text(size=14))
intermedgrp = ggplot(data.frame(intermedall), aes(intermedall)) + 
  geom_histogram(binwidth=6, aes(y = ..count../sum(..count..))) + 
  scale_y_continuous(labels = percent_format(), limits=c(0,0.30)) +
  theme_classic() + xlab("distance between recaptures") + ggtitle("intermediate")  + ylab("percent") +
  scale_x_continuous(breaks = seq(0,550, by=100), limits = c(0,550)) + theme(text = element_text(size=14))
transgrp = ggplot(data.frame(transall), aes(transall)) + 
  geom_histogram(binwidth=6, aes(y = ..count../sum(..count..))) + 
  scale_y_continuous(labels = percent_format(), limits=c(0,0.30))+
  theme_classic() + xlab("distance between recaptures") + ggtitle("transient") + ylab("percent") + 
  scale_x_continuous(breaks = seq(0,550, by=100), limits = c(0,550)) + theme(text = element_text(size=14))

  grid.arrange(coregrp, intermedgrp, transgrp, nrow = 1)
dev.off()


#------------------------ FIGURE A1 - plot histograms of each of the species movements
splist = names(meterlist)
breaks = c(0, seq(3,600, by=6))
i = 1
plot = list() 

for (i in 1:length(splist)){
  df = data.frame(meterlist[i])
  names(df) = "spp"
  plot[[i]] = ggplot(df, aes(spp)) + geom_histogram(breaks=breaks, position="dodge") + theme_classic() + ggtitle(splist[i]) +
    theme(text = element_text(size=20)) + xlab("recapture distance")
  i = i + 1
}

#core 
grid.arrange(plot[[3]], plot[[10]], plot[[21]], plot[[12]], plot[[13]], 
             plot[[8]], plot[[4]], plot[[7]], plot[[1]], plot[[11]], ncol = 4)
#intermediate 
grid.arrange(plot[[9]], plot[[2]], plot[[5]], plot[[16]], ncol = 4)
#transient 
grid.arrange(plot[[15]], plot[[19]], plot[[20]], plot[[14]], plot[[6]], 
             plot[[17]], plot[[17]], ncol = 4)


#--------------------------- Figure A4. Survival-Movement tradeoffs
colnames(zscore) = c("persistence", "mean_N", "fecundity", "benchmark", "Phi", "p", "Psi")
zscoredf = data.frame(zscore)
zscoredf$status = catdat$status
zscoredf$family = catdat$family

png("FigA4-survival-movment.png", res=600, height=3.7, width=7.7, units="in")
  PhiPsi = ggplot(zscoredf, aes(Psi, Phi)) + geom_point(aes(shape=status, col=status, size=2)) + 
    stat_smooth(method="lm", fill="gray80") + 
    theme_classic() + theme(text = element_text(size=14)) + theme(legend.position = "none")

  PhiBench = ggplot(zscoredf, aes(benchmark, Phi)) + geom_point(aes(shape=status, col=status, size=2)) + 
    stat_smooth(method="lm", fill="gray80") + 
    theme_classic() + theme(text = element_text(size=14)) + theme(legend.position = "none")

  grid.arrange(PhiBench, PhiPsi, nrow = 1)
dev.off()


#------------------------Figure A5. Movement-reproduction trade-offs
png("FigA5-movment-reproduction.png", res=600, height=3.7, width=7.7, units="in")
PsiReprod = ggplot(zscoredf, aes(Psi, fecundity)) + geom_point(aes(shape=status, col=status, size=2)) + 
    stat_smooth(method="lm", fill="gray80") + 
    theme_classic() + theme(text = element_text(size=14)) + theme(legend.position = "none")

  BenchReprod = ggplot(zscoredf, aes(benchmark, fecundity)) + geom_point(aes(shape=status, col=status, size=2)) + 
    stat_smooth(method="lm", fill="gray80") + 
    theme_classic() + theme(text = element_text(size=14)) + theme(legend.position = "none")

  grid.arrange(PsiReprod, BenchReprod, nrow = 1)
dev.off()


#------------------------ Figure A6 - plot the Mark results as potential trade-offs
# Add status, Method 1
toStatus = catdat[catdat$species %in% estimates$species,"status"]
estimates = cbind(estimates, toStatus)

png("FigA6-MARK-results.png", res=600, height=3.5, width=11.1, units="in")
SbyPsi = ggplot(estimates, aes(Psi, S, col=toStatus)) + geom_point(size = 3) + theme_classic() + 
  theme(text = element_text(size=14)) + scale_colour_hue(guide = "none") +
  xlab("Psi") + ylab("Phi") + 
  geom_errorbar(aes(x = Psi, ymin = S - S_se, ymax = S + S_se), width=0.01) +
  geom_errorbarh(aes(xmin = Psi - Psi_se, xmax = Psi + Psi_se))

Sbyp = ggplot(estimates, aes(p, S, col=toStatus)) + geom_point(size = 3) + theme_classic() + 
  theme(text = element_text(size=14)) + scale_colour_hue(guide = "none")+
  xlab("p") + ylab("Phi") + 
  geom_errorbar(aes(x = p, ymin = S - S_se, ymax = S + S_se), width=0.01) +
  geom_errorbarh(aes(xmin = p - p_se, xmax = p + p_se))

Psibyp =  ggplot(estimates, aes(Psi, p, col=toStatus)) + geom_point(size = 3) + theme_classic() + 
  theme(text = element_text(size=14)) + scale_colour_hue(guide = "none") +
  xlab("Psi") + ylab("p") + 
  geom_errorbar(aes(x = Psi, ymin = p - p_se, ymax = p + p_se), width=0.01) +
  geom_errorbarh(aes(xmin = Psi - Psi_se, xmax = Psi + Psi_se))

  grid.arrange(SbyPsi, Sbyp, Psibyp, nrow=1)
dev.off


#--------------------- Results for the manuscript text
lm2 = lm(Psi~family, data = mall)
lm3 = lm(S~family, data = mall)
lm4 = lm(meanabun~family, data=mall)
lm5 = lm(propyrs~family, data=mall)

lm6 = lm(S~Psi, data=mall)

zmodal = (mall$modal_distance - mean(mall$modal_distance))/sd(mall$modal_distance)
cbind(zscore,zmodal)

lm7 = lm(zmodal~persistence, data.frame(zscore))
lm8 = lm(fecundity~Psi, data.frame(zscore))


#-------------------------------------------------------------------------------------------
#               PHYLOGENETIC Analyses
#-------------------------------------------------------------------------------------------
# get phylo data from the published mammal tree
data(BinindaEmondsEtAl2007)
tree = BinindaEmondsEtAl2007[[1]]

# see prune.sample, normally this takes in a siteXspp matrix, have to fool it here
d <- matrix(nrow = 1, ncol = 21)
colnames(d) <- c("Dipodomys_merriami", "Dipodomys_ordii", "Dipodomys_spectabilis",
                 "Chaetodipus_penicillatus", "Chaetodipus_baileyi", "Perognathus_flavus",
                 "Peromyscus_maniculatus", "Peromyscus_eremicus", "Peromyscus_leucopus",
                 "Reithrodontomys_megalotis", "Reithrodontomys_montanus", "Reithrodontomys_fulvescens",
                 "Baiomys_taylori", "Chaetodipus_hispidus", "Chaetodipus_intermedius",
                 "Sigmodon_hispidus", "Sigmodon_fulviventer", "Sigmodon_ochrognathus",
                 "Neotoma_albigula", "Onychomys_torridus", "Onychomys_leucogaster")

# prune the tree
tree.p <- prune.sample(samp=d, phylo=tree)
plot(tree.p)

#prune again, removing PH, since we don't have enough of this species for analysis
d <- matrix(nrow = 1, ncol = 20)
colnames(d) = c("Dipodomys_merriami", "Dipodomys_ordii", "Dipodomys_spectabilis",
                "Chaetodipus_penicillatus", "Chaetodipus_baileyi", "Perognathus_flavus",
                "Peromyscus_maniculatus", "Peromyscus_eremicus", "Peromyscus_leucopus",
                "Reithrodontomys_megalotis", "Reithrodontomys_montanus", "Reithrodontomys_fulvescens",
                "Baiomys_taylori", "Chaetodipus_intermedius",
                "Sigmodon_hispidus", "Sigmodon_fulviventer", "Sigmodon_ochrognathus",
                "Neotoma_albigula", "Onychomys_torridus", "Onychomys_leucogaster")
tree.p <- prune.sample(samp=d, phylo=tree)

# get distance matrix for all species pairs
trx <- cophenetic(tree.p)

# Lets use a subset of the traits we are interested in and order by the tree tips
#TODO: Need to translate the traits into z-scores so they are on the same scale
keepcols = c("propyrs", "meanabun", "maxabun", "reprod", "bodysize", "breakpoint", "modal_distance", "S", "p", "Psi")
traitDF = mall[,names(mall) %in% keepcols] 
traitDF = traitDF[tree.p$tip.label, ] 
traitDF = traitDF[complete.cases(traitDF),]

# Standardize the matrix to correct for different units by subtracting means and dividing by sd
zscoret = apply(traitDF, 2, function(x) {
  y = (x - mean(x))/sd(x)
  return(y)
})
rownames(zscoret) <- rownames(traitDF)
zscoret = as.data.frame(zscoret)

# make a pairs plot to look at correlation structure of standardized data
pairs(zscoret[,names(zscoret) %in% c("propyrs", "meanabun", "reprod", "S", "Psi", "breakpoint")], pch = 19,
      diag.panel=panel.hist, upper.panel=panel.cor)

# the correlation structure expected if traits evolve by Brownian motion 
# and fit a generalized least squares model assuming this correlation structure.
bmRodents <- corBrownian(phy=tree.p) 

bm.gls <- gls(meanabun ~ propyrs, correlation = bmRodents, data = zscoret) 
summary(bm.gls)

bm.gls <- gls(S ~ Psi, correlation = bmRodents, data = zscoret) 
summary(bm.gls)

bm.gls <- gls(modal_distance ~ propyrs, correlation = bmRodents, data = zscoret) 
summary(bm.gls)

bm.gls <- gls(reprod ~ Psi, correlation = bmRodents, data = zscoret) 
summary(bm.gls)


#-----------------------------------------FIGURE A3 - Reproduction
#-------------------------- Reproduction Figures
pdf("reproductive_events_per_year.pdf")
par(mfrow=c(3,3))

#plot the relative abundance of females who represent each number of reproductive events per year
plot(NA, NA, xlim = c(0, 4), xaxp = c(0, 4, 4), xlab = "number reproductive events per year", 
     ylab = "proportion females", type = "b", pch = 19, ylim = c(0, 1), yaxp = c(0, 1, 4), bty = "n", 
     main = "core granivores", cex.axis = 1.5, cex.lab = 1.5)
points(c(0:3), table(DOirep$num_reprod)/sum(table(DOirep$num_reprod)), type = "b", pch = 19, col = "black")
points(c(0:4), table(DMirep$num_reprod)/sum(table(DMirep$num_reprod)), type = "b", pch = 19, col = "black")
points(c(0:4), table(PBirep$num_reprod)/sum(table(PBirep$num_reprod)), type = "b", pch = 19, col = "black")
points(c(0:4), table(PPirep$num_reprod)/sum(table(PPirep$num_reprod)), type = "b", pch = 19, col = "black")
points(c(0:3), table(RMirep$num_reprod)/sum(table(RMirep$num_reprod)), type = "b", pch = 19, col = "black")
points(c(0:2), table(PEirep$num_reprod)/sum(table(PEirep$num_reprod)), type = "b", pch = 19, col = "black")
points(c(0:4), table(PFirep$num_reprod)/sum(table(PFirep$num_reprod)), type = "b", pch = 19, col = "black")

plot(NA, NA, xlim = c(0, 4), xaxp = c(0, 4, 4), xlab = "number reproductive events per year", 
     ylab = "proportion females", type = "b", pch = 19, ylim = c(0, 1), yaxp = c(0, 1, 4), bty = "n", 
     main = "intermediate granivores", cex.axis = 1.5, cex.lab = 1.5)
points(c(0:2), table(PMirep$num_reprod)/sum(table(PMirep$num_reprod)), type = "b", pch = 15, col = "black")
points(c(0,1,3), table(DSirep$num_reprod)/sum(table(DSirep$num_reprod)), type = "b", pch = 15, col = "black")

plot(NA, NA, xlim = c(0, 4), xaxp = c(0, 4, 4), xlab = "number reproductive events per year", 
     ylab = "proportion females", type = "b", pch = 19, ylim = c(0, 1), yaxp = c(0, 1, 4), bty = "n", 
     main = "transient granivores", cex.axis = 1.5, cex.lab = 1.5)
points(c(0:1), table(PLirep$num_reprod)/sum(table(PLirep$num_reprod)), type = "b", pch = 15, col = "black")
points(c(0:1), table(ROirep$num_reprod)/sum(table(ROirep$num_reprod)), type = "b", pch = 15, col = "black")
points(0, table(RFirep$num_reprod)/sum(table(RFirep$num_reprod)), type = "b", pch = 15, col = "black")
points(c(0:1), table(PIirep$num_reprod)/sum(table(PIirep$num_reprod)), type = "b", pch = 15, col = "black")
points(c(0:1), table(PHirep$num_reprod)/sum(table(PHirep$num_reprod)), type = "b", pch = 15, col = "black")
points(c(0:2), table(BAirep$num_reprod)/sum(table(BAirep$num_reprod)), type = "b", pch = 15, col = "black")

# For Folivores:
plot(NA, NA, xlim = c(0, 4), xaxp = c(0, 4, 4), xlab = "number reproductive events per year", 
     ylab = "proportion females", type = "b", pch = 19, ylim = c(0, 1), yaxp = c(0, 1, 4), bty = "n", 
     main = "core folivores", cex.axis = 1.5, cex.lab = 1.5)
points(c(0:3), table(NAOirep$num_reprod)/sum(table(NAOirep$num_reprod)), type = "b", pch = 15, col = "black")

plot(NA, NA, xlim = c(0, 4), xaxp = c(0, 4, 4), xlab = "number reproductive events per year", 
     ylab = "proportion females", type = "b", pch = 19, ylim = c(0, 1), yaxp = c(0, 1, 4), bty = "n", 
     main = "intermediate folivores", cex.axis = 1.5, cex.lab = 1.5)
points(c(0:1), table(SHirep$num_reprod)/sum(table(SHirep$num_reprod)), type = "b", pch = 15, col = "black")
points(c(0:1), table(SFirep$num_reprod)/sum(table(SFirep$num_reprod)), type = "b", pch = 15, col = "black")

plot(NA, NA, xlim = c(0, 4), xaxp = c(0, 4, 4), xlab = "number reproductive events per year", 
     ylab = "proportion females", type = "b", pch = 19, ylim = c(0, 1), yaxp = c(0, 1, 4), bty = "n", 
     main = "transient folivores", cex.axis = 1.5, cex.lab = 1.5)
points(c(0:2), table(SOirep$num_reprod)/sum(table(SOirep$num_reprod)), type = "b", pch = 15, col = "black")

#For Carnivores
plot(NA, NA, xlim = c(0, 4), xaxp = c(0, 4, 4), xlab = "number reproductive events per year", 
     ylab = "proportion females", type = "b", pch = 19, ylim = c(0, 1), yaxp = c(0, 1, 4), bty = "n", 
     main = "core carnivores", cex.axis = 1.5, cex.lab = 1.5)
points(c(0:3), table(OTirep$num_reprod)/sum(table(OTirep$num_reprod)), type = "b", pch = 15, col = "black")
points(c(0:2), table(OLirep$num_reprod)/sum(table(OLirep$num_reprod)), type = "b", pch = 15, col = "black")

dev.off()


#-------------------------------------------------------------------------------------------------------- 
#                               EXTRA FIGURES AND DATA THAT MIGHT NOT GET INTO MS
#--------------------------------------------------------------------------------------------------------

#------------------------- plot abundance for all species across timeseries
ggplot(yrcontrolabuns, aes(x=year, y=abun, group=species)) + 
  geom_line(aes(col=status), size=1.5) + theme_bw() +
  theme(text = element_text(size=20))


#------------------------- plot monthly reproduction
ggplot(avg_mo_reprod, aes(month, proprepro)) + geom_point() + theme_bw() +
  geom_line() + facet_wrap(~species)


#------------------------- plot meters traveled by all species
#plot modal distance by persistence for all granivores, color code points by status
modal_dist = ggplot(graniv_persist, aes(propyrs, modal_distance)) + theme_classic() +
  geom_point(aes(col=as.factor(status), shape = as.factor(status)), size=5)  + 
  xlab("proportion of years present") + ylab("Modal Distance between trap locations") +
  geom_text(aes(label=species), hjust=0, vjust=0) +
  theme(text = element_text(size=20))

#plot modal distance by persistence for all granivores, color code points by status
modal_all = ggplot(persistence, aes(propyrs, modal_distance)) + theme_classic() +
  geom_point(aes(col=as.factor(status), shape = as.factor(status)), size=5)  + 
  xlab("proportion of years present") + ylab("Modal Distance between trap locations") +
#  geom_text(aes(label=species), hjust=0, vjust=0) +
  theme(text = element_text(size=20))

grid.arrange(modal_dist, modal_all, nrow = 1)


#-------------------------- Comparing the CMR analysis estimates
lm1 = lm(S ~ Psi, data = mall)
PsiS = ggplot(mall, aes(Psi, S)) + geom_point(size = 2) + stat_smooth(method = "lm") + theme_classic() + 
  theme(text = element_text(size=20)) + ggtitle(paste("r2 =", round(summary(lm1)$r.squared,2)))

lm1 = lm(p ~ Psi, data = mall)
Psip = ggplot(mall, aes(Psi, p)) + geom_point(size = 2) + stat_smooth(method = "lm") + theme_classic() + 
  theme(text = element_text(size=20)) + ggtitle(paste("r2 =", round(summary(lm1)$r.squared,2)))

lm1 = lm(Psi ~ propyrs, data = mall)
yrsPsi = ggplot(mall, aes(propyrs, Psi)) + geom_point(size = 2) + stat_smooth(method = "lm") + theme_classic() + 
  theme(text = element_text(size=20)) + ggtitle(paste("r2 =", round(summary(lm1)$r.squared,2)))

lm1 = lm(S ~ propyrs, data = mall)
yrsS = ggplot(mall, aes(propyrs, S)) + geom_point(size = 2) + stat_smooth(method = "lm") + theme_classic() + 
  theme(text = element_text(size=20)) + ggtitle(paste("r2 =", round(summary(lm1)$r.squared,2)))

lm1 = lm(modal_distance ~ propyrs, data = mall)
yrsdist = ggplot(mall, aes(propyrs, modal_distance)) + geom_point(size = 2) + stat_smooth(method = "lm") + theme_classic() + 
  theme(text = element_text(size=20)) + ggtitle(paste("r2 =", round(summary(lm1)$r.squared,2)))

lm1 = lm(maxabun ~ propyrs, data = mall)
yrsabun = ggplot(mall, aes(propyrs, maxabun)) + geom_point(size = 2) + stat_smooth(method = "lm") + theme_classic() + 
  theme(text = element_text(size=20)) + ggtitle(paste("r2 =", round(summary(lm1)$r.squared,2)))

lm1 = lm(meanabun ~ propyrs, data = mall)
yrsmean = ggplot(mall, aes(propyrs, meanabun)) + geom_point(size = 2) + stat_smooth(method = "lm") + theme_classic() + 
  theme(text = element_text(size=20)) + ggtitle(paste("r2 =", round(summary(lm1)$r.squared,2)))

yrsreprod = ggplot(mall, aes(propyrs, reprod)) + geom_point(size = 2) + theme_classic() + 
  theme(text = element_text(size=20))

yrssd = ggplot(mall, aes(propyrs, breakpoint)) + geom_point(size = 2) + theme_classic() + 
  theme(text = element_text(size=20))

grid.arrange(PsiS, Psip, yrsPsi, yrsS, yrsdist, yrssd, yrsabun, yrsmean, yrsreprod, nrow = 3)


# other plots
ggplot(mall, aes(Psi, reprod)) + geom_point(size = 2) + stat_smooth(method = "lm") + theme_classic() + 
  theme(text = element_text(size=20))

ggplot(mall, aes(bodysize, Psi)) + geom_point(size = 2) + stat_smooth(method = "lm") + theme_classic() + 
  theme(text = element_text(size=20))

ggplot(mall, aes(bodysize, S)) + geom_point(size = 2) + stat_smooth(method = "lm") + theme_classic() + 
  theme(text = element_text(size=20))


#-------------------------- Boxplots for all species
ggplot(mall, aes(status, S)) + geom_boxplot() + theme_classic()
ggplot(mall, aes(status, p)) + geom_boxplot() + theme_classic()
ggplot(mall, aes(status, Psi)) + geom_boxplot() + theme_classic()
ggplot(mall, aes(status, bodysize)) + geom_boxplot() + theme_classic()
ggplot(mall, aes(status, modal_distance)) + geom_boxplot() + theme_classic()
ggplot(mall, aes(status, breakpoint)) + geom_boxplot() + theme_classic()
ggplot(mall, aes(status, reprod)) + geom_boxplot() + theme_classic()
ggplot(mall, aes(status, meanabun)) + geom_boxplot() + theme_classic()




# #------------------------------------------ FIGURE - for ESA talk 
# #                                 REQUIRES DATA FROM MARK ANALYSES!
# 
# #granivore data from dissertation chapter table 2-3 #FIXME - should use data from estimates (above)
# Psi = c(0.09, 0.13, 0.08, 0.14, 0.48, 0.23, 0.49, 0.41)
# S = c(0.76, 0.78, 0.80, 0.81, 0.67, 0.81, 0.76, 0.62)
# distbench = c(28.36, 32.64, 25.57, 36.22, 93.05, 74.29, 29.12, 93.30)
# littersize = c(2.37, 2, 3.6, 4.72, 2.53, 3.6, 4, 4.29) #from Mammals of Arizona - Hoffmeister
# 
# lm1 = lm(S~Psi)
# lm2 = lm(S~distbench)
# lm3 = lm(littersize~S)
# lm4 = lm(littersize~distbench)
# 
# # S vs. Psi
# plot(Psi, S, pch = 19, xlim = c(0:1), ylim = c(0:1), bty = "n", cex.axis = 1.5, cex.lab = 1.5, cex = 1.5,
#      ylab = "survival probability", xlab = "long distance movement probability")
#     abline(lm1, col = "turquoise")
# # S vs. distance benchmark
# plot(distbench, S, pch = 19, bty = "n", xlab = "transition distance (meters)", ylab = "survival", 
#      xlim = c(0,100), ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, cex = 1.5)
#     abline(lm2, col = "turquoise")
# # littersize vs. S
# plot(S, littersize, pch = 19, bty = "n", xlab = "survival probability", ylab = "mean litter size",
#      xlim = c(0,1), ylim = c(1,6))
# #littersize vs. distance benchmark
# plot(distbench, littersize, pch = 19)


#------------------------------------------------------------------------------------------------ 
#         Summarize results for granivores only
#------------------------------------------------------------------------------------------------
# merge GRANIVORE data for analysis and pca plots 
mgran = merge(graniv_persist, estimates)
mgran2 = merge(mgran, species_table, by="species")

keepcols = c("species", "propyrs", "maxabun", "reprod", "bodysize", 
             "breakpoint", "modal_distance", "oneval", "S", "p", "Psi")
keepdesc = c("species", "family", "guild", "status", "status2")

pcagraniv = mgran[,names(mgran) %in% keepcols]
catgraniv = mgran[,names(mgran) %in% keepdesc]
rownames(pcagraniv) = pcagraniv$species
pcagraniv = pcagraniv[,-1]

c = mgran[which(mgran$status == "core"),]
i = mgran[which(mgran$status == "intermediate"),]
t = mgran[which(mgran$status == "transient"),]

print (paste("median breakpoint for core:", round(median(c$breakpoint),2)))
print (paste("median breakpoint for intermediate:", round(median(i$breakpoint),2)))
print (paste("median breakpoint for transient:", round(median(t$breakpoint),2)))

print (paste("mean Psi for core:", round(mean(c$Psi),2)))
print (paste("mean Psi for intermediate:", round(mean(i$Psi),2)))
print (paste("mean Psi for transient:", round(mean(t$Psi),2)))

print (paste("mean p for core:", round(mean(c$p),2)))
print (paste("mean p for intermediate:", round(mean(i$p),2)))
print (paste("mean p for transient:", round(mean(t$p),2)))

print (paste("mean S for core:", round(mean(c$S),2)))
print (paste("mean S for intermediate:", round(mean(i$S),2)))
print (paste("mean S for transient:", round(mean(t$S),2)))



#------------------------ PCA biplot of species traits and estimates

# Standardize the matrix to correct for different units by subtracting the
# means and dividing by sd
zscore <- apply(pcagraniv, 2, function(x) {
  y <- (x - mean(x))/sd(x)
  return(y)
})
rownames(zscore) <- rownames(pcagraniv)

#run pca analysis
trait_pc = prcomp(zscore)

#Use ggplot biplot to color by grouped categories
toCol = catgraniv[catgraniv$species %in% rownames(trait_pc$x),"status"]
toCol2 =  catgraniv[catgraniv$species %in% rownames(trait_pc$x),"status2"]

#Label species names and clades, circles cover normal distribuiton of groups
ggbiplot(trait_pc, groups=toCol, labels=rownames(trait_pc$x), label.size = 3, varname.size = 4) + theme_classic() +
  theme(text = element_text(size=20))

#Label species names and clades, circles cover normal distribuiton of groups
ggbiplot(trait_pc, groups=toCol2, labels=rownames(trait_pc$x), label.size = 3, varname.size = 4) + theme_classic() +
  theme(text = element_text(size=20))

