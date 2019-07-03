# for manipulating  rodent movement data in MARK
## analyze survival and dispseral probabilities for species
# Use multistrata models that give S(survival), p(capture probability) and Psi(transition probability)
# Will compare data among species and guilds in post-hoc analyses. 
# Input files (.txt) generated using rodent_wrapper.r and movement_fxns.r

# Do psi and S significantly differ among species/guilds?

ms_data <- read.table("mark_PP_EKB.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
ms_data <- head(ms_data, 50)

runRMARK = function(wd){
  

#---------------------------------------------------------------------------------
#          bring in the data and source files
#---------------------------------------------------------------------------------

# #grab all the .txt files to loop over for analysis
# files = list.files(wd, pattern = "mark.txt", full.name=T, recursive=T)
# 
# rm(list=ls()[!ls() %in% c("files")])   # clears the memory of everything except the file list, since R MARK is very memory hungry
# 
# for (f in 1:length(files)){
#   
#   require(RMark)
#   
#   # bring in the inp files and convert to RMark format 
#   ms_data = import.chdata(files[f], field.types=c("n","f"))
#   
#   #convert to factor
#   ms_data$species = as.factor(ms_data$species)
#   spname = ms_data$species[1]
#   
#   print(spname)
#   cat("Imported data.", file="outfile.txt", sep="\n")

#---------------------------------------------------------------------------------
#          process multistrata data, includes capture at home, and dipsersal transitions 
#---------------------------------------------------------------------------------
  # Build up the model. 
  # begin.time == first period number
  ms_process = process.data(ms_data, model = "Multistrata", begin.time = 130) #group = c("species")
  
  #ddl = design data list
  ms_ddl = make.design.data(ms_process) 

#---------------------------------------------------------------------------------
#          make dummy variables and covariates
#---------------------------------------------------------------------------------
  # Add dummy variables for operating on specific states(strata) or transitions
  # Species start in 1, movement between 1 and 2 (or vice versa) indicates a relatively long distance movement
  # Switching states indicates making a transition
  
  # SURVIVAL probability given that the individual is in A
  ms_ddl$S$inA = 0
  ms_ddl$S$inA[ms_ddl$S$stratum == "1"] = 1
  
  # SURVIVAL probability given that the individual is in B
  ms_ddl$S$inB = 0
  ms_ddl$S$inB[ms_ddl$S$stratum == "2"] = 1
  
  # RECAPTURE probability given that the individual is in A
  ms_ddl$p$strataA = 0
  ms_ddl$p$strataA[ms_ddl$p$stratum == "1"] = 1
  
  # RECAPTURE probability given that the individual is in B
  ms_ddl$p$strataB = 0
  ms_ddl$p$strataB[ms_ddl$p$stratum == "2"] = 1
  
  # TRANSITION probability given that the individual switches states
  # fix 1-->2 = 2-->1, since we are now interested in "switching"
  #TODO: Are the dummy variables below needed?
  # Transition probability given that the individual  A ---> B
  ms_ddl$Psi$toA = 0
  ms_ddl$Psi$toA[ms_ddl$Psi$stratum == "2" & ms_ddl$Psi$tostratum == "1"] = 1
  
  # Transition probability given that the individual  B ---> A
  ms_ddl$Psi$toB = 0
  ms_ddl$Psi$toB[ms_ddl$Psi$stratum == "1" & ms_ddl$Psi$tostratum == "2"] = 1 

  
#--------------------------------------------------------------------------------
#           Build up the models
#---------------------------------------------------------------------------------
#          Define model structures for S (survival probability)
#---------------------------------------------------------------------------------
  Snull = list(formula = ~1)          

#---------------------------------------------------------------------------------
#          Define model structures for p (capture probability)
#---------------------------------------------------------------------------------
# fix recapture probabilities for unsampled or omitted months
#    skipped_periods = c(237, 241, 267, 277, 278, 283, 284, 300, 311, 313, 314, 318, 321, 323, 337, 339, 344, 351): p = 0

  # select periods that were omitted from the study - untrapped
  p237 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 237,]))
  p241 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 241,]))
  p267 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 267,])) 
  p277 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 277,]))
  p278 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 278,]))
  p283 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 283,]))
  p284 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 284,]))
  p300 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 300,]))
  p311 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 311,]))
  p313 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 313,]))
  p314 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 314,]))
  p318 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 318,]))
  p321 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 321,]))
  p323 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 323,]))
  p337 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 337,]))
  p339 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 339,]))
  p344 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 344,]))
  p351 = as.numeric(row.names(ms_ddl$p[ms_ddl$p$time == 351,]))

  # set those periods to p = 0, because they *can't* be anything else
  p237val = rep(0, length(p237))
  p241val = rep(0, length(p241))
  p267val = rep(0, length(p267))
  p277val = rep(0, length(p277))
  p278val = rep(0, length(p278))
  p283val = rep(0, length(p283))
  p284val = rep(0, length(p284))
  p300val = rep(0, length(p300))
  p311val = rep(0, length(p311))
  p313val = rep(0, length(p313))
  p314val = rep(0, length(p314))
  p318val = rep(0, length(p318))
  p321val = rep(0, length(p321))
  p323val = rep(0, length(p323))
  p337val = rep(0, length(p337))
  p339val = rep(0, length(p339))
  p344val = rep(0, length(p344))
  p351val = rep(0, length(p351))

  cat("Fixed omitted periods to zero.", sep="\n", file="outfile.txt", append=TRUE)


  # look for effects on recapture probability, given that some p are fixed to 0 (listed below)
  # link = "logit" is the default. "cloglog" may be esp. useful when there are fewer recaptures

  #Null Model
  pnull = list(formula = ~1, fixed = list(index = c(p237, p241, p267, p277, p278, p283, p284, p300, p311, p313, p314,
                                                     p318, p321, p323, p337, p339, p344, p351), 
                                           value = c(p237val, p241val, p267val, p277val, p278val, p283val, p284val, p300val, p311val,
                                                     p313val, p314val, p318val, p321val, p323val, p337val, p339val,
                                                     p344val, p351val), link = "cloglog"))
  
  cat("Model for period effect on recapture probability.", sep="\n", file="outfile.txt", append=TRUE)


#---------------------------------------------------------------------------------
#          Define model structures for Psi (transition probability)
#---------------------------------------------------------------------------------
  Psinull = list(formula = ~1, link = "logit")   # KTS: Psinull is correct if we want psi 1 >> 2 == psi 2 >> 1

  cat("Defined model structure for Psi", sep="\n", file="outfile.txt", append=TRUE)

  
#---------------------------------------------------------------------------------
#          Run Models and collect results
#---------------------------------------------------------------------------------
  #send results to new folder - change working directory
  #wd = "~/portal-rodent-dispersal/mark_output/"
  #setwd("C://Users//ellen.bledsoe//'Dropbox (UFL)'//Git//portal-rodent-dispersal_FORK//mark_output_EKB//")
  
  cat("running the multistrata models", sep="\n", file="outfile.txt", append=TRUE)
  
  # SIMANNEAL should be best for multistrata models, but may take longer to run
  movemodel = mark(ms_process, ms_ddl, model.parameters = list(S=Snull,  p=pnull, Psi=Psinull),
                   options="SIMANNEAL", external=FALSE)
  
  cat("Null model is finished", sep="\n", file="outfile.txt", append=TRUE)
  
  
#-----------------------------------------------------
#             summarize results
#-----------------------------------------------------
  #ms_results <- collect.models(type = "Multistrata")
  #print(ms_results)
  cat("Summarized results.", sep="\n", file="outfile.txt", append=TRUE)
  
  print (movemodel$results$beta[1:3,])
  print (movemodel$results$real[1:3,])
  

#---------------------------------------------------------------------------------
#          Write result data to csv files
#---------------------------------------------------------------------------------
  # write.csv(paste(movemodel$results$beta, paste(wd, "movemodel_beta_", spname, ".csv", sep=""))
  # write.csv(movemodel$results$real, paste(wd, "movemodel_real_", spname, ".csv", sep=""))
  # 
  # cat("End Code. Look for your csv files.", sep="\n", file="outfile.txt", append=TRUE)
  # print( paste(spname, " is done.", sep = ""))
  # 
  # rm(list=ls()[!ls() %in% c("f", "files")])   # clears the memory of everything except the file list and iterator
}

#END OF FUNCTION
#}
library(RMark)

runRMARK("C://Users//ellen.bledsoe//'Dropbox (UFL)'//Git//portal-rodent-dispersal_FORK//")
