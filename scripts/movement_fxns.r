# Module containing functions for looking at individual rodent data pertaining to movement


id_unknowns = function(dat, tag_col){
  # give unique numbers to blank tags
  # note: these are 7 digit numbers, so they are longer than any other tag type
  # note: in the Portal data, column 12 is tag, so we are looking for blank or "0" tags to rename
  
unk = 1000000
for (irow in 1:nrow(dat)){
    tag = dat[irow,tag_col]
    unk = unk + 1
    if (tag == "") {
      dat[irow,tag_col] = unk
    }
    else if (tag == "0") {
      dat[irow,tag_col] = unk
    }}
return(dat)
}


starred_tags = function(dat, tags, spp_col, tag_col){
  #Automate checking the flagged data for where the individual breaks should be
  #check for *, which indicates a new tag
  #tags with multiple rows are sorted by species, then checked for *
  #if a * exists, then each time it is given a new unique tag, that ends with "s" for "star" (from note2 column)
  
  numcount = 1

  for (t in 1:length(tags)){
    
    #only run on ear and toe tags, pit tags are very unlikely to be duplicated
    #NOTE: there are some 6-character toe tags (e.g.1200DM, how to deal with these?)
    if (nchar(tags[t]) < 6){ 
      tmp <- which(dat$tag == tags[t])
      
      # if indiv was captured multiple times  
      if (nrow(dat[tmp,]) > 1) {    
       
      # check num species recorded. If more than one, does data look OK if separated on species?
      spp_list = unique(dat[tmp,spp_col])
        
      for (sp in 1:length(spp_list)) {
      tmp2 = which(dat$tag == tags[t] & dat$species == spp_list[sp])

       isnew = as.vector(dat[tmp2,]$note2)
  
       if ("*" %in% isnew) {
         #print(dat[tmp2,])
         rowbreaks = which(isnew == "*", arr.in=TRUE) #find rows where * indicates a new tag
        
        for (r in 1:length(rowbreaks)){
          if (r == 1) {
          #GIVE an ID up to the first *
          newtag = paste(tags[t], numcount, "s", sep = "") #make a new tag to keep separate
          dat[tmp2,][1:rowbreaks[r]-1, tag_col] = newtag
          numcount = numcount + 1 
        
          #AND an ID to everything after the first * (the loop should take care of the next set and so on)
          newtag = paste(tags[t], numcount, "s", sep = "") #make a new tag to keep separate
          dat[tmp2,][rowbreaks[r]:nrow(dat[tmp2,]),tag_col] = newtag
          numcount = numcount + 1
        }
        else if (r > 1) {
          #GIVE an ID to everything after the next * 
          newtag = paste(tags[t], numcount, "s", sep = "") #make a new tag to keep separate
          dat[tmp2,][rowbreaks[r]:nrow(dat[tmp2,]),tag_col] = newtag
          numcount = numcount + 1
        }
      }
    }
    }
    }
   }
  }
  return(dat)
}


is_dead = function(dat, tags, spp_col, tag_col){
  #checks note5 for "D", which indicated a dead rat. 
  #by definition, all captures with the same tagID afterwards, must be a different individual
  #assign these captures with a new tag ID that ends with 'm' for 'mortality.
  numcount = 1
  
  for (t in 1:length(tags)){
    tmp <- which(dat$tag == tags[t])
    
    # if indiv was captured multiple times  
    if (nrow(dat[tmp,]) > 1) {    
      
      # check num species recorded. If more than one, does data look OK if separated on species?
      spp_list = unique(dat[tmp,spp_col])
      
      for (sp in 1:length(spp_list)) {
        tmp2 = which(dat$tag == tags[t] & dat$species == spp_list[sp])
        
        isdead = as.vector(dat[tmp2,]$note5)
        
        if ("D" %in% isdead) {
          rowbreaks = which(isdead == "D", arr.in=TRUE) #find rows where D indicates a dead individuals
          endrow = nrow(dat[tmp2,])
          #print (endrow)
          
          for (r in 1:length(rowbreaks)){
            if (r == 1) {
              if (rowbreaks[r] == endrow) {
                #GIVE an ID up to the first *
                newtag = paste(tags[t], numcount, "m", sep = "") #make a new tag to keep separate
                numrows = nrow(dat[tmp2,][1:rowbreaks[r],])  
                newtagvector = as.vector(rep(newtag, numrows))
                dat[tmp2,][1:rowbreaks[r], tag_col] = newtag
                numcount = numcount + 1 
                #print(dat[tmp2,]
              }
              else{
              #GIVE an ID up to the first *
              newtag = paste(tags[t], numcount, "m", sep = "") #make a new tag to keep separate
                numrows = nrow(dat[tmp2,][1:rowbreaks[r],])  
                newtagvector = as.vector(rep(newtag, numrows))
              dat[tmp2,][1:rowbreaks[r], tag_col] = newtag
              numcount = numcount + 1 
              #print(dat[tmp2,])
              
              #AND an ID to everything after the first "D" (the loop should take care of the next set and so on)
             startrow = rowbreaks[r] + 1
              newtag = paste(tags[t], numcount, "m", sep = "") #make a new tag to keep separate
                numrows = nrow(dat[tmp2,][(startrow:endrow),])  
                newtagvector = as.vector(rep(newtag, numrows))
              dat[tmp2,][(startrow:endrow),tag_col] = newtag
              numcount = numcount + 1
            }
            }
            else if (r > 1) {
              if (rowbreaks[r] == endrow) {
                break
              }
              else{
              #print (t)
              #GIVE an ID to everything after the next "D"
                startrow = rowbreaks[r] + 1
                newtag = paste(tags[t], numcount, "m", sep = "") #make a new tag to keep separate
                numrows = nrow(dat[tmp2,][(startrow:endrow),])  
                newtagvector = as.vector(rep(newtag, numrows))
                dat[tmp2,][(startrow:endrow),tag_col] = newtag
                numcount = numcount + 1
              }
            }
          }
        }
      }}}
  return(dat)
}


is_duplicate_tag = function(dat, tags, sex_col, spp_col, tag_col){
  # check the min to max year for a given tag. 
  # If > 4, considered suspicious
  # If multiple species, considered suspicious
  # If adequately resolved, given a new unique tag number, that ends with d for "duplicate"
  # returns a list with 2 elements [1] altered data, [2] flagged data
  numcount = 100
  flagged_rats = data.frame("tag"=1, "reason"=1, "occurrences"=1)
  outcount = 0
  
  for (t in 1:length(tags)){
    #only run on ear and toe tags, pit tags are very unlikely to be duplicated
    if (nchar(tags[t]) < 6){ 
      tmp <- which(dat$tag == tags[t])
    
    # if indiv was captured multiple times  
    if (nrow(dat[tmp,]) > 1) {    
      
      #more than 3 years between recaptures? Rodents are short-lived.
      if (max(dat[tmp,1]) - min(dat[tmp,1]) >= 3){ 
        
        # check num species recorded. If more than one, does data look OK if separated on species?
        spp_list = unique(dat[tmp,spp_col])
        
        for (sp in 1:length(spp_list)) {
          tmp2 = which(dat$tag == tags[t] & dat$species == spp_list[sp])
          
          #Check for duplicate tags in the same period and same species. This likely indicates multiple individuals with the same tag.
          if(anyDuplicated(dat[tmp2,]) > 0) {
            outcount = outcount + 1
            flagged_rats[outcount,] <- c(tags[t], "sameprd", nrow(dat[tmp,]))
          }
          
          #Dipodomys are long-lived. Raise the threshold for these indivs
          if(spp_list[sp] %in% list("DO", "DM", "DS")){ 
            
            if (max(dat[tmp2,1]) - min(dat[tmp2,1]) < 5) {  
              newtag = paste(tags[t], numcount, "d", sep = "") #make a new tag to keep separate
              dat[tmp2,tag_col] = newtag
              numcount = numcount + 1 
            }
            else {
              outcount = outcount + 1
              flagged_rats[outcount,] <- c(tags[t], "year", nrow(dat[tmp,]))
              #print(dat[tmp2,])
            }
          }
          
          #Other genera are very short-lived. Flag data if same individual appears to occur >= 3 years.
          else {
            if(max(dat[tmp2,1]) - min(dat[tmp2,1]) < 3) {
              newtag = paste(tags[t], numcount, "d", sep = "") #make a new tag to keep separate
              dat[tmp2,tag_col] = newtag
              numcount = numcount + 1
            }
            else {
              #print(dat[tmp2,])
              outcount = outcount + 1
              flagged_rats[outcount,] <- c(tags[t], "year", nrow(dat[tmp,]))
            }
          }
        }
    }}}}
  info = list(data = dat, bad = flagged_rats)
  return (info)
}


same_period = function(dat, tags){
  # multiple individuals with same tag captured in same period? Questionable daata
  flagged_rats = data.frame("tag"=1, "reason"=1, "occurrences"=1)
  outcount = 0
  
  for (t in 1:length(tags)){
      tmp <- which(dat$tag == tags[t])
      
      if (nrow(dat[tmp,]) > 1){
        periods = unique(dat[tmp,]$period)
        for (p in 1:length(periods)){
          ptmp <- which(dat$tag == tags[t] & dat$period == periods[p])
          if (nrow(dat[ptmp,]) > 1){
            outcount = outcount + 1
            flagged_rats[outcount,] <- c(tags[t], "sameprd", nrow(dat[ptmp,]))
            break
          }
        }
      }
  }
  return (flagged_rats)
}

# only remove inconsistent species IDs, not inconsistent sex
find_bad_spp_data = function(dat, tags, spp_col){
  
  flagged_rats = data.frame("tag"=1, "reason"=1, "occurrences"=1)
  outcount = 0
  
  for (t in 1:length(tags)){
    tmp <- which(dat$tag == tags[t])
    
    if (nrow(dat[tmp,]) > 1) {    # if indiv was captured multiple times
      spp_list = dat[tmp,spp_col]
      spp = spp_list[1]
      
      for (s in 2:length(spp_list)){  # check for consistent species
        if (spp_list[s] != spp){
          outcount = outcount + 1
          flagged_rats[outcount,] <- c(tags[t], "spp", nrow(dat[tmp,]))
          break
        }}
    }}
  return(flagged_rats)
}


find_bad_data2 = function(dat, tags, sex_col, spp_col){
  # check for consistent sex and species, outputs flagged tags to check, or to remove from study
  
  flagged_rats = data.frame("tag"=1, "reason"=1, "occurrences"=1)
  outcount = 0
  
  for (t in 1:length(tags)){
    tmp <- which(dat$tag == tags[t])

    if (nrow(dat[tmp,]) > 1) {    # if indiv was captured multiple times
      sex_list = dat[tmp,sex_col]
      sex = sex_list[1]
      for (i in 2:length(sex_list)){  # check for consistent sex
        if (sex_list[i] != sex) {
         outcount = outcount + 1
          flagged_rats[outcount,] <- c(tags[t], "sex", nrow(dat[tmp,]))
          break
        }}
      spp_list = dat[tmp,spp_col]
      spp = spp_list[1]
      for (s in 2:length(spp_list)){  # check for consistent species
        if (spp_list[s] != spp){
          outcount = outcount + 1
          flagged_rats[outcount,] <- c(tags[t], "spp", nrow(dat[tmp,]))
          break
        }}
    }}
  return(flagged_rats)
}
  

subsetDat = function(dataset){
  ## function to subset out proper data 
  ##### will find bad data, then delete it from the dataset and get rid of incompletely sampled periods
  tags = as.character(unique(dataset$tag)) # get list of unique tags
  flags = find_bad_data2(dataset, tags, 10, 9)   # list of flagged data
  #first, mark all uncertain or unmarked sex as "U" for unknown
  badsextags = unique(flags[which(flags$reason == "sex"),1])
  dataset[which(dataset$tag %in% badsextags),10] = "U"
  dataset[which(dataset$sex %in% c("", "P", "Z")),10] = "U" #get rid of other weird typos in sex column
  #get rid of results where we don't know the species for sure
  badspptags = unique(flags[which(flags$reason == "spp"), 1])    
  dataset = dataset[-which(dataset$tag %in% badspptags),] #delete rows where species is unsure
  
  #don't use negative period numbers and periods with only one day of trapping
  #TODO: add periods from 1980-1999 that were incompletely sampled
  dataset = subset(dataset, period != 111 & period != 237 & period != 241 &
                          period != 267 & period != 277 & period != 278 & period != 283 &
                          period != 284 & period != 300 & period != 311 & period != 313 &
                          period != 314 & period != 318 & period != 321 & period != 323 &
                          period != 337 & period != 339 & period != 344 & period != 351)
  return (dataset)
  }



distance_moved = function (data, tags) {
  # Calculatemoved from one time frame to the next, for an individual
  distance = as.numeric()
  # for each individual
  for (t in 1:length(tags)){
    ind_data = data[which(data$tag == tags[t]),] #get data for indiv with tag t
    ind_data = ind_data[order(ind_data$period),] #order chronologically
    # if it was captured more than once
    if (nrow(ind_data) > 1) { 
      for (i in 1:nrow(ind_data)){
        if (i+1 <= nrow(ind_data)){
          meters = sqrt((ind_data[i,8]-ind_data[i+1,8])**2 + (ind_data[i,7]-ind_data[i+1,7])**2)
          distance=append(distance,meters)
        }
      }
    }
  }
  return(distance)
}


sd_avg_mass = function (ind_dat, spp_mean, spp_sd) {
  # Then takes the average mass for an individual (across all recaptures) 
  # and calculates the proportional difference away from that avg. species level mass.

  ind_mean = mean(ind_dat$wgt, na.rm = TRUE)
  
  if (is.na(ind_mean)) {
    # if no mass is recorded (returns NA), make ind_sd = 0 so we don't lose data. 
    #This shouldn't skew the results, but it ISN'T a true zero. Denote appropriately in methods.
    ind_sd = 0
  }
  
  else {
  # num of standard deviations the indiv's mean wgt is from spp mean wgt
  ind_sd = round((ind_mean - spp_mean) / spp_sd , 4) 
  }
  
  return (ind_sd) #number of standard deviations individual is away from capture weight mean
}


feeding_guild = function(speciesname) {
  # grab the species name and decide what feeding guild it is in, based on the lit
  
  # heteromyidae granivores == 1
  if (speciesname %in% list("DO", "DM", "DS", "PB", "PP", "PF", "PH", "PI",
                            "PE", "PM", "PL", "RM", "RF", "RO", "BA")) {guild = 1}
  # folivores == 2
  else if (speciesname %in% list("SH", "SF", "SO", "NAO")) {guild = 3 }  
  # insectivores == 3
  else {guild = 3}
  
  return(guild)
}


enumerate_species = function(speciesname) {
  #each species needs to be replaced with a number instead of a name,
  #for input into Program MARK later
  
  if (speciesname == "DO") {speciesnum = 1}
  else if (speciesname == "DM") {speciesnum = 2}
  else if (speciesname == "DS") {speciesnum = 3}
  else if (speciesname == "PB") {speciesnum = 4}
  else if (speciesname == "PP") {speciesnum = 5}
  else if (speciesname == "PF") {speciesnum = 6}
  else if (speciesname == "PE") {speciesnum = 7}
  else if (speciesname == "PM") {speciesnum = 8}
  else if (speciesname == "PL") {speciesnum = 10} #transient G
  else if (speciesname == "PH") {speciesnum = 10} #transient G
  else if (speciesname == "PI") {speciesnum = 10} #transient G
  else if (speciesname == "RM") {speciesnum = 9}
  else if (speciesname == "RF") {speciesnum = 10} #transient G
  else if (speciesname == "RO") {speciesnum = 10} #transient G
  else if (speciesname == "BA") {speciesnum = 10} #transient G
  else if (speciesname == "SH") {speciesnum = 12}
  else if (speciesname == "SF") {speciesnum = 13}
  else if (speciesname == "SO") {speciesnum = 14} #transient F
  else if (speciesname == "NAO") {speciesnum = 11}
  else if (speciesname == "OT") {speciesnum = 15}
  else if (speciesname == "OL") {speciesnum = 16}
  
  return(speciesnum)
}

temporal_status = function (speciesname){
  #assigns temporal status to each species based on its across year and within year presence thru span of the entire 30+ year dataset
  #definitions based on output from corespecies, intermediatespecies and transientspecies variables in main script
  #core = 1, intermediate = 2, transient = 3 -- NEEDS TO BE DOUBLE CHECKED WITH DEFINITION
  
  if (speciesname %in% list( "OT","DM","RM","NAO","OL","PE","DO","PP","PF","PB")) {status = 1}
  else if (speciesname %in% list("PM","SH","DS","SF")) {status = 2}
  else if (speciesname %in% list("RF","BA","PH","RO","SO","PI","PL")) {status = 3}
  
  return(status)
}


noplacelikehome = function (dat, prd, exclosures, breakpoint){
  ### Create a set of MARK capture histories by home vs away from home
  # Creates a movement history to be used in Mark. Matrix is filled in with zeroes (not captured) and later filled in 
  # An individual always starts in 1, and is moved to state 2 only if it moves a distance larger than the threshold
  # set by the core species movement distribution. If it is again recaptured at a distance larger than the threshold,
  # is is moved from state 2 back to state 1. This will be used to calculate "transition" (or long distance movement)
  # probability in Rmark.
  ## Home is determined using the mean + 1 sd of the logged data.
  # species - alpha code
  
  tags = unique(dat$tag)
  capture_history = matrix(0, nrow = length(tags), ncol = length(prd))
  covariates = matrix(0, nrow = length(tags), ncol = 2)
  colnames(covariates) = c("freq", "species")
  
  # fill freq in with 1. 1 indicates a normal capture history, -1 indicates a right-censored capture history
  covariates[,1] = as.numeric(1)
  
  # since data is imported by species, we only need to check the first row of data to grab the species name and decide what guild it is in 
  covariates[,2] = as.character(dat[1,]$species)
  
  #loop through each tag to get individual-level data
  for (t in 1:length(tags)) {
    ind_dat = dat[which(dat$tag == tags[t]),] #get data for indiv with tag t
    ind_dat = ind_dat[order(ind_dat$period),] #order chronologically
    
    p1 = min(ind_dat$period) # record first capture period for the individual
    index = match(p1, prd) # match the period with the index number for the list of periods (will correspond to col num in matrix)
    state = 1
    capture_history[t,index] = state  #mark first capture with 1 ("home")   
    
    for (i in 1:nrow(ind_dat)){ #record capture history data
      
      if (i+1 <= nrow(ind_dat)){
        meters = sqrt((ind_dat[i,8]-ind_dat[i+1,8])**2 + (ind_dat[i,7]-ind_dat[i+1,7])**2)
        pnext = ind_dat[i+1,]$period #next capture period, where the distance will be recorded in the matrix (first capture period is always marked as "home")
        
        if (meters <= breakpoint) {dist = state} #captured close to "home"
        
        else if (meters > breakpoint) {
          if (state == 1) {
            dist = 2
            }
          else if (state == 2) {
            dist = 1
            }
        }
        #was it captured on an exclosure? If yes, remove from study at this point.
        if (ind_dat[i+1,]$plot %in% exclosures) {
          covariates[t,1] = as.numeric(covariates[t,1]) * -1 }
        
        #was it found dead or was it removed from the plot? If yes, remove from study at this point.
        if (ind_dat[i+1,]$note5 %in% list("D", "R")) {
          covariates[t,1] = as.numeric(covariates[t,1]) * -1 }
        
        index = match(pnext, prd)
        capture_history[t,index] = dist #mark subsequent captures 
        state = dist
      }
    }  
  }
  mark_df = concat_ch(capture_history, covariates)
  return(mark_df)
}


concat_ch = function (ch_matrix, cov_matrix){
  #concatenates columns representing capture histories, to be used in later MARK analyses
  ch_df <- data.frame(ch_matrix)
  encounters <- do.call(paste, c(ch_df[c(names(ch_df))], sep = '')) # makes a vector of all the capture histories
      semicol <- rep(";", nrow(ch_df)) # makes a vector of semicolons
      mark_df <- cbind(encounters, cov_matrix, semicol) # binds the capture, cov, and semicolon data together into a dataframe
  return (mark_df)
}

count_months = function (data, years) {
  #counts the number of unique months sampled in each year. Returns an ordered list (should be same length as number of years)
  months = as.numeric()
  
  for (y in 1:length(years)){
    yrdata = data[which(data$yr == years[y]),]
    num_mos = length(unique(yrdata$mo))
    months = append(months, num_mos)
  }
  return(months)
}

mean_win_yr_occ = function (data, years, uniq_mos){
  #finds the mean within year occupancy for each month for a given species, returns a single value
  
  proportion_mos = c()
  
  for (y in 1:length (years)){
    yr_data = subset(data, yr == years[y])
    if(nrow(yr_data) > 0) {  #don't use years where it wasn't captured
      m = length(unique(yr_data$mo))/uniq_mos[y]
      proportion_mos = append(proportion_mos, m)
    }
  }
    mean_mos = round(mean(proportion_mos),4)
    
  return (mean_mos)
}


mean_win_yr_alldat = function (data, years, uniq_mos){
  #finds the mean within year occupancy for each month for a given species, returns a single value
  #uniq_mos is the number of months sampled in a given year (not all mos sampled in all years)
  #years should be the number of years that you are interested in
  #THIS FXN NO LONGER OCCURS IN RODENT_WRAPPER - DON'T NEED?
  
  mos = c(1:12)
  
  proportion_mos = c()
  
  for (y in 1:length (years)){
    yr_data = subset(data, yr == years[y])
    if(nrow(yr_data) > 0) {  #don't use years where it wasn't captured
      m = length(unique(yr_data$mo))/uniq_mos[y]
      proportion_mos = append(proportion_mos, m)
    }
  }
  months = round(mean(proportion_mos),4)
  
  return (months)
}


mean_mo_repro = function (femaledata, years){
  #returns the proportion of females that are reproductive (nipples enlarged - E - red - R- or both - B - or pregnant - P-)
  #on average in a given month across all the years. Only looks at data during years and months in which the species is present. 
  month = c(1:12)
  species = rep(femaledata[1,9],12)
    
  proprepro = c()
  
  for (m in 1:length(month)){
    mo_repros = c()
    
    for (y in 1:length(years)){
      tmp = subset(femaledata, yr == years[y] & mo == month[m])
      
      if (nrow(tmp) > 0){
        num_females = nrow(tmp)
        repro = subset(tmp, nipples == "E" | nipples == "B" | nipples == "R" | pregnant == "P")
        prop_repro = nrow(repro)/num_females
        mo_repros = append(mo_repros, prop_repro)
      }
      else {
        mo_repros = append(mo_repros, NA) 
      }
    }
    avg_mo = round(mean(mo_repros, na.rm = T),4)
    proprepro = append(proprepro, avg_mo)
  }

  avg_r_mo = as.data.frame(cbind(species, month, proprepro))
  
  return(avg_r_mo)
}


mo_repro = function (femaledata){
  #returns the proportion of females that are reproductive (nipples enlarged - E - red - R- or both - B - or pregnant - P-)
  #in each year. Only looks at data during years and months in which the species is present. 
  mos = c(1:12)
  years = sort(unique(femaledata$yr)) #only look at data during years in which the species is present
  species = femaledata[1,9]
  
  r_mo_df =data.frame("year" = 1, "month" = 1, "proprepro" = 1, "numfemales" = 1, "species" = 1)
  
  for (m in 1:length(mos)){
    for (y in 1:length(years)){
      tmp = subset(femaledata, yr == years[y] & mo == mos[m])
      
      if (nrow(tmp) > 0){
        num_females = nrow(tmp)
        repro = subset(tmp, nipples == "E" | nipples == "B" | nipples == "R" | pregnant == "P")
        prop_repro = round(nrow(repro)/num_females, 4)
        mo_repros = c(years[y], mos[m], prop_repro, num_females, species)
      }
      else {
        num_females = 0
        mo_repros = c(years[y], mos[m], NA, num_females, species)
      }
      r_mo_df = rbind(r_mo_df, mo_repros)
    }
  }
  
  return(r_mo_df[-1,])
}


count_repro = function(reprodata){
  #input a df of reproductive data. Outputs the number of times that the individual was "uniquely" reproductive.
  # unique reproduction is defined by having a break between reproductive status in the trapping record.
  rh = 1   #reproductive history counter. Starts at one. Increment when the difference between reproductive events is > 1
  
  if (nrow(reprodata) > 1) { 
    prds = reprodata$period
    
    for (p in 1:length(prds)){
      
      if ((p + 1) <= length(prds)){ #can't go past the number of periods that exist for the individual
        diff = prds[p+1] - prds[p] #subtract the periods
        
        if (diff > 1){
          rh = rh + 1 #increment by one if the difference between reproductive events is > 1 period
        }
      }
    }  
  }
  return (rh)
}

indiv_repro = function (femaledata){
  # returns the reproductive history of females in the time series
  # for each individual, all rows of data where nipples are red, enlarged or both, or it is pregnant,
  # are set aside. Within a year, the trapping periods are ordered to detect timing of reproduction
  # and the number of unique reproduction events, per year, for each individual female.
  tags = unique(femaledata$tag)

  reprod_df = data.frame("tag" = 1, "year" = 1, "num_reprod" = 1)
  
  for (t in 1:length(tags)) {
    indiv = subset(femaledata, tag == tags[t]) #get individual data, sorted by chronologically by period
    indiv = indiv[order(indiv$period),] #order chronologically
    
    years = sort(unique(indiv$yr))
    
    for (y in 1:length(years)){
      tmp = subset(indiv, yr == years[y])
      repro = subset(tmp, nipples == "E" |  nipples == "B" | nipples == "R" | pregnant == "P") #count as reproductive if pregnant or nursing, NOT if swollen vagina
      
      if (nrow(repro) > 0){
        numreprod = count_repro(repro)
      }
      
      else {
        numreprod = 0
      }
      
    }
    data = c(tags[t], years[y], numreprod)
    reprod_df = rbind(reprod_df, data)
  }
  return (reprod_df[-1,])
}


allyrs_abun = function(spdata, years){
  #function to find abundance for a species across the timeseries. 
  # input the species dataframe, calculates total number of unique individuals. Returns a vector.
  abun = c()
  
  for (y in 1:length(years)){
    dat = subset(spdata, spdata[,1] == years[y])
    if (length(dat) > 0) {
      indivs = sort(unique(dat$tag))  #grab unique individuals by tag ID
      abun = append(abun, length(indivs)) #count the number of unique tags
    }
    else { abun = append(abun, 0)}
  }
  return (abun)
}


# pairs plot functions
panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "black", ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 2)
}

#-----------------------------------------------OLD CODE

# noplacelikehome = function (dat, prd, exclosures, breakpoint){
#   ### Create a set of MARK capture histories by home vs away from home
#   # Creates a movement history to be used in Mark. Matrix is filled in with zeroes (not captured) and later filled in 
#   ## 1 (stayed home), and 2 (away from home). 
#   ## Home is determined using the mean + 1 sd of the logged data.
#   # guild (1 = granivore, 2 = folivore, 3 = carnivore)
#   # species (assigned using function enumerate_species)
#   # status (1 = core, 2 = intermediate, 3 = transient)
#   
#   tags = unique(dat$tag)
#   capture_history = matrix(0, nrow = length(tags), ncol = length(prd))
#   covariates = matrix(0, nrow = length(tags), ncol = 7)
#     colnames(covariates) = c("male", "female", "unidsex", "sd_mass", "guild", "species", "status")
#     group = c(1,2,3) #represent the "group" (core, transient, intermediate)
# 
#   # finds the average of all the masses for all non-pregnant adults in the species, as a baseline. 
#   # remove juveniles and pregnant females for adult mass estimation
#   adult_dat = dat[which(dat$reprod != "J" & dat$pregnant != "P"),] 
#   mean_mass = mean(adult_dat$wgt, na.rm = TRUE) 
#   sd_mass = sd(adult_dat$wgt, na.rm = TRUE)
#   
#   # since data is imported by species, we only need to check the first row of data to grab the species name and decide what guild it is in
#   # record guild in col 5 of covariates
#   covariates[,5] = feeding_guild(dat[1,]$species)
#  
#   # record species in col 6 of covariates    
#   covariates[,6] = enumerate_species(dat[1,]$species)
#  
#   # record hypothesized status in col 7 of covariates   
#   covariates[,7] = temporal_status(dat[1,]$species)
# 
#   #loop through each tag to get individual-level data
#   for (t in 1:length(tags)) {
#     ind_dat = dat[which(dat$tag == tags[t]),] #get data for indiv with tag t
#     ind_dat = ind_dat[order(ind_dat$period),] #order chronologically
#     
#     #mark sex in Male, Female, or Unidentified columns of Covariates
#     sex = ind_dat[1,]$sex 
#     if (sex == "M") { covariates[t,1] = 1 } 
#     else if (sex == "F") { covariates[t,2] = 1 }
#     else { covariates[t,3] = 1 }
#     
#     # record standard deviations away from species average mass as another Covariate
#     covariates[t,4] = sd_avg_mass(ind_dat, mean_mass, sd_mass) 
#     
#     p1 = min(ind_dat$period) # record first capture period for the individual
#     index = match(p1, prd) # match the period with the index number for the list of periods (will correspond to col num in matrix)
#     capture_history[t,index] = 1  #mark first capture with 1 ("home")      
#     
#     for (i in 1:nrow(ind_dat)){ #record capture history data
#       
#       if (i+1 <= nrow(ind_dat)){
#         meters = sqrt((ind_dat[i,8]-ind_dat[i+1,8])**2 + (ind_dat[i,7]-ind_dat[i+1,7])**2)
#         pnext = ind_dat[i+1,]$period #next capture period, where the distance will be recorded in the matrix (first capture period is always marked as "home")
#         
#         if (meters <= breakpoint) {dist = 1} #captured close to "home"
#         
#         else if (meters > breakpoint) {dist = 2} #captured far from "home"
# 
#         #was it captured on an exclosure? If yes, remove from study at this point.
#         if (ind_dat[i+1,]$plot %in% exclosures) {
#           covariates[t,6] = covariates[t,6] * -1 }
#         
#         #was it found dead or was it removed from the plot? If yes, remove from study at this point.
#         if (ind_dat[i+1,]$note5 %in% list("D", "R")) {
#           covariates[t,6] = covariates[t,6] * -1 }
#         
#         index = match(pnext, prd)
#         capture_history[t,index] = dist #mark subsequent captures 
#       }
#     }  
#   }
#   mark_df = concat_ch(capture_history, covariates)
#   return(mark_df)
# }

