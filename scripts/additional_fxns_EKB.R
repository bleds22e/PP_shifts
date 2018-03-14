# Functions for PP data

repo_data_to_Supp_data <- function(data, species_data){
  
  target <- species_data$speciescode[species_data$censustarget == 1]
  
  data <- data %>% 
    filter(period > 0, period < 436, #remove negative periods and periods after plot switch
           year > 1987, #remove before first plot switch
           plot > 0, species %in% target)
  
  ## make dataframe look like Sarah's raw data
  
  # new columns
  data$Treatment_Number = NA
  data$east = NA
  data$north = NA
  
  # remove unneeded columns
  data <- select(data, -recordID, -day, -note1, -age, -testes, -lactation, -hfl, 
                 -(prevrt:note4))
  
  # reorganize columns
  data <- data[, c("year", "month", "period", "Treatment_Number", 
                   "plot", "stake", "east", "north", "species", "sex", 
                   "reprod", "vagina", "nipples", "pregnant", "wgt",
                   "tag", "note2", "ltag", "note3", "note5")]
  
  for (i in 1:length(data$period)){
    if (data$plot[i] %in% c(1, 2, 4, 8, 9, 11, 12, 14, 17, 22)){
      data$Treatment_Number[i] = 1
    } else if (data$plot[i] %in% c(3, 6, 13, 15, 18, 19, 20, 21 )){
      data$Treatment_Number[i] = 2
    } else {
      data$Treatment_Number[i] = 3 # 5, 7, 10, 16, 23, 24
    }
  }
  return(data)
}


clean_data_for_capture_histories <- function(data){
  
  # Sarah Supp's code for cleaning up data 
  
  # change some cols from factor to character class
  data$tag = as.character(data$tag)
  data$species = as.character(data$species)
  data$sex = as.character(data$sex)
  
  #subset data where species are known (e.g., no "unidentified rodents" or genus-only)
  data2 = subset(data, species!="DX" & species!="UR" & species!="RX" & species!="SX" & species!="PX" & species != "OX")
  
  # give untagged individuals a unique 7-number code
  data2 = id_unknowns(data2, 16)
  
  # make sure when note2 == "*" it is counted as a new tag
  # necessary if using data data (ear and toe tags)
  # returns the dataset with new IDs for checking for duplicate tags that occur over a suspiciously long time period
  tags = unique(data2$tag)
  data3 = starred_tags(data2, tags, 9, 16)
  
  #check for dead individuals, give data with same tag after marked dead a new unique ID
  tags = unique(data3$tag)
  data4 = is_dead(data3, tags, 9, 16)
  
  # check for individuals with same tag, but captured over long timespan (may be able to separate them) 
  # necessary if using data data (ear and toe tags)
  # returns the dataset with new IDs for those that can easily be sorted out based on sex and year
  tags = unique(data4$tag)
  dups = is_duplicate_tag(data4, tags, 10, 9, 16) #check to see if can separate tags based
  
  #eliminate bad data based on tags identified in dups$bad
  duptags = unique(dups$bad$tag)
  data5 = dups$data[-which(dups$data$tag %in% duptags),] #delete rows flagged as duplicates without clear resolution
  
  tags = unique(data5$tag)
  same = same_period(data5, tags)
  
  #eliminate tags that appear more than once in the same period - questionable data
  sametags = unique(same$tag)
  data6 = data5[-which(data5$tag %in% sametags),]
  
  # get rid of 'bad data'; deletes data where species is inconsistent. 
  data7 = subsetDat(data6)
  
}

run.ms = function() {
  
  # RMark function for Portal data
  
  #  Define range of models for S: survival for each stratum
  S.stratum = list(formula =  ~ -1 + stratum)
  
  #  Define a null model for p
  p.dot = list(formula =  ~ 1)
  
  #  Define range of models for Psi: value for each possible transition
  #     in the Mark example for Psi is accomplished by -1+stratum:tostratum,
  #     which nests tostratum within stratum.
  Psi.s = list(formula =  ~ -1 + stratum:tostratum, link = "logit")
  
  # Create model list and run assortment of models
  ms.model.list = create.model.list("Multistrata")
  
  ms.results = mark.wrapper(ms.model.list,
                            data = ms.pr, ddl = ms.ddl,
                            options="SIMANNEAL")
  
  # Return model table and list of models
  return(ms.results)
  
}
