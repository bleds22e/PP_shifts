# Functions in the paper written by Ellen K. Bledsoe

repo_data_to_Supp_data <- function(data, species_data){
  # function to convert rodent data downloaded from the PortalData repo
  # to match up with the format of Sarah Supp's data to use her functions
  
  # get only target species
  target <- species_data$speciescode[species_data$censustarget == 1]
  
  data <- data %>% 
    filter(period > 0, #remove negative periods and periods after plot switch
           year > 1987 & year < 2015, #remove before first plot switch
           plot > 0, species %in% target) # remove non-target animals
  
  ## make dataframe look like Sarah's raw data
  
  # new columns
  data$Treatment_Number = NA
  data$east = NA
  data$north = NA
  data$plot_type = NA
  
  # remove unneeded columns
  data <- select(data, -recordID, -day, -note1, -age, -testes, -lactation, -hfl,
                 -(prevrt:note4))
  
  # reorganize columns
  data <- data[, c("year", "month", "period", "Treatment_Number", 
                   "plot", "stake", "east", "north", "species", "sex", 
                   "reprod", "vagina", "nipples", "pregnant", "wgt",
                   "tag", "note2", "ltag", "note3", "note5", "plot_type")]
  
  # add a plot_type column for easier plotting down the road
  for (i in 1:length(data$period)){
    if (data$plot[i] %in% c(1, 2, 4, 8, 9, 11, 12, 14, 17, 22)){
      data$Treatment_Number[i] = 1
      data$plot_type[i] = 'Control'
    } else if (data$plot[i] %in% c(3, 6, 13, 15, 18, 19, 20, 21 )){
      data$Treatment_Number[i] = 2
      data$plot_type[i] = 'Krat_Exclosure'
    } else {
      data$Treatment_Number[i] = 3 # 5, 7, 10, 16, 23, 24
      data$plot_type[i] = 'Removal'
    }
  }
  
  return(data)
  
}

clean_data_for_capture_histories <- function(data){
  
  # Sarah Supp's code for cleaning up data 
  # specifically for making capture histories for MARK
  # these functions were originally found in 'movement_fxns.r'
  # but are now called from 'functions_SRS.r'
  
  # change some cols from factor to character class
  data$tag = as.character(data$tag)
  data$species = as.character(data$species)
  data$sex = as.character(data$sex)
  
  # subset data where species are known (e.g., no "unidentified rodents" or genus-only)
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
  dups = is_duplicate_tag(data4, tags, 9, 16) #check to see if can separate tags based
  
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


# will need to create MARK function
run.ms = function(S_dot = list(formula = ~ 1), 
                  S_stratum = list(formula =  ~ -1 + stratum + PB_time), 
                  p_dot = list(formula =  ~ 1), 
                  p_stratum = list(formula =  ~ -1 + stratum + PB_time), 
                  Psi_s = list(formula =  ~ -1 + stratum:tostratum + PB_time, link = "logit")) {

  # RMark function for Portal data
  if (is.null(S_dot)) {
    S.stratum = S_stratum
  } else if (is.null(S_stratum)) {
    S.dot = S_dot
  } else {
    S.stratum = S_stratum
    S.dot = S_dot
  }
  
  if (is.null(p_dot)) {
    p.stratum = p_stratum
  } else if (is.null(p_stratum)) {
    p.dot = p_dot
  } else {
    p.stratum = p_stratum
    p.dot = p_dot
  }

  Psi.s = Psi_s

  # Create model list and run assortment of models
  ms.model.list = create.model.list("Multistrata")

  ms.results = mark.wrapper(ms.model.list,
                            data = ms.pr, ddl = ms.ddl,
                            options="SIMANNEAL")

  # Return model table and list of models
  return(ms.results)

}
