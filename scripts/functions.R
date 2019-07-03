# Functions for the Manuscript
# Ellen K. Bledsoe

### LIBRARIES ### ==============================================================

library(tidyverse)
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")


### DATA FUNCTIONS ### =========================================================

repo_data_to_Supp_data <- function(data, species_data){
  
  # function to convert rodent data downloaded from the PortalData repo
  # to match up with the format of Sarah Supp's data to use her functions
  # EKB
  
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


id_unknowns <- function(dat, tag_col) {
  
  # used in 'clean_data_for_capture_histories' function
  # give unique numbers to blank tags
  # note: these are 7 digit numbers, so they are longer than any other tag type
  # note: in the Portal data, column 16 is tag, so we are looking for blank or "0" tags to rename
  
  unk = 1000000
  
  for (irow in 1:nrow(dat)) {
    
    tag = dat[irow, tag_col]
    unk = unk + 1
    
    if (rapportools::is.empty(tag)) {
      dat[irow, tag_col] = unk
    } else if (tag == '0') {
      dat[irow, tag_col] = unk
    }
  }
  
  return(dat)
  
}


starred_tags <- function(dat, tags, spp_col, tag_col) {
  
  # used in 'clean_data_for_capture_histories' function
  # Automate checking the flagged data for where the individual breaks should be
  # check for *, which indicates a new tag
  # tags with multiple rows are sorted by species, then checked for *
  # if a * exists, then each time it is given a new unique tag, that ends with "s" for "star" (from note2 column)
  
  # find tags that are 6 characters but are toe tags, not PIT tags
  tags_6 <-
    dat[nchar(dat$tag) >= 6, ] # tags with 6 or more characters
  no_PITtags <- tags_6 %>%
    filter(stringr::str_detect(tag, "[HIMNOPRSTUX]")) %>% # have characters not found in PIT tags
    filter(grepl('\\d{4}\\w{2}', tag)) %>% # have 4 digits followed by 2 characters (unlikely to be a PIT tag)
    select(tag)
  
  numcount = 1
  
  for (t in 1:length(tags)) {
    # only run on ear and toe tags, pit tags are very unlikely to be duplicated
    
    if (nchar(tags[t]) < 6 | tags[t] %in% no_PITtags$tag) {
      tmp <- which(dat$tag == tags[t])
      
      # if indiv was captured multiple times
      if (nrow(dat[tmp, ]) > 1) {
        # check num species recorded. If more than one, does data look OK if separated on species?
        spp_list = unique(dat[tmp, spp_col]) # num of species with that tag
        
        for (sp in 1:length(spp_list)) {
          tmp2 = which(dat$tag == tags[t] & dat$species == spp_list[sp])
          
          isnew = as.vector(dat[tmp2, ]$note2)
          
          if ("*" %in% isnew) {
            rowbreaks = which(isnew == "*", arr.ind = TRUE) # find rows where * indicates a new tag
            
            for (r in 1:length(rowbreaks)) {
              if (r == 1) {
                # GIVE an ID up to the first *
                newtag = paste(tags[t], numcount, "s", sep = "") #make a new tag to keep separate
                dat[tmp2, ][1:rowbreaks[r] - 1, tag_col] = newtag # dataframe with rows before the next star
                numcount = numcount + 1
                
                # AND an ID to everything after the first * (the loop should take care of the next set and so on)
                newtag = paste(tags[t], numcount, "s", sep = "") # make a new tag to keep separate
                dat[tmp2, ][rowbreaks[r]:nrow(dat[tmp2, ]), tag_col] = newtag # identifies this as different
                numcount = numcount + 1
              } else if (r > 1) {
                # GIVE an ID to everything after the next *
                newtag = paste(tags[t], numcount, "s", sep = "") # make a new tag to keep separate
                dat[tmp2, ][rowbreaks[r]:nrow(dat[tmp2, ]), tag_col] = newtag
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


is_dead <- function(dat, tags, spp_col, tag_col) {
  
  # used in 'clean_data_for_capture_histories' function
  # checks note5 for "D", which indicated a dead rat.
  # by definition, all captures with the same tagID afterwards, must be a different individual
  # assign these captures with a new tag ID that ends with 'm' for 'mortality.
  
  numcount = 1
  
  for (t in 1:length(tags)) {
    tmp <- which(dat$tag == tags[t])
    
    # if indiv was captured multiple times
    if (nrow(dat[tmp, ]) > 1) {
      # check num species recorded. If more than one, does data look OK if separated on species?
      spp_list = unique(dat[tmp, spp_col])
      
      for (sp in 1:length(spp_list)) {
        tmp2 = which(dat$tag == tags[t] & dat$species == spp_list[sp])
        
        isdead = as.vector(dat[tmp2, ]$note5)
        
        if ("D" %in% isdead) {
          rowbreaks = which(isdead == "D", arr.ind = TRUE) # find rows where D indicates a dead individuals
          endrow = nrow(dat[tmp2, ])                        # number of rows w/ that tag and species code
          
          for (r in 1:length(rowbreaks)) {
            # length(rowbreaks) = number times D recorded
            if (r == 1) {
              # first row break for that tag
              if (rowbreaks[r] == endrow) {
                # only one time where the tag and species combo is recorded
                
                # GIVE an ID up to the first *
                newtag = paste(tags[t], numcount, "m", sep = "") # make a new tag to keep separate
                numrows = nrow(dat[tmp2, ][1:rowbreaks[r], ])
                newtagvector = as.vector(rep(newtag, numrows))
                dat[tmp2, ][1:rowbreaks[r], tag_col] = newtag
                numcount = numcount + 1
                
              } else {
                # if number of rows w/ combo is higher than 1
                
                # GIVE an ID up to the first *
                newtag = paste(tags[t], numcount, "m", sep = "") # make a new tag to keep separate
                numrows = nrow(dat[tmp2, ][1:rowbreaks[r], ])
                newtagvector = as.vector(rep(newtag, numrows))
                dat[tmp2, ][1:rowbreaks[r], tag_col] = newtag
                numcount = numcount + 1
                
                # AND an ID to everything after the first "D" (the loop should take care of the next set and so on)
                startrow = rowbreaks[r] + 1
                newtag = paste(tags[t], numcount, "m", sep = "") # make a new tag to keep separate
                numrows = nrow(dat[tmp2, ][(startrow:endrow), ])
                newtagvector = as.vector(rep(newtag, numrows))
                dat[tmp2, ][(startrow:endrow), tag_col] = newtag
                numcount = numcount + 1
                
              }
            } else if (r > 1) {
              # if this is not the first time a D is encountered for this tag
              if (rowbreaks[r] == endrow) {
                break
              } else {
                # GIVE an ID to everything after the next "D"
                startrow = rowbreaks[r] + 1
                newtag = paste(tags[t], numcount, "m", sep = "") # make a new tag to keep separate
                numrows = nrow(dat[tmp2, ][(startrow:endrow), ])
                newtagvector = as.vector(rep(newtag, numrows))
                dat[tmp2, ][(startrow:endrow), tag_col] = newtag
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


is_duplicate_tag <- function(dat, tags, spp_col, tag_col) {
  
  # used in 'clean_data_for_capture_histories' function
  # check the min to max year for a given tag.
  # If > 4, considered suspicious
  # If multiple species, considered suspicious
  # If adequately resolved, given a new unique tag number, that ends with d for "duplicate"
  # returns a list with 2 elements [1] altered data, [2] flagged data
  
  numcount = 100
  flagged_rats = data.frame("tag" = 1,
                            "reason" = 1,
                            "occurrences" = 1)
  outcount = 0
  
  # find tags that are 6 characters but are toe tags, not PIT tags
  tags_6 <-
    dat[nchar(dat$tag) >= 6, ] # tags with 6 or more characters
  no_PITtags <- tags_6 %>%
    filter(stringr::str_detect(tag, "[HIMNOPRSTUX]")) %>% # have characters not found in PIT tags
    filter(grepl('\\d{4}\\w{2}', tag)) %>% # have 4 digits followed by 2 characters (unlikely to be a PIT tag)
    select(tag)
  
  all_tags <- c(tags, as.list(unlist(no_PITtags)))
  unique_tags <- unique(all_tags)
  
  for (t in 1:length(unique_tags)) {
    # only run on ear and toe tags, pit tags are very unlikely to be duplicated
    if (nchar(tags[t]) < 6 | tags[t] %in% no_PITtags$tag) {
      tmp <- which(dat$tag == tags[t])
      
      # if indiv was captured multiple times
      if (nrow(dat[tmp, ]) > 1) {
        # more than 3 years between recaptures? Rodents are short-lived.
        if (max(dat[tmp, 1]) - min(dat[tmp, 1]) >= 3) {
          # check num species recorded. If more than one, does data look OK if separated on species?
          spp_list = unique(dat[tmp, spp_col])
          
          for (sp in 1:length(spp_list)) {
            tmp2 = which(dat$tag == tags[t] & dat$species == spp_list[sp])
            
            # Check for duplicate tags in the same period and same species.
            # This likely indicates multiple individuals with the same tag.
            if (anyDuplicated(dat[tmp2, ]) > 0) {
              outcount = outcount + 1
              flagged_rats[outcount, ] <-
                c(tags[t], "sameprd", nrow(dat[tmp, ]))
            }
            
            # Dipodomys are long-lived. Raise the threshold for these indivs
            if (spp_list[sp] %in% list("DO", "DM", "DS")) {
              if (max(dat[tmp2, 1]) - min(dat[tmp2, 1]) < 5) {
                newtag = paste(tags[t], numcount, "d", sep = "") # make a new tag to keep separate
                dat[tmp2, tag_col] = newtag
                numcount = numcount + 1
              } else {
                outcount = outcount + 1
                flagged_rats[outcount, ] <-
                  c(tags[t], "year", nrow(dat[tmp, ]))
              }
            }
            
            # Other genera are very short-lived. Flag data if same individual appears to occur >= 3 years.
            else {
              if (max(dat[tmp2, 1]) - min(dat[tmp2, 1]) < 3) {
                newtag = paste(tags[t], numcount, "d", sep = "") # make a new tag to keep separate
                dat[tmp2, tag_col] = newtag
                numcount = numcount + 1
              } else {
                outcount = outcount + 1
                flagged_rats[outcount, ] <-
                  c(tags[t], "year", nrow(dat[tmp, ]))
              }
            }
          }
        }
      }
    }
  }
  
  info = list(data = dat, bad = flagged_rats)
  return (info)
  
}


same_period <- function(dat, tags){
  
  # used in 'clean_data_for_capture_histories' function
  # multiple individuals with same tag captured in same period = questionable
  
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


find_bad_data2 <- function(dat, tags, sex_col, spp_col) {
  
  # used in 'subsetDat' function
  # check for consistent sex and species, outputs flagged tags to check, 
  #     or to remove from study
  
  flagged_rats = data.frame("tag" = 1,
                            "reason" = 1,
                            "occurrences" = 1)
  outcount = 0
  
  for (t in 1:length(tags)) {
    tmp <- which(dat$tag == tags[t])
    
    if (nrow(dat[tmp, ]) > 1) {
      # if indiv was captured multiple times
      spp_list = dat[tmp, spp_col]
      spp = spp_list[1]
      for (s in 2:length(spp_list)) {
        # check for consistent species
        if (spp_list[s] != spp) {
          outcount = outcount + 1
          flagged_rats[outcount, ] <-
            c(tags[t], "spp", nrow(dat[tmp, ]))
          break
        }
      }
    }
  }
  
  return(flagged_rats)
  
}


subsetDat <- function(dataset){
  
  # used in 'clean_data_for_capture_histories' function
  # function to subset out proper data 
  # will find bad data, then delete it from the dataset
  
  tags = as.character(unique(dataset$tag)) # get list of unique tags
  flags = find_bad_data2(dataset, tags, 10, 9)   # list of flagged data
  
  # first, mark all uncertain or unmarked sex as "U" for unknown
  #     and get rid of other weird typos in sex column
  dataset[which(dataset$sex %in% c("", "P", "Z")), 10] = "U" 
  
  # get rid of results where we don't know the species for sure
  badspptags = unique(flags[which(flags$reason == "spp"), 1])    
  dataset = dataset[-which(dataset$tag %in% badspptags),] 
    # delete rows where species is unsure

  return (dataset)
  
}


clean_data_for_capture_histories <- function(data){
  
  # specifically for making capture histories for MARK
  
  # change some cols from factor to character class
  data$tag = as.character(data$tag)
  data$species = as.character(data$species)
  data$sex = as.character(data$sex)
  
  # subset data where species are known 
  #   (e.g., no "unidentified rodents" or genus-only)
  data2 = subset(data, species!="DX" & species!="UR" & species!="RX" & species!="SX" & species!="PX" & species != "OX")
  
  # give untagged individuals a unique 7-number code
  data2 = id_unknowns(data2, 16)
  
  # make sure when note2 == "*" it is counted as a new tag
  # necessary if using data data (ear and toe tags)
  # returns the dataset with new IDs for checking for duplicate tags 
  #     that occur over a suspiciously long time period
  tags = unique(data2$tag)
  data3 = starred_tags(data2, tags, 9, 16)
  
  # check for dead individuals 
  # give data with same tag after marked dead a new unique ID
  tags = unique(data3$tag)
  data4 = is_dead(data3, tags, 9, 16)
  
  # check for individuals with same tag, but captured over long timespan 
  # necessary if using data data (ear and toe tags)
  # returns the dataset with new IDs for those that can easily be sorted 
  tags = unique(data4$tag)
  dups = is_duplicate_tag(data4, tags, 9, 16) 
  
  # eliminate bad data based on tags identified in dups$bad
  duptags = unique(dups$bad$tag)
  data5 = dups$data[-which(dups$data$tag %in% duptags),] 
    # delete rows flagged as duplicates without clear resolution
  
  tags = unique(data5$tag)
  same = same_period(data5, tags)
  
  # eliminate tags that appear more than once in the same period
  sametags = unique(same$tag)
  data6 = data5[-which(data5$tag %in% sametags),]
  
  # get rid of 'bad data'; deletes data where species is inconsistent. 
  data7 = subsetDat(data6)
  
}


create_trmt_hist = function(dat, tags, prd) {
  
  MARK_data = data.frame("captures" = 1,
                         "censored" = 1,
                         "tags" = 1)
  
  outcount = 0
  
  for (t in 1:length(tags)) {
    
    capture_history = "" # create empty string
    
    for (p in 1:length(prd)) {
      
      tmp <- which(dat$tag == tags[t] & dat$period == prd[p])
      
      if (nrow(dat[tmp, ]) == 0) {
        state = "0"
        capture_history = paste(capture_history, state, sep = "")
      } else {
        if (dat[tmp, 4] == 1) {
          state = "A"
          capture_history = paste(capture_history, state, sep = "")
        } else if (dat[tmp, 4] == 2) {
          state = "B"
          capture_history = paste(capture_history, state, sep = "")
        } else if (dat[tmp, 4] == 3) {
          state = "C"
          capture_history = paste(capture_history, state, sep = "")
        }
      }
    }
    
    tmp2 <- which(dat$tag == tags[t])
    censored = 1
    
    outcount = outcount + 1
    MARK_data[outcount, ] <- c(capture_history, censored, tags[t])
    
  }
  
  return(MARK_data)
  
}


run.ms <- function(S_dot = list(formula = ~ 1), 
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
  ms.model.list <- create.model.list("Multistrata")
  
  ms.results <- mark.wrapper(ms.model.list,
                            data = ms.pr, ddl = ms.ddl,
                            options="SIMANNEAL")
  
  # Return model table and list of models
  return(ms.results)
  
}


### PLOTTING FUNCTIONS ### =====================================================

plot_PB_timeseries_by_treament <- function(data){
  
  # function for plotting C. baileyi avg. abundance per plot
  # through the time series by plot treatment type
  # Figure S4
  
  y_axis_title <- expression(atop(paste(italic("C. baileyi"), " individuals"),
                                  "(average per plot)"))
  
  plot <- ggplot(data, aes(x = year, y = avg_ind_per_prd, color = plot_type, group = plot_type)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .5) +
      scale_color_manual(values = cbbPalette) +
      xlab("Year") +
      ylab(y_axis_title) +
      labs(color = "Plot type") +
      theme_classic() +
      theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
            axis.title.x = element_text(size = 12, margin = margin(t = 10)),
            axis.title.y = element_text(size = 12, margin = margin(r = 10)),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            legend.title = element_blank(),
            plot.margin = margin(r = 15, l = 10))
  
  return(plot)
  
}

plot_PP_regression <- function(data, model){
  
  # function for plotting C. penicillatus residual abundances
  # against C. baileyi average abundance per plot
  # Figure 1c and Figure S1c
  
  x_axis_title <- expression(paste(italic("C. baileyi"), " individuals (average per plot)"))
  y_axis_title <- expression(paste(italic("C. penicillatus"), " residual abundance"))
  
  # plot 1c
  plot <- ggplot(data, aes(x = PB_avg_indiv, y = PP_residuals)) +
    geom_hline(aes(yintercept = 0), color = 'dark gray') +
    geom_smooth(aes(y = fitted(model)),  size = 1, color = "black") +
    geom_point(size = 2) +
    xlab(x_axis_title) +
    ylab(y_axis_title) +
    labs(subtitle = 'c') +
    theme_classic()+
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 10, hjust = -.075),
          axis.line = element_line(size = .25),
          axis.title.x = element_text(size = 9, margin = margin(t = 10)),
          axis.title.y = element_text(size = 9, margin = margin(r = 5)),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.margin = margin(l = 25))
  
  return(plot)
  
}


plot_PB_timeseries <- function(data){
  
  # function for plotting C. baileyi average abundance per plot
  # Figure 1a and Figure S1a
  
  y_axis_title <- expression(atop(paste(italic("C. baileyi"), " individuals"),
                                  "(average per plot)"))
  
  plot <- ggplot(data, aes(x = year, y = PB_avg_indiv)) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 1995, xmax = 1998,
             ymin = -Inf, ymax = Inf) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 2008, xmax = 2010,
             ymin = -Inf, ymax = Inf) +
    geom_point(size = 2) +
    geom_line() +
    xlab("Year") +
    ylab(y_axis_title) +
    labs(subtitle = expression(paste("a", italic("    C. baileyi")))) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 10, hjust = -.1),
          axis.line = element_line(size = .25),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 9, margin = margin(r = 5)),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.margin = margin(r = 15, l = 5, b = 15))
  
  return(plot)
  
}


plot_PP_residuals_timeseries <- function(data){
  
  # function for plotting C. penicillatus residual
  # abundances through time
  # Figure 1b and Figure S1b
  
  y_axis_title <- expression(atop("Residual abundance"),
                             phantom('W'))
  
  plot <- ggplot(data, aes(x = year, y = PP_residuals)) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 1995, xmax = 1998,
             ymin = -Inf, ymax = Inf) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 2008, xmax = 2010,
             ymin = -Inf, ymax = Inf) +
    geom_hline(aes(yintercept = 0), color = 'black') +
    geom_point(size = 2) +
    geom_line()+
    xlab("Year") +
    ylab(y_axis_title) +
    labs(subtitle = expression(paste("b", italic("    C. penicillatus")))) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 10, hjust = -.1),
          axis.line = element_line(size = .25),
          axis.title.x = element_text(size = 9, margin = margin(t = 10)),
          axis.title.y = element_text(size = 9, margin = margin(r = 5)),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.margin = margin(r = 15, l = 5))
  
  return(plot)
  
}


prep_RMark_data_for_plotting <- function(data){
  
  # prep RMark results for plotting
  data$time = c("Before", "After", "Before", "After", "Before", "After", NA, 
                         "Before", "After", "Before", "After", "Before", "After",
                         "Before", "After", "Before", "After", "Before", "After")
  
  # add descriptive columns
  data$metric = rep("S", nrow(data))
  data$metric[7] = "p"
  data$metric[8:19] = "Psi"
  
  data$stratum = c("A", "A", "B", "B", "C", "C", NA, 
                            "AB", "AB",  "AC", "AC", "BA", "BA",
                            "BC", "BC", "CA", "CA", "CB", "CB")
  data$Treatment = c("Control", "Control", "KR Exclosure", "KR Exclosure", "Removal", "Removal", NA,
                              "Control to KR Exclosure", "Control to KR Exclosure", "Control to Removal", "Control to Removal",
                              "KR Exclosure to Control", "KR Exclosure to Control", "KR Exclosure to Removal", "KR Exclosure to Removal",
                              "Removal to Control", "Removal to Control", "Removal to KR Exclosure", "Removal to KR Exclosure")
  
  data <- data %>% 
    filter(metric != "p", stratum == "A" | stratum == "B" | stratum == "AB" | stratum == "BA")
  data$time <- factor(data$time, levels = c("Before", "After"))
  
  return(data)
  
}


plot_estimated_survival <- function(data){
  
  # plot estimated survival metrics from RMark
  # Figure 2a and Figure S2a
  
  x_axis_title <- expression(paste(italic("C. baileyi"), " establishment"))
  
  plot <- ggplot(data[(data$metric == "S"),], color = Treatment) +
    geom_pointrange(aes(x = time, y = estimate, 
                        ymin = (estimate - se), ymax = (estimate + se), 
                        color = Treatment), 
                    position = position_dodge(.1), size = .75) +
    scale_colour_manual(values = cbbPalette) + 
    xlab(x_axis_title) +
    ylab("Residency") + 
    labs(subtitle = 'a') +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 14, hjust = -.35, vjust = -.5),
          axis.line = element_line(size = .25),
          axis.title.x = element_text(size = 12, margin = margin(t = 10)),
          axis.title.y = element_text(size = 12, margin = margin(r = 10)),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "top",
          legend.title = element_blank(),
          plot.margin = margin(l = 5, t = 20))
  
  return(plot)
  
}


plot_transition_probability <- function(data){
  
  # plot transition probability  metrics from RMark
  # Figure 2b and Figure S2b
  
  x_axis_title <- expression(paste(italic("C. baileyi"), " establishment"))
  
  plot <- ggplot(data[(data$metric == "Psi"),]) +
    geom_pointrange(aes(x = time, y = estimate,
                        ymin = (estimate - se), ymax = (estimate + se), 
                        color = Treatment), 
                    position = position_dodge(.1), size = .75) +
    scale_colour_manual(values = cbbPalette) + 
    xlab(x_axis_title) +
    ylab("Transition probability") +
    guides(color = guide_legend(nrow = 2)) +
    labs(subtitle = 'b') +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 14, hjust = -.35, vjust = -.5),
          axis.line = element_line(size = .25),
          axis.title.x = element_text(size = 12, margin = margin(t = 10)),
          axis.title.y = element_text(size = 12, margin = margin(r = 10)),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "top", 
          legend.title = element_blank(),
          plot.margin = margin(l = 20))
  
  return(plot)
  
}


plot_new_PP_individuals <- function(data){
  
  # plot number of new (i.e., untagged) PP individuals per year by treatment
  # Figure 2c and Figure S2c
  
  # rename plot_treatments for plotting
  data$plot_type <- plyr::revalue(data$plot_type, c("Krat_Exclosure" = "KR Exclosure"))
  
  y_axis_title <- expression(atop(paste(italic("C. penicillatus"), " individuals")))
  
  plot <- ggplot(data, aes(x = year,
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
    ylab(y_axis_title) +
    xlab("Year") +
    labs(subtitle = 'c') +
    #guides(color = guide_legend(override.aes = list(size = 3))) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 14, hjust = -.14, vjust = -.5), 
          axis.title.x = element_text(size = 12, margin = margin(t = 10)),
          axis.title.y = element_text(size = 12, margin = margin(r = -10)),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "top", 
          legend.title = element_blank(),
          plot.margin = margin(r = 10, t = 15))
  
  return(plot)
  
}


plot_energy_ratio <- function(data){
  
  # function for plotting KR exclosure:control energy ratio
  # Figure 3 & Figure S3
  
  y_axis_title <- expression(atop("Energy ratio",
                                  "(kangaroo rat exclosure:control)"))
  
  plot <- ggplot(data, aes(year, EX_to_CO_ratio, group = 1)) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 1995, xmax = 1998,
             ymin = -Inf, ymax = Inf) +
    annotate(geom = "rect", fill = "grey", alpha = 0.4,
             xmin = 2008, xmax = 2010, 
             ymin = -Inf, ymax = Inf) +
    geom_point(size = 2) +
    geom_line() +
    ylab(y_axis_title) +
    xlab("Year") + 
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          axis.title.x = element_text(size = 10, margin = margin(t = 10)),
          axis.title.y = element_text(size = 10, margin = margin(r = 10)),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          plot.margin = margin(10, 15, 10, 10))
  
  return(plot)
  
}


plot_avg_competitors <- function(data){
  
  # function for plotting avg of total competitors per treatment
  # Figure S5
  
  # rename plot_treatments for plotting
  data$plot_type <- plyr::revalue(data$plot_type, 
                                  c("Krat_Exclosure" = "KR Exclosure"))
  
  y_axis_title <- expression(atop("Total competitors"),
                             "(average per plot)")
  
  plot <- ggplot(data, aes(x = year,
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
    ylab(y_axis_title) +
    xlab("Year") +
    labs(subtitle = 'c') +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, colour = "black", size = 1.25),
          plot.subtitle = element_text(size = 14, hjust = -.14, vjust = -.5), 
          axis.title.x = element_text(size = 12, margin = margin(t = 10)),
          axis.title.y = element_text(size = 12, margin = margin(r = -10)),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "top", 
          legend.title = element_blank(),
          plot.margin = margin(r = 10, t = 15))
  
  return(plot)
  
}
