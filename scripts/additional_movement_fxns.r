# Functions that accompany the Supp, Koons, and Ernest rodent movement project using data at the Portal Project 2000-2009
# These functions were used to create figures and summarizing data, but may not be included in the final paper.

# # # list of dataframes                                                 WORKS NOW, TO USE? OR NOT TO USE? 
# datLS = list(het, cricet, foliv, insec)
# names(datLS) = c('het','cricet', 'foliv', 'insec')
# for (df in seq_along(datLS)) datLS[[df]]$tag = as.character(datLS[[df]]$tag)

#---------------------------------------------------------------------------------------------------------------
#################### OLD FUNCTIONS ? TO FIX AND WORK ON...
#---------------------------------------------------------------------------------------------------------------

### Create a set of capture histories by treatment type
create_trmt_hist = function(dat, tags, prd){
  
  MARK_data = data.frame("captures"=1, 
                         "censored"=1, 
                         "tags"=1)
  
  outcount = 0
  
  for (t in 1:length(tags)){
    
    capture_history = "" #create empty string
    
    for (p in 1:length(prd)){
      
      tmp <- which(dat$tag==tags[t] & dat$period==prd[p])
      
      if (nrow(dat[tmp,]) == 0) {
        state = "0"
        capture_history = paste(capture_history, state, sep="")}
      else if (nrow(dat[tmp,] > 0)) {
        if (dat[tmp, 4] == 1){
          state = "A"
          capture_history = paste(capture_history, state, sep="")}
        else if (dat[tmp, 4] == 2){
          state = "B"
          capture_history = paste(capture_history, state, sep="")}
        else if (dat[tmp, 4] == 3){
          state = "C"
          capture_history = paste(capture_history, state, sep="")}
      }
    }
    
    tmp2<-which(dat$tag==tags[t])
    censored = 1
    
    #for (irow in nrow(dat[tmp2,])){
    #  if (dat[tmp2, 4] == 3) {
    #    censored = -1
    #    break}}
    
    #spp = unique(dat[which(dat$tag == tags[t]), 9])
    #sex = unique(dat[which(dat$tag == tags[t]), 10])
    #avg_mass = mean(dat[which(dat$tag==tags[t]), 15])
    
    outcount = outcount + 1
    MARK_data[outcount,] <- c(capture_history, censored, tags[t])
    
  }
  return(MARK_data)
}

## Create a set of capture histories by plot
create_plot_hist = function(dat, tags, prd) {
  
  MARK_data_by_plot = data.frame("captures"=1, 
                                 "censored"=1, 
                                 "tags"=1, 
                                 "species"=1, 
                                 "sex" = 1,
                                 "avg_weight" = 1)
  
  outcount = 0
  
  for (t in 1:length(tags)){
    capture_history = "" #create empty string
    
    for (p in 1:length(prd)){
      
      tmp<-which(dat$tag == tags[t] & dat$period == prd[p])
      
      if (nrow(dat[tmp,]) == 0) {
        state = "0"
        capture_history = paste(capture_history, state, sep = ",")}
      else if (nrow(dat[tmp,] > 0)) {
        state = as.character(dat[tmp, 5])
        capture_history = paste(capture_history, state, sep = ",")}
    }
    
    tmp2<-which(dat$tag == tags[t])
    
    censored = 1
    
    for (irow in nrow(dat[tmp2,])){
      if (dat[tmp2, 4] == 3) {
        censored = -1
        break}}
    
    spp = unique(dat[which(dat$tag == tags[t]), 9])
    sex = unique(dat[which(dat$tag == tags[t]), 10])
    avg_mass = mean(dat[which(dat$tag == tags[t]), 15])
    
    outcount = outcount + 1
    MARK_data_by_plot[outcount, ] <- c(capture_history, censored, tags[t], spp, sex, avg_mass)
    
  }
  
  return(MARK_data_by_plot)
  
}

# Find rats that are moving around
find_rats_that_move = function(dat, tags){
  
  moving_rats = data.frame("tag"=1, "spp"=1, "sex"=1, "loc"=1, "occurrences"=1)
  outcount = 0
  
  for (t in 1:length(tags)){
    
    tmp <- which(dat$tag == tags[t])
    spp = unique(dat[tmp, 9])
    sex = unique(dat[tmp, 10])
    
    if (nrow(dat[tmp,]) > 1) {
      trmt_list = dat[tmp, 4]
      trmt = trmt_list[1]
      for (i in 2:length(trmt_list)){
        if (trmt_list[i] != trmt) {
          outcount = outcount + 1
          moving_rats[outcount,] <- c(tags[t], spp, sex, "trmt", nrow(dat[tmp,]))
          break
        }}
      plot_list = dat[tmp, 5]
      plot = plot_list[1]
      for (p in 2:length(plot_list)){
        if (plot_list[p] != plot){
          outcount = outcount + 1
          moving_rats[outcount,] <- c(tags[t], spp, sex, "plot", nrow(dat[tmp,]))
          break
        }}
    }}
  return(moving_rats)
}

# find num captures/rat
num_captures = function(dat, tags){
  
  rat_catches = data.frame("tag"=1, "spp"=1, "sex"=1, "captures"=1)
  outcount = 0
  
  for (t in 1:length(tags)){
    tmp <- which(dat$tag == tags[t])
    spp = unique(dat[tmp, 9])
    sex = unique(dat[tmp, 10])
    captures = nrow(dat[tmp,])
    outcount = outcount + 1
    rat_catches[outcount,] <- c(tags[t], spp, sex, captures)
  }
  rat_catches$captures = as.numeric(rat_catches$captures)
  return(rat_catches)
}

### Creates a dataframe to look at movement
examine_trmt_moves = function(dat, tags){
  
  moves_trmt = data.frame("tags"=1, "sp"=1, "sex"=1, "num_moves"=1, "move_list"=1, "censored"=1,
                          "c2r"=1, "r2c"=1, "c2e"=1, "r2e"=1)
  outcount = 0
  
  for (t in 1:length(tags)){
    move_list = as.character() #create empty string
    ind_dat=dat[which(dat$tag==tags[t]),]  # subset data for individual with tag t
    if (nrow(ind_dat) > 1) {  # only record data for indivs with multiple captures
      for (i in 1:nrow(ind_dat)){
        if (ind_dat[i, 4] == 1){
          state = "A"
          move_list <- append(move_list, state)
          censored = 1 }
        else if (ind_dat[i,4] == 2){
          state = "B"
          move_list <- append(move_list, state)
          censored = 1 }
        else if (ind_dat[i,4] == 3){
          state = "C"
          move_list <- append(move_list, state)
          censored = -1 }
      }
      type = move_list[1]
      moves = 0
      new_list = ""
      c2r = 0
      r2c = 0
      c2e = 0
      r2e = 0
      for (l in 1:length(move_list)){ # changed from 1 to 2, no effect
        new_list = paste(new_list, move_list[l], sep="")
        # moved c2r = 0, etc. from here
        if (move_list[l]!=type) { 
          moves = moves + 1
          if (type == "A" &  move_list[l] == "B") {
            c2r = c2r + 1}
          else if (type == "A" & move_list[l] == "C") {
            c2e = c2e + 1}
          else if (type == "B" & move_list[l] == "A") {
            r2c = r2c + 1}
          else if (type == "B" & move_list[l] == "C") {
            r2e = r2e + 1}
          type = move_list[l] # this part currently not working fully
        }}
      spp = unique(ind_dat[,9])
      sex = unique(ind_dat[,10])
      outcount = outcount + 1
      moves_trmt[outcount,] <- c(tags[t], spp, sex, moves, new_list, censored, c2r, r2c, c2e, r2e)
    }}
  return(moves_trmt)
}

### Creates a dataframe to look at movement among plots
examine_plot_moves = function(dat, tags){
  
  moves_plot = data.frame("tags"=1, "sp"=1, "sex"=1, "num_moves"=1, "neighborhood"=1, "move_list"=1)
  outcount = 0
  
  for (t in 1:length(tags)){
    ind_dat=dat[which(dat$tag==tags[t]),]  # subset data for individual with tag t
    if (nrow(ind_dat) > 1) {  # only record data for indivs with multiple captures
      move_list = as.numeric()
      for (i in 1:nrow(ind_dat)){
        state = ind_dat[i, 5]
        move_list <- append(move_list, state)}
    }
    type = move_list[1]
    neighbor = ""
    moves = 0
    new_list = ""
    for (l in 1:length(move_list)){
      new_list = paste(new_list, move_list[l], sep=",")
      if (move_list[l] != type) {
        moves = moves + 1
        near = is.neighbor(move_list[l], type) 
        neighbor = paste(neighbor, near, sep = "")
        type = move_list[l] }}
    spp = unique(ind_dat[, 9])
    sex = unique(ind_dat[, 10])
    outcount = outcount + 1
    moves_plot[outcount,] <- c(tags[t], spp, sex, moves, neighbor, new_list)
  }
  return(moves_plot)
}

# determine if a plot is a neighbor
is.neighbor = function(plotA, plotB){
  
  if (plotA == 1) {
    if (plotB == 2 | plotB == 7) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 2) {
    if (plotB == 1 | plotB == 3 | plotB == 8 ) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 3) {
    if (plotB == 2 | plotB == 4 | plotB == 8 | plotB == 9 | plotB==10) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 4) {
    if (plotB == 3 | plotB == 5 | plotB == 9 | plotB == 10 | plotB == 11) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 5) {
    if (plotB == 4 | plotB == 6 | plotB == 11 | plotB == 12 | plotB == 24) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 6) {
    if (plotB == 5 | plotB==24 | plotB == 12) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 7) {
    if (plotB == 1 | plotB == 2 | plotB == 13 | plotB == 8 ) {neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 8 ) {
    if (plotB == 2 | plotB == 7 | plotB == 9 | plotB == 14 ) { neighbor = "Y"}
    else { neighbor = "N" }}
  else if (plotA == 9 ) {
    if (plotB == 8 | plotB == 10 | plotB==3 | plotB == 4 | plotB == 16 | plotB == 15) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 10 ) {
    if (plotB == 9 | plotB == 11 | plotB == 16 | plotB == 4 | plotB == 17 ) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 11 ) {
    if (plotB == 10 | plotB == 12 | plotB == 5 | plotB == 16 | plotB == 17 | plotB == 18) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 12 ) {
    if (plotB == 11 | plotB == 5 | plotB == 6 | plotB == 24 | plotB == 19 | plotB == 18) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 13) {
    if (plotB == 7 | plotB == 14 | plotB == 8) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 14) {
    if (plotB == 13 | plotB == 8 | plotB == 15 | plotB == 7) {neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 15) {
    if (plotB == 14 | plotB == 16 | plotB == 9 | plotB == 8 | plotB == 20) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 16) {
    if (plotB == 15 | plotB == 17 | plotB == 9 | plotB == 10 | plotB == 21 | plotB == 20) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 17) {
    if (plotB == 16 | plotB == 18 | plotB == 10 | plotB == 11 | plotB == 21 | plotB == 22) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 18) {
    if (plotB == 17 | plotB == 19 | plotB == 12 | plotB == 22 | plotB == 23) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 19) {
    if (plotB == 12 | plotB == 18 | plotB == 24) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 20) {
    if (plotB == 15 | plotB == 16 | plotB == 21) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 21) {
    if (plotB == 20 | plotB == 22 | plotB == 16 | plotB == 15) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 22) {
    if (plotB == 21 | plotB == 23 | plotB == 17 | plotB == 18) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 23) {
    if (plotB == 22 | plotB == 18) { neighbor = "Y" }
    else { neighbor = "N" }}
  else if (plotA == 24) {
    if (plotB == 6 | plotB == 19 | plotB == 12) { neighbor == "Y" }
    else { neighbor = "N" }}
  else { neighbor = "ERROR" }
  
  return (neighbor)
}

### Creates a dataframe to look at movement among plots          # FIX ME   #WHY ARE THERE MULTIPLE CAPS PER PERIOD?? FIX!
examine_stake_moves = function(dat, tags, stake_col, e_col, n_col, spp_col, sex_col){
  
  moves_stake = data.frame("tags"=1, "sp"=1, "sex"=1, "avg_mass"=1, "num_moves"=1, "stake_list"=1)
  outcount = 0
  #distances = as.numeric()
  
  for (t in 1:length(tags)){
    stake_list = as.numeric() #create empty string
    ind_dat=dat[which(dat$tag==tags[t]),]  # subset data for individual with tag t
    ind_dat = ind_dat[order(ind_dat[,2]),] # order by period number so time is sequential
    if (nrow(ind_dat) > 1) {  # only record data for indivs with multiple captures
      for (i in 1:nrow(ind_dat)){
        stake = ind_dat[i,stake_col]
        stake_list <- append(stake_list, stake)}  
      type = stake_list[1]
      moves = 0
      new_stake_list = ""
      for (l in 1:length(stake_list)){
        new_stake_list = paste(new_stake_list, stake_list[l], sep=" ")
        if (stake_list[l]!=type) {
          moves = moves + 1
          type = stake_list[l] }}
      if (moves > 1) {  
        #    dist_list = as.numeric()
        #    for (i in 1:nrow(ind_dat)){
        #      if (i > 1){
        #      dist = round(UTM_dist(ind_dat[i,e_col], ind_dat[i-1,e_col], ind_dat[i,n_col], ind_dat[i-1,n_col]),3)
        #      dist_list = append(dist_list, dist)
        #      }}
        avg_mass = averageMass(ind_dat)
        spp = unique(ind_dat[,spp_col])
        sex = unique(ind_dat[,sex_col])
        outcount = outcount + 1
        moves_stake[outcount,] <- c(tags[t], spp, sex, avg_mass, moves, new_stake_list)
      }}}
  return(moves_stake)
}

# calculte dist between two UTMs, dist=sqrt((x2-x1)^2 + (y2-y1)^2) 
UTM_dist = function (e1, e2, n1, n2){
  dist = sqrt((e2-e1)^2 + (n2-n1)^2)
  return(dist)
}

# Get average mass from all rows of data pertaining to individual, don't count Juvenile or pregnant mass
averageMass = function(data) {
  mass_data = subset(data, reprod != "J" & pregnant != "P")
  masses = subset(mass_data, mass > 0, na.rm=T)
  avg_mass = sum(masses$mass)/length(masses$mass)
  return(round(avg_mass,3))}

# break data up into x and y coordinates
xy_data = function (stake) {
  
  if (stake==71|stake==72|stake==73|stake==74|stake==75|stake==76|stake==77) {
    x = 7  }
  else if (stake==61|stake==62|stake==63|stake==64|stake==65|stake==66|stake==67) {
    x = 6 }
  else if (stake==51|stake==52|stake==53|stake==54|stake==55|stake==56|stake==57) {
    x = 5 }
  else if (stake==41|stake==42|stake==43|stake==44|stake==45|stake==46|stake==47) {
    x = 4 }
  else if (stake==31|stake==32|stake==33|stake==34|stake==35|stake==36|stake==37) {
    x = 3 }
  else if (stake==21|stake==22|stake==23|stake==24|stake==25|stake==26|stake==27) {
    x = 2 }
  else { x = 1}
  if (stake==71|stake==61|stake==51|stake==41|stake==31|stake==21|stake==11) {
    y = 1 }
  else if (stake==72|stake==62|stake==52|stake==42|stake==32|stake==22|stake==12) {
    y = 2 }
  else if (stake==73|stake==63|stake==53|stake==43|stake==33|stake==32|stake==31) {
    y = 3 }
  else if (stake==74|stake==64|stake==54|stake==44|stake==34|stake==24|stake==14) {
    y = 4 }
  else if (stake==75|stake==65|stake==55|stake==45|stake==35|stake==25|stake==15) {
    y = 5 }
  else if (stake==76|stake==66|stake==56|stake==46|stake==36|stake==26|stake==16) {
    y = 6 }
  else  { y = 7 }
  xy=c(x,y)
  return(xy)
}

# plot stake moves, takes data from those in one plot that are recaptured at least three times
plot_stake_moves = function (dat, tags, stake_col, plot_col, spp_col, sex_col) {
  for (t in 1:length(tags)){
    coords = data.frame("x"=1,"y"=1)
    outcount = 0
    ind_dat=dat[which(dat$tag==tags[t]),]  # subset data for individual with tag t
    ind_dat = ind_dat[order(ind_dat[,2]),] # order by period number so time is sequential
    if (nrow(ind_dat) > 2) {  # only record data for indivs with multiple captures
      for (i in 1:nrow(ind_dat)){
        stake = ind_dat[i,stake_col]
        xy = xy_data(stake)
        outcount = outcount + 1
        coords[outcount,]<-xy
      } 
      avg_mass = averageMass(ind_dat)
      spp = unique(ind_dat[,spp_col])
      sex = unique(ind_dat[,sex_col])
      plot_num = unique(ind_dat[,plot_col])
      plot(coords$x, coords$y, xlim = c(1,7), ylim = c(1,7), type = "b", 
           main = paste(avg_mass, "g", sex, spp, "in plot", plot_num, ",recaptures =", nrow(coords), sep = " "))
      points(coords[1,1], coords[1,2], pch = 17, col = "indianred") #start
      points(coords[nrow(coords),1],coords[nrow(coords),2],pch = 15, col = "blue") #stop
    }}}


# plot number of individuals captured in each period (since each record has its own row, we can just count the number
### of occurences of each period)
plot_freq_by_prd = function(data, name) {
  require(plyr)
  
  prdcount = count(data, vars = "period")
  plot(prdcount$period, prdcount$freq, type = "b", pch = 19, xlab = "Period", 
       ylab = paste("num individual", name, "captured", sep = ' '))
}


#plot number of recaptures as a histogram for each species
plot_recap_hist = function (data, name) {
  recapcount = count(data, vars = "tag")
  recaps = recapcount$freq
  bw = seq(min(recaps), max(recaps), 1)
  hist(recaps, breaks = bw, xlab = paste("num", name, "recaptures", sep = " "), ylab = "count",
       col = "gray60", border = "white", main = "")
}

# 
# ######################### plotting commands - mostly density plots, not exactly correct ##############################
# 
# # plot density of movment by guild for 2000-2009 - DENSITY PLOTS AREN'T REALLY "CORRECT" BECAUSE DATA ARE DISCRETE
# plot(density(Hgran), main = 'Portal movement by guild', xlab = 'meters', lwd = 2, col = 'hotpink2')
# lines(density(Cgran), col = 'deepskyblue3', lwd = 3, lty = 6)
# lines(density(naometers), col = 'indianred4', lwd = 4, lty = 3)
# lines(density(foli), col = 'mediumpurple4', lwd = 4, lty = 3)
# lines(density(insectiv), col = 'darkgreen', lwd = 2)
# legend('topright', c('Hgran', 'Neotoma', 'foliv', 'Cgran', 'insec'), bty = 'n', lty = c(1,3,6,3,1), lwd = 5, seg.len = 2,
#        col = c('hotpink2', 'indianred4', 'mediumpurple4', 'deepskyblue3', 'darkgreen'))
# 
# 
# ############## MOVING
# # get list of indivs that moved plots or treatment, species is included
# moving_rats = find_rats_that_move(heteros, tags, 8, 9, 3, 4)
# 
# # subset tags that never leave a plot
# stationary_hets = heteros
# plotmovers = unique(moving_rats$tag)
# for (i in 1:length(plotmovers)) {
#   stationary_hets = subset(stationary_hets, tag != plotmovers[i])
# }
# 
# # list the stakes it inhabits
# tags = unique(stationary_hets$tag)
# stakemoves = examine_stake_moves(stationary_hets, tags, 5, 6, 7, 8, 9)
# 
# # calculate the distances between each trapping location
# plot_stake_moves(stationary_hets, tags, 5, 4, 8, 9)
# 
# 
# # plot density of all consecutive movement from rodents within a species 2000-2009, NOT A GREAT WAY TO PLOT THIS DATA
# plot(density(pf_meters), main = paste("P. flavus (", length(pftags), " = i, ", length(pf_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
# plot(density(pp_meters), main = paste("C. penicillatus (", length(pptags), " = i, ", length(pp_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
# plot(density(pb_meters), main = paste("C. baileyi (", length(pbtags), " = i, ", length(pb_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
# plot(density(do_meters), main = paste("D. ordii (", length(dotags), " = i, ", length(do_meters), " = N)", sep = ''), lwd = 2, xlim = c(0,500), ylim = c(0,.1))
# plot(density(dm_meters), main = paste("D. merriami (", length(dmtags), "= i, ", length(dm_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
# 
# plot(density(pe_meters), main = paste("P. eremicus (", length(petags), " = i, ", length(pe_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
# plot(density(pm_meters), main = paste("P. maniculatus (", length(pmtags), " = i, ", length(pm_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
# plot(density(rm_meters), main = paste("R. megalotis (", length(rmtags), "= i, ", length(rm_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
# 
# plot(density(sh_meters), main = paste("S. hispidus (", length(shtags), " = i, ", length(sh_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
# plot(density(sf_meters), main = paste("S. fulviventer (", length(sftags), " = i, ", length(sf_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
# plot(density(nao_meters), main = paste("N. albigula (", length(naotags), " = i, ", length(nao_meters), " = N)", sep = ''), lwd = 2, xlim = c(0,500), ylim = c(0,.1))
# 
# plot(density(ot_meters), main = paste("O. torridus (", length(ottags), " = i, ", length(ot_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
# plot(density(ol_meters), main = paste("O. leucogaster (", length(oltags), " = i, ", length(ol_meters), " = N)", sep = ''), xlab = 'meters', lwd = 2, xlim = c(0,500), ylim = c(0,.1))
# 
# 
# #plot all together using density WHICH IS NOT REALLY A GOOD WAY TO PLOT THIS DATA
# plot(density(pb_meters), main = 'Portal rodent movement', xlab = 'meters', lwd = 2, col = 'peru')
# lines(density(pp_meters), col = 'black', lwd = 2)
# lines(density(do_meters), col = 'mediumpurple4', lwd = 4, lty = 3)
# lines(density(dm_meters), col = 'maroon4', lwd = 4, lty = 3)
# lines(density(pf_meters), col = 'hotpink', lwd = 2)
# 
# lines(density(pe_meters), col = 'deepskyblue3', lwd = 3, lty = 6)
# lines(density(pm_meters), col = 'royalblue4', lwd = 3, lty = 6)
# lines(density(rm_meters), col = 'cadetblue', lwd = 3, lty = 6)
# 
# lines(density(sh_meters), col = 'indianred', lwd = 3)
# lines(density(sf_meters), col = 'brown', lwd = 3)
# lines(density(nao_meters), col = 'gray60', lwd = 3)
# 
# lines(density(ot_meters), col = 'darkgreen', lwd = 2)
# lines(density(ol_meters), col = 'darkolivegreen', lwd = 2)
# 
# legend('topright', c('PB','PP','DO','DM','PE','PM','OT','OL'), bty = 'n', lty = c(1,1,3,3,6,6,1,1), lwd = 10, seg.len = 2, 
#        col = c('peru', 'black', 'mediumpurple4','maroon4','deepskyblue3', 'royalblue3','darkgreen', 'darkolivegreen'))
# abline(v = 70.71, lty = 2, col = 'gray60', lwd = 2)

#---------------------------------------------------------------------------------
#           mark winter months
#---------------------------------------------------------------------------------
# small-bodied species may be rare during winter months (go into torpor)
# we want to mark the months that this is, so we can later denote them properly in MARK
#pp = subset(het, species == "PP")
#barplot(table(pp$mo))
#pf = subset(het, species == "PF")
#barplot(table(pf$mo))


#prds = c(261:380)
#winter = c()

#for (p in 1:length(prds)){
#  dat = subset(allrats, period == prds[p])
#  mo = dat[1,2]
#  if (mo %in% list(12, 1, 2)){
#    winter = append(winter, prds[p])
#  }
#}

