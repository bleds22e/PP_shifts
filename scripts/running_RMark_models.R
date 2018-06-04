# Running Multiple MARK Models
# Determine significance of PB_time/Stratum?
# EKB, June 2018

library(dplyr)
library(RMark)

# download data
all <- RCurl::getURL("https://raw.githubusercontent.com/bleds22e/PP_shifts/master/data/MARKdata/PP_capture_history_all.csv")
mark_trmt_all <- read.csv(text = all, header = TRUE, stringsAsFactors = FALSE)

# prep data for RMark
all_ms <- select(mark_trmt_all, captures) %>% rename(ch = captures)
PB_max = 233
first_PP = 118

# Process data
ms.pr = process.data(all_ms, begin.time = first_PP, model = "Multistrata")

# Create default design data
ms.ddl = make.design.data(ms.pr)

# add design covariates for PB era
PB_time_after = as.factor(seq(PB_max, 435))

ms.ddl$S$PB_time = 0
ms.ddl$S$PB_time[ms.ddl$S$time %in% PB_time_after] = 1

ms.ddl$p$PB_time = 0
ms.ddl$p$PB_time[ms.ddl$p$time %in% PB_time_after] = 1

ms.ddl$Psi$PB_time = 0
ms.ddl$Psi$PB_time[ms.ddl$Psi$time %in% PB_time_after] = 1

# Run the models and examine the output

MarkViewer="open -a TextEdit" # edit to make results pop up on a Mac

run.ms = function(){
  
  S.dot = list(formula = ~ 1) 
  S.stratum = list(formula =  ~ -1 + stratum)
  S.time = list(formula = ~ -1 + PB_time)
  S.strat_time = list(formula = ~ -1 + stratum + PB_time)
  S.strat_x_time = list(formula = ~ -1 + stratum*PB_time)
  
  p.dot = list(formula = ~ 1) 
  p.stratum = list(formula =  ~ -1 + stratum)
  p.time = list(formula = ~ -1 + PB_time)
  p.strat_time = list(formula = ~ -1 + stratum + PB_time)
  p.strat_x_time = list(formula = ~ -1 + stratum*PB_time)
  
  Psi.dot = list(formula = ~ 1) 
  Psi.stratum = list(formula =  ~ -1 + stratum:tostratum, link = "logit")
  Psi.time = list(formula = ~ -1 + PB_time, link = "logit")
  Psi.strat_time = list(formula = ~ -1 + stratum:tostratum + PB_time, link = "logit")
  Psi.strat_x_time = list(formula = ~ -1 + stratum:tostratum*PB_time, link = "logit")
  
  ms.model.list = create.model.list("Multistrata")
  
  ms.results = mark.wrapper(ms.model.list,
                            data = ms.pr, ddl = ms.ddl,
                            options = "SIMANNEAL")
  
  return(ms.results)
  
}

ms.results = run.ms()
ms.results

ms.summary = ms.results$S.stratum.p.dot.Psi.s
ms.summary