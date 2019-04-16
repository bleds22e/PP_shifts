# Running Multiple MARK Models
# Determine significance of PB_time/Stratum?
# EKB, June 2018

library(dplyr)
library(RMark)

# download data
mark_trmt_all 

# prep data for RMark
all_ms <- select(mark_trmt_all, ch = captures)

first_PP = 123
last_PP = 388

num_periods = last_PP - first_PP + 1
PB_max = 233

# PB max period = 233

# Process data
ms.pr = process.data(all_ms, begin.time = first_PP, model = "Multistrata")

# Create default design data
ms.ddl = make.design.data(ms.pr)

# add design covariates for PB era
PB_time_after = as.factor(seq(PB_max, last_PP))

ms.ddl$S$PB_time = 0
ms.ddl$S$PB_time[ms.ddl$S$time %in% PB_time_after] = 1

ms.ddl$p$PB_time = 0
ms.ddl$p$PB_time[ms.ddl$p$time %in% PB_time_after] = 1

ms.ddl$Psi$PB_time = 0
ms.ddl$Psi$PB_time[ms.ddl$Psi$time %in% PB_time_after] = 1

# Run the models and examine the output

#MarkViewer="Notepad" # edit to make results pop up on a Mac

run.ms = function(){
  
  S.dot = list(formula = ~ 1) 
  S.stratum = list(formula =  ~ -1 + stratum)
  S.time = list(formula = ~ -1 + PB_time)
  S.strat_time = list(formula = ~ -1 + stratum + PB_time)
  S.strat_x_time = list(formula = ~ -1 + stratum*PB_time)
  
  p.dot = list(formula = ~ 1) 
  #p.stratum = list(formula =  ~ -1 + stratum)
  #p.time = list(formula = ~ -1 + PB_time)
  #p.strat_time = list(formula = ~ -1 + stratum + PB_time)
  #p.strat_x_time = list(formula = ~ -1 + stratum*PB_time)
  
  Psi.dot = list(formula = ~ 1, link = "logit") 
  Psi.s = list(formula =  ~ -1 + stratum:tostratum, link = "logit")
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
model.table <- as.data.frame(ms.results$model.table)
write.csv(model.table, "MARK_results_summary_20190416.csv")

top_model_summary <- ms.results$S.strat_x_time.p.dot.Psi.strat_x_time$results$real
write.csv(top_model_summary, "top_model_summary_20190416.csv")
