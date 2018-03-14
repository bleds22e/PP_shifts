#---------------------------------------------------------------
# Run MARK analyses on both data sets
#---------------------------------------------------------------

# LIBRARIES
library(RMark)
library(dplyr)
library(RCurl)

# DATA

# load in capture histories 
pre <- getURL("https://raw.githubusercontent.com/bleds22e/PP_shifts/master/data/MARKdata/PP_capture_history_prePBmax.csv")
mark_trmt_pre <- read.csv(text = pre, header = TRUE, stringsAsFactors = FALSE)

post <- getURL("https://raw.githubusercontent.com/bleds22e/PP_shifts/master/data/MARKdata/PP_capture_history_postPBmax.csv")
mark_trmt_post <- read.csv(text = post, header = TRUE, stringsAsFactors = FALSE)

# prep data for RMark
pre_ms <- select(mark_trmt_pre, captures) %>% rename(ch = captures)
post_ms <- select(mark_trmt_post, captures) %>% rename(ch = captures)

# FUNCTIONS

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

# RUN RMARK

first_PP = 213 # code for these elsewhere
PB_max = 233

## Before PBs Infiltrated

# Process data
ms.pr = process.data(pre_ms_test, begin.time = first_PP, model = "Multistrata")

# Create default design data
ms.ddl = make.design.data(ms.pr)

# Run the models and examine the output
ms.results = run.ms()
ms.summary = ms.results$S.stratum.p.dot.Psi.s
#write.csv(ms.summary$results$real, "data/MARKdata/MARKoutput_PP_prePBmax_real.csv")

### After PBs Infiltrated

# Process data
ms.pr = process.data(post_ms, begin.time = PB_max, model = "Multistrata")

# Create default design data
ms.ddl = make.design.data(ms.pr)

# Run the models and examine the output
ms.results = run.ms()
ms.summary = ms.results$S.stratum.p.dot.Psi.s
#write.csv(ms.summary$results$real, "data/MARKdata/MARKoutput_PP_postPBmax_real.csv")
