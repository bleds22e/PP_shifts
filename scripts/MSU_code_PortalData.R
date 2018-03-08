# Try MSU code with Portal data

#Here, you'll find code for running each of the models used in lab this week with the "lab_06.inp" data set. You'll also find the model-selection table, model-specific results, and results based on some follow-up work with the output from the top model.

### Bring in the Data

library(RMark)
library(dplyr)

# read in data
ms <- read.table("mark_PP_EKB.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
ms_test <- head(ms, 200)

#ms <- mark_trmt %>% filter(species == 'PP') %>% select(captures) %>% rename(ch = captures) 
# write.table(ms, "mark_PP_EKB.txt", sep = "\t", row.names = FALSE)
#   group_by(captures) %>% 
#   summarise(count = n()) 

# head(ms)
# colnames(ms) <- c("ch", "freq")  
# ms$freq <- as.numeric(ms$freq)

### Process the Data

# Process data
ms.pr = process.data(ms_test, begin.time = 130, model = "Multistrata")
#  
# Create default design data
ms.ddl = make.design.data(ms.pr)

# examine the data
# Because the output is so long, here, only the head of each 
# piece of design data is shown
head(ms.ddl$S)
head(ms.ddl$p)
head(ms.ddl$Psi)

### Build Function for Creating Models

#Here, we set up a function that contains the structures for 'S', 'p', and 'Psi'. It will run all combinations of the structures for each parameter type when executed.

run.ms = function() {
  #  Define range of models for S; 
  #  Note: when you use RMark, "S.stratum = list(formula =  ~ stratum)"
  #   will create 3 beta's: 1st beta = intercept (state A = baseline),
  #   2nd beta = intercept adjustment for state B, 
  #   3rd beta = intercept adjustment for state C
  #   i.e., RMark's default design matrix using treatment contrasts 
  #  Below, I use "S.stratum = list(formula =  ~ -1 + stratum)" instead,
  #   which creates a Design Matrix that's an identity matrix such that
  #   the 3 resulting betas are each used alone to estimate rates for 
  #   survival in states A, B and C.
  #  Also, RMark will use logit links for survival and detection and 
  #   the multinomial logit for the probabilities of leaving a stratum
#  S.dot = list(formula =  ~ 1)
  S.stratum = list(formula =  ~ -1 + stratum)
  #
  #  Define range of models for p
#  p.dot = list(formula =  ~ 1)
  
  # select periods that were omitted from the study - untrapped
  p237 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 237,]))
  p241 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 241,]))
  p267 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 267,])) 
  p277 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 277,]))
  p278 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 278,]))
  p283 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 283,]))
  p284 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 284,]))
  p300 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 300,]))
  p311 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 311,]))
  p313 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 313,]))
  p314 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 314,]))
  p318 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 318,]))
  p321 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 321,]))
  p323 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 323,]))
  p337 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 337,]))
  p339 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 339,]))
  p344 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 344,]))
  p351 = as.numeric(row.names(ms.ddl$p[ms.ddl$p$time == 351,]))
  
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
  
  p.dot = list(formula =  ~ 1, fixed = list(index = c(p237, p241, p267, p277, p278, p283, p284, p300, p311, p313, p314,
                                                          p318, p321, p323, p337, p339, p344, p351), 
                                                value = c(p237val, p241val, p267val, p277val, p278val, p283val, p284val, p300val, p311val,
                                                          p313val, p314val, p318val, p321val, p323val, p337val, p339val,
                                                          p344val, p351val), link = "cloglog"))
  #
  #  Define range of models for Psi; what is denoted as s for Psi
  #  in the Mark example for Psi is accomplished by -1+stratum:tostratum,
  #  which nests tostratum within stratum.
  Psi.s = list(formula =  ~ -1 + stratum:tostratum, link = "logit")
  
  # Create model list and run assortment of models
  ms.model.list = create.model.list("Multistrata")
  
  # NOTE: if you do not want to see the output for each model, add the text
  # ", output=FALSE" after "ddl=ms.ddl" below. Here, I don't do that
  # so you can see the output for each model, but this might not be
  # desired if you have a lot of models!
  ms.results = mark.wrapper(ms.model.list,
                            data = ms.pr, ddl = ms.ddl,
                            options="SIMANNEAL")
  #
  # Return model table and list of models
  #
  return(ms.results)
}


### Run the models and examine the output

#It's very simple to then run the function and each of the models. As implemented here, the output from each of the models appears in the console where it can be reviewed when the function written above is called. If you don't want the output from every model to appear automatically, see the note above the "mark.wrapper" command for how to set the option of suppressing the output. One can also examine model-specific output in other ways as shown below.

ms.results = run.ms()

### Examine Model-Selection Table

#Once the models are run, we can examine the model-selection table. We can also examine model-specific output.

ms.results
names(ms.results)

# examine the output from top-ranked model (#3) and
# store top model in object with short name 
top = ms.results$S.stratum.p.dot.Psi.s

# look at summary of top model's output
summary(top, showall = FALSE)

# store and examine estimates of 'S' and 'p'

# First examine the first 5 rows of output
# to see how things are stored
head(top$results$real)

# for 'S' in top model there are 3 estimates
top.S = top$results$real[1:3, ]
top.S

# for 'p' in top model there is 1 estimate
# and it's in the 4th row of output
top.p = top$results$real[4, ]
top.p

# Store and examine the estimates of 'Psi'
Psilist = get.real(top, "Psi", vcv = TRUE)
Psi.values = Psilist$estimates
top.psi = TransitionMatrix(Psi.values[Psi.values$time == 1, ], 
                           vcv.real = Psilist$vcv.real)
top.psi