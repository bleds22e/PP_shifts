# pulled from "http://www.montana.edu/rotella/documents/502/lab06RMark.Rmd"
# other sites of interest: "https://rdrr.io/cran/RMark/man/mstrata.html"
#                         "http://www.phidot.org/software/mark/rmark/RMarkWorkshopNotes.pdf"

---
title: "Lab 06 - Analyses in RMark"
author: "WILD 502 - Jay Rotella"
output: html_document
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
#Here, you'll find code for running each of the models used in lab this week with the "lab_06.inp" data set. You'll also find the model-selection table, model-specific results, and results based on some follow-up work with the output from the top model.

### Bring in the Data

```{r, warning=FALSE}
# Lab 06 for WILD 502 at Montana State University
library(RMark)
ms <- convert.inp("http://www.montana.edu/rotella/documents/502/lab_06.inp",
                  group.df = NULL, covariates = NULL)
head(ms)
```

### Process the Data

```{r}
options(width = 150)
# Process data
ms.pr = process.data(ms, begin.time = 1, model = "Multistrata")
#  
# Create default design data
ms.ddl = make.design.data(ms.pr)

# examine the data
# Because the output is so long, here, only the head of each 
# piece of design data is shown
head(ms.ddl$S)
head(ms.ddl$p)
head(ms.ddl$Psi)
```

### Build Function for Creating Models

Here, we set up a function that contains the structures for 'S', 'p', and 'Psi'. It will run all combinations of the structures for each parameter type when executed.

```{r}
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
  S.dot = list(formula =  ~ 1)
  S.stratum = list(formula =  ~ -1 + stratum)
  #
  #  Define range of models for p
  p.dot = list(formula =  ~ 1)
  p.stratum = list(formula =  ~ stratum)
  #
  #  Define range of models for Psi; what is denoted as s for Psi
  #  in the Mark example for Psi is accomplished by -1+stratum:tostratum,
  #  which nests tostratum within stratum.
  Psi.s = list(formula =  ~ -1 + stratum:tostratum)
  
  # Create model list and run assortment of models
  ms.model.list = create.model.list("Multistrata")
  
  # NOTE: if you do not want to see the output for each model, add the text
  # ", output=FALSE" after "ddl=ms.ddl" below. Here, I don't do that
  # so you can see the output for each model, but this might not be
  # desired if you have a lot of models!
  ms.results = mark.wrapper(ms.model.list,
                            data = ms.pr, ddl = ms.ddl)
  #
  # Return model table and list of models
  #
  return(ms.results)
}
```

### Run the models and examine the output

It's very simple to then run the function and each of the models. As implemented here, the output from each of the models appears in the console where it can be reviewed when the function written above is called. If you don't want the output from every model to appear automatically, see the note above the "mark.wrapper" command for how to set the option of suppressing the output. One can also examine model-specific output in other ways as shown below.

```{r}
ms.results = run.ms()
```

### Examine Model-Selection Table

Once the models are run, we can examine the model-selection table. We can also examine model-specific output.

```{r}
options(width = 150)
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
```

### Work with the Results: Projection through Time

To see how results of such modeling can be used, you worked out how numbers of animals in each habitat would change through time in each patch type if they were projected forward in time using the survival and transition rates you estimated. Here, we see how to use R to do the work.

```{r}
# store the point estimates of 'S' and 'Psi'
phi.hat = matrix(top.S$estimate, 3, 1)
# for Psi, transpose before storing
# for ease of use in code below
psi.hat = t(top.psi$TransitionMat)

# set up a population with 1,000 animals in each patch type
N0 = matrix(c(1000, 1000, 1000), 3, 1)

# Subject animals to survival process
(N0.surv = N0 * phi.hat)

# Move survivors according to transition matrix
# using matrix multiplication
(N1 = psi.hat %*% N0.surv)

# Note: we can do survial & transition in 1 step instead
psi.hat %*% (N0 * phi.hat)

# Repeat the process for years 2-5 using 1-step approach
N2 = psi.hat %*% (N1 * phi.hat)
N3 = psi.hat %*% (N2 * phi.hat)
N4 = psi.hat %*% (N3 * phi.hat)
N5 = psi.hat %*% (N4 * phi.hat)

# view output
N.t = cbind(N0, N1, N2, N3, N4, N5)
# calculate overall population size for each year
Pop.t = apply(N.t, 2, sum)
# view output
rbind(N.t, Pop.t)

# Calculate propotions in each patch over time
(ppn.patch = apply(N.t, 2, prop.table))

# Calculate annual survival rate across patches
(yrly.surv = Pop.t[2:5] / Pop.t[1:4])
# Note that this result matches what you get if you
# calculate the weighted mean of phi for each year
# across patches based on the numbers or ppn in
# each of the patches as shown for 2nd year below
(yr2.surv = weighted.mean(phi.hat, ppn.patch[, 2]))
```