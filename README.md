# PP Shifts
## "Temporal changes in species composition affect a ubiquitous species' views of patch quality"

This repo contains analyses for my project on how _C. penicillatus's_ perception of patch quality has changed through time in response to the colonization of _c. baileyi_ at The Protal Project.

The manuscript for this project is currently under review at Ecology.


### Manuscript Analysis

To conduct the analyses in the manuscript, you'll need the two scripts in the "scripts" folder:

  * _scripts/PP_shifts.R_: contains all the analyses and figures in the paper
  
    * You can technically source this code, and it should run smoothly, assuming all packages are installed
    * **HOWEVER!**--this includes running some extensive models in RMark, which will take a long time (see _Tips_ section below)
<br>  
  * _scripts/functions.R_: contains all functions used in the PP_shifts.R script
  
    * sourced in the PP_shifts.R script
    * includes both data-focused functions and plotting functions

<br>

### Tips for Running These Analyses

#### 1) Make sure all of the packages listed in the _PP_shifts.R_ script have been installed.

[`portalr`](https://github.com/weecology/portalr), a package for easily downloading data from the Portal Project, can now be downloaded from CRAN.

I used the `patchwork` package to make the multipanel figures. You'll need to download that package from GitHub:

```
# install.packages("devtools")
devtools::install_github("thomasp85/patchwork")
```

#### 2) The script is broken into 3 main parts

In addition to calling in relevant libraries, source code, and data, the script has the following three sections:

* Patch Preference of _C. penicillatus_ in Response to _C. baileyi_ Abundance [line 77]
* _C. penicillatus_ Population-level Metrics and RMark [line 200]
* System-level Aspects of Patch Preference [line 335]


#### 3) Choose whether you want to run the RMark analyses yourself or not

If you choose to run the entire script in its current state, it will run all of the RMark analyses from scratch. This will likely take a couple hours and potentially crash your computer.

To avoid this, you can load in the data from the latest run of the model. If you would prefer to do this, you can skip lines 216-280 and run line 283 instead. There are also warnings and directions about this in the script itself.

<br>

### Other Files in the Repo