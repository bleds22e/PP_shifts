[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3371700.svg)](https://doi.org/10.5281/zenodo.3371700)

# PP Shifts
## Temporal changes in species composition affect a ubiquitous species' use of habitat patches

This repo contains analyses for my project on how _C. penicillatus's_ use of habitat patches has changed through time in response to the colonization of _C. baileyi_ at The Protal Project.

The manuscript for this project is currently in press at Ecology.


### Manuscript Analysis

To conduct the analyses in the manuscript, you'll need the two scripts in the "scripts" folder:

  * _scripts/PP_shifts.R_: contains all the analyses and figures in the paper
    * You should be able to source this code and have it should run smoothly, assuming all packages are installed
    * As written, revelant results files are imported in; to run the original RMark analysis, see _Tip #3_ below
  * _scripts/functions.R_: contains all functions used in the PP_shifts.R script
    * sourced in the PP_shifts.R script
    * includes both data-focused functions and plotting functions


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

* Patch Preference of _C. penicillatus_ in Response to _C. baileyi_ Abundance [line 83]
* _C. penicillatus_ Population-level Metrics and RMark [line 211]
* System-level Aspects of Patch Preference [line 367]

#### 3) Choose whether you want to run the RMark analyses yourself or not

If you choose to run the entire script in its current state, it will NOT run all of the RMark analyses from scratch. Instead, it will import the necessary results files to complete the analyses.

The code to run the original RMark files can be found in the main analysis file, but these lines of code are commented out. If you would prefer to run the original code, you just need to remove the pound sign from those lines and comment out the lines of code that read in the results. To run the original RMark code, you will need to have MARK downloaded on your machine; running code will likely take a couple hours and potentially crash your computer.

See lines 226-235 and lines 247-256 in the _scripts/PP_shifts.R_ file for more information

### Other Files in the Repo

  * _data/_
    * _all_model_results_20180712.csv_ output from all models from the latest RMark run 
    * _PP_capture_history_all_20180711.csv_ results from `create_trmt_hist()`(line 240)
    * _test_data_prepared.csv_ test data for `testthat` functions
    * _top_model_summary_20190416.csv_ output from top model from latest RMark run (line 294)  
  * _figures/_ 
    * manuscript figures
  * _testthat/_ 
    * _test_fuctions.r_ script with test functions to ensure functions are giving the expected outputs   
  * _preliminary_work/_ 
    * all the bits and pieces used in the project but not in the manuscript
