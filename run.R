###############################################################################
###############################################################################
###############################################################################
#######--------------------######## RUN.R ########---------------------########

###---------------### Benramdane, Ali & Maestri, Andrea ###-----------------###
###----------------------------### March 2025 ###---------------------------###

###############################################################################
###############################################################################
###############################################################################

###-------### This master script runs all the code in the package ###-------###
###---------------### to replicate all the main results ###-----------------###
###-------------------### of Casaburi & Willis, 2018 ###--------------------###


##### 1. Instantiate required packages #####
source("./src/renv_init.R")

##### 2. Reproduce main experiment results #####
### Figure 3, Tables 2, 3, 6 
source("./src/a_analysis_main_experiment.R")
rm(list = ls())

##### 3. Reproduce cash experiment results #####
### Figure 4 + Tables 4
source("./src/b_analysis_cash_experiment.R")
rm(list = ls())

##### 3. Reproduce cash experiment results #####
### Figure 5 + Table 5 
source("./src/c_analysis_time_experiment.R")
rm(list = ls())


