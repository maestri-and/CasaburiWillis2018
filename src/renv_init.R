###############################################################################
#####----##### INITIALISING RENV FOR REPRODUCIBILITY OF RESULTS #####-----#####
###############################################################################


# Initialise RENV - No need to re-do
# renv::init()

# Instantiate repo dependencies
renv::restore()

# Installing required packages, if needed
packages <- c("dplyr", "ggplot2", "sandwich", "fixest", 
              "stargazer", "haven", "skimr", "lmtest", "car")

# Detect and install only missing packages
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# Take snapshot - Use after updating repo dependencies
# renv::snapshot()
