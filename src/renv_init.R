###############################################################################
#####----##### INITIALISING RENV FOR REPRODUCIBILITY OF RESULTS #####-----#####
###############################################################################


# Initialise RENV - No need to re-do
# renv::init()

# Instantiate repo dependencies
renv::restore()

# Installing main packages 
install.packages("dplyr")
install.packages("ggplot2")
install.packages("sandwich")
install.packages("fixest")
install.packages("stargazer")   # To output nice tables 
install.packages("haven")       # To read .dta files
install.packages("skimr")       # To produce descriptive statistics
install.packages("lmtest")
install.packages("car")

# Take snapshot - Use after updating repo dependencies
renv::snapshot()
