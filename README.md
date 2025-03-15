# R Replication Project for Casaburi &amp; Willis (AER, 2018).

**Authors:** MAESTRI Andrea & BENRAMDANE Ali

**Course:** Development Economics, Spring 2025 - M2, Master's in Economics, Sciences Po

**Paper reference:** Casaburi, Lorenzo, and Jack Willis. 2018. "Time versus State in Insurance: Experimental Evidence from Contract Farming in Kenya." American Economic Review 108 (12): 3778–3813.

## Overview of the package

The R code scripts replicate five key exhibits published by Casaburi &amp; Willis in "Time versus State in Insurance: Experimental Evidence from Contract Farming in Kenya" (AER, 2018).

All data was made publicly available by the authors. The original code, written in STATA, along with the data, can be found in the folder 'original_package/do' within this remote repository. Access to the published original package can be accessed [here](https://www.aeaweb.org/articles?id=10.1257/aer.20171526) with the appropriate credentials.

## Description of key folders
- `./data`: contains the three datasets used for this project, including `data_cashexp.dta`, `data_mainexp.dta` and `data_timeexp.dta`
- `./original_package/do`: contains the original STATA code files
- `./output`: folder where all outputs produced by the R code scripts are saved; figures are stored in jpg format, while tables are saved in LaTeX file formats.
- `./src`: contains the three key R script files replicating the five chosen exhibits. 
  - `a_analysis_main_experiment.R` replicates Exhibit 1 (Table 2 + Figure 3), Exhibit 2 (Table 3), Exhibit 3 (Table 6)
  - `b_analysis_main_experiment.R` replicates Exhibit 4 (Table 4 + Figure 4)
  - `c_analysis_main_experiment.R` replicates Exhibit 5 (Table 5 + Figure 5)