################################################################################################
#######---------######## A_ANALYSIS_MAIN_EXPERIMENT.R ########---------#########################
################################################################################################
### This script replicates three exhibits of the main experiment conducted by
### Casaburi & Willis (2018)
### Exhibit 1: Table 2 + Figure 3 - Treatment Effects on Take-Up
### Exhibit 2: Table 3 - Hetereogenous Treatment Effect by Wealth and Liquidity Constraints Proxies
### Exhibit 3: Table 6 - Heterogenous Treatment Effect by Proxies for Expectations of Default
###############################################################################################

###############################################################################################
### LOADING ALL REQUIRED LIBRARIES
###############################################################################################
library(dplyr)
library(haven)
library(stargazer)
library(ggplot2)
library(car)     # for linearHypothesis()
library(lmtest)  # for nobs()

###############################################################################################
### REPLICATION OF TABLE 2 (MAIN RESULTS) 
###############################################################################################

# 1. Importing the data
data_main <- read_dta("data/data_mainexp.dta")

# 2. Defining varlists
a_list <- c("l_area", "l_yield")
hh_basics <- c("man", "age")
wealth <- c("acres", "any_cow", "share_income_cane", "savings_1", "savings_5")
expectations <- c("exp_yield_normal", "exp_yield_good", "exp_yield_bad")
relationship <- c("good_relation", "trust_fa", "trust_manager")
x_list <- c(hh_basics, wealth, expectations, relationship)

# 3. Creating a function to replace missing values and create missing indicators
process_list <- function(data, var_list) {
  new_vars <- c()
  for (x in var_list) {
    if (x %in% colnames(data)) {
      # Create missing indicator: 1 if NA, else 0
      data[[paste0("m_", x)]] <- ifelse(is.na(data[[x]]), 1, 0)
      # Replace NA with -1 in the original variable
      data[[x]][is.na(data[[x]])] <- -1
      new_vars <- c(new_vars, x, paste0("m_", x))
    } else {
      warning(paste("Variable not found in data:", x))
    }
  }
  list(data = data, new_vars = new_vars)
}

# 4. Process a_list first, then x_list on the updated data
# These are the plot-related controls
res_a <- process_list(data_main, a_list)
data_main <- res_a$data
a_list_with_m <- res_a$new_vars

# These are the farmer-related controls
res_x <- process_list(data_main, x_list)
data_main <- res_x$data
x_list_with_m <- res_x$new_vars

# 5. Building the five regression models to replicate the STATA code

# Column (1): areg y A2 B, omitted group = A1
mod1 <- lm(tookup ~ A2 + B + factor(fid1), data = data_main)
# STATA quietly sum y if A1==1
y_mean_1 <- mean(data_main$tookup[data_main$A1 == 1], na.rm = TRUE)

# Column (2): areg y B, omitted group = A1 + A2
mod2 <- lm(tookup ~ B + factor(fid1), data = data_main)
# STATA quietly sum y if A1==1 or A2==1
y_mean_2 <- mean(data_main$tookup[data_main$A1 == 1 | data_main$A2 == 1], na.rm = TRUE)

# Column (3): areg y A2 B a_list_with_m, omitted group = A1
formula3 <- as.formula(
  paste("tookup ~ A2 + B +", paste(a_list_with_m, collapse = " + "), "+ factor(fid1)")
)
mod3 <- lm(formula3, data = data_main)
y_mean_3 <- y_mean_1  # same as col (1)

# Column (4): areg y A2 B x_list_with_m, omitted group = A1
formula4 <- as.formula(
  paste("tookup ~ A2 + B +", paste(x_list_with_m, collapse = " + "), "+ factor(fid1)")
)
mod4 <- lm(formula4, data = data_main)
y_mean_4 <- y_mean_1  # same as col (1)

# Column (5): areg y A2 B a_list_with_m x_list_with_m, omitted group = A1
formula5 <- as.formula(
  paste("tookup ~ A2 + B +",
        paste(a_list_with_m, collapse = " + "), "+",
        paste(x_list_with_m, collapse = " + "), "+ factor(fid1)")
)
mod5 <- lm(formula5, data = data_main)
y_mean_5 <- y_mean_1  # same as col (1)

# 6. Using stargazer to replicate the table

# Rounding the means for display
mean_dep_vars <- c(y_mean_1, y_mean_2, y_mean_3, y_mean_4, y_mean_5)
mean_dep_vars_rounded <- round(mean_dep_vars, 3)

# For the add.lines in stargazer, we replicate STATA’s “Plot Controls” and “Farmer Controls” flags:
# - col1: field_FE = Y, p_contr = N, f_contr = N
# - col2: same as col1 (field_FE=Y, no additional controls)
# - col3: field_FE=Y, p_contr=Y, f_contr=N
# - col4: field_FE=Y, p_contr=N, f_contr=Y
# - col5: field_FE=Y, p_contr=Y, f_contr=Y

plot_controls <- c("No", "No", "Yes", "No", "Yes")
farmer_controls <- c("No", "No", "No", "Yes", "Yes")

# The following section will print the table to the console as text:
stargazer(mod1, mod2, mod3, mod4, mod5,
          type = "text",
          title = "Main Experiment: Treatment Effects on Take-Up",
          keep = c("A2", "B"),
          order = c("A2", "B"),
          covariate.labels = c("Pay-up-front with 30% Discount", "Pay-at-harvest"),
          omit = c("factor\\(fid1\\)", "Constant"),
          omit.stat = c("rsq", "adj.rsq", "f", "ser"),
          add.lines = list(
            c("Plot Controls", plot_controls),
            c("Farmer Controls", farmer_controls),
            c("Mean dep. var.", mean_dep_vars_rounded)
          ))

# This section on the other hand will output the LaTeX code to a file:
stargazer(mod1, mod2, mod3, mod4, mod5,
          type = "latex",
          title = "Main Experiment: Treatment Effects on Take-Up",
          keep = c("A2", "B"),
          order = c("A2", "B"),
          covariate.labels = c("Pay-up-front with 30% Discount", "Pay-at-harvest"),
          omit = c("factor\\(fid1\\)", "Constant"),
          omit.stat = c("rsq", "adj.rsq", "f", "ser"),
          add.lines = list(
            c("Plot Controls", plot_controls),
            c("Farmer Controls", farmer_controls),
            c("Mean dep. var.", mean_dep_vars_rounded)
          ),
          out = "output/Tables/Table2.tex",
          header = FALSE)

###############################################################################################
### REPLICATION OF FIGURE 3 (MAIN RESULTS) 
###############################################################################################

# 1) Extract standard errors from regression of tookup on A2 and B
mod_reg <- lm(tookup ~ A2 + B, data = data_main)
coef_se <- sqrt(diag(vcov(mod_reg)))
se_A2 <- coef_se["A2"]
se_B  <- coef_se["B"]

# 2) Overall sample size
N <- nrow(data_main)

# 3) Collapse data by treatment group (A1, A2, B)
collapsed_data <- data_main %>%
  group_by(treatment) %>%
  summarise(
    mean_tookup = mean(tookup, na.rm = TRUE),
    sd_tookup   = sd(tookup, na.rm = TRUE),
    n_tookup    = n()
  ) %>%
  ungroup()

# 4) Numeric indicator for treatments (1=A1, 2=A2, 3=B)
collapsed_data <- collapsed_data %>%
  mutate(
    treat_num = case_when(
      treatment == "A1" ~ 1,
      treatment == "A2" ~ 2,
      treatment == "B"  ~ 3,
      TRUE ~ NA_real_
    )
  )

# 5) Compute top (h) and bottom (l) of error bars
#    For A1, h and l both equal mean_tookup (no error bar from this regression)
collapsed_data <- collapsed_data %>%
  mutate(
    h = case_when(
      treatment == "A2" ~ mean_tookup + 1.96 * se_A2,
      treatment == "B"  ~ mean_tookup + 1.96 * se_B,
      TRUE ~ mean_tookup
    ),
    l = case_when(
      treatment == "A2" ~ mean_tookup - 1.96 * se_A2,
      treatment == "B"  ~ mean_tookup - 1.96 * se_B,
      TRUE ~ mean_tookup
    ),
    mean3 = round(mean_tookup, 2)
  )

# 6) Define x-axis labels
x_labels <- c("Pay Upfront", "Pay Upfront w/30% Discount", "Pay At Harvest")

# 7) Plot with label above the error bar
p_fig3 <- ggplot(collapsed_data, aes(x = factor(treat_num), y = mean_tookup, fill = factor(treat_num))) +
  # 1. Bars
  geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
  
  # 2. Error bars
  geom_errorbar(aes(ymin = l, ymax = h), width = 0.15, color = "black") +
  
  # 3. Text offset to the side for the first bar only
  #    Here we shift x slightly if treat_num == 1
  geom_text(
    aes(label = mean3, x = as.numeric(factor(treat_num)) + ifelse(treat_num == 1, 0.075, 0)),
    vjust = -0.8, size = 5
  ) +
  
  # 4. Custom fill colors for each treat_num
  scale_fill_manual(values = c("1" = "#94B3D7",  # Light blue
                               "2" = "#4A6990",  # Dark blue
                               "3" = "#FFA64D"   # Orange
  )) +
  
  # 5. Title, axis labels
  labs(
    title = paste("Insurance Take-Up (N =", N, ")"),
    x = "", 
    y = ""
  ) +
  
  # 6. Rename x-axis categories
  scale_x_discrete(labels = c("1" = "Pay Upfront", 
                              "2" = "Pay Upfront w/30% Discount", 
                              "3" = "Pay At Harvest")) +
  
  # 7. Y-axis scale and breaks
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.2), limits = c(0, 0.9)) +
  
  # 8. Minimal theme, with black axis lines
  theme_minimal() +
  theme(
    plot.title       = element_text(size = 16, hjust = 0.5),
    axis.text.x      = element_text(size = 14, color = "black"),
    axis.text.y      = element_text(size = 14, color = "black"),
    axis.title.y     = element_text(size = 14, color = "black"),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line        = element_line(color = "black", linewidth = 1),
    axis.ticks       = element_line(color = "black", linewidth = 1)
  )

# Finally, save or print the plot
ggsave("output/Figures/Figure3.jpg", plot = p_fig3, width = 8, height = 6, dpi = 300)
print(p_fig3)


ggsave("output/Figures/Figure3.jpg", plot = p_fig3, width = 8, height = 6, dpi = 300)
print(p_fig3)

###############################################################################
#######---------######## REPLICATION OF TABLES 3 & 6 (MAIN EXPERIMENT) ########
###############################################################################

###############################################################################
### 1. PREPARE DATA TO MATCH STATE LEVEL OF CLEANING
###############################################################################

# Replace -1 with NA for relevant variables
vars_to_clean <- c("acres","l_area","l_yield","share_income_cane","savings_1","savings_5",
                   "any_cow","good_relation","trust_fa","trust_manager",
                   "deliv_harvest_share_plots_fh","deliv_harvest_share_plots_sh")
for(v in vars_to_clean){
  if(v %in% names(data_main)){
    data_main[[v]][ data_main[[v]] == -1 ] <- NA
  }
}

# Ensure we have 'B' = Pay At Harvest
if(! "B" %in% names(data_main)){
  stop("Variable 'B' not found. Make sure 'B' is your 'Pay At Harvest' dummy.")
}

# Create interaction terms: B_<var> = B * <var>
all_vars <- c("acres","l_yield","l_area","share_income_cane","savings_1","savings_5",
              "any_cow","good_relation","trust_fa","trust_manager",
              "deliv_harvest_share_plots_fh","deliv_harvest_share_plots_sh")
for(var in all_vars){
  if(var %in% names(data_main)){
    data_main[[paste0("B_", var)]] <- data_main$B * data_main[[var]]
  }
}

# Standardize selected variables
for(s in c("acres","l_yield","l_area")){
  if(s %in% names(data_main)){
    data_main[[s]] <- as.numeric(scale(data_main[[s]]))
    data_main[[paste0("B_", s)]] <- data_main$B * data_main[[s]]
  }
}

###############################################################################
### 2. RENAME COEFFICIENTS TO "X × Pay At Harvest", "X", "Pay At Harvest"
###############################################################################
rename_coefs <- function(mod, var_i) {
  old_names <- names(mod$coefficients)
  new_names <- old_names
  
  # If var_i is present, rename it to "X"
  if(var_i %in% old_names){
    new_names[ old_names == var_i ] <- "X"
  }
  # If B_var_i is present, rename it to "X × Pay At Harvest"
  if(paste0("B_", var_i) %in% old_names){
    new_names[ old_names == paste0("B_", var_i) ] <- "X × Pay At Harvest"
  }
  # If B is present, rename it to "Pay At Harvest"
  if("B" %in% old_names){
    new_names[ old_names == "B" ] <- "Pay At Harvest"
  }
  names(mod$coefficients) <- new_names
  
  # Also rename rows in summary() so SE lines up
  s <- summary(mod)
  if(!is.null(s$coefficients)){
    row_old <- rownames(s$coefficients)
    row_new <- row_old
    if(var_i %in% row_old){
      row_new[row_old == var_i] <- "X"
    }
    if(paste0("B_", var_i) %in% row_old){
      row_new[row_old == paste0("B_", var_i)] <- "X × Pay At Harvest"
    }
    if("B" %in% row_old){
      row_new[row_old == "B"] <- "Pay At Harvest"
    }
    rownames(s$coefficients) <- row_new
    mod$summary_object <- s
  }
  return(mod)
}

###############################################################################
### 3. HELPER FUNCTION: RUN MULTIPLE REGRESSIONS & OUTPUT REGRESSION TABLES
###############################################################################
run_het_regressions <- function(vars_list, col_labels, table_title, out_file,
                                col_numbers = TRUE) {
  
  model_list  <- list()
  x_mean_vec  <- c()
  x_sd_vec    <- c()
  p_value_vec <- c()
  y_mean_vec  <- c()
  obs_vec     <- c()
  
  # Mean dependent var for pay-upfront group => A1==1 or A2==1
  overall_y_mean <- round(mean(data_main$tookup[data_main$A1==1 | data_main$A2==1], na.rm=TRUE), 3)
  
  for (var_i in vars_list) {
    # Subset to non-missing X (if used) or B_X
    # For regressions 4 and 5 of table 6, only B_<var> is used. This is because while the areg command in STATA 
    # automatically dropped B_<var> due to multicollinearity issues, R does not drop them due to computational differences
    # of the tolerance, although the outputed SEs are very high (signalling increasing multicollinearity). Therefore, for
    # the purposes of this replication, and informed by the STATA code, we have decided to manually drop the heterogeneity 
    # variable (X) for these two regressions. The results perfectly matches those in the published paper, confirming the
    # validity of the method.
    if (var_i %in% c("deliv_harvest_share_plots_fh", "deliv_harvest_share_plots_sh")) {
      data_sub <- data_main[ !is.na(data_main[[paste0("B_", var_i)]]) , ]
    } else {
      data_sub <- data_main[ !is.na(data_main[[var_i]]) & !is.na(data_main[[paste0("B_", var_i)]]) , ]
    }
    
    if(nrow(data_sub) == 0){
      model_list[[var_i]] <- NULL
      x_mean_vec  <- c(x_mean_vec, "NA")
      x_sd_vec    <- c(x_sd_vec,   "NA")
      p_value_vec <- c(p_value_vec, "NA")
      y_mean_vec  <- c(y_mean_vec, "NA")
      obs_vec     <- c(obs_vec,    "NA")
      next
    }
    
    # Build formula:
    # For Table 6 - regressions 4 and 5, as explained above, we omit X so that the model is: tookup ~ B_<var> + B + factor(fid1)
    # For others, use: tookup ~ <var> + B_<var> + B + factor(fid1)
    if(var_i %in% c("deliv_harvest_share_plots_fh", "deliv_harvest_share_plots_sh")){
      frm <- as.formula(paste("tookup ~", paste0("B_", var_i), "+ B + factor(fid1)"))
    } else {
      frm <- as.formula(paste("tookup ~", var_i, "+", paste0("B_", var_i), "+ B + factor(fid1)"))
    }
    
    mod <- lm(frm, data = data_sub)
    
    # Rename coefficients: This will rename "B" to "Pay At Harvest", "B_<var>" to "X × Pay At Harvest", and X to "X" if present.
    mod <- rename_coefs(mod, var_i)
    
    # For non Table 6 variables, test: X + (X × Pay At Harvest) = 0
    if(!(var_i %in% c("deliv_harvest_share_plots_fh", "deliv_harvest_share_plots_sh"))) {
      lh_out <- tryCatch({
        linearHypothesis(mod, c("X + X × Pay At Harvest = 0"), test="F")
      }, error = function(e) NULL)
      if(!is.null(lh_out)){
        p_val_val <- lh_out[2, "Pr(>F)"]
        p_val_val <- ifelse(is.na(p_val_val), "NA", round(p_val_val, 3))
      } else {
        p_val_val <- "NA"
      }
    } else {
      p_val_val <- "NA"
    }
    p_value_vec <- c(p_value_vec, p_val_val)
    
    # Calculate mean & SD for X
    if(var_i %in% c("deliv_harvest_share_plots_fh", "deliv_harvest_share_plots_sh")) {
      # Use a subset that requires non-missing values for both the original X and B_X for the regressions where X is dropped
      data_sub_x <- data_main[ !is.na(data_main[[paste0("B_", var_i)]]) & !is.na(data_main[[var_i]]), ]
      x_m <- round(mean(data_sub_x[[var_i]], na.rm=TRUE), 3)
      x_s <- round(sd(data_sub_x[[var_i]], na.rm=TRUE), 3)
    } else {
      x_m <- round(mean(data_sub[[var_i]], na.rm=TRUE), 3)
      x_s <- round(sd(data_sub[[var_i]], na.rm=TRUE), 3)
    }
    x_mean_vec <- c(x_mean_vec, ifelse(!is.na(x_m), x_m, "NA"))
    x_sd_vec   <- c(x_sd_vec,   ifelse(!is.na(x_s), x_s, "NA"))
    
    y_mean_vec <- c(y_mean_vec, overall_y_mean)
    obs_vec    <- c(obs_vec, nobs(mod))
    model_list[[var_i]] <- mod
  }
  
  # Filter out NULL models from the regression table output
  keep_idx <- !sapply(model_list, is.null)
  final_models <- model_list[keep_idx]
  final_labels <- col_labels[keep_idx]
  if(length(final_models)==0){
    message("No valid models for: ", table_title)
    return()
  }
  
  y_mean_vec  <- y_mean_vec[keep_idx]
  x_mean_vec  <- x_mean_vec[keep_idx]
  x_sd_vec    <- x_sd_vec[keep_idx]
  p_value_vec <- p_value_vec[keep_idx]
  obs_vec     <- obs_vec[keep_idx]
  
  # 1) Print to console 
  stargazer(
    final_models,
    type = "text",
    title = table_title,
    dep.var.labels.include = FALSE,
    keep = c("^X × Pay At Harvest$", "^X$", "^Pay At Harvest$"),
    order = c("X × Pay At Harvest", "X", "Pay At Harvest"),
    column.labels = final_labels,
    omit.stat = c("n", "rsq", "adj.rsq", "f", "ser"),  # remove stargazer's "Observations" line
    star.cutoffs = NA,    # no stars
    star.char = c(),      # ensure no star characters
    model.numbers = col_numbers,  # (1), (2), (3), ...
    omit = c("factor\\(fid1\\)", "Constant"),
    add.lines = list(
      c("Mean dep. var. (pay-upfront group)", y_mean_vec),
      c("Mean hetereogeneity variable (X)", x_mean_vec),
      c("SD hetereogeneity variable (X)", x_sd_vec),
      c("Observations", obs_vec)
    )
  )
  
  # 2) Save LaTeX version (same approach)
  stargazer(
    final_models,
    type = "latex",
    title = table_title,
    dep.var.labels.include = FALSE,
    keep = c("^X × Pay At Harvest$", "^X$", "^Pay At Harvest$"),
    order = c("X × Pay At Harvest", "X", "Pay At Harvest"),
    column.labels = final_labels,
    omit.stat = c("n", "rsq", "adj.rsq", "f", "ser"),
    star.cutoffs = NA,
    star.char = c(),
    model.numbers = col_numbers,
    omit = c("factor\\(fid1\\)", "Constant"),
    add.lines = list(
      c("Mean dep. var. (pay-upfront group)", y_mean_vec),
      c("Mean hetereogeneity variable (X)", x_mean_vec),
      c("SD hetereogeneity variable (X)", x_sd_vec),
      c("Observations", obs_vec)
    ),
    out = out_file,
    header = FALSE
  )
}

###############################################################################
### 4. TABLE 3
###############################################################################
# If your screenshot has slightly different text, edit these below:
table3_vars <- c("acres","any_cow","l_yield","l_area","share_income_cane","savings_1","savings_5")
table3_labels <- c(
  "Land cultivated",  # col 1
  "Own cow(s)",               # col 2
  "Previous yield",          # col 3
  "Plot size",                # col 4
  "Portion of income from cane", # col 5
  "Savings for Sh 1,000",      # col 6
  "Savings for Sh 5,000"      # col 7
)

run_het_regressions(
  vars_list   = table3_vars,
  col_labels  = table3_labels,
  table_title = "TABLE 3 — Main Experiment: Heterogeneous Treatment Effects by Wealth and Liquidity Constraints Proxies",
  out_file    = "output/Tables/Table3.tex",
  col_numbers = TRUE   # produce (1), (2), (3), etc.
)

###############################################################################
### 5. TABLE 6
###############################################################################
# If your screenshot has different text, change these:
table6_vars <- c("good_relation","trust_fa","trust_manager","deliv_harvest_share_plots_fh","deliv_harvest_share_plots_sh")
table6_labels <- c(
  "Good relationship with company",  # col 1
  "Trust company field assistants",           # col 2
  "Trust company managers",                   # col 3
  "Past share of plots harvested in field",              # col 4
  "Past share of plots harvested in sublocation"               # col 5
)

run_het_regressions(
  vars_list   = table6_vars,
  col_labels  = table6_labels,
  table_title = "TABLE 6 — Main Experiment: Heterogeneous Treatment Effects by Proxies for Expectations of Default",
  out_file    = "output/Tables/Table6.tex",
  col_numbers = TRUE
)

