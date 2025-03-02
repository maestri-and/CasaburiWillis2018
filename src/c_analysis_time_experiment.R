###############################################################################
#######---------######## C_ANALYSIS_TIME_EXPERIMENT.R ########---------########
###############################################################################

###-----------### This script replicates Figure 5 and Table 5 ###-----------###
###-------------------### of Casaburi & Willis, 2018 ###--------------------###

##### 0. IMPORTING LIBRARIES AND FUNCTIONS #####
library(dplyr)
library(haven)
library(skimr)
library(fixest)
library(stargazer)
library(lmtest)
library(car)
library(ggplot2)


##### 1. IMPORTING AND PREPARING DATA #####

data <- read_dta("data/data_timeexp.dta")
skim(data) # No missing values in the data!

# Define varlists
a_list <- c("l_area", "l_yield")
hh_basics <- c("man", "age")
wealth <- c("acres", "any_cow", "share_income_cane", "savings_1", "savings_5")
expectations <- c("exp_yield_normal", "exp_yield_good", "exp_yield_bad")
relationship <- c("good_relation", "trust_fa", "trust_manager")
x_list <- c(hh_basics, wealth, expectations, relationship)


# Replace missing values
# Combine lists into a named list
lists <- list(a_list = a_list, x_list = x_list)

# Initialize new lists
a_list_with_m <- c()
x_list_with_m <- c()

# Define a function to process each list
process_list <- function(var_list) {
  new_vars <- c()
  for (x in var_list) {
    if (x %in% colnames(data)) {
      # Generate missing indicator
      data[[paste0("m_", x)]] <- ifelse(is.na(data[[x]]), 1, 0)
      
      # Replace NA with -1 in original variable
      data[[x]][is.na(data[[x]])] <- -1
      
      # Append original variable and its missing indicator
      new_vars <- c(new_vars, x, paste0("m_", x))
    }
  }
  return(new_vars)
}

# Process lists and store the results
a_list_with_m <- process_list(a_list)
x_list_with_m <- process_list(x_list)

##### 2. ESTIMATING TABLE 5: TIME EXPERIMENT TABLE #####

# Perform fixed effects regression
# Col 1 - no controls
no_c_fe <- feols(tookup ~ C | fid3, data, se = "standard")
no_c_fe_lm <- lm(tookup ~ C + factor(fid3), data = data)

stargazer(no_c_fe_lm, 
          type = "text",
          omit = c("fid3", "Constant"))

# Store mean for treated 
sum_y <- skim(data %>% filter(A == 1) %>% select(tookup))
y_mean_round = round(sum_y$numeric.mean, digits=3)

# Col 2 - Plot controls
p_c_fe_lm <- lm(as.formula(
  paste("tookup", "~ C +", paste(a_list_with_m, collapse = " + "), "+ factor(fid3)")), 
  data = data)

stargazer(p_c_fe_lm, 
          type = "text",
          omit = c("fid3", "Constant", a_list_with_m))


# Col 3 - Farmer controls
f_c_fe_lm <- lm(as.formula(
  paste("tookup", "~ C +", paste(x_list_with_m, collapse = " + "), "+ factor(fid3)")), 
  data = data)

stargazer(f_c_fe_lm, 
          type = "text",
          omit = c("fid3", "Constant", x_list_with_m))



# Col 4 - Plot and farmer controls
all_c_fe_lm <- lm(as.formula(
  paste("tookup", "~ C +", paste(a_list_with_m, collapse = " + "), " + ",
        paste(x_list_with_m, collapse = " + "),"+ factor(fid3)")), 
  data = data)

stargazer(all_c_fe_lm, 
          type = "text",
          omit = c("fid3", "Constant", a_list_with_m, x_list_with_m))


# OUTPUT TABLE
tab5_otpt_fp_tex = "output/Tables/Table5.tex"
stargazer(no_c_fe_lm, p_c_fe_lm, f_c_fe_lm, all_c_fe_lm,
          type = "text",
          title = "Table 5 — Intertemporal Preferences Experiment: Treatment Effect on Take-Up",
          align = TRUE,
          dep.var.labels = "Take-Up", 
          dep.var.labels.include = FALSE,
          omit = c("fid3", "Constant", a_list_with_m, x_list_with_m),
          omit.stat = c("rsq", "adj.rsq", "f", "ser"),
          covariate.labels = c("Receive-choice-in-one-month"),
          add.lines = list(
            c("Plot Controls", "No", "Yes", "No", "Yes"),
            c("Farmer Controls", "No", "No", "Yes", "Yes"),
            c("Mean dependent variable (receive now group)", y_mean_round, y_mean_round, y_mean_round, y_mean_round)
          ),
          out = tab5_otpt_fp_tex,
          header = FALSE
)

##### 3. FIGURE 5:  Insurance Take-Up by Treatment Group #####
# Sample size
N <- nrow(data)

# Calculate means by treatment groups
t_C1 <- data %>% filter(treatment == "Receive-choice-now") %>% summarise(mean = mean(tookup, na.rm = TRUE)) %>% pull(mean)
t_C2 <- data %>% filter(treatment == "Receive-choice-in-one-month") %>% summarise(mean = mean(tookup, na.rm = TRUE)) %>% pull(mean)

# Run regression and extract standard errors
fig5_reg <- lm(tookup ~ C, data = data)
ses <- sqrt(diag(vcov(fig5_reg)))

se_C <- ses["C"]

# Collapse data by treatment level
collapsed_data <- data %>%
  group_by(treatment) %>%
  summarise(
    mean_tookup = mean(tookup, na.rm = TRUE),
    sd_tookup = sd(tookup, na.rm = TRUE),
    n_tookup = n()
  ) 

# Generate confidence intervals
collapsed_data <- collapsed_data %>%
  mutate(
    ci95 = ifelse(treatment == 1, 1.96 * se_C, 0),
    l = mean_tookup - ci95,
    h = mean_tookup + ci95,
    mean2 = round(mean_tookup + 0.03, 2),
    mean3 = round(mean_tookup, 2)
  )

# Plot the figure
p <- ggplot(collapsed_data, aes(x = factor(treatment), y = mean_tookup, fill = factor(treatment))) +
  geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = l, ymax = h), width = 0.15, color = "black") +
  geom_text(aes(label = mean3, x = as.numeric(factor(treatment)) + ifelse(treatment == 1, 0.075, 0)), 
            vjust = -0.8, size = 5) +
  scale_fill_manual(values = c("#3B5A7A", "#5E7836")) +
  labs(
    title = paste("Insurance Take-Up (N =", as.numeric(N), ")"),
    x = "", y = ""
  ) +
  scale_x_discrete(labels = c("Receive-choice-now", "Receive-choice-in-one-month")) +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.2), limits = c(0, 0.9)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 1)
  )


# Save the figure
ggsave("output/Figures/Figure5.jpg", plot = p, width = 8, height = 6, units = "in", dpi = 300)




