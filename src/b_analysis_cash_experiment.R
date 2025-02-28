###############################################################################
#######---------######## B_ANALYSIS_CASH_EXPERIMENT.R ########---------########
###############################################################################

###-----------### This script replicates Figure 4 and Table 4 ###-----------###
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

data <- read_dta("data/data_cashexp.dta")
skim(data) # No missing values in the data!


##### 2. ESTIMATING TABLE 4: CASH EXPERIMENT TABLE #####

### NO INTERACTIONS
# Perform fixed effects regression
no_int_fe <- feols(tookup ~ B + cash | fid2, data, se = "standard")
no_int_fe_lm <- lm(tookup ~ B + cash + factor(fid2), data = data)

stargazer(no_int_fe_lm, 
          type = "text",
          omit = c("fid2", "Constant"))

# Store values
sum_y <- skim(data %>% filter(A1 == 1) %>% select(tookup))
y_mean_round = sum_y$numeric.mean
# Test joint independence hypothesis
testBvsCash <- linearHypothesis(no_int_fe_lm, "B - cash = 0") %>% filter(!is.na(`Pr(>F)`))
p_val_round <- round(testBvsCash$`Pr(>F)`, 3)

# Add plot controls
no_int_fe_lm_wc <- lm(tookup ~ B + cash + factor(fid2) + l_area + l_yield, data = data)

stargazer(no_int_fe_lm_wc, 
          type = "text",
          omit = c("fid2", "Constant", "l_area", "l_yield"))


### WITH INTERACTIONS
# Perform fixed effects regression
int_fe <- feols(tookup ~ B + cash + cash*B | fid2, data, se = "standard")
int_fe_lm <- lm(tookup ~ B + cash + cash*B + factor(fid2), data = data)

stargazer(int_fe_lm, 
          type = "text",
          omit = c("fid2", "Constant"))

# Store values
sum_y <- skim(data %>% filter(A1 == 1) %>% select(tookup))
y_mean_round = sum_y$numeric.mean
# Test joint independence hypothesis
testBvsCash <- linearHypothesis(int_fe_lm, "B - cash = 0") %>% filter(!is.na(`Pr(>F)`))
p_val_round <- round(testBvsCash$`Pr(>F)`, 3)

# Add plot controls
int_fe_lm_wc <- lm(tookup ~ B + cash + B*cash + factor(fid2) + l_area + l_yield, data = data)

stargazer(int_fe_lm_wc, 
          type = "text",
          omit = c("fid2", "Constant", "l_area", "l_yield"))



### OUTPUT TABLE
tab4_otpt_fp_tex = "output/Tables/Table4.tex"
stargazer(no_int_fe_lm, no_int_fe_lm_wc, int_fe_lm, int_fe_lm_wc,
          type = "text",
          title = "Table 4 — Cash Drop Experiment: Treatment Effects on Take-Up",
          align = TRUE,
          dep.var.labels = "Take-Up",  
          omit = c("fid2", "Constant", "l_area", "l_yield"),
          omit.stat = c("rsq", "adj.rsq", "f", "ser"),
          covariate.labels = c("Pay-at-harvest", "Cash", "Pay-at-harvest × cash"),
          add.lines = list(
            c("Mean dependent variable (pay-up-front group)", y_mean_round, y_mean_round, y_mean_round, y_mean_round),
            c("Plot Controls", "No", "Yes", "No", "Yes"),
            c("p-value: pay at harvest = cash", p_val_round, p_val_round, p_val_round, p_val_round)),
          out = tab4_otpt_fp_tex,
          header = FALSE
          )


##### 3. FIGURE 4: TAKE-UP GRAPH FOR CASH EXPERIMENT #####
N = nrow(data)

# Create Treatment means
t_A1 = as.numeric((data %>% filter(treatment == "A1") %>% summarise(mean = mean(tookup)))[1,1])
t_A2 = as.numeric((data %>% filter(treatment == "A2") %>% summarise(mean = mean(tookup)))[1,1])
t_B1 = as.numeric((data %>% filter(treatment == "B1") %>% summarise(mean = mean(tookup)))[1,1])
t_B1 = as.numeric((data %>% filter(treatment == "B2") %>% summarise(mean = mean(tookup)))[1,1])


#Run regression and save SEs
fig4_reg = lm(tookup ~ A2 + B1 + B2, data = data)
ses = sqrt(diag(vcov(fig4_reg)))

for (i in 2:length(ses)){
  var = names(ses[i])
  assign(paste0("se_", var), as.numeric(ses[i]))
}

# Collapse data by treatment level
collapsed_data <- data %>%
  group_by(treatment) %>%
  summarize(
    mean_tookup = mean(tookup, na.rm = TRUE),  
    sd_tookup = sd(tookup, na.rm = TRUE),      
    n_tookup = n()                             
  )

# Generate treat_num based on treatment
collapsed_data <- collapsed_data %>%
  mutate(
    treat_num = case_when(
      treatment == "A1" ~ 1,
      treatment == "A2" ~ 2,
      treatment == "B1" ~ 3,
      treatment == "B2" ~ 4
    )
  )

# Generate CI bounds
collapsed_data <- collapsed_data %>%
  mutate(
    ci95 = case_when(
      treatment == "A2" ~ 1.96 * se_A2,
      treatment == "B1" ~ 1.96 * se_B1,
      treatment == "B2" ~ 1.96 * se_B2,
      .default = 0
    )
  ) %>%
  mutate(l = mean_tookup - ci95, h = mean_tookup + ce95)

# Prepare 'mean2' and 'mean3' for scatter plot
collapsed_data <- collapsed_data %>%
  mutate(
    mean2 = round(mean_tookup + 0.03, 2),
    mean3 = round(mean_tookup, 2)
  )

# Plot
p <- ggplot(collapsed_data, aes(x = factor(treat_num), y = mean_tookup)) +
  geom_bar(stat = "identity", aes(fill = factor(treat_num)), width = 0.6) +
  geom_errorbar(aes(ymin = l, ymax = h), width = 0.2, color = "gray40") +
  geom_text(aes(label = mean3), vjust = -0.5, size = 4.5) +
  scale_fill_manual(values = c("#ADD8E6", "#4682B4", "#FFA500", "#8B0000")) + 
  labs(
    title = "Insurance take-up (N = 120)",
    x = "", 
    y = "",
    fill = "Treatment"
  ) +
  scale_x_discrete(labels = c("Pay-up-front", "Pay-up-front\n+ cash", 
                              "Pay-at-harvest", "Pay-at-harvest\n+ cash")) +
  scale_y_continuous(breaks = seq(0, 1.2, by = 0.2), limits = c(0, 1.2)) +  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5), 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid.major.y = element_line(color = "gray85"), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

# Save plot
ggsave("output/Figures/Figure4.jpg", plot = p, width = 8, height = 6, units = "in", dpi = 300)
