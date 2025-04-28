# Required libraries
library(dplyr)
library(MASS)
library(sjPlot)

# Read data
data1 <- read.csv("250428_Data_all.csv")

# Remove \"Don't know\" responses (== NA)
data2 <- data1 %>%
  dplyr::filter(!is.na(pr_level_mean))

# Multiple linear regression
model.result <- lm(pr_level_mean ~ 
  s_NR +                   # Nature relatedness
  s_conservation +         # Conservation attitude
  s_dread +                 # Risk perception: dread
  knowledge_dna +           # Knowledge: molecular biology
  knowledge_biochem +       # Knowledge: biochemistry
  knowledge_ecol +          # Knowledge: ecology
  knowledge_chem +          # Knowledge: chemistry
  species_id_ability +      # Species identification ability
  gender +                  # Gender
  age +                     # Age
  education +               # Education
  income,                   # Income
  data = data2)

# Show regression results (with standardized coefficients and standard errors)
sjPlot::tab_model(
  model.result,
  show.std = TRUE,          # Show standardized coefficients
  string.p = "p value",      # Rename p-value column
  show.ci = NULL,            # Do not show confidence intervals
  show.se = TRUE,            # Show standard errors
  string.se = "SE"           # Rename SE column
)