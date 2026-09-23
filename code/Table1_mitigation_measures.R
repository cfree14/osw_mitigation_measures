

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
tabledir <- "tables"
plotdir <- "figures"

# Read data
actions_orig <- readxl::read_excel("data/actions/action_database.xlsx", sheet=1)


# Prep data
################################################################################

actions <- actions_orig %>% 
  # Remove ignore
  filter(is.na(ignore_yn)) %>% 
  # Id unique
  select(strategy, measure) %>% 
  unique() %>% 
  # Sort
  arrange(strategy, measure)

# Number of measures
nrow(actions)

# Number of strategies
n_distinct(actions$strategy)


write.csv(actions, file=file.path(tabledir, "Table1_mitigation_measures.csv"), row.names=F)
