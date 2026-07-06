

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
  filter(is.na(ignore_yn)) %>% 
  select(strategy, measure) %>% 
  unique()

write.csv(actions, file=file.path(tabledir, "Table1_mitigation_measures.csv"), row.names=F)
