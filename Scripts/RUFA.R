library(tidyverse)

# Reading in RUFA data
rufa = read.csv("./Raw.Data/HA_RUFA_Research.csv")

table(rufa$Assessment.Year)

