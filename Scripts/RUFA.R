library(tidyverse)

# Reading in RUFA data
rufa = read.csv("./Raw.Data/HA_RUFA_Research.csv")

table(rufa$Assessment.Year)

# New Baldwin RUFA data

bald.2018 = read.csv("./Formatted.Data/Baldwin_WCS_RUFA_2018.csv")
bald.2025 = read.csv("./Formatted.Data/Baldwin_WCS_RUFA_2025.csv")

# merge

bald.2018$Year = "2018"
bald.2025$Year = "2025"

all.bald = full_join(bald.2018,bald.2025)

# partition by R_Pre_TREA

t.test(all.bald$HERB_TOTAL~all.bald$Year)
# lower total herbaceous species
t.test(all.bald$SEEDL_TOTA~all.bald$Year)
# same total seedlings species
t.test(all.bald$SS_IND_TOT~all.bald$Year)
# significant increase in the total number of shrub and sapling individuals
t.test(all.bald$SS_SPC_TOT~all.bald$Year)
# significant increase in the total number of shrub and sapling species
t.test(all.bald$M_TREE_IND~all.bald$Year)
# same total number of mature tree individuals
t.test(all.bald$M_TREE_S_1~all.bald$Year)
# same total number of mature tree species
t.test(all.bald$INV_SV_TOT~all.bald$Year)
# decrease in total invasive shrub vine individuals
t.test(all.bald$INV_HERB_T~all.bald$Year)
# decrease in total invasive herbaceous individuals
table(all.bald$BLD,all.bald$Year)
table(all.bald$BLD_SEVER,all.bald$Year)



