setwd("R:/THA Research/Stuble Lab/WorkingWoods/Invasive Cover Surveys/2021")


#Upload current data
inv_cov_21<-read.csv("WorkingWoods_InvasiveCoverClass_9152021.csv", fileEncoding="UTF-8-BOM")
#this line is for removing any completely blank entries
inv_cov_21<-inv_cov_21[complete.cases(inv_cov_21$Tree), ]

#Upload tree list used for survey-- we'll use this to match up what should have been surveyed/see if any tree #'s were misentered
trees<-read.csv("Invasive_cover_survey_2021_tree_list.csv", fileEncoding="UTF-8-BOM")


#Get trees surveyed
inv_tre<-as.data.frame(unique(inv_cov_21$Tree))
names(inv_tre)[1]<-"Tree"
names(trees)[1]<-"Tree"
inv_tre$survey<-"Y"

#Merge with tree list
test<-merge(inv_tre, trees, by="Tree", all=T)


#we have an extra tree-- 298, which doesn't exist. Am guessing it was misentered (maybe should be 268, which is missing?)
#also missing severl trees, most of which are "not valid." Valid trees that are missing are: 205, 914, 139, 79



#Count number of data entries done for each tree-- should have perfectly 14 for each tree (for N S survey of each target species)
library(plyr)

x<-count(inv_cov_21, "Tree")

#Several trees with mismatches:
#-trees 278, 298, and 308 only have 7 entries (perhapse 298 should be the other half of 278?)
#196, 349 have 42 entries
#14, 76, 105, 188, 203, 246 have 28 entries
#249, 380 have 21 entries

#checking these; manually enterding and editing excel file with corrections:
#308-- missing S, found, entered manually
#298 actually the S of 278
#79 mistakenly entered as 76 (which was a duplicate value)
#914 accidentally entered as 14 (9 looked weird so probably thought it was a mistake)
#205 misentered as 203
#105, 249, 246 and 188 196 and 349 all on same pages, probably just duplicate entered by mistake, deleted 1 of each
#308 S misentered as 380


#checking again
#Upload current data
inv_cov_21<-read.csv("WW_invasive_cover_2021 09 17 21.csv", fileEncoding="UTF-8-BOM")
inv_cov_21<-inv_cov_21[complete.cases(inv_cov_21$Tree), ]

#Get trees surveyed
inv_tre<-as.data.frame(unique(inv_cov_21$Tree))
names(inv_tre)[1]<-"Tree"
names(trees)[1]<-"Tree"
inv_tre$survey<-"Y"

#Merge with tree list
test<-merge(inv_tre, trees, by="Tree", all=T)
#count
x<-count(inv_cov_21, "Tree")

#everything now has the right # of entries, only valid tree missing is #139-- double checking the data then will mark as not done
#indeed, 139 still missing-- going to recollect


