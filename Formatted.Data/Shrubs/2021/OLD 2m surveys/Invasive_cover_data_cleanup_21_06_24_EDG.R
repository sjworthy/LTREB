setwd("R:/THA Research/Stuble Lab/WorkingWoods/Invasive Cover Surveys/2021")


#Upload current data
inv_cov_21<-read.csv("Invasive_cover_data_6_01_2021.csv")

#Upload tree list used for survey
trees<-read.csv("Invasive_cover_survey_2021_tree_list.csv")

#Get trees surveyed
inv_tre<-as.data.frame(unique(inv_cov_21$Tree))
names(inv_tre)[1]<-"Tree"
names(trees)[1]<-"Tree"
inv_tre$survey<-"Y"

#Merge with tree list
test<-merge(inv_tre, trees, by="Tree", all=T)

#Count number of data entries done for each tree
library(plyr)

x<-count(inv_cov_21, "Tree")


#Data that is  missing or needs fixing (all done manually in excel): 

#Missing 291-- found in data, misenetered as 241
#Missing 243-- resurveyed
#Missing 318, 321, 382, 383-- were on a datasheet that was missed and not entered
#Tree 111 missing entry for a maple leaf viburnum, checked and re-enetered
#38, 114, 117, 122, 241, 926-- accidentally surveyed twice, usually had same covers, if not deleted the one with more conservative values
#412-- had duplicate for S's but one was blank, deleted blank
#927 entered blank (couldn't find in the field) deleted entries