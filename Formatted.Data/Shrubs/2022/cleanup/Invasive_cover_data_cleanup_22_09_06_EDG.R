setwd("C:/Users/edawson-glass/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Working Woods/Invasive Cover Surveys/2022/raw entered data 2022")

#Upload current data
sg<-read.csv("Cover_2022 SG.csv", fileEncoding="UTF-8-BOM")
lj<-read.csv("invasive_cover_2022 LJ.csv", fileEncoding="UTF-8-BOM")
lj$Notes<-NULL
invcov22<-rbind(sg, lj)
#this line is for removing any completely blank entries
invcov22<-invcov22[complete.cases(invcov22$Cover.Class), ]

setwd("C:/Users/edawson-glass/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Working Woods/Invasive Cover Surveys/2022/cleanup")

#Upload tree list used for survey-- we'll use this to match up what should have been surveyed/see if any tree #'s were misentered
trees<-read.csv("Invasive_cover_survey_2021_tree_list.csv", fileEncoding="UTF-8-BOM")
ml<-read.csv("Master_list_2021 12 01 21.csv")
names(ml)
mlinv<- ml[,c(1:4, 12, 43)]

#Get trees surveyed
inv_tre<-as.data.frame(unique(invcov22$Tree))
names(inv_tre)[1]<-"Tree"
names(trees)[1]<-"Tree"
inv_tre$survey<-"Y"

#Merge with tree list
test<-merge(inv_tre, mlinv, by="Tree", all=T)

#no misentries but some trees missing
test2<-test[!complete.cases(test$survey), ]
test2<-subset(test2, VisualCover2021=="Yes")
test2$Tree<-as.numeric(test2$Tree)
#missing trees in below list:
test2
#will see if we have, if not will go collect

#Count number of data entries done for each tree-- should have perfectly 14 for each tree (for N S survey of each target species)
library(plyr)


x<-count(invcov22, "Tree")
#a few trees have duplicate entries, will remove duplicates



#one missing tree was misentered and came out as a duplicate, fix
invcov22$Tree[invcov22$Tree==71&invcov22$Date=="26-May"]<-77


#trees that need resurvey 092322
write.csv(test2, "inv_cov_missing_trees_092322.csv")



#make sure have right number of entries for N and S
table(invcov22$Species)#have 1 extra for N but guessing that's a minor misentry issue
#_____________________________________________________________________________

#add in missing data
inv_missing<-read.csv("WW_missing_trees_invasive_cover_2022.csv", fileEncoding="UTF-8-BOM")
inv_missing$Notes<-NULL

#combine data
invcov22_1<-rbind(invcov22, inv_missing)


#Get trees surveyed
inv_tre<-as.data.frame(unique(invcov22_1$Tree))
names(inv_tre)[1]<-"Tree"
names(trees)[1]<-"Tree"
inv_tre$survey<-"Y"

#Merge with tree list
test<-merge(inv_tre, mlinv, by="Tree", all=T)

#no misentries but some trees missing
test2<-test[!complete.cases(test$survey), ]
test2<-subset(test2, VisualCover2021=="Yes")
test2$Tree<-as.numeric(test2$Tree)
#missing trees in below list:
test2

#missing tree 358 which could not be found


invcov22_1$Species[invcov22_1$Species=="Mapleleaf Viburnum"]<-"Mapleleaf viburnum"
table(invcov22_1$Species)#looks good
table(invcov22_1$Direction)

#one entry is missing a direction 
invcov22_1$Direction[invcov22_1$Direction==""]<-"S"

#data looks good!!

write.csv(invcov22_1, "Invasive cover survey master 2022 092922.csv")
