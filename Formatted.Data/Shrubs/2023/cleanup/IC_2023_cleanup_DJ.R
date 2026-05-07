################################################################################
#invasive cover cleanup 2023                                                   #
#david jenkins                                                                 #
#06/13/2023                                                                    #
################################################################################

require(dplyr)

setwd("C:/Users/djenkins/The Holden Arboretum dba Holden Forests and Gardens/Stuble Lab - Documents/Working Woods/Invasive Cover Surveys/2023/cleanup")

AC_518<-read.csv("051823_AC.csv")
LL_522<-read.csv("052223_LL.csv")
AC_525<-read.csv("052523_AC.csv")
IC_Un<-read.csv("IC_Un.csv")
tree_list<-read.csv("tree list 2023.csv")

#remove extraneous NAs at bottom of data and combine into single df

AC_518<-AC_518%>%
  filter(is.na(Tree) == F)
AC_525<-AC_525%>%
  filter(is.na(Tree) == F)
LL_522<-LL_522%>%
  filter(is.na(Tree) == F)
IC_Un<-IC_Un%>%
  filter(is.na(Tree) == F)

combined_data<-rbind(AC_518, AC_525, LL_522, IC_Un)

#count observations for each tree/direction group (should be 7 for each)
#create df of trees with missing or extra obs

check<-combined_data%>%
  group_by(Tree, Direction)%>%
  summarize(n=n())%>%
  filter(n != 7 & is.na(Tree) == F)

#check for missing trees

missing<-full_join(combined_data, tree_list, by = "Tree")
missing<-missing%>%
  filter(is.na(Species.x)==T)

cleaned<-unique(combined_data) #remove duplicates

check<-cleaned%>%
  group_by(Tree, Direction)%>%
  summarize(n=n())%>%
  filter(n != 7 & is.na(Tree) == F)

cleaned<-cleaned[-2720,]#this is clumsy and bad

check<-cleaned%>%
  group_by(Tree, Direction)%>%
  summarize(n=n())%>%
  filter(n != 7 & is.na(Tree) == F)

#remove incorrect extra values for tree 322

cleaned%>%
  filter(!((Tree==322 & Species == "Multiflora rose" & Direction == 'N' & Cover.Class == 0) |
           (Tree==322 & Species == "Rubus" & Direction == 'S' & Cover.Class == 0)))->cleaned

check<-cleaned%>%
  group_by(Tree, Direction)%>%
  summarize(n=n())%>%
  filter(n != 7 & is.na(Tree) == F)

#tree 345 entered as 395

to_add<-data.frame(matrix(ncol=5, nrow=0))
colnames(to_add)=c('Tree', 'Species', 'Direction', 'Cover.Class', 'Notes')

to_add<-rbind(to_add, filter(cleaned, Tree == 395 & is.na(Notes) == F))
to_add<-mutate(to_add, Tree = 345) #will continually update this df and append at end

cleaned<-cleaned%>%
  filter(!(Tree == 395 & is.na(Notes) == F))

check<-cleaned%>%
  group_by(Tree, Direction)%>%
  summarize(n=n())%>%
  filter(n != 7 & is.na(Tree) == F)

#tree 325 is missing entry for Mapleleaf Viburnum south

to_add<-rbind(to_add, c(325, 'Mapleleaf Viburnum', 'S', 0, ''))
check<-check[-28,]#see previous note about clumsiness

#create list of trees that are missing or were partially entered
missing<-missing[-1,]#:(

to_find<-data.frame(c(missing$Tree, check$Tree))

#add missing data, the rest will be entered in a separate file
cleaned<-rbind(cleaned, to_add)

check<-cleaned%>%
  group_by(Tree, Direction)%>%
  summarize(n=n())%>%
  filter(n != 7 & is.na(Tree) == F)

#remove trees with only 1 entry so they can be added in the separate file

cleaned<-cleaned%>%
  filter(!(Tree %in% check$Tree))
cleaned$Tree<-as.numeric(cleaned$Tree) #makes sorting easier

#read file of missing data and enter it
missing<-read.csv("missing.csv")

cleaned<-rbind(cleaned, missing)

check<-cleaned%>%
  group_by(Tree, Direction)%>%
  summarize(n=n())%>%
  filter(n != 7) #yay!

missing2<-full_join(cleaned, tree_list, by = "Tree") #recheck for missing trees

#trees in list, not in data
not_in_data<-missing2%>%
  filter(is.na(Species.x)==T)%>%
  distinct(Tree)%>%
  filter(Tree != 85 & Tree != 121) 

#trees in data, not in tree list
not_in_list<-missing2%>%
  filter(is.na(Species.y)==T)%>%
  distinct(Tree)

cleaned<-cleaned%>%
  mutate(Tree = case_when(
    Tree == 838 ~ 888,
    .default = Tree))


cleaned<-rbind(cleaned, read.csv("missing2.csv"))

missing2<-full_join(cleaned, tree_list, by = "Tree") #recheck for missing trees

#trees in list, not in data
not_in_data<-missing2%>%
  filter(is.na(Species.x)==T)%>%
  distinct(Tree)%>%
  filter(Tree != 85 & Tree != 121) 

#trees in data, not in tree list
not_in_list<-missing2%>%
  filter(is.na(Species.y)==T)%>%
  distinct(Tree)

#trees 899 and 560 do not exist. trees 884 and 411 still missing. missing datasheet
#for 899 and 560 a well as 884 and 411.

write.csv(cleaned, file="Invasive cover survey master 2023 06142023.csv", 
          row.names = F)
