#Shrubs----


#setwd(paste0("C:/Users/djenkins.HOLDEN/",
             #"The Holden Arboretum dba Holden Forests and Gardens/",
             #"Stuble Lab - Documents/Working Woods/"))

library(dplyr)
library(tidyr)

ic2018<-read.csv("./Formatted.Data/Shrubs/2018/All_woody_estimates_2018.csv")
ic2019<-read.csv("./Formatted.Data/Shrubs/2019/All_woody_estimates_2019.csv")
ic2020<-read.csv("./Formatted.Data/Shrubs/2020/Invasive Cover Surveys 2020_11_9_20_EDG.csv")
ic2021<-read.csv("./Formatted.Data/Shrubs/2021/WW_invasive_cover_2021 09 20 21.csv")
ic2022<-read.csv("./Formatted.Data/Shrubs/2022/Invasive cover survey master 2022 092922.csv")
ic2023<-read.csv("./Formatted.Data/Shrubs/2023/Invasive cover survey master 2023 06142023.csv")
ic2024<-read.csv("./Formatted.Data/Shrubs/2024/ww_ic_2024_combined_DJ.csv")

#2018

ic2018<-ic2018%>%
  mutate(Species = case_when(
    Species == "Lonicera" ~ "Honeysuckle",
    Species == "Rose" ~ "Multiflora rose",
    .default = Species
  ))%>%
  select(!X)%>%
  mutate(year = 2018)

#2019

ic2019<-ic2019%>%
  mutate(Species = case_when(
    Species == "Blackberry" ~ "Rubus",
    Species == "Rose" ~ "Multiflora rose",
    .default = Species
  ))%>%
  select(!X)%>%
  mutate(year = 2019)

#2020

ic2020<-ic2020%>%
  mutate(Species = case_when(
    Species == "Privot" ~ "Privet",
    Species == "Multiflora Rose " ~ "Multiflora rose",
    Species == "Mapleleaf Vibernum " ~ "Mapleleaf Viburnum",
    .default = Species
  ))%>%
  select(Tree, Direction, Species, Cover.Class)%>%
  mutate(year = 2020)

#2021

ic2021<-ic2021%>%
  mutate(Species = case_when(
    Species == "Privot" ~ "Privet",
    Species == "Multiflora Rose" ~ "Multiflora rose",
    .default = Species
  ))%>%
  select(Tree, Direction, Species, Cover.Class)%>%
  drop_na()%>%
  mutate(year = 2021)

#2022

ic2022<-ic2022%>%
  mutate(Species = case_when(
    Species == "Privot" ~ "Privet",
    .default = Species
  ))%>%
  select(Tree, Direction, Species, Cover.Class)%>%
  mutate(year = 2022)

#2023

ic2023<-ic2023%>%
  mutate(Species = case_when(
    Species == "Privot" ~ "Privet",
    .default = Species
  ))%>%
  select(Tree, Direction, Species, Cover.Class)%>%
  mutate(year = 2023)

#2024
ic2024<-ic2024%>%
  select(Tree, Direction, Species, Cover.Class)%>%
  mutate(Tree=case_when(Tree == '' ~ NA,
                        .default = Tree),
         year = "2024",
         Species = case_when(
           Species == "bUCKthorn" ~ "Buckthorn",
           Species == "SPicebush" ~ "Spicebush",
           .default = Species),
         Cover.Class=case_when(Species=="Honeysuckle"&Cover.Class==6 ~ 0,
                               .default = Cover.Class
         ))%>%
  fill(Tree, .direction="down")%>%
  filter(Tree != '?' & Tree != '940?') %>% 
  drop_na()

ic_all<-rbind(ic2018, ic2019, ic2020, ic2021, ic2022, ic2023, ic2024)  


tree_list_II<-read.csv("./Formatted.Data/Shrubs/Master Data List_12.10.2019.csv")

tree_list_II$Tree=as.numeric(tree_list_II$Tree)

tree_list<-tree_list_II%>%
  drop_na(Tree)%>%
  select(Tree, Plot_ID, Treatment)

ic_all$Tree<-as.numeric(ic_all$Tree)
ic_all<-ic_all%>%
  mutate(percent = case_when(
    Cover.Class == 0 ~ 0,
    Cover.Class == 1 ~ 2.5,
    Cover.Class == 2 ~ 15,
    Cover.Class == 3 ~ 37.5,
    Cover.Class == 4 ~ 62.5,
    Cover.Class == 5 ~ 85,
    Cover.Class == 6 ~ 97.5),
    Species = case_when(Species == "Mapleleaf Viburnum" ~ "Mapleleaf viburnum",
                        .default = Species))

shrub_covers<-left_join(ic_all, tree_list, by="Tree")

#### Abundance ####
# abundance
abundance.plot = shrub_covers %>% 
  group_by(Plot_ID,Treatment,year) %>% 
  summarize(Count = n(), .groups = "drop") %>% 
  ungroup() %>% 
  drop_na() %>% 
  filter(Treatment %in% c("IC","Control"))


abundance.plot$year = as.numeric(abundance.plot$year)
abundance.plot$scale.Year = scale(abundance.plot$year, center = TRUE, scale = FALSE)
abundance.plot$Treatment = as.factor(abundance.plot$Treatment)

abund.model = glmer.nb(Count~Treatment*scale.Year + (1|Plot_ID), data = abundance.plot)
summary(abund.model)
plot(abund.model)
slopes = emtrends(abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

ggplot(abundance.plot, aes(x = year, y = Count, color = Treatment))+
  geom_smooth(method = "lm")

# making year a factor for plotting
abundance.plot$Year.fact = as.factor(abundance.plot$year)

ggplot(abundance.plot, aes(x = Year.fact, y = Count, color = Treatment))+
  geom_boxplot()

#### Cover ####
cover = shrub_covers %>% 
  group_by(Plot_ID,Treatment,year) %>% 
  summarize(total.cover = sum(percent), .groups = "drop") %>% 
  ungroup() %>% 
  drop_na() %>% 
  filter(Treatment %in% c("IC","Control"))


cover$year = as.numeric(cover$year)
cover$scale.Year = scale(cover$year, center = TRUE, scale = FALSE)
cover$Treatment = as.factor(cover$Treatment)
cover$scale.cover = scale(cover$total.cover, center = TRUE, scale = TRUE)

cover.model = lmer(scale.cover~Treatment*scale.Year + (1|Plot_ID), data = cover)
summary(cover.model)
plot(cover.model)
slopes = emtrends(cover.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

ggplot(abundance.plot, aes(x = year, y = Count, color = Treatment))+
  geom_smooth(method = "lm")

# making year a factor for plotting
abundance.plot$Year.fact = as.factor(abundance.plot$year)

ggplot(abundance.plot, aes(x = Year.fact, y = Count, color = Treatment))+
  geom_boxplot()








shrub_covers<-shrub_covers%>%
  mutate(Species = case_when(
    Species == "Multiflora Rose" | Species == "Rose" | Species == "Multiflora Rose " ~ "Multiflora rose",
    Species == "Mapleleaf Vibernum" ~ "Mapleleaf viburnum",
    .default = Species
  ))

ggshrubs<-shrub_covers%>%
  drop_na(Treatment)%>%
  group_by(Treatment, year, Species)%>%
  summarize(mean=mean(percent, na.rm=T), sd=sd(percent, na.rm=T), se=sd/sqrt(n()))

rose<-ggplot(filter(ggshrubs, Species == "Multiflora rose"), aes(x=year, y=mean))+
  geom_pointrange(aes(ymin=mean-se, ymax=mean+se))+
  geom_line(group=1)+
  facet_wrap(~Treatment)+
  ylab("Mean multiflora rose cover (%)")+
  theme_bw()

buck<-ggplot(filter(ggshrubs, Species == "Buckthorn"), aes(x=year, y=mean))+
  geom_pointrange(aes(ymin=mean-se, ymax=mean+se))+
  geom_line(group=1)+
  ylab("Mean glossy buckthorn cover (%)")+
  facet_wrap(~Treatment)+
  theme_bw()

rubus<-ggplot(filter(ggshrubs, Species == "Rubus"), aes(x=year, y=mean))+
  geom_pointrange(aes(ymin=mean-se, ymax=mean+se))+
  ylab("Mean Rubus spp. cover (%)")+
  geom_line(group=1)+
  facet_wrap(~Treatment)+
  theme_bw()

setwd("C:/Users/djenkins.HOLDEN/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Desktop")
pdf("rose.pdf", width=9, height=7)
rose
dev.off()

pdf("buckthorn.pdf", width=9, height=7)
buck
dev.off()

pdf("rubus.pdf", width=9, height=7)
rubus
dev.off()

Native = c("Spicebush", "Mapleleaf viburnum", "Rubus")
library(stringr)
shrub_covers<-shrub_covers%>%
  mutate(status = case_when(str_detect(Plot_ID, "19") & (year==2018 | year == 2019) ~ "Before Removal",
                         str_detect(Plot_ID, "18") & (year==2018) ~ "Before Removal",
                         .default = "After Removal"),
         Origin = case_when(Species %in% Native ~ "native",
                            .default = "non-native"))

test<-shrub_covers%>%
  group_by(Tree, Direction, Origin, year, Treatment, Plot_ID, status)%>%
  summarize(sum_cover=sum(percent))

anova(lm(sum_cover~status+Treatment, data=filter(test, Origin == "non-native")))
anova(lm(sum_cover~status+Treatment, data=filter(test, Origin == "native")))

ggshrubs<-test%>%
  drop_na()%>%
  group_by(Origin, status, Treatment)%>%
  summarize(mean=mean(sum_cover, na.rm=T), sd=sd(sum_cover, na.rm=T), se=sd/sqrt(n()))
library(ggplot2)
library(forcats)
ggshrubs$status<-fct_relevel(ggshrubs$status, c("Before Removal", "After Removal"))

plot<-ggplot(ggshrubs, aes(x=status, y=mean))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width =.1)+
  facet_grid(Origin~Treatment, scales="free_y")+
  theme_bw()+
  ylab(expression("Mean shrub cover " %+-% "SE"))+
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none",
        strip.text=element_text(size=10, face="bold", color="white"),
        strip.background = element_rect(fill="black"),
        axis.title.y=element_text(size=10, face = "bold"),
        axis.text.x=element_text(size=9, color="black"),
        axis.text.y = element_text(size = 10,color="black"),
        axis.title.x=element_blank())

setwd("C:/Users/djenkins.HOLDEN/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Desktop")
pdf("ltreb_shrubs.pdf", width=9, height=7)
plot
dev.off()

#redo without 0's in non natives

ggshrubs2<-test%>%
  filter(!(Origin == "non-native" & sum_cover == 0))%>%
  drop_na()%>%
  group_by(Origin, status, Treatment)%>%
  summarize(mean=mean(sum_cover, na.rm=T), sd=sd(sum_cover, na.rm=T), se=sd/sqrt(n()))

ggshrubs2$status<-fct_relevel(ggshrubs2$status, c("Before Removal", "After Removal"))

plot2<-ggplot(ggshrubs2, aes(x=status, y=mean))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width =.1)+
  facet_grid(Origin~Treatment, scales="free_y")+
  theme_bw()+
  ylab(expression("Mean shrub cover " %+-% "SE"))+
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none",
        strip.text=element_text(size=10, face="bold", color="white"),
        strip.background = element_rect(fill="black"),
        axis.title.y=element_text(size=10, face = "bold"),
        axis.text.x=element_text(size=9, color="black"),
        axis.text.y = element_text(size = 10,color="black"),
        axis.title.x=element_blank())

setwd("C:/Users/djenkins.HOLDEN/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Desktop")
pdf("ltreb_shrubs2.pdf", width=9, height=7)
plot2
dev.off()

test2$status<-fct_relevel(test2$status, c("Before Removal", "After Removal"))
test$year<-as.numeric(test$year)
test2<-test%>%
  drop_na()%>%
  mutate(yom = case_when(str_detect(Plot_ID, "18") == T ~ year-2018,
                        str_detect(Plot_ID, "19") == T ~ year-2019))%>%
  mutate(yom=case_when(yom==-1 ~ 0,
                       .default = yom))

years=c(2018,2019,2024)

plot3<-ggplot(init_final, aes(x=status, y=sum_cover))+
  geom_violin(color="blue")+
  geom_jitter()+
  facet_grid(Origin~Treatment, scales="free_y")+
  scale_x_discrete(labels=c("Before treatment", "After treatment"))+
  theme_bw()+
  ylab(expression("Total shrub cover %"))+
  #xlab("Years since treatment")+
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none",
        strip.text=element_text(size=10, face="bold", color="white"),
        strip.background = element_rect(fill="black"),
        axis.title.y=element_text(size=10, face = "bold"),
        axis.text.x=element_text(size=9, color="black"),
        axis.text.y = element_text(size = 10,color="black"))

setwd("C:/Users/djenkins.HOLDEN/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Desktop")
pdf("ltreb_shrubs5.pdf", width=10, height=7)
plot3
dev.off()

init_final<-filter(test2, (str_detect(Plot_ID, "18") == T & (year==2018|year==2024))|
                    str_detect(Plot_ID, "19") == T & (year==2019|year==2024))

#Seedlings----

setwd("C:/Users/djenkins.HOLDEN/The Holden Arboretum dba Holden Forests and Gardens/Stuble Lab - Documents/Grants/NSF_LTREB_2025/Datasets")

raw<-read.csv("WW_Seedlings.csv")

library(dplyr)

soi<-raw%>%
  filter(Species %in% c("ACSA", "ACRU", "LITU", "QURU"))

#Recruits through time
 recruits<-soi%>%   
  group_by(Tree, Band, Species, Treatment)%>%   
  filter(Year_Period == min(Year_Period) & Year !=2019)%>%   
  ungroup()%>%group_by(Species, Year, Treatment)%>%
  summarize(n=n())

r_list<-soi%>%
  group_by(Tree, Band, Species)%>%
  filter(Year_Period == min(Year_Period) & Year !=2019)

r_list<-r_list$Band

library(ggplot2)
library(ggsci)

add<-data.frame(Species = c("ACSA", "QURU"), Year=c(2023, 2024), n=c(0,0))
recruits<-rbind(recruits,add)

r<-ggplot(recruits, aes(x=Species, y=n, fill=factor(Year)))+
  facet_wrap(~Treatment)+
  geom_col(color="black", position="dodge")+
  theme_bw()+
  theme(legend.position="bottom")+
  scale_fill_npg()+
  ylab("Number of recruits")+
  xlab("Cohort")

pdf("recruits_time_flipped.pdf", width=10, height=7)
r
dev.off()

#survival in first year

survival<-soi%>%
  filter(Band %in% r_list)%>%
  select(Band, Species, Year, Banded_Year, Death_Year, Treatment)%>%
  mutate(dead_first=case_when(is.na(Death_Year)==F & Death_Year-Banded_Year<=1 ~ 1,
                              .default = 0))

prop_dead<-survival%>%
  distinct(Band, .keep_all = T)%>%
  group_by(Species, Year, Treatment)%>%
  summarize(prop=sum(dead_first)/n())%>%
  filter(Year !=2019)%>%
  mutate(surv=1-prop)

s<-ggplot(prop_dead, aes(x=Species, y=surv, fill=factor(Year)))+
  facet_wrap(~Treatment)+
  geom_col(color="black", position="dodge")+
  theme_bw()+
  theme(legend.position="bottom")+
  scale_fill_npg()+
  ylab("Proportion surviving first year")+
  xlab("Cohort")

pdf("cohort_survival.pdf", width=10, height=7)
s
dev.off()

library(ggpubr)

pdf("seedling_cohorts.pdf", width=8.5, height=11)
ggarrange(r,s,nrow=2, common.legend = T, legend="bottom")
dev.off()

rec_25<-read.csv("WW_Seedlings_2025_new_recruits.csv")

tree_list<-raw%>%
  select(Tree, Treatment)%>%
  distinct()

r25<-rec_25%>%
  filter(Species %in% c("ACSA", "ACRU", "LITU", "QURU"))

r25<-left_join(r25, tree_list, by="Tree")%>%
  mutate(Year = 2025)%>%
  tidyr::drop_na(Treatment)%>%
  group_by(Species, Year, Treatment)%>%
  summarize(n=n())

recruits<-rbind(recruits, r25)

r<-ggplot(recruits, aes(x=Year, y=n, fill=Species))+
  facet_wrap(~Treatment)+
  geom_col(color="black", position="dodge")+
  theme_bw()+
  theme(legend.position="bottom")+
  scale_fill_npg()+
  ylab("Number of recruits")+
  xlab("Cohort")

r2<-ggplot(recruits, aes(x=Species, y=n, fill=factor(Year)))+
  facet_wrap(~Treatment)+
  geom_col(color="black", position="dodge")+
  theme_bw()+
  theme(legend.position="bottom")+
  scale_fill_npg()+
  ylab("Number of recruits")+
  xlab("Species")

pdf("recruits_time.pdf", width=10, height=7)
r
dev.off()

pdf("recruits_time_flipped.pdf", width=10, height=7)
r2
dev.off()
