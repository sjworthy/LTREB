library(tidyverse)

# working with Mike's Natural Areas BLD survey data

canopy = read.csv("./Formatted.Data/NatAreas_BLD_Canopy.csv")

# mean dbh by year
canopy.mean.dbh = canopy %>%
  filter(DBH > 0) %>% 
  group_by(Year) %>% 
  summarize(mean.growth = mean(DBH, na.rm = TRUE),
            sd.growth = sd(DBH, na.rm = TRUE))

# slim down to season

fall = canopy %>% 
  filter(Season == "F")
spring = canopy %>% 
  filter(Season == "S")

# make the fall data wide format
fall.wide = pivot_wider(data = fall, 
                        id_cols = c(Canopy_Tag_ID),
                        names_from = Year,
                        values_from = DBH,
                        names_prefix = "DBH_")

# start with 2017 since 2016 is all NAs

fall.wide$growth = fall.wide$DBH_2024 - fall.wide$DBH_2017

# fix a 0 that should be an NA

fall.wide[15,9] = NA

fall.wide$log.2017 = log(fall.wide$DBH_2017)
fall.wide$log.2018 = log(fall.wide$DBH_2018)
fall.wide$log.2019 = log(fall.wide$DBH_2019)
fall.wide$log.2020 = log(fall.wide$DBH_2020)
fall.wide$log.2021 = log(fall.wide$DBH_2021)
fall.wide$log.2022 = log(fall.wide$DBH_2022)
fall.wide$log.2023 = log(fall.wide$DBH_2023)
fall.wide$log.2024 = log(fall.wide$DBH_2024)

fall.wide$RGR.17.18 = (fall.wide$log.2018-fall.wide$log.2017)/1
fall.wide$RGR.18.19 = (fall.wide$log.2019-fall.wide$log.2018)/1
fall.wide$RGR.19.20 = (fall.wide$log.2020-fall.wide$log.2019)/1
fall.wide$RGR.20.21 = (fall.wide$log.2021-fall.wide$log.2020)/1
fall.wide$RGR.21.22 = (fall.wide$log.2022-fall.wide$log.2021)/1
fall.wide$RGR.22.23 = (fall.wide$log.2023-fall.wide$log.2022)/1
fall.wide$RGR.23.24 = (fall.wide$log.2024-fall.wide$log.2023)/1

# convert data to long format
fall.long = pivot_longer(data = fall.wide, cols = 22:28, 
                             names_to = "Period", values_to = "RGR")

fall.long$RGR[is.na(fall.long$RGR)] <- 0

# means
fall.rgr.means = fall.long %>% 
  group_by(Period) %>% 
  summarise(mean.rgr = mean(RGR,na.rm = TRUE),
            sd.rgr = sd(RGR,na.rm = TRUE))

ggplot(fall.long, aes(x = Period, y = RGR))+
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15)+
  labs(y = "Relative Growth Rate", title = "RGR over time from 2017-2024 Fall")

ggsave("Plots/NatAreas.BLD.RGR.Fall.png", height = 7, width = 9)


ggplot(fall.long, aes(x = Period, y = RGR)) +
  geom_line(aes(group = Canopy_Tag_ID), alpha = 0.3) +
  geom_line(data = fall.rgr.means, aes(y = mean.rgr, group = 1), color = "red", size = 1) +
  geom_line(data = fall.rgr.means, aes(y = mean.rgr + sd.rgr, group = 1), color = "red", linetype = "dashed") +
  geom_line(data = fall.rgr.means, aes(y = mean.rgr - sd.rgr, group = 1), color = "red", linetype = "dashed") +
  theme_classic(base_size = 15) +
  labs(y = "Relative Growth Rate (RGR)", x = "Census Period", 
       title = "RGR over time with mean ± SD for NatAreas Canopy")

# make the fall data wide format
fall.wide.BLD = pivot_wider(data = fall, 
                        id_cols = c(Canopy_Tag_ID),
                        names_from = Year,
                        values_from = Percent_Class,
                        names_prefix = "Percent_Class")

fall.BLD.mean = fall %>%
  group_by(Year) %>% 
  summarise(mean.BLD = mean(Percent_Class, na.rm = TRUE),
            sd.BLD = sd(Percent_Class, na.rm = TRUE))

ggplot(fall, aes(x = Year, y = Percent_Class, group = Year)) +
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15) +
  labs(y = "Percent Class\nlower value = fewer leaves symptomatic", x = "Year")

ggsave("Plots/NatAreas.BLD.Class.Fall.png", height = 7, width = 7)

spring.BLD.mean = spring %>%
  group_by(Year) %>% 
  summarise(mean.BLD = mean(Percent_Class, na.rm = TRUE),
            sd.BLD = sd(Percent_Class, na.rm = TRUE))

ggplot(spring, aes(x = Year, y = Percent_Class, group = Year)) +
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15) +
  labs(y = "Percent Class\nlower value = fewer leaves symptomatic", x = "Year")

ggsave("Plots/NatAreas.BLD.Class.Spring.png", height = 7, width = 7)


fall.cover.mean = fall %>%
  group_by(Year) %>% 
  summarise(mean.BLD = mean(Canopy.Cover, na.rm = TRUE),
            sd.BLD = sd(Canopy.Cover, na.rm = TRUE))

ggplot(fall, aes(x = Year, y = Canopy.Cover, group = Year)) +
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15) +
  labs(y = "Canopy Cover\nlower value = less canopy remaining", x = "Year")

ggsave("Plots/NatAreas.BLD.Canopy.Cover.Fall.png", height = 7, width = 7)

spring.cover.mean = spring %>%
  group_by(Year) %>% 
  summarise(mean.BLD = mean(Canopy.Cover, na.rm = TRUE),
            sd.BLD = sd(Canopy.Cover, na.rm = TRUE))

ggplot(spring, aes(x = Year, y = Canopy.Cover, group = Year)) +
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15) +
  labs(y = "Canopy Cover\nlower value = less canopy remaining", x = "Year")

ggsave("Plots/NatAreas.BLD.Canopy.Cover.Spring.png", height = 7, width = 7)

#### Small Trees ####

small = read.csv("./Formatted.Data/NatAreas_BLD_Small_Tree.csv")

fall.small = small %>% 
  filter(Season == "F")
spring.small = small %>% 
  filter(Season == "S")

fall.BLD.mean.small = fall.small %>%
  group_by(Year) %>% 
  summarise(mean.BLD = mean(Percent_Class, na.rm = TRUE),
            sd.BLD = sd(Percent_Class, na.rm = TRUE))

ggplot(fall.small, aes(x = Year, y = Percent_Class, group = Year)) +
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15) +
  labs(y = "Percent Class\nlower value = fewer leaves symptomatic", x = "Year",
       title = "Small Trees: 2-5m")

ggsave("Plots/NatAreas.BLD.Class.Small.Tree.Fall.png", height = 7, width = 7)

spring.BLD.mean.small = spring.small %>%
  group_by(Year) %>% 
  summarise(mean.BLD = mean(Percent_Class, na.rm = TRUE),
            sd.BLD = sd(Percent_Class, na.rm = TRUE))

ggplot(spring.small, aes(x = Year, y = Percent_Class, group = Year)) +
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15) +
  labs(y = "Percent Class\nlower value = fewer leaves symptomatic", x = "Year",
       title = "Small Trees: 2-5m")

ggsave("Plots/NatAreas.BLD.Class.Small.Tree.Spring.png", height = 7, width = 7)


fall.cover.mean = fall %>%
  group_by(Year) %>% 
  summarise(mean.BLD = mean(Canopy.Cover, na.rm = TRUE),
            sd.BLD = sd(Canopy.Cover, na.rm = TRUE))

ggplot(fall, aes(x = Year, y = Canopy.Cover, group = Year)) +
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15) +
  labs(y = "Canopy Cover\nlower value = less canopy remaining", x = "Year")

ggsave("Plots/NatAreas.BLD.Canopy.Cover.Fall.png", height = 7, width = 7)

spring.cover.mean = spring %>%
  group_by(Year) %>% 
  summarise(mean.BLD = mean(Canopy.Cover, na.rm = TRUE),
            sd.BLD = sd(Canopy.Cover, na.rm = TRUE))

ggplot(spring, aes(x = Year, y = Canopy.Cover, group = Year)) +
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15) +
  labs(y = "Canopy Cover\nlower value = less canopy remaining", x = "Year")

ggsave("Plots/NatAreas.BLD.Canopy.Cover.Spring.png", height = 7, width = 7)

#### Merge Small Tree and Canopy Data ####

canopy = read.csv("./Formatted.Data/NatAreas_BLD_Canopy.csv") %>% 
  select(Canopy_Tag_ID, Year,Season,Canopy.Cover,Percent_Class,Alive)
small = read.csv("./Formatted.Data/NatAreas_BLD_Small_Tree.csv") %>% 
  select(SmallTree_Tag_ID, Year,Season,Canopy.Cover,Percent_Class,Alive)

canopy <- canopy %>%
  rename(Tag_ID = Canopy_Tag_ID)

small <- small %>%
  rename(Tag_ID = SmallTree_Tag_ID)

combined <- bind_rows(canopy, small)

fall = combined %>% 
  filter(Season == "F")
spring = combined %>% 
  filter(Season == "S")

fall.BLD.mean = fall %>%
  group_by(Year) %>% 
  summarise(mean.BLD = mean(Percent_Class, na.rm = TRUE),
            sd.BLD = sd(Percent_Class, na.rm = TRUE))

ggplot(fall, aes(x = Year, y = Percent_Class, group = Year)) +
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15) +
  labs(y = "Percent Class\nlower value = fewer leaves symptomatic", x = "Year",
       title = "Canopy and Small Trees")

ggsave("Plots/NatAreas.BLD.Class.Canopy.Small.Trees.Fall.png", height = 7, width = 7)

spring.BLD.mean = spring %>%
  group_by(Year) %>% 
  summarise(mean.BLD = mean(Percent_Class, na.rm = TRUE),
            sd.BLD = sd(Percent_Class, na.rm = TRUE))

ggplot(spring, aes(x = Year, y = Percent_Class, group = Year)) +
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15) +
  labs(y = "Percent Class\nlower value = fewer leaves symptomatic", x = "Year",
       title = "Canopy and Small Trees")

ggsave("Plots/NatAreas.BLD.Class.Canopy.Small.Tree.Spring.png", height = 7, width = 7)

fall.cover.mean = fall %>%
  group_by(Year) %>% 
  summarise(mean.BLD = mean(Canopy.Cover, na.rm = TRUE),
            sd.BLD = sd(Canopy.Cover, na.rm = TRUE))

ggplot(fall, aes(x = Year, y = Canopy.Cover, group = Year)) +
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15) +
  labs(y = "Canopy Cover\nlower value = less canopy remaining", x = "Year",
       title = "Canopy and Small Trees")

ggsave("Plots/NatAreas.BLD.Canopy.Cover.Canopy.Small.Tree.Fall.png", height = 7, width = 7)

spring.cover.mean = spring %>%
  group_by(Year) %>% 
  summarise(mean.BLD = mean(Canopy.Cover, na.rm = TRUE),
            sd.BLD = sd(Canopy.Cover, na.rm = TRUE))

ggplot(spring, aes(x = Year, y = Canopy.Cover, group = Year)) +
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15) +
  labs(y = "Canopy Cover\nlower value = less canopy remaining", x = "Year",
       title="Canopy and Small Trees")

ggsave("Plots/NatAreas.BLD.Canopy.Cover.Canopy.Small.Tree.Spring.png", height = 7, width = 7)

# mortality

combined$Alive[combined$Alive == "y"] <- "Y"
combined$Alive[combined$Alive == "n"] <- "N"

mort.table.fall = combined %>%
  filter(Alive %in% c("Y","N")) %>% 
  filter(Season == "F") %>% 
  count(Year, Alive)

mort.table.fall.wide = pivot_wider(mort.table.fall,
                              id_cols = Year,
                              names_from = Alive,
                              values_from = n,
                              names_prefix = "Status_")

mort.table.fall.wide[1,3] = 0
mort.table.fall.wide$Status_Y = as.numeric(mort.table.fall.wide$Status_Y)
mort.table.fall.wide$Status_N = as.numeric(mort.table.fall.wide$Status_N)
mort.table.fall.wide$Total = mort.table.fall.wide$Status_Y + mort.table.fall.wide$Status_N
mort.table.fall.wide$Mortality.Rate = (mort.table.fall.wide$Status_N/mort.table.fall.wide$Total)*100

ggplot(mort.table.fall.wide, aes(x = factor(Year), y = Mortality.Rate))+
  geom_line(aes(group = 1))+
  theme_classic(base_size = 15) +
  labs(y = "Mortality Rate (%)", x = "Year",
       title="Canopy and Small Trees")

ggsave("Plots/NatAreas.BLD.Mortality.Rate.Canopy.Small.Tree.Fall.png", height = 7, width = 7)

mort.table.spring = combined %>%
  filter(Alive %in% c("Y","N")) %>% 
  filter(Season == "S") %>% 
  count(Year, Alive)

mort.table.spring.wide = pivot_wider(mort.table.spring,
                                   id_cols = Year,
                                   names_from = Alive,
                                   values_from = n,
                                   names_prefix = "Status_")

mort.table.spring.wide[1,3] = 0
mort.table.spring.wide$Status_Y = as.numeric(mort.table.spring.wide$Status_Y)
mort.table.spring.wide$Status_N = as.numeric(mort.table.spring.wide$Status_N)
mort.table.spring.wide$Total = mort.table.spring.wide$Status_Y + mort.table.spring.wide$Status_N
mort.table.spring.wide$Mortality.Rate = (mort.table.spring.wide$Status_N/mort.table.spring.wide$Total)*100

ggplot(mort.table.spring.wide, aes(x = factor(Year), y = Mortality.Rate))+
  geom_line(aes(group = 1))+
  theme_classic(base_size = 15) +
  labs(y = "Mortality Rate (%)", x = "Year",
       title="Canopy and Small Trees")

ggsave("Plots/NatAreas.BLD.Mortality.Rate.Canopy.Small.Tree.Spring.png", height = 7, width = 7)

combined.fall = combined %>% 
  filter(Season == "F")

combined.fall$Dead = ifelse(combined.fall$Alive == "Y", 0, 1)

model1 <- glmer(Dead ~ as.numeric(Year) + (1|Tag_ID), data = combined.fall, family = binomial)
summary(model1)

combined.spring = combined %>% 
  filter(Season == "S")

combined.spring$Dead = ifelse(combined.spring$Alive == "Y", 0, 1)

model2 <- glmer(Dead ~ as.numeric(Year) + (1|Tag_ID), data = combined.spring, family = binomial)
summary(model2)

install.packages("ordinal")
library(ordinal)

fall.2 = fall %>% 
  filter(!is.na(Percent_Class))

fall.3 = fall.2 %>% 
  filter(!Tag_ID %in% c("SB1-26","SB1-21","SB1-35"))

fall.4 = fall.3 %>% 
  filter(Percent_Class != 5)

fall.4$Percent_Class_Factor = factor(fall.4$Percent_Class, ordered = TRUE,
                                   levels = c("0","1","2","3","4"))
fall.4$Year.Numeric = as.numeric(fall.4$Year)

model3 = clmm(Percent_Class_Factor ~ Year.Numeric + (1|Tag_ID), data = fall.4)

### Individual Tree Photo ####

gap = read.csv("./Formatted.Data/NatAreas_Tree_Photo.csv")

#fitler out the duplicates per year

gap.2 = gap %>% 
  filter(Delete != "Y")

colnames(gap.2)[7] = "Percent.Gap"

ggplot(gap.2, aes(y = Percent.Gap, x = Year))+
  geom_smooth(method = "lm")+
  geom_point

gap.means = gap.2 %>% 
  group_by(Year) %>% 
  summarize(mean.gap = mean(Percent.Gap),
            sd.gap = sd(Percent.Gap))

ggplot(gap.2, aes(y = Percent.Gap, x = as.factor(Year)))+
  geom_boxplot()+
  geom_jitter()


library(lme4)
library(lmerTest)
model <- lmer(Percent.Gap ~ as.numeric(Year) + (1 | Tree_Id), data = gap.2)
summary(model)

ggplot(gap.2, aes(x = as.numeric(Year), y = Percent.Gap, group = Tree_Id)) +
  geom_line(alpha = 0.3) +
  geom_smooth(aes(group = 1),method = "lm", color = "blue", se = TRUE)

ggsave("Plots/NatAreas.Canopy.Gap.Fall.png", height = 7, width = 7)


ggplot(gap.2, aes(x = as.numeric(Year), y = Percent.Gap)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line", color = "blue") +
  geom_line(aes(group = Tree_Id), alpha = 0.2)



test = lm(Percent.Gap~as.factor(Year),data = gap.2)
anova(test)
TukeyHSD(aov(test))
