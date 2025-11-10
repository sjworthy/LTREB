library(tidyverse)

# read in stebbins tree data
# data subset by hand to just include tree tag ID and growth data
# not considering 2007 data since it was potentially taken at a different spot on the tree
steb.trees = read.csv("Formatted.Data/Stebbins.trees.csv")

# subset for only beech
steb.beech = steb.trees %>% 
  filter(Species == "Beech")

# 6 individuals died 

# make the columns numeric
steb.beech$dbh.2014 = as.numeric(steb.beech$dbh.2014)
steb.beech$dbh.2017 = as.numeric(steb.beech$dbh.2017)
steb.beech$dbh.2020 = as.numeric(steb.beech$dbh.2020)
steb.beech$dbh.2022 = as.numeric(steb.beech$dbh.2022)
steb.beech$dbh.2024 = as.numeric(steb.beech$dbh.2024)

# calculate complete growth
# all individuals have positive growth overall 2012-2024
steb.beech$growth = steb.beech$dbh.2024 - steb.beech$dbh.2012

steb.means.dbh = steb.beech %>% 
  summarise(mean.dbh.12 = mean(dbh.2012, na.rm = TRUE),
            sd.dbh.12 = sd(dbh.2012, na.rm = TRUE),
            mean.dbh.14 = mean(dbh.2014, na.rm = TRUE),
            sd.dbh.14 = sd(dbh.2014, na.rm = TRUE),
            mean.dbh.17 = mean(dbh.2017, na.rm = TRUE),
            sd.dbh.17 = sd(dbh.2017, na.rm = TRUE),
            mean.dbh.20 = mean(dbh.2020, na.rm = TRUE),
            sd.dbh.20 = sd(dbh.2020, na.rm = TRUE),
            mean.dbh.22 = mean(dbh.2022, na.rm = TRUE),
            sd.dbh.22 = sd(dbh.2022, na.rm = TRUE),
            mean.dbh.24 = mean(dbh.2024, na.rm = TRUE),
            sd.dbh.24 = sd(dbh.2024, na.rm = TRUE))



# changes in relative rate of growth
# expect growth rate to decreases over time

steb.beech$log.dbh.2012 = log(steb.beech$dbh.2012)
steb.beech$log.dbh.2014 = log(steb.beech$dbh.2014)
steb.beech$log.dbh.2017 = log(steb.beech$dbh.2017)
steb.beech$log.dbh.2020 = log(steb.beech$dbh.2020)
steb.beech$log.dbh.2022 = log(steb.beech$dbh.2022)
steb.beech$log.dbh.2024 = log(steb.beech$dbh.2024)

steb.beech$RGR.12.14 = (steb.beech$log.dbh.2014 - steb.beech$log.dbh.2012)/2
steb.beech$RGR.14.17 = (steb.beech$log.dbh.2017 - steb.beech$log.dbh.2014)/3
steb.beech$RGR.17.20 = (steb.beech$log.dbh.2020 - steb.beech$log.dbh.2017)/3
steb.beech$RGR.20.22 = (steb.beech$log.dbh.2022 - steb.beech$log.dbh.2020)/2
steb.beech$RGR.22.24 = (steb.beech$log.dbh.2024 - steb.beech$log.dbh.2022)/2

t.test(steb.beech$RGR.12.14,steb.beech$RGR.20.22) # significant

# change NA RGR values to 0 because that individual died

steb.beech = steb.beech %>% 
  mutate(across(c(RGR.12.14,RGR.14.17,RGR.17.20,RGR.20.22,RGR.22.24),
         ~ replace(., is.na(.),0)))

# convert data to long format

steb.beech.long = pivot_longer(data = steb.beech, cols = 17:21, 
                               names_to = "Period", values_to = "RGR")

ggplot(steb.beech.long, aes(x = Period, y = RGR, group = Tree.ID.Tag))+
  geom_line()

ggplot(steb.beech.long, aes(x = Period, y = RGR))+
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15)+
  labs(y = "Relative Growth Rate", title = "Box plot with all points (n = 36ish)")

ggsave("Plots/Stebbins.beech.RGR.boxplot.png", height = 7, width = 7)

# making the time period numeric
steb.beech.long = steb.beech.long %>% 
  mutate(Time = recode(Period,
                         "RGR.12.14" = "2014",
                         "RGR.14.17" = "2017",
                         "RGR.17.20" = "2020",
                         "RGR.20.22" = "2022",
                         "RGR.22.24" = "2024"))

steb.beech.long$Time = as.numeric(steb.beech.long$Time)

steb.means.rgr = steb.beech.long %>% 
  group_by(Period) %>% 
  summarise(mean.RGR = mean(RGR, na.rm = TRUE),
            sd.RGR = sd(RGR, na.rm = TRUE))

steb.beech.long$Period = as.factor(steb.beech.long$Period)
steb.means.rgr$Period = as.factor(steb.means.rgr$Period)

ggplot(steb.beech.long, aes(x = Period, y = RGR)) +
  geom_line(aes(group = Tree.ID.Tag), alpha = 0.3) +
  geom_line(data = steb.means.rgr, aes(y = mean.RGR, group = 1), color = "red", size = 1) +
  geom_line(data = steb.means.rgr, aes(y = mean.RGR + sd.RGR, group = 1), color = "red", linetype = "dashed") +
  geom_line(data = steb.means.rgr, aes(y = mean.RGR - sd.RGR, group = 1), color = "red", linetype = "dashed") +
  theme_classic(base_size = 15) +
  labs(y = "Relative Growth Rate (RGR)", x = "Census Period", 
       title = "RGR over time with mean ± SD")

ggsave("Plots/Stebbins.beech.RGR.Time.png", height = 7, width = 7)

test = lm(RGR~Time, steb.beech.long)
# negative (-0.0003), significant (p = 0.02)

summary(lm(RGR~Period, steb.beech.long))

plot(RGR~Time, data = steb.beech.long)

anova_model <- aov(RGR ~ Period, data = steb.beech.long)
summary(anova_model)
TukeyHSD(anova_model)
kruskal.test(RGR ~ Period, data = steb.beech.long)



