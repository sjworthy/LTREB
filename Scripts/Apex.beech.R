library(tidyverse)

# working with the APEX beech data
# 3 blocks/sites, PC and SC on site, SV (CWRU)
# not sure which plots are the control so working at the plot level for now

apex = read.csv("Formatted.Data/Apex.beech.csv")

# read in info about treatments

apex.trt = read.csv("Raw.Data/APEX Treatments.csv")
apex.trt.2 = apex.trt %>% 
  select(Block,GPS.Plot..,correct.treatments)

colnames(apex.trt.2)[2] = "Plot"
apex.trt.2$Block[apex.trt.2$Block == "SF"] = "SC"

# merge

apex = left_join(apex,apex.trt.2)
apex$correct.treatments[apex$correct.treatments == "control"] = "Control"
apex$correct.treatments[apex$correct.treatments == "lime"] = "Lime"
apex$correct.treatments[apex$correct.treatments == "x-trt"] = "X-Trt"

# DBH of 0 is dead
# blanks for DBH for years it wasn't measured/had not recruited

# change the 0 to NA
apex$DBH2010[apex$DBH2010 == 0] = NA
# one weird row where DBH2010 recorded as no measurement
apex[4054,5] = NA
apex$DBH2010 = as.numeric(apex$DBH2010)

apex.growth = apex %>% 
  group_by(Year,correct.treatments) %>% 
  summarise(mean.dbh = mean(DBH2010, na.rm = TRUE),
            sd.dbh = sd(DBH2010, na.rm = TRUE))

ggplot(apex.growth, aes(y = mean.dbh, x = Year, group = correct.treatments))+
  geom_line()

apex$Year.factor = as.factor(apex$Year)

ggplot(apex, aes(y = DBH2010, x = Year.factor))+
  geom_boxplot()+
  facet_wrap(vars(correct.treatments,Block))

# subset for controls

apex.controls = apex %>% 
  filter(correct.treatments == "Control")

# make wide formate

apex.ctrl.wide = pivot_wider(data = apex.controls, 
                             id_cols = c(Block, Plot, Tag, correct.treatments),
                             names_from = Year,
                             values_from = DBH2010,
                             names_prefix = "DBH_")

colSums(is.na(apex.ctrl.wide))
# need to differentiate between NA b/c not recruited yet and NA for death

# all mortality at SV
# no death 2010, 2011, 2012
# 2013 = 1
# 2014,2015 = 0
# 2016 = 3 
# 2017 = 3
# 2018 = 1
# 2019 = 2
# 2020 = 4
# 2021 = 3
# 2022 = 11
# 2023 = 4
# 2024 = 4
# 2025 = 5

# looking only on property

Arb = apex.ctrl.wide %>% 
  filter(Block %in% c("PC","SC"))

Arb$growth = Arb$DBH_2025-Arb$DBH_2012

Arb$log.2010 = log(Arb$DBH_2010)
Arb$log.2011 = log(Arb$DBH_2011)
Arb$log.2012 = log(Arb$DBH_2012)
Arb$log.2013 = log(Arb$DBH_2013)
Arb$log.2014 = log(Arb$DBH_2014)
Arb$log.2015 = log(Arb$DBH_2015)
Arb$log.2016 = log(Arb$DBH_2016)
Arb$log.2017 = log(Arb$DBH_2017)
Arb$log.2018 = log(Arb$DBH_2018)
Arb$log.2019 = log(Arb$DBH_2019)
Arb$log.2020 = log(Arb$DBH_2020)
Arb$log.2021 = log(Arb$DBH_2021)
Arb$log.2022 = log(Arb$DBH_2022)
Arb$log.2023 = log(Arb$DBH_2023)
Arb$log.2024 = log(Arb$DBH_2024)
Arb$log.2025 = log(Arb$DBH_2025)

Arb$RGR.10.11 = (Arb$log.2011-Arb$log.2010)/1
Arb$RGR.11.12 = (Arb$log.2012-Arb$log.2011)/1
Arb$RGR.12.13 = (Arb$log.2013-Arb$log.2012)/1
Arb$RGR.13.14 = (Arb$log.2014-Arb$log.2013)/1
Arb$RGR.14.15 = (Arb$log.2015-Arb$log.2014)/1
Arb$RGR.15.16 = (Arb$log.2016-Arb$log.2015)/1
Arb$RGR.16.17 = (Arb$log.2017-Arb$log.2016)/1
Arb$RGR.17.18 = (Arb$log.2018-Arb$log.2017)/1
Arb$RGR.18.19 = (Arb$log.2019-Arb$log.2018)/1
Arb$RGR.19.20 = (Arb$log.2020-Arb$log.2019)/1
Arb$RGR.20.21 = (Arb$log.2021-Arb$log.2020)/1
Arb$RGR.21.22 = (Arb$log.2022-Arb$log.2021)/1
Arb$RGR.22.23 = (Arb$log.2023-Arb$log.2022)/1
Arb$RGR.23.24 = (Arb$log.2024-Arb$log.2023)/1
Arb$RGR.24.25 = (Arb$log.2025-Arb$log.2024)/1

# convert data to long format
apex.arb.long = pivot_longer(data = Arb, cols = 38:52, 
                               names_to = "Period", values_to = "RGR")

ggplot(apex.arb.long, aes(x = Period, y = RGR, group = Tag))+
  geom_line()

ggplot(apex.arb.long, aes(x = Period, y = RGR))+
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15)+
  labs(y = "Relative Growth Rate", title = "Box plot with all points (n = 14ish)")

ggplot(apex.arb.long, aes(x = Period, y = RGR))+
  geom_boxplot()+
  geom_jitter()+
  theme_classic(base_size = 15)+
  labs(y = "Relative Growth Rate", title = "Box plot with all points. Apex Ctrl Arb Sites")+
  facet_wrap(vars(Block), nrow=2)

ggsave("Plots/Apex.beech.Arb.Ctrol.RGR.boxplot.png", height = 7, width = 15)


# making the time period numeric
apex.arb.long = apex.arb.long %>% 
  mutate(Time = recode(Period,
                       "RGR.10.11" = "2011",
                       "RGR.11.12" = "2012",
                       "RGR.12.13" = "2013",
                       "RGR.13.14" = "2014",
                       "RGR.14.15" = "2015",
                       "RGR.15.16" = "2016",
                       "RGR.16.17" = "2017",
                       "RGR.17.18" = "2018",
                       "RGR.18.19" = "2019",
                       "RGR.19.20" = "2020",
                       "RGR.20.21" = "2021",
                       "RGR.21.22" = "2022",
                       "RGR.22.23" = "2023",
                       "RGR.23.24" = "2024",
                       "RGR.24.25" = "2025"))

apex.arb.long$Time = as.numeric(apex.arb.long$Time)

apex.arb.means.rgr = apex.arb.long %>% 
  group_by(Period) %>% 
  summarise(mean.RGR = mean(RGR, na.rm = TRUE),
            sd.RGR = sd(RGR, na.rm = TRUE))

apex.arb.long$Period = as.factor(apex.arb.long$Period)
apex.arb.means.rgr$Period = as.factor(apex.arb.means.rgr$Period)

ggplot(apex.arb.long, aes(x = Period, y = RGR)) +
  geom_line(aes(group = Tag), alpha = 0.3) +
  geom_line(data = apex.arb.means.rgr, aes(y = mean.RGR, group = 1), color = "red", size = 1) +
  geom_line(data = apex.arb.means.rgr, aes(y = mean.RGR + sd.RGR, group = 1), color = "red", linetype = "dashed") +
  geom_line(data = apex.arb.means.rgr, aes(y = mean.RGR - sd.RGR, group = 1), color = "red", linetype = "dashed") +
  theme_classic(base_size = 15) +
  labs(y = "Relative Growth Rate (RGR)", x = "Census Period", 
       title = "RGR over time with mean ± SD for Apex Ctrl Arb Sites")

ggsave("Plots/Apex.beech.Arb.Ctrol.RGR.Time.png", height = 7, width = 15)


test = lm(RGR~Time, apex.arb.long)
# negative (-0.006), but not significant

summary(lm(RGR~Period, apex.arb.long))

anova_model <- aov(RGR ~ Period, data = apex.arb.long)
summary(anova_model)
TukeyHSD(anova_model)

kruskal.test(RGR ~ Period, data = apex.arb.long)

apex.arb.long.12 = apex.arb.long %>% 
  filter(Time > 2011)

test = lm(RGR~Time, apex.arb.long.12)
# negative (-0.006), but not significant

summary(lm(RGR~Period, apex.arb.long.12))

anova_model <- aov(RGR ~ Period, data = apex.arb.long.12)
summary(anova_model)
TukeyHSD(anova_model)

kruskal.test(RGR ~ Period, data = apex.arb.long.12)


PC = apex.arb.long %>% 
  filter(Block == "PC")

PC.arb.means.rgr = PC %>% 
  group_by(Period) %>% 
  summarise(mean.RGR = mean(RGR, na.rm = TRUE),
            sd.RGR = sd(RGR, na.rm = TRUE))

SC = apex.arb.long %>% 
  filter(Block == "SC")

SC.arb.means.rgr = SC %>% 
  group_by(Period) %>% 
  summarise(mean.RGR = mean(RGR, na.rm = TRUE),
            sd.RGR = sd(RGR, na.rm = TRUE))

# both sites declining but not significantly
test.PC = lm(RGR~ Time, PC)
test.SC = lm(RGR~Time, SC)

PC.12 = apex.arb.long %>% 
  filter(Time > 2011)
SC.12 = apex.arb.long %>% 
  filter(Time > 2011)

# both sites declining but not significantly
test.PC = lm(RGR~ Time, PC.12)
test.SC = lm(RGR~Time, SC.12)


