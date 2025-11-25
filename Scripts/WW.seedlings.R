library(tidyverse)
library(lmerTest)
library(performance)

seedlings = read.csv("./Formatted.Data/WW_Seedlings.csv")

# split by early and late

early = seedlings %>% 
  filter(Period == "early")
late = seedlings %>% 
  filter(Period == "late")

#### total abundance over years for each treatment ####

abundance = early %>% 
  group_by(Treatment, Year) %>% 
  summarize(alive = sum(Use_Alive, na.rm = TRUE),
            dead = sum(Use_Dead, na.rm = TRUE)) %>% 
  ungroup()

relative.abundance = abundance %>% 
  group_by(Treatment) %>% 
  mutate(baseline_2019 = alive[Year == 2019],
         rel.abund = alive / baseline_2019) %>%
  ungroup()

ggplot(relative.abundance, aes(x = Year, y = rel.abund, color = Treatment))+
  geom_line()

# summarizing by plot 
abundance.plot = early %>% 
  group_by(Plot_ID,Treatment,Year,Block) %>% 
  summarize(alive = sum(Use_Alive, na.rm = TRUE),
            dead = sum(Use_Dead, na.rm = TRUE)) %>% 
  ungroup()

relative.abundance.plot = abundance.plot %>% 
  group_by(Plot_ID,Treatment,Block) %>% 
  mutate(baseline_2019 = alive[Year == 2019],
         rel.abund = alive / baseline_2019) %>%
  ungroup()

# making year a factor for plotting
relative.abundance.plot$Year.fact = as.factor(relative.abundance.plot$Year)

ggplot(relative.abundance.plot, aes(x = Year.fact, y = rel.abund, color = Treatment))+
  geom_boxplot()

# remove 2019 that is 0 rel.abund
relative.abundance.plot.2 = relative.abundance.plot %>% 
  filter(Year != 2019)

ggplot(relative.abundance.plot.2, aes(x = Year.fact, y = rel.abund, color = Treatment))+
  geom_boxplot()

# log transform relative abundance
relative.abundance.plot.2$log.rel.abund = log(relative.abundance.plot.2$rel.abund)


# freq
log.rel.abund.model = lmer(log.rel.abund~Treatment*Year + Block + (1|Plot_ID), data = relative.abundance.plot.2)
plot(log.rel.abund.model, which = 1)
qqnorm(residuals(log.rel.abund.model))
qqline(residuals(log.rel.abund.model))
hist(residuals(log.rel.abund.model))
slopes = emtrends(log.rel.abund.model, specs = "Treatment", var = "Year")
pairs(slopes)

### late surveys

abundance.plot = late %>% 
  group_by(Plot_ID,Treatment, Year, Block) %>% 
  summarize(alive = sum(Use_Alive, na.rm = TRUE),
            dead = sum(Use_Dead, na.rm = TRUE)) %>% 
  ungroup()

relative.abundance.plot = abundance.plot %>% 
  group_by(Plot_ID,Treatment,Block) %>% 
  mutate(baseline_2019 = alive[Year == 2019],
         rel.abund = alive / baseline_2019) %>%
  ungroup()

relative.abundance.plot$Year.fact = as.factor(relative.abundance.plot$Year)

ggplot(relative.abundance.plot, aes(x = Year.fact, y = rel.abund, color = Treatment))+
  geom_boxplot()

# remove 2019 that is 0 rel.abund
relative.abundance.plot.2 = relative.abundance.plot %>% 
  filter(Year != 2019)

ggplot(relative.abundance.plot.2, aes(x = Year.fact, y = rel.abund, color = Treatment))+
  geom_boxplot()

relative.abundance.plot.2$log.rel.abund = log(relative.abundance.plot.2$rel.abund)

log.rel.abund.model = lmer(log.rel.abund~Treatment*Year + Block+ (1|Plot_ID), data = relative.abundance.plot.2)
summary(log.rel.abund.model)
plot(log.rel.abund.model, which = 1)
qqnorm(residuals(log.rel.abund.model))
qqline(residuals(log.rel.abund.model))
hist(residuals(log.rel.abund.model))
slopes = emtrends(log.rel.abund.model, specs = "Treatment", var = "Year")
pairs(slopes)

### species abundance ####

abundance.plot = early %>% 
  group_by(Treatment,Year,Species) %>% 
  summarize(alive = sum(Use_Alive, na.rm = TRUE),
            dead = sum(Use_Dead, na.rm = TRUE)) %>% 
  ungroup()

ggplot(abundance.plot, aes(x = Year, y = alive, color = Treatment))+
  geom_line()+
  facet_wrap(~Species)

relative.abundance.plot = abundance.plot %>% 
  group_by(Treatment,Species) %>% 
  mutate(baseline_2019 = alive[Year == 2019],
         rel.abund = alive / baseline_2019) %>%
  ungroup()

#### Recruits ####

# eliminate individuals in 2019

seedlings.2 = seedlings %>% 
  filter(Banded_Year != 2019)

early = seedlings.2 %>% 
  filter(Period == "early")
late = seedlings.2 %>% 
  filter(Period == "late")

recruits = early %>% 
  group_by(Plot_ID,Treatment,Year,Block) %>%
  summarise(recruits = sum(Banded_Year == Year, na.rm = TRUE), .groups = "drop")

test = early %>% 
  filter(Plot_ID == "C-18-M") %>% 
  filter(Treatment == "Control") %>% 
  filter(Year == "2020")

recruits$Year.fact = as.factor(recruits$Year)

ggplot(recruits, aes(x = Year.fact, y = recruits, color = Treatment))+
  geom_boxplot()

recruit.mod = brm(recruits ~ Treatment*Year + Block + (1|Plot_ID),
                   family = negbinomial(),
                  data = recruits)
summary(recruit.mod)
slope = emtrends(recruit.mod, ~ Treatment, var = "Year")
pairs(slopes)

#### Mortality Rate ####

abundance.plot = early %>% 
  group_by(Plot_ID,Treatment, Year,Block) %>% 
  summarize(alive = sum(Use_Alive, na.rm = TRUE),
            dead = sum(Use_Dead, na.rm = TRUE)) %>% 
  ungroup()
mortality.rate = abundance.plot %>%
  mutate(mort.rate = dead/(dead+alive),
         total = dead+alive)

mortality.rate$Year.num = as.numeric(mortality.rate$Year)
mortality.rate$Year.fact = as.factor(mortality.rate$Year)
mortality.rate$Treatment.fact = as.factor(mortality.rate$Treatment)
mortality.rate$Block.fact = as.factor(mortality.rate$Block)
mortality.rate$Plot_ID_fact = as.factor(mortality.rate$Plot_ID)

priors = c(prior(normal(0,10),class = b))
mort.mod = brm(dead | trials(total) ~ 1 + Treatment.fact*Year + Block.fact + (1|Block/Plot_ID_fact),
               family = binomial(),
               prior = priors,
               data = mortality.rate)
summary(mort.mod)
conditional_effects(mort.mod)
slopes = emtrends(mort.mod, specs = "Treatment.fact", var = "Year")
pairs(slopes)

ggplot(mortality.rate, aes(x = Year.fact, y = mort.rate, color = Treatment))+
  geom_boxplot()

mort.rate.mod = lmer(mort.rate ~ Treatment*Year + Block + (1|Plot_ID), data = mortality.rate)
summary(mort.rate.mod)
slopes = emtrends(mort.rate.mod, specs = "Treatment", var = "Year")
pairs(slopes)

# late
abundance.plot = late %>% 
  group_by(Plot_ID,Treatment, Year,Block) %>% 
  summarize(alive = sum(Use_Alive, na.rm = TRUE),
            dead = sum(Use_Dead, na.rm = TRUE)) %>% 
  ungroup()
mortality.rate = abundance.plot %>%
  mutate(mort.rate = dead/(dead+alive))

mortality.rate$Year.fact = as.factor(mortality.rate$Year)

ggplot(mortality.rate, aes(x = Year.fact, y = mort.rate, color = Treatment))+
  geom_boxplot()

mort.rate.mod = lmer(mort.rate ~ Treatment*Year + Block+ (1|Plot_ID), data = mortality.rate)
summary(mort.rate.mod)
slopes = emtrends(mort.rate.mod, specs = "Treatment", var = "Year")
pairs(slopes)
