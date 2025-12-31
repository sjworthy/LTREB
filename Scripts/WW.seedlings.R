library(tidyverse)
library(lmerTest)
library(performance)
library(emmeans)

# merge all years together
og.seedlings = read.csv("./Formatted.Data/WW_Seedlings.csv")
seeds.2025 = read.csv("./Formatted.Data/WW_Seedlings_2025.csv")
seeds.2025$Leaves = as.character(seeds.2025$Leaves)
seeds.2025$Herb = as.character(seeds.2025$Herb)

seedlings = full_join(og.seedlings,seeds.2025)

write.csv(seedlings, file = "./Formatted.Data/all.seedling.years.csv")

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

abundance.plot$scale.Year = scale(abundance.plot$Year, center = TRUE, scale = FALSE)

abund.model = glmer.nb(alive~Treatment*scale.Year + Block + (1|Plot_ID), data = abundance.plot)
summary(abund.model)
plot(abund.model)
slopes = emtrends(abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

relative.abundance.plot = abundance.plot %>% 
  group_by(Plot_ID,Treatment,Block) %>% 
  mutate(baseline_2019 = alive[Year == 2019],
         rel.abund = alive / baseline_2019) %>%
  ungroup()

ggplot(relative.abundance.plot, aes(x = Year, y = rel.abund, color = Treatment))+
  geom_smooth(method = "lm")

# making year a factor for plotting
relative.abundance.plot$Year.fact = as.factor(relative.abundance.plot$Year)

ggplot(relative.abundance.plot, aes(x = Year.fact, y = rel.abund, color = Treatment))+
  geom_boxplot()

# remove 2019 that is 0 rel.abund
relative.abundance.plot.2 = relative.abundance.plot %>% 
  filter(Year != 2019)

ggplot(relative.abundance.plot.2, aes(x = Year.fact, y = rel.abund, color = Treatment))+
  geom_boxplot()

rel.abund.model = lmer(rel.abund~Treatment*scale.Year + Block + (1|Plot_ID), data = relative.abundance.plot.2)
plot(rel.abund.model, which = 1)
qqnorm(residuals(rel.abund.model))
qqline(residuals(rel.abund.model))
hist(residuals(rel.abund.model))
slopes = emtrends(rel.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

# log transform relative abundance
relative.abundance.plot.2$log.rel.abund = log(relative.abundance.plot.2$rel.abund)

log.rel.abund.model = lmer(log.rel.abund~Treatment*scale.Year + Block + (1|Plot_ID), data = relative.abundance.plot.2)
plot(log.rel.abund.model, which = 1)
qqnorm(residuals(log.rel.abund.model))
qqline(residuals(log.rel.abund.model))
hist(residuals(log.rel.abund.model))
slopes = emtrends(log.rel.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

AIC(rel.abund.model,log.rel.abund.model)

### late surveys

abundance.plot = late %>% 
  group_by(Plot_ID,Treatment, Year, Block) %>% 
  summarize(alive = sum(Use_Alive, na.rm = TRUE),
            dead = sum(Use_Dead, na.rm = TRUE)) %>% 
  ungroup()

abundance.plot$scale.Year = scale(abundance.plot$Year, center = TRUE, scale = FALSE)

abund.model = glmer.nb(alive~Treatment*scale.Year + Block + (1|Plot_ID), data = abundance.plot)
summary(abund.model)
plot(abund.model)
check_model(abund.model)
slopes = emtrends(abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

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

rel.abund.model = lmer(rel.abund~Treatment*scale.Year + Block + (1|Plot_ID), data = relative.abundance.plot.2)
plot(rel.abund.model, which = 1)
qqnorm(residuals(rel.abund.model))
qqline(residuals(rel.abund.model))
hist(residuals(rel.abund.model))
slopes = emtrends(rel.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

relative.abundance.plot.2$log.rel.abund = log(relative.abundance.plot.2$rel.abund)

log.rel.abund.model = lmer(log.rel.abund~Treatment*scale.Year + Block+ (1|Plot_ID), data = relative.abundance.plot.2)
summary(log.rel.abund.model)
plot(log.rel.abund.model, which = 1)
qqnorm(residuals(log.rel.abund.model))
qqline(residuals(log.rel.abund.model))
hist(residuals(log.rel.abund.model))
slopes = emtrends(log.rel.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

### common species abundance ####

# ACRU, ACSA, LITU, QURU

abundance.plot = early %>% 
  group_by(Treatment,Year,Species) %>% 
  summarize(alive = sum(Use_Alive, na.rm = TRUE),
            dead = sum(Use_Dead, na.rm = TRUE)) %>% 
  ungroup()

abundance.plot.sub = abundance.plot %>% 
  filter(Species %in% c("ACRU","ACSA","LITU","QURU"))

ggplot(abundance.plot.sub, aes(x = Year, y = alive, color = Treatment))+
  geom_line()+
  facet_wrap(~Species)

abundance.plot.sub$scale.Year = scale(abundance.plot.sub$Year, center = TRUE, scale = FALSE)

ACRU = abundance.plot.sub %>% 
  filter(Species == "ACRU")
ACSA = abundance.plot.sub %>% 
  filter(Species == "ACSA")
LITU = abundance.plot.sub %>% 
  filter(Species == "LITU")
QURU = abundance.plot.sub %>% 
  filter(Species == "QURU")

ACRU.abund.model = glm.nb(alive~Treatment*scale.Year, data = ACRU)
summary(ACRU.abund.model)
plot(ACRU.abund.model)
check_model(ACRU.abund.model)
slopes = emtrends(ACRU.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

ACSA.abund.model = glm.nb(alive~Treatment*scale.Year, data = ACSA)
summary(ACSA.abund.model)
plot(ACSA.abund.model)
check_model(ACSA.abund.model)
slopes = emtrends(ACSA.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

LITU.abund.model = glm.nb(alive~Treatment*scale.Year, data = LITU)
summary(LITU.abund.model)
plot(LITU.abund.model)
check_model(LITU.abund.model)
slopes = emtrends(LITU.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

QURU.abund.model = glm.nb(alive~Treatment*scale.Year, data = QURU)
summary(QURU.abund.model)
plot(QURU.abund.model)
check_model(QURU.abund.model)
slopes = emtrends(QURU.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

# add a row for QURU IC+TSI 2019 since 0 recorded
abundance.plot.sub[nrow(abundance.plot.sub) + 1, ] <- list(
  Treatment = "IC+TSI",
  Year      = 2019,
  Species   = "QURU",
  dead      = 0,
  alive     = 0,
  Year_c    = -2.5352113
)

relative.abundance.plot = abundance.plot.sub %>% 
  group_by(Treatment,Species) %>% 
  mutate(baseline_2019 = alive[Year == 2019],
         rel.abund = alive / baseline_2019) %>%
  ungroup()

# remove 2019 that is 0 rel.abund
relative.abundance.plot.2 = relative.abundance.plot %>% 
  filter(Year != 2019)

# making year a factor for plotting
relative.abundance.plot.2$Year.fact = as.factor(relative.abundance.plot.2$Year)

ggplot(relative.abundance.plot, aes(x = scale.Year, y = rel.abund, color = Treatment))+
  geom_line()+
  facet_wrap(~Species)

ggplot(relative.abundance.plot.2, aes(x = scale.Year, y = rel.abund, color = Treatment))+
  geom_line()+
  facet_wrap(~Species)

ACRU = relative.abundance.plot.2 %>% 
  filter(Species == "ACRU")
ACSA = relative.abundance.plot.2 %>% 
  filter(Species == "ACSA")
LITU = relative.abundance.plot.2 %>% 
  filter(Species == "LITU")
# doesn't work
QURU = relative.abundance.plot.2 %>% 
  filter(Species == "QURU")

ACRU.rel.abund.model = lm(log(rel.abund)~Treatment*scale.Year, data = ACRU)
summary(ACRU.rel.abund.model)
plot(ACRU.rel.abund.model, which = 1)
qqnorm(residuals(ACRU.rel.abund.model))
qqline(residuals(ACRU.rel.abund.model))
hist(residuals(ACRU.rel.abund.model))
slopes = emtrends(ACRU.rel.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

ACSA.rel.abund.model = lm(log(rel.abund)~Treatment*scale.Year, data = ACSA)
summary(ACSA.rel.abund.model)
plot(ACSA.rel.abund.model, which = 1)
qqnorm(residuals(ACSA.rel.abund.model))
qqline(residuals(ACSA.rel.abund.model))
hist(residuals(ACSA.rel.abund.model))
slopes = emtrends(ACSA.rel.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

LITU.rel.abund.model = lm(log(rel.abund)~Treatment*scale.Year, data = LITU)
summary(LITU.rel.abund.model)
plot(LITU.rel.abund.model, which = 1)
qqnorm(residuals(LITU.rel.abund.model))
qqline(residuals(LITU.rel.abund.model))
hist(residuals(LITU.rel.abund.model))
slopes = emtrends(LITU.rel.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

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

recruits$Year.fact = as.factor(recruits$Year)

ggplot(recruits, aes(x = Year.fact, y = recruits, color = Treatment))+
  geom_boxplot()

recruits$scale.Year = scale(recruits$Year, center = TRUE, scale = FALSE)

recruit.mod = glmer.nb(recruits ~ Treatment*scale.Year + Block + (1|Plot_ID), data = recruits)
summary(recruit.mod)
slope = emtrends(recruit.mod, ~ Treatment, var = "scale.Year")
pairs(slope)

## late survey

recruits = late %>% 
  group_by(Plot_ID,Treatment,Year,Block) %>%
  summarise(recruits = sum(Banded_Year == Year, na.rm = TRUE), .groups = "drop")

recruits$Year.fact = as.factor(recruits$Year)

ggplot(recruits, aes(x = Year.fact, y = recruits, color = Treatment))+
  geom_boxplot()

recruits$scale.Year = scale(recruits$Year, center = TRUE, scale = FALSE)

recruit.mod = glmer.nb(recruits ~ Treatment*scale.Year + Block + (1|Plot_ID), data = recruits)
summary(recruit.mod)
slope = emtrends(recruit.mod, ~ Treatment, var = "scale.Year")
pairs(slope)

#### common species recruits ####
seedlings.2 = seedlings %>% 
  filter(Banded_Year != 2019)

early = seedlings.2 %>% 
  filter(Period == "early")

early.sub = early %>% 
  filter(Species %in% c("ACRU","ACSA","LITU","QURU"))

recruits = early.sub %>% 
  group_by(Plot_ID,Treatment,Year,Block,Species) %>%
  summarise(recruits = sum(Banded_Year == Year, na.rm = TRUE), .groups = "drop")

recruits$Year.fact = as.factor(recruits$Year)

ggplot(recruits, aes(x = Year.fact, y = recruits, color = Treatment))+
  geom_boxplot()+
  facet_wrap(~Species)

recruits$scale.Year = scale(recruits$Year, center = TRUE, scale = FALSE)

ACRU = recruits %>% 
  filter(Species == "ACRU")
ACSA = recruits %>% 
  filter(Species == "ACSA")
LITU = recruits %>% 
  filter(Species == "LITU")
QURU = recruits %>% 
  filter(Species == "QURU")

ACRU.recruit.mod = glmer.nb(recruits ~ Treatment*scale.Year+ (1|Plot_ID), 
                            data = ACRU,
                            control = glmerControl(
                              optimizer = "bobyqa"))
summary(ACRU.recruit.mod)
slope = emtrends(ACRU.recruit.mod, ~ Treatment, var = "scale.Year")
pairs(slope)

ACSA.recruit.mod = glmer.nb(recruits ~ Treatment*scale.Year+ (1|Plot_ID), 
                            data = ACSA,
                            control = glmerControl(
                              optimizer = "bobyqa"))
summary(ACSA.recruit.mod)
slope = emtrends(ACSA.recruit.mod, ~ Treatment, var = "scale.Year")
pairs(slope)

LITU.recruit.mod = glmer.nb(recruits ~ Treatment*scale.Year+ (1|Plot_ID), 
                            data = LITU,
                            control = glmerControl(
                              optimizer = "bobyqa"))
summary(LITU.recruit.mod)
slope = emtrends(LITU.recruit.mod, ~ Treatment, var = "scale.Year")
pairs(slope)

QURU.recruit.mod = glmer.nb(recruits ~ Treatment*scale.Year+ (1|Plot_ID), 
                            data = QURU,
                            control = glmerControl(
                              optimizer = "bobyqa"))
summary(QURU.recruit.mod)
slope = emtrends(QURU.recruit.mod, ~ Treatment, var = "scale.Year")
pairs(slope)

#### oak recruitment ####
seedlings.2 = seedlings %>% 
  filter(Banded_Year != 2019)

early = seedlings.2 %>% 
  filter(Period == "early")

early.sub = early %>% 
  filter(Species %in% c("QURU","QUAL"))

recruits = early.sub %>% 
  group_by(Plot_ID,Treatment,Year,Block,Species) %>%
  summarise(recruits = sum(Banded_Year == Year, na.rm = TRUE), .groups = "drop")

recruits$Year.fact = as.factor(recruits$Year)

ggplot(recruits, aes(x = Year.fact, y = recruits, color = Treatment))+
  geom_boxplot()+
  facet_wrap(~Species)

recruits$scale.Year = scale(recruits$Year, center = TRUE, scale = FALSE)

QURU = recruits %>% 
  filter(Species == "QURU")

QURU.recruit.mod = glmer.nb(recruits ~ Treatment*scale.Year+ (1|Plot_ID), 
                            data = QURU,
                            control = glmerControl(
                              optimizer = "bobyqa"))
summary(QURU.recruit.mod)
slope = emtrends(QURU.recruit.mod, ~ Treatment, var = "scale.Year")
pairs(slope)

#### Mortality Rate ####

abundance.plot = early %>% 
  group_by(Plot_ID,Treatment, Year,Block) %>% 
  summarize(alive = sum(Use_Alive, na.rm = TRUE),
            dead = sum(Use_Dead, na.rm = TRUE)) %>% 
  ungroup()

mortality.rate = abundance.plot %>%
  mutate(mort.rate = dead/(dead+alive),
         total = dead+alive)

mortality.rate$scale.Year = scale(mortality.rate$Year, center = TRUE, scale = FALSE)

y = cbind(mortality.rate$dead, mortality.rate$alive)
mort.rate.mod = glmer(y ~ Treatment*scale.Year + Block + (1|Plot_ID), 
                      family = binomial(),
                      data = mortality.rate)
summary(mort.rate.mod)
slopes = emtrends(mort.rate.mod, specs = "Treatment", var = "scale.Year")
pairs(slopes)

# late
abundance.plot = late %>% 
  group_by(Plot_ID,Treatment, Year,Block) %>% 
  summarize(alive = sum(Use_Alive, na.rm = TRUE),
            dead = sum(Use_Dead, na.rm = TRUE)) %>% 
  ungroup()

mortality.rate = abundance.plot %>%
  mutate(mort.rate = dead/(dead+alive))

mortality.rate$scale.Year = scale(mortality.rate$Year, center = TRUE, scale = FALSE)

y = cbind(mortality.rate$dead, mortality.rate$alive)
mort.rate.mod = glmer(y ~ Treatment*scale.Year + Block + (1|Plot_ID), 
                      family = binomial(),
                      data = mortality.rate)
summary(mort.rate.mod)
slopes = emtrends(mort.rate.mod, specs = "Treatment", var = "scale.Year")
pairs(slopes)


#### Seeding to Sapling ###
# sapling is dbh > 1 cm

sapling = early %>% 
  filter(Diameter >= 10)

# split by treatment
C.sapling = sapling %>% 
  filter(Treatment == "Control")
IC.sapling = sapling %>% 
  filter(Treatment == "IC")
IC.TSI.sapling = sapling %>% 
  filter(Treatment == "IC+TSI")

# split early by treatment
C.early = early %>% 
  filter(Treatment == "Control")
IC.early = early %>% 
  filter(Treatment == "IC")
IC.TSI.early = early %>% 
  filter(Treatment == "IC+TSI")


