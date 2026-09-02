library(tidyverse)
library(lmerTest)
library(performance)
library(emmeans)
library(MASS)
library(sjPlot)
library(cowplot)

# merge all years together
og.seedlings = read.csv("./Formatted.Data/WW_Seedlings.csv")
seeds.2025 = read.csv("./Formatted.Data/WW_Seedlings_2025.csv")
seeds.2025$Leaves = as.character(seeds.2025$Leaves)
seeds.2025$Herb = as.character(seeds.2025$Herb)

seedlings = full_join(og.seedlings,seeds.2025)

write.csv(seedlings, file = "./Formatted.Data/all.seedling.years.csv")

seedlings = read.csv("./Formatted.Data/all.seedling.years.csv", row.names = 1)

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

ggplot(abundance.plot, aes(x = Year, y = alive, color = Treatment))+
  geom_smooth(method = "lm")

# making year a factor for plotting
abundance.plot$Year.fact = as.factor(abundance.plot$Year)

ggplot(abundance.plot, aes(x = Year.fact, y = alive, color = Treatment))+
  geom_boxplot()

ggplot(abundance.plot, aes(x = Year.fact, y = alive, color = Treatment))+
  geom_boxplot()+
  theme_classic(base_size = 15)+
  labs(x = "Year", y = "Seedling Abundance")

ggsave(file = "./Plots/abund.seedlings.WW.png", height = 6, width = 8, dpi = 300)

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

ggplot(relative.abundance.plot, aes(x = Year.fact, y = rel.abund, color = Treatment))+
  geom_boxplot()+
  theme_classic(base_size = 15)+
  labs(x = "Year", y = "Relative Seedling Abundance")

ggsave(file = "./Plots/relative.abund.seedlings.WW.png", height = 6, width = 8, dpi = 300)

# remove 2019 that is 0 rel.abund
relative.abundance.plot.2 = relative.abundance.plot %>% 
  filter(Year != 2019)

ggplot(relative.abundance.plot.2, aes(x = Year.fact, y = rel.abund, color = Treatment))+
  geom_boxplot()

rel.abund.model = lmer(rel.abund~Treatment*scale.Year + Block + (1|Plot_ID), data = relative.abundance.plot.2)
summary(rel.abund.model)
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

### late surveys (Doesn't include late 2025)

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

# ACRU, ACSA, LITU, QURU, FRAM, PRSE

abundance.plot = early %>% 
  group_by(Treatment,Year,Species) %>% 
  summarize(alive = sum(Use_Alive, na.rm = TRUE),
            dead = sum(Use_Dead, na.rm = TRUE)) %>% 
  ungroup()

abundance.plot.sub = abundance.plot %>% 
  filter(Species %in% c("ACRU","ACSA","LITU","QURU","PRSE","FRAM"))

abundance.plot.sub$Treatment = as.factor(abundance.plot.sub$Treatment)

ggplot(abundance.plot.sub, aes(x = Year, y = alive, color = Treatment))+
  geom_line()+
  facet_wrap(~Species)

abundance.plot.sub$scale.Year = as.numeric(scale(abundance.plot.sub$Year, center = TRUE, scale = FALSE))

ACRU = abundance.plot.sub %>% 
  filter(Species == "ACRU")
ACSA = abundance.plot.sub %>% 
  filter(Species == "ACSA")
LITU = abundance.plot.sub %>% 
  filter(Species == "LITU")
QURU = abundance.plot.sub %>% 
  filter(Species == "QURU")
PRSE = abundance.plot.sub %>% 
  filter(Species == "PRSE")
FRAM = abundance.plot.sub %>% 
  filter(Species == "FRAM")

QURU.pre.mast = QURU %>% 
  filter(Year != 2025)

ACRU.abund.model = glm.nb(alive~Treatment*scale.Year, data = ACRU)
summary(ACRU.abund.model)
plot(ACRU.abund.model)
check_model(ACRU.abund.model)
slopes = emtrends(ACRU.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)
# significant increase in all treatments, treatments don't differ

ACRU.plot = plot_model(ACRU.abund.model, type = "pred", terms = c("scale.Year","Treatment"),
           line.size = 1.5, alpha = 0.1)+
  theme_classic(base_size = 15)+
  scale_x_continuous(
    breaks = -3.02399999999989:2.97600000000011,
    labels = 2019:2025)+
  scale_color_manual(
    values = c(
      "Control" = "#CFA3EE",
      "IC" = "#548F01",
      "IC+TSI" = "#4E5462"),
    labels = c(
      "Control" = "C",
      "IC" = "CD",
      "IC+TSI" = "CD+ISR"))+
  scale_fill_manual(
    values = c(
      "Control" = "#CFA3EE",
      "IC" = "#548F01",
      "IC+TSI" = "#4E5462"),
    labels = c(
      "Control" = "C",
      "IC" = "CD",
      "IC+TSI" = "CD+ISR"))+
  theme(legend.position = "none")+
  labs(x = "Year", y= "Abundance", title = "Acer rubra (red maple)")
ACRU.plot

ACSA.abund.model = glm.nb(alive~Treatment*scale.Year, data = ACSA)
summary(ACSA.abund.model)
plot(ACSA.abund.model)
check_model(ACSA.abund.model)
slopes = emtrends(ACSA.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)
# significant decrease in control and IC+TSI, Control and IC significantly differ

ACSA.plot = plot_model(ACSA.abund.model, type = "pred", terms = c("scale.Year","Treatment"),
                       line.size = 1.5, alpha = 0.1)+
  theme_classic(base_size = 15)+
  scale_x_continuous(
    breaks = -3.02399999999989:2.97600000000011,
    labels = 2019:2025)+
  scale_color_manual(
    values = c(
      "Control" = "#CFA3EE",
      "IC" = "#548F01",
      "IC+TSI" = "#4E5462"),
    labels = c(
      "Control" = "C",
      "IC" = "CD",
      "IC+TSI" = "CD+ISR"))+
  scale_fill_manual(
    values = c(
      "Control" = "#CFA3EE",
      "IC" = "#548F01",
      "IC+TSI" = "#4E5462"),
    labels = c(
      "Control" = "C",
      "IC" = "CD",
      "IC+TSI" = "CD+ISR"))+
  theme(legend.position = "none")+
  labs(x = "Year", y= "Abundance", title = "Acer saccharum (sugar maple)")
ACSA.plot

LITU.abund.model = glm.nb(alive~Treatment*scale.Year, data = LITU)
summary(LITU.abund.model)
plot(LITU.abund.model)
check_model(LITU.abund.model)
slopes = emtrends(LITU.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)
# significant increase in IC and IC+TSI, no difference in treatments

LITU.plot = plot_model(LITU.abund.model, type = "pred", terms = c("scale.Year","Treatment"),
                       line.size = 1.5, alpha = 0.1)+
  theme_classic(base_size = 15)+
  scale_x_continuous(
    breaks = -3.02399999999989:2.97600000000011,
    labels = 2019:2025)+
  scale_color_manual(
    values = c(
      "Control" = "#CFA3EE",
      "IC" = "#548F01",
      "IC+TSI" = "#4E5462"),
    labels = c(
      "Control" = "C",
      "IC" = "CD",
      "IC+TSI" = "CD+ISR"))+
  scale_fill_manual(
    values = c(
      "Control" = "#CFA3EE",
      "IC" = "#548F01",
      "IC+TSI" = "#4E5462"),
    labels = c(
      "Control" = "C",
      "IC" = "CD",
      "IC+TSI" = "CD+ISR"))+
  theme(legend.position = "none")+
  labs(x = "Year", y= "Abundance", title = "Liriodendron tulipifera (tulip tree)")
LITU.plot

QURU.abund.model = glm.nb(alive~Treatment*scale.Year, data = QURU)
summary(QURU.abund.model)
plot(QURU.abund.model)
check_model(QURU.abund.model)
slopes = emtrends(QURU.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)
# significant increase in control, no difference in treatments

QURU.plot = plot_model(QURU.abund.model, type = "pred", terms = c("scale.Year","Treatment"),
                       line.size = 1.5, alpha = 0.1)+
  theme_classic(base_size = 15)+
  scale_x_continuous(
    breaks = -3.02399999999989:2.97600000000011,
    labels = 2019:2025)+
  scale_color_manual(
    values = c(
      "Control" = "#CFA3EE",
      "IC" = "#548F01",
      "IC+TSI" = "#4E5462"),
    labels = c(
      "Control" = "C",
      "IC" = "CD",
      "IC+TSI" = "CD+ISR"))+
  scale_fill_manual(
    values = c(
      "Control" = "#CFA3EE",
      "IC" = "#548F01",
      "IC+TSI" = "#4E5462"),
    labels = c(
      "Control" = "C",
      "IC" = "CD",
      "IC+TSI" = "CD+ISR"))+
  theme(legend.position = "none")+
  labs(x = "Year", y= "Abundance", title = "Quercus rubra (red oak)")
QURU.plot

PRSE.abund.model = glm.nb(alive~Treatment*scale.Year, data = PRSE)
summary(PRSE.abund.model)
plot(PRSE.abund.model)
check_model(PRSE.abund.model)
slopes = emtrends(PRSE.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

PRSE.plot = plot_model(PRSE.abund.model, type = "pred", terms = c("scale.Year","Treatment"),
                       line.size = 1.5, alpha = 0.1)+
  theme_classic(base_size = 15)+
  scale_x_continuous(
    breaks = -3.02399999999989:2.97600000000011,
    labels = 2019:2025)+
  scale_color_manual(
    values = c(
      "Control" = "#CFA3EE",
      "IC" = "#548F01",
      "IC+TSI" = "#4E5462"),
    labels = c(
      "Control" = "C",
      "IC" = "CD",
      "IC+TSI" = "CD+ISR"))+
  scale_fill_manual(
    values = c(
      "Control" = "#CFA3EE",
      "IC" = "#548F01",
      "IC+TSI" = "#4E5462"),
    labels = c(
      "Control" = "C",
      "IC" = "CD",
      "IC+TSI" = "CD+ISR"))+
  theme(legend.position = "none")+
  labs(x = "Year", y= "Abundance", title = "Prunus serotina (black cherry)")
PRSE.plot

FRAM.abund.model = glm.nb(alive~Treatment*scale.Year, data = FRAM)
summary(FRAM.abund.model)
plot(FRAM.abund.model)
check_model(FRAM.abund.model)
slopes = emtrends(FRAM.abund.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)
# significant decreases for all treatments

FRAM.plot = plot_model(FRAM.abund.model, type = "pred", terms = c("scale.Year","Treatment"),
                       line.size = 1.5, alpha = 0.1)+
  theme_classic(base_size = 15)+
  scale_x_continuous(
    breaks = -3.02399999999989:2.97600000000011,
    labels = 2019:2025)+
  scale_color_manual(
    values = c(
      "Control" = "#CFA3EE",
      "IC" = "#548F01",
      "IC+TSI" = "#4E5462"),
    labels = c(
      "Control" = "C",
      "IC" = "CD",
      "IC+TSI" = "CD+ISR"))+
  scale_fill_manual(
    values = c(
      "Control" = "#CFA3EE",
      "IC" = "#548F01",
      "IC+TSI" = "#4E5462"),
    labels = c(
      "Control" = "C",
      "IC" = "CD",
      "IC+TSI" = "CD+ISR"))+
  theme(legend.position = "none")+
  labs(x = "Year", y= "Abundance", title = "Fraxinus americana (white ash)")
FRAM.plot

plot_grid(FRAM.plot,ACRU.plot,ACSA.plot,PRSE.plot,LITU.plot,QURU.plot)

ggsave("./Plots/common.sp.abundance.png", width = 14, height = 8.5, dpi = 300)












QURU.abund.model.pre.mast = glm.nb(alive~Treatment*scale.Year, data = QURU.pre.mast)
summary(QURU.abund.model.pre.mast)
plot(QURU.abund.model.pre.mast)
check_model(QURU.abund.model.pre.mast)
slopes = emtrends(QURU.abund.model.pre.mast, specs = "Treatment", var = "scale.Year")
pairs(slopes)

ggplot(QURU.pre.mast, aes(x = Year, y = alive, color = Treatment))+
  geom_smooth(method = "lm")

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
# control significantly declining, IC+TSI significantly increasing, IC increasing
# control and IC+TSI significantly differ from each other.


plot_model(mort.rate.mod, type = "pred", terms = c("scale.Year","Treatment"),
           line.size = 1.5, alpha = 0.1)+
  theme_classic(base_size = 15)+
  scale_x_continuous(
    breaks = -3:3,
    labels = 2019:2025)+
  scale_color_manual(
    values = c(
      "Control" = "#CFA3EE",
      "IC" = "#548F01",
      "IC+TSI" = "#4E5462"),
    labels = c(
      "Control" = "C",
      "IC" = "CD",
      "IC+TSI" = "CD+ISR"))+
  scale_fill_manual(
    values = c(
      "Control" = "#CFA3EE",
      "IC" = "#548F01",
      "IC+TSI" = "#4E5462"),
    labels = c(
      "Control" = "C",
      "IC" = "CD",
      "IC+TSI" = "CD+ISR"))+
  labs(x = "Year", y= "Probability of Mortality", title = NULL)

ggsave("./Plots/mortality.png", width = 6, height = 4, dpi = 300)

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


#### Seeding to Sapling ####
# sapling is dbh > 1 cm

sapling = early %>% 
  filter(Diameter >= 10) %>% 
  filter(Banded_Year != 2025)

# split by treatment
C.sapling = sapling %>% 
  filter(Treatment == "Control")
unique(C.sapling$Band)
IC.sapling = sapling %>% 
  filter(Treatment == "IC")
unique(IC.sapling$Band)
# mistake for 1914, 173, 141
IC.TSI.sapling = sapling %>% 
  filter(Treatment == "IC+TSI")
unique(IC.TSI.sapling$Band)
# mistake for 1623,651, 146, 123, 520, 612, 596, 1718, 1738, 728, 434, 

# split early by treatment
C.early = early %>% 
  filter(Treatment == "Control") %>% 
  filter(Banded_Year != 2025)
unique(C.early$Band)
IC.early = early %>% 
  filter(Treatment == "IC") %>% 
  filter(Banded_Year != 2025)
unique(IC.early$Band)
IC.TSI.early = early %>% 
  filter(Treatment == "IC+TSI") %>% 
  filter(Banded_Year != 2025)
unique(IC.TSI.early$Band)

6/502*100
32/425*100
34/926*100

table(sapling$Species)
sapling.2 = sapling %>% 
  filter(!Band %in% c(1914, 173, 141,1623,651, 146, 123, 520, 612, 596, 1718, 1738, 728, 434))
table(sapling.2$Species)

#### richness over years for each treatment ####

richness = early %>% 
  group_by(Treatment, Year, Species) %>% 
  summarise(Count = n(), .groups = "drop") %>%  
  ungroup()

species_richness <- early %>%
  group_by(Treatment, Year) %>%
  summarise(
    n_species = n_distinct(Species),
    .groups = "drop"
  )

# summarizing by plot 
species.richness.plot = early %>% 
  group_by(Plot_ID,Treatment,Year,Block) %>% 
  summarise(
    n_species = n_distinct(Species),
    .groups = "drop"
  )

species.richness.plot$scale.Year = scale(species.richness.plot$Year, center = TRUE, scale = FALSE)

richness.model = glmer.nb(n_species~Treatment*scale.Year + Block + (1|Plot_ID), data = species.richness.plot)
summary(richness.model)
plot(richness.model)
slopes = emtrends(richness.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

ggplot(species.richness.plot, aes(x = Year, y = n_species, color = Treatment))+
  geom_smooth(method = "lm")

#### species diversity metrics ####

library(vegan)

# make the community data matrix
species.richness.plot$Full_Plot_info = paste(species.richness.plot$Treatment,species.richness.plot$Year,species.richness.plot$Block, sep = "_")
abund.cdm = matrix(data=NA, nrow = 63, ncol = 40)
row.names(abund.cdm) = as.factor(unique(species.richness.plot$Full_Plot_info))
colnames(abund.cdm) = sort(as.factor(unique(richness$Species)))

#write.csv(abund.cdm, file = "./Formatted.Data/abund.cdm.csv")

richness.block = early %>% 
  group_by(Treatment, Year, ,Block, Species) %>% 
  summarise(Count = n(), .groups = "drop") %>%  
  ungroup()

abund.cdm = read.csv("./Formatted.Data/abund.cdm.csv", header = T, row.names = 1)

# calculate species richness
richness = specnumber(abund.cdm)

# Fisher's Alpha
alpha = fisher.alpha(abund.cdm)

# Inverse Simpson's Diversity
invsimp = diversity(abund.cdm, index = "invsimpson")

# evenness
even = invsimp/richness

# merge metrics together
diversity.dat = as.data.frame(cbind(alpha,even,richness))
diversity.dat = rownames_to_column(diversity.dat, "Full_Plot_info")
diversity.dat.2 = diversity.dat %>% 
  separate_wider_delim(cols = Full_Plot_info, delim = "_", names = c("Treatment","Year","Block"))

diversity.dat.2$Year = as.numeric(diversity.dat.2$Year)
diversity.dat.2$scale.Year = scale(diversity.dat.2$Year, center = TRUE, scale = FALSE)

# alpha diversity model
alpha.model = lm(alpha~Treatment*scale.Year + Block, data = diversity.dat.2)
summary(alpha.model)
plot(alpha.model)
slopes = emtrends(alpha.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

ggplot(diversity.dat.2, aes(x = scale.Year, y = alpha, color = Treatment))+
  geom_smooth(method = "lm")

# evenness model
evenness.model = lm(even~Treatment*scale.Year + Block, data = diversity.dat.2)
summary(evenness.model)
plot(evenness.model)
slopes = emtrends(evenness.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

ggplot(diversity.dat.2, aes(x = scale.Year, y = alpha, color = Treatment))+
  geom_smooth(method = "lm")

# richness model
richness.model = lm(richness~Treatment*scale.Year + Block, data = diversity.dat.2)
summary(richness.model)
plot(richness.model)
slopes = emtrends(richness.model, specs = "Treatment", var = "scale.Year")
pairs(slopes)

ggplot(diversity.dat.2, aes(x = scale.Year, y = alpha, color = Treatment))+
  geom_smooth(method = "lm")



test = early %>% 
  filter(Year == 2019)
test.2 = test %>% 
  filter(Tree == 4)
