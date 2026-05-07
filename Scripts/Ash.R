library(tidyverse)
library(lmerTest)
library(performance)
library(emmeans)
library(MASS)

# read in data, includes 2019-2025
seedlings = read.csv("./Formatted.Data/all.seedling.years.csv", row.names = 1)

# just want early censuses 
early = seedlings %>% 
  filter(Period == "early")

# Remove ash # 3858
early.NA = early %>% 
  filter(!Species == "FRAM")

3858/7573 # 51% of all seedlings over the study were ash

table(early$Year,early$Species)
rowSums(table(early$Year,early$Species))
# 2019: 612/1005 = 61%
# 2020: 605/1132 = 53%
# 2021: 577/1114 = 52%
# 2022: 551/1034 = 53%
# 2023: 521/1012 = 51%
# 2024: 500/1125 = 44%
# 2025: 492/1151 = 43%

#### Abundance over Time ####

abundance = early %>% 
  group_by(Treatment, Year) %>% 
  summarize(alive = sum(Use_Alive, na.rm = TRUE),
            dead = sum(Use_Dead, na.rm = TRUE)) %>% 
  ungroup()

# summarizing by plot 
abundance.plot = early %>% 
  group_by(Plot_ID,Treatment,Year,Block) %>% 
  summarize(alive = sum(Use_Alive, na.rm = TRUE),
            dead = sum(Use_Dead, na.rm = TRUE)) %>% 
  ungroup()

# Make year continuous
abundance.plot$scale.Year = scale(abundance.plot$Year, center = TRUE, scale = FALSE)

# negative binomial model of abundance
abund.model = glmer.nb(alive~Treatment*scale.Year + Block + (1|Plot_ID), data = abundance.plot)
summary(abund.model)
plot(abund.model)
slopes = emtrends(abund.model, specs = "Treatment", var = "scale.Year")
# slopes are not significant for any treatement
pairs(slopes)
# No significant differences among treatments

abund.pred = as.data.frame(emmip(abund.model, Treatment~scale.Year, CIs = TRUE, 
                   at = list(scale.Year = unique(abundance.plot$scale.Year)),
                   type = "response", plotit = FALSE))

abund.model.plot = ggplot(abundance.plot, aes(x = scale.Year, y = alive, colour = Treatment))+
  geom_point(alpha = 0.5)+
  theme_classic(base_size = 15)+
  labs(y = "Number of Seedlings", x = "Year")
abund.model.plot

abund.model.plot.2 = abund.model.plot +
  geom_line(data = abund.pred, aes(x = scale.Year, y = yvar, colour = Treatment),
            linewidth = 2)+
  ggtitle("All Seedlings")
  #geom_errorbar(data = abund.pred, aes(x = scale.Year, ymin = LCL, ymax = UCL, colour = Treatment),
                #inherit.aes = FALSE, width = 0.2)
  
abund.model.plot.2

## Without Ash

# summarizing by plot 
abundance.NA.plot = early.NA %>% 
  group_by(Plot_ID,Treatment,Year,Block) %>% 
  summarize(alive = sum(Use_Alive, na.rm = TRUE),
            dead = sum(Use_Dead, na.rm = TRUE)) %>% 
  ungroup()

# Make year continuous
abundance.NA.plot$scale.Year = scale(abundance.NA.plot$Year, center = TRUE, scale = FALSE)

# negative binomial model of abundance
abund.NA.model = glmer.nb(alive~Treatment*scale.Year + Block + (1|Plot_ID), data = abundance.NA.plot)
summary(abund.NA.model)
plot(abund.NA.model)
slopes = emtrends(abund.NA.model, specs = "Treatment", var = "scale.Year")
# Abundance significantly increases in all treatments over time
pairs(slopes)
# No significant differences among treatments

abund.pred.NA = as.data.frame(emmip(abund.NA.model, Treatment~scale.Year, CIs = TRUE, 
                                 at = list(scale.Year = unique(abundance.NA.plot$scale.Year)),
                                 type = "response", plotit = FALSE))

abund.model.NA.plot = ggplot(abundance.NA.plot, aes(x = scale.Year, y = alive, colour = Treatment))+
  geom_point(alpha = 0.5)+
  theme_classic(base_size = 15)+
  labs(y = "Number of Seedlings", x = "Year")
abund.model.NA.plot

abund.model.NA.plot.2 = abund.model.NA.plot +
  geom_line(data = abund.pred.NA, aes(x = scale.Year, y = yvar, colour = Treatment),
            linewidth = 2)+
  ggtitle("No Ash Seedlings")
#geom_errorbar(data = abund.pred, aes(x = scale.Year, ymin = LCL, ymax = UCL, colour = Treatment),
#inherit.aes = FALSE, width = 0.2)

abund.model.NA.plot.2



#### Relative Abundance over Time ####

relative.abundance = abundance %>% 
  group_by(Treatment) %>% 
  mutate(baseline_2019 = alive[Year == 2019],
         rel.abund = alive / baseline_2019) %>%
  ungroup()


