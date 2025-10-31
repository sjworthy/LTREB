

Steb.herbs = read.csv("Formatted.Data/LT.Plots.Total.Herb.Cover.csv")

ggplot(Steb.herbs, aes(x = Year, y = Mean.Total.Cover, group = Plot)) +
  geom_line()

steb.means.herbs = Steb.herbs %>% 
  group_by(Year) %>% 
  summarise(mean.cover = mean(Mean.Total.Cover, na.rm = TRUE),
            sd.cover = sd(Mean.Total.Cover, na.rm = TRUE))

ggplot(Steb.herbs, aes(x = Year, y = Mean.Total.Cover)) +
  geom_line(aes(group = Plot), alpha = 0.3) +
  geom_line(data = steb.means.herbs, aes(y = mean.cover, group = 1), color = "red", size = 1) +
  geom_line(data = steb.means.herbs, aes(y = mean.cover + sd.cover, group = 1), color = "red", linetype = "dashed") +
  geom_line(data = steb.means.herbs, aes(y = mean.cover - sd.cover, group = 1), color = "red", linetype = "dashed") +
  theme_classic(base_size = 15) +
  labs(y = "Mean Total Herbaceous Cover", x = "Year", 
       title = "Total cover over time with mean ± SD")

herb.lm = lm(Mean.Total.Cover~Year, data = Steb.herbs)
# relationship is positive (0.22), but not significant (p = 0.48)

# filter to just look at since BLD started (2012)
Steb.herbs.2 = Steb.herbs %>% 
  filter(Year > 2011)

ggplot(Steb.herbs.2, aes(x = Year, y = Mean.Total.Cover, group = Plot)) +
  geom_line()

steb.means.herbs.2 = Steb.herbs.2 %>% 
  group_by(Year) %>% 
  summarise(mean.cover = mean(Mean.Total.Cover, na.rm = TRUE),
            sd.cover = sd(Mean.Total.Cover, na.rm = TRUE))

ggplot(Steb.herbs.2, aes(x = Year, y = Mean.Total.Cover)) +
  geom_line(aes(group = Plot), alpha = 0.3) +
  geom_line(data = steb.means.herbs.2, aes(y = mean.cover, group = 1), color = "red", size = 1) +
  geom_line(data = steb.means.herbs.2, aes(y = mean.cover + sd.cover, group = 1), color = "red", linetype = "dashed") +
  geom_line(data = steb.means.herbs.2, aes(y = mean.cover - sd.cover, group = 1), color = "red", linetype = "dashed") +
  theme_classic(base_size = 15) +
  labs(y = "Mean Total Herbaceous Cover", x = "Year", 
       title = "Total cover over time with mean ± SD since 2012")

herb.2.lm = lm(Mean.Total.Cover~Year, data = Steb.herbs.2)
# relationship is negative (-0.38), not significant (p = 0.41)
