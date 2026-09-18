library(tidyverse)
library(BIEN)
library(maps)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(terra)
library(readxl)
library(wesanderson)
library(cowplot)

# get range map from BEIN
# BIEN ranges are WG84
(FAGR.range.sf <- BIEN_ranges_load_species('Fagus grandifolia'))
test = BIEN_ranges_species(species = "Fagus grandifolia")

# plot beech range
ggplot(FAGR.range.sf)+
  geom_sf()

# need map of North America with states
maps::map("usa")

# Add Canada on top
maps::map("world", regions = "Canada", add = TRUE)
plot(FAGR.range.sf$geometry, add = T)

# need map of US states with counties
maps::map(database = "county")
plot(FAGR.range.sf$geometry, add = T)

us_states <- states(cb = TRUE)
# subset for only continental states
continental_states <- us_states %>%
  filter(!NAME %in% (c("Alaska","American Samoa","Guam","Commonwealth of the Northern Mariana Islands","Hawaii","United States Virgin Islands",
                       "Puerto Rico")))

# match the crs between fagus range and states map
states.map = continental_states %>%
  st_as_sf %>%
  st_transform(st_crs(FAGR.range.sf))

# us map with beech range outlined
ggplot()+
  geom_sf(data = states.map)+
  geom_sf(data = FAGR.range.sf, col = "red")

# changing range and states map to vectors so that they can be nicely intersected
FAGR.range.2 = terra::vect(FAGR.range.sf)
states.map.2 = terra::vect(states.map)
FAGR.range.3 = terra::intersect(FAGR.range.2, states.map.2)
FAGR.range.4 = st_as_sf(FAGR.range.3) # convert to an sf object

# US map with beech range and state labels
ggplot()+
  geom_sf(data = states.map, fill = "white")+
  geom_sf(data = FAGR.range.4)+
  geom_sf_text(data = states.map, aes(label = STUSPS), size = 4)+
  labs(x = "Longitude", y = "Latitude")+
  theme_classic(base_size = 15)

#ggsave("./Plots/us.map.png", width = 8, height = 10, dpi = 300)
#ggsave("./Plots/us.map.svg", width = 8, height = 10)

map.1 = 
  ggplot()+
  geom_sf(data = states.map, fill = "white")+
  geom_sf(data = FAGR.range.4)+
  geom_sf_text(data = states.map, aes(label = STUSPS), size = 4)+
  labs(x = "Longitude", y = "Latitude")+
  theme_classic(base_size = 15)

map.2 = 
  ggplot()+
  geom_sf(data = states.map, fill = "white")+
  geom_sf(data = FAGR.range.4)+
  geom_sf_text(data = states.map, aes(label = STUSPS), size = 4)+
  labs(x = "Longitude", y = "Latitude")+
  theme_classic(base_size = 15)

plot_grid(map.1, map.2, labels = c("A.", "B."), ncol = 2) # doing this to get letters for making figure

#ggsave("./Plots/Fig.1.letters.png", width = 8, height = 10, dpi = 300)

# Combine all polygons into a single geometry for the outer border of beech range
FAGR.range.outer <- st_union(FAGR.range.4)

ggplot() +
  geom_sf(data = states.map, fill = "white", color = "black") +
  geom_sf(data = FAGR.range.outer, fill = NA, color = "red", size = 1) +
  theme_classic()

BLD_counties = counties(state = c("MI","OH","PA","NY","NJ","MD","DE","VT","NH",
                                  "MA","CT","RI","ME","VA","WV","NC"), cb = TRUE)

counties_beech_range = counties(state = c("MI","OH","PA","NY","NJ","MD","DE","VT","NH",
                                          "MA","CT","RI","ME","VA","WV","NC","WI",
                                          "IL","AR","OK","TX","LA","MS","AL","GA","FL",
                                          "IN","KY","TN","SC","MO"), cb = TRUE)

BLD_counties.2 = st_drop_geometry(BLD_counties)
#write.csv(BLD_counties.2, file = "Formatted.Data/BLD.counties.csv")

beech_counties = st_drop_geometry(counties_beech_range)
write.csv(beech_counties, file = "Formatted.Data/beech.counties.csv")

# matching counties crs to beech range crs
BLD.counties.sf = BLD_counties %>% 
  st_as_sf %>%
  st_transform(st_crs(FAGR.range.sf))

beech.counties.sf = counties_beech_range %>% 
  st_as_sf %>%
  st_transform(st_crs(FAGR.range.sf))

# Find counties that intersect the FAGR range
beech.counties.overlap <- beech.counties.sf[
  lengths(st_intersects(beech.counties.sf, FAGR.range.4)) > 0,]

beech_counties_overlap = st_drop_geometry(beech.counties.overlap)
write.csv(beech_counties_overlap, file = "Formatted.Data/beech.counties.overlap.csv")

# map of US with fagus range with counties for states with BLD
ggplot()+
  geom_sf(data = states.map, fill = "white")+
  geom_sf(data = FAGR.range.4)+
  geom_sf(data = BLD.counties.sf)+
  theme_classic()

# map of US with fagus range with counties for states with and without BLD
ggplot()+
  geom_sf(data = states.map, fill = "white")+
  geom_sf(data = FAGR.range.4)+
  geom_sf(data = beech.counties.sf)+
  theme_classic()

# subset states to only those with BLD
states.map.BLD = states.map %>% 
  filter(NAME %in% c("Michigan","Ohio","Pennsylvania",
                     "Maryland","West Virginia",
                     "New Jersey","New York", "Rhode Island",
                     "Virgnia","Delaware","Connecticut",
                     "Massachusetts","New Hampshire",
                     "Vermont", "Maine", "Illinois", "Indiana",
                     "Kentucky","Wisconsin","North Carolina", "Tennessee"))

# subset states to only those with beech
states.map.Beech = states.map %>% 
  filter(NAME %in% c("Michigan","Ohio","Pennsylvania",
                     "Maryland","West Virginia",
                     "New Jersey","New York", "Rhode Island",
                     "Virgnia","Delaware","Connecticut",
                     "Massachusetts","New Hampshire",
                     "Vermont", "Maine", "Illinois", "Indiana",
                     "Kentucky","Wisconsin","North Carolina", "Tennessee",
                     "Texas","Wisconsin","Alabama","Arkansas","Florida",
                     "Georgia","Louisiana","Mississippi","Missouri","Oklahoma",
                     "South Carolina"))

# map of only states with BLD with counties and surrounding states
ggplot()+
  geom_sf(data = states.map.BLD)+
  geom_sf(data = BLD.counties.sf)+
  theme_classic()

ggplot()+
  geom_sf(data = states.map.Beech)+
  geom_sf(data = beech.counties.overlap)+
  theme_classic()

# read in the dataframe with counties names and year of infection output from shp file
BLD.years = read_excel("Formatted.Data/BLD.counties.xlsx")
BLD.years.2 = BLD.years %>% 
  mutate(across(c(INTPTLAT,INTPTLON), as.character))

# read in the dataframe with all beech counties 
# plus names and year of infection output from shp file
beech.counties.years = read_excel("Formatted.Data/beech.counties.xlsx")
beech.counties.years.2 = beech.counties.years %>% 
  mutate(across(c(INTPTLAT,INTPTLON), as.character))

# slimming data to what we need
BLD.years.3 = BLD.years.2 %>% 
  select(COUNTYNS, BLD.Year)

# slimming data to what we need for beech
beech.counties.years.3 = beech.counties.years.2 %>% 
  select(COUNTYNS, BLD.Year)

# joining the dataframes 
BLD.counties.sf.2 <- BLD.counties.sf %>%
  left_join(BLD.years.3)

# joining the dataframes for beech counties
beech.counties.sf.2 <- beech.counties.sf %>%
  left_join(beech.counties.years.3)

test = st_drop_geometry(beech.counties.sf)
test.2 = st_drop_geometry(beech.counties.sf.2)

write.csv(test, file = "test.csv")


# changing NA to "NA" so it doesn't print
BLD.counties.sf.3 <- BLD.counties.sf.2 %>%
  mutate(BLD.Year = na_if(BLD.Year, "NA"))

ggplot()+
  geom_sf(data = BLD.counties.sf.3,aes(fill = BLD.Year), color = "black",linewidth = 0.15) +
  scale_fill_viridis_d(option = "plasma", name = "Year", na.value = "gray80") +
  geom_sf(data = states.map.BLD,fill = NA, color = "black", linewidth = 1)+
  theme_classic()

# changing colors of years
cols = wes_palette("Zissou1", n=14, type = "continuous")
cols.2 = rev(cols)

ggplot()+
  geom_sf(data = BLD.counties.sf.3,aes(fill = BLD.Year), color = "black",linewidth = 0.15) +
  scale_fill_manual(values = c(cols.2,"gray80"), na.value = "gray80", name = "Year",
                    drop = TRUE, na.translate = FALSE)+
  geom_sf(data = states.map.BLD,fill = NA, color = "black", linewidth = 1)+
  theme_classic(base_size = 15)+
  theme(legend.position = "none")+
  labs(x = "Longitude", y = "Latitude")

#ggsave("./Plots/BLD.map.no.legend.png", width = 8, height = 10, dpi = 300)
#ggsave("./Plots/BLD.map.legend.png", width = 8, height = 10, dpi = 300)

# changing colors of years
cols = wes_palette("Zissou1", n=14, type = "continuous")
cols.2 = rev(cols)
cols.3 = c("#F11B00", "#EF4902", "#ED6904", "#EA8305", "#E69A05", "#E3B20E", "#DEC336", "#D0C961",
           "#B9C786", "#A6C2A0", "#97BCB0", "#81B6BB", "#62ACBD", "#3A9AB2", "lightgray")

year_colors <- setNames(
  cols.3,
  c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019,
    2020, 2021, 2022, 2023, 2024, 2025, 0))

ggplot()+
  geom_sf(data = beech.counties.sf.2, 
          aes(fill = factor(BLD.Year, levels = c(2012,2013,2014,2015,2016,2017,2018,
                                                 2019,2020,2021,2022,2023,2024,2025,0))), color = "black",linewidth = 0.15) +
  scale_fill_manual(values = year_colors, na.value = NA, name = "Year of Infection",
                    labels = c(
                      "2012", "2013", "2014", "2015", "2016", "2017", "2018",
                      "2019", "2020", "2021", "2022", "2023", "2024", "2025",
                      "Uninfected"),
                    drop = TRUE, na.translate = FALSE)+
  geom_sf(data = states.map.Beech,fill = NA, color = "black", linewidth = 1)+
  theme_classic(base_size = 15)+
  #theme(legend.position = "none")+
  labs(x = "Longitude", y = "Latitude")

#ggsave("./Plots/Beech.plus.BLD.map.no.legend.png", width = 8, height = 10, dpi = 300)
#ggsave("./Plots/Beech.plus.BLD.map.legend.png", width = 8, height = 10, dpi = 300)

# state map with labels

library(usmap)

plot_usmap(regions = "states", exclude = c("AK","HI"), labels = TRUE) + 
  theme(panel.background=element_blank())

test = st_drop_geometry(beech.counties.sf.2)
