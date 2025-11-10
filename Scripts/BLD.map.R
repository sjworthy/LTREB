library(tidyverse)
library(BIEN)
library(maps)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(terra)
library(readxl)


# get range map from BEIN
# BIEN ranges are WG84

(FAGR.range.sf <- BIEN_ranges_load_species('Fagus grandifolia'))

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
continental_states <- us_states %>%
  filter(!NAME %in% (c("Alaska","American Samoa","Guam","Commonwealth of the Northern Mariana Islands","Hawaii","United States Virgin Islands",
                       "Puerto Rico")))
states.map = continental_states %>%
  st_as_sf %>%
  st_transform(st_crs(FAGR.range.sf))

ggplot()+
  geom_sf(data = states.map)+
  geom_sf(data = FAGR.range.sf, col = "red")

FAGR.range.2 = terra::vect(FAGR.range.sf)
states.map.2 = terra::vect(states.map)
FAGR.range.3 = terra::intersect(FAGR.range.2, states.map.2)
FAGR.range.4 = st_as_sf(FAGR.range.3)

ggplot()+
  geom_sf(data = states.map, fill = "white")+
  geom_sf(dat = FAGR.range.4)+
  theme_classic()

# Combine all polygons into a single geometry for the outer border
FAGR.range.outer <- st_union(FAGR.range.4)

ggplot() +
  geom_sf(data = states.map, fill = "white", color = "black") +
  geom_sf(data = FAGR.range.outer, fill = NA, color = "red", size = 1) +
  theme_classic()

BLD_counties = counties(state = c("MI","OH","PA","NY","NJ","MD","DE","VT","NH",
                                  "MA","CT","RI","ME","VA","WV"))

BLD_counties.2 = st_drop_geometry(BLD_counties)
write.csv(BLD_counties.2, file = "Formatted.Data/BLD.counties.csv")

BLD.counties.sf = BLD_counties %>% 
  st_as_sf %>%
  st_transform(st_crs(FAGR.range.sf))

ggplot()+
  geom_sf(data = states.map, fill = "white")+
  geom_sf(data = FAGR.range.4)+
  geom_sf(data = BLD.counties.sf)+
  theme_classic()

# subset states

states.map.BLD = states.map %>% 
  filter(NAME %in% c("Michigan","Ohio","Pennsylvania",
                     "Maryland","West Virginia",
                     "New Jersey","New York", "Rhode Island",
                     "Virgnia","Delaware","Connecticut",
                     "Massachusetts","New Hampshire",
                     "Vermont", "Maine", "Illinois", "Indiana",
                     "Kentucky","Wisconsin","North Carolina", "Tennessee"))

ggplot()+
  geom_sf(data = states.map.BLD)+
  geom_sf(data = BLD.counties.sf)+
  theme_classic()

# read in the years

BLD.years = read_excel("Formatted.Data/BLD.counties.xlsx")
BLD.years.2 = BLD.years %>% 
  mutate(across(c(INTPTLAT,INTPTLON), as.character))

BLD.years.3 = BLD.years.2 %>% 
  select(COUNTYNS, BLD.Year)

BLD.counties.sf.2 <- BLD.counties.sf %>%
  left_join(BLD.years.3)

BLD.counties.sf.3 <- BLD.counties.sf.2 %>%
  mutate(BLD.Year = na_if(BLD.Year, "NA"))

ggplot()+
  geom_sf(data = states.map.BLD,fill = NA, color = "black", linewidth = 1)+
  geom_sf(data = BLD.counties.sf.3,aes(fill = BLD.Year), color = "black",linewidth = 0.15) +
  scale_fill_viridis_d(option = "plasma", name = "Year", na.value = "gray80") +
  theme_classic()




