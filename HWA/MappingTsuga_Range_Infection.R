###################Mapping Tsuga range and HWA infection year bins############# 
#
#
##################################################################################

library(ggspatial) #for north arrow and scale bar--- note this package can make an function using map() weird.. .

#Inputting range data from US Tree Atlas https://github.com/wpetry/USTreeAtlas
setwd("C:/Users/rschiafo/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Stuble Lab - Hemlock - Forest Health/Analysis/FIA Analysis/USTreeAtlas/shp")
TSCAN.range = st_read("tsugcana.geojson")
TSCAR.range = st_read("tsugcaro.geojson")
#plotting
  geom_sf()
ggplot(TSCAR.range)+
  geom_sf()

#

#Adding in CSV file with HWA infection bins based on shpfile
#Reading in HWA infection year data (#This is the data generated from the map by RS and is based on 10 year bins)
setwd("C:/Users/rschiafo/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Stuble Lab - Hemlock - Forest Health/Analysis/FIA Analysis/Input Data")
hwa_infect_years_bins<-read.csv("HWACounties_ALLYears_NoDuplicates_CSV_Working_Copy.csv")

#

#Adding in raster file with HWA infection year bins
hwa_raster <- rast("C:/Users/rschiafo/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Stuble Lab - Hemlock - Forest Health/Maps/HWASpreadAnalysis/HWASpreadAnalysis.gdb", 
                   subds = "HWACounties_AllYears_All_PolygonToRaster")
hwa_raster_df <- as.data.frame(hwa_raster, xy = TRUE)
hwa_raster_map<-ggplot(hwa_raster_df, aes(x = x, y = y, fill = HWACounties_AllYears_All_PolygonToRaster)) +
  geom_raster() +
  scale_fill_grey(name="Decade of HWA Infection") +
  coord_sf() # Important to keep spatial aspect ratios
hwa_raster_map

#

#Adding in shape file with HWA infection bins
setwd("C:/Users/rschiafo/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Stuble Lab - Hemlock - Forest Health/Maps/HWASpreadAnalysis")
hwa_shape<-st_read("HWACounties_AllYears_NoDuplicates_Shapefile.shp")

#

#Adding In State Map for clipping Tsuga boundaries to US (exclude canada)
states.map <- states(cb = TRUE, year = 2022) %>% 
  st_as_sf()
# subset for only continental states
states.map<- states.map %>%
  filter(!NAME %in% (c("Alaska","American Samoa","Guam","Commonwealth of the Northern Mariana Islands","Hawaii","United States Virgin Islands",
                       "Puerto Rico")))

#

#Adding in County Data
#Filtering my counties data by all target states
counties <- counties(cb = TRUE, year = 2020) #NOTE: using 2020 b/c after 2021 the CT counties changed to a 9-region setup that doesnt match the data I pulled from GIS
fips_master <- counties[, c("STATEFP", "COUNTYFP", "NAME", "NAMELSAD")]

counties<- counties %>% rename(STATEFP=STATEFP)
counties_working <- counties %>%
  filter(STATEFP=="01" | STATEFP=="09" | STATEFP=="10" | STATEFP=="13"  |STATEFP=="18" |	STATEFP=="21"  |	STATEFP=="23" |	STATEFP=="24" |	STATEFP=="25" |	STATEFP=="26" |	STATEFP=="33" |	STATEFP=="34" |	STATEFP=="36" |	STATEFP=="37" |	STATEFP=="39" |	STATEFP=="42" |	STATEFP=="44" |	STATEFP=="45" |	STATEFP=="47" |	STATEFP=="50" |	STATEFP=="51" |	STATEFP=="54" |	STATEFP=="55"|
           STATEFP=="17"| STATEFP=="28" |  STATEFP=="27") #note including Minnesota, Illinois and Mississippi as neighboring states to help reference this map compared to the whole US map... these are NOT target states included in other counties_working

#

###############################Edits to HWA infection years for plotting 
#Adding in Sf geometry to hwa_infect_years_bins
#HWA infection year bins from GIS- rename and select columns
hwa_infect_years_bins<-hwa_infect_years_bins %>%
  rename(COUNTY= NAMELSAD, INFECTION_YEAR_BIN= Infection_Year) #STATEFP= STATEFP, COUNTYFP= COUNTYFP, 


#FIPs - rename and select columns and make FIPS column with State and COUNTYFP combined (for merging with hwa_infect_years)
fips_working<- fips_master %>% 
  rename(STATEFP= STATEFP, COUNTYFP= COUNTYFP, COUNTY= NAMELSAD) %>% 
  mutate(FIPS = paste(STATEFP, COUNTYFP, sep = "")) %>% 
  relocate(FIPS, .before = STATEFP)


fips_working$STATEFP<-as.numeric(fips_working$STATEFP)
fips_working$COUNTYFP<-as.numeric(fips_working$COUNTYFP)
fips_working$FIPS<-as.numeric(fips_working$FIPS)


#merging hwa infection year bins with State and County IDs
hwa_infect_years_bins_full<-merge(hwa_infect_years_bins, fips_working, by=c("STATEFP", "COUNTYFP", "NAME", "COUNTY"), all = TRUE)
hwa_infect_years_bins_sf<-hwa_infect_years_bins_full %>% filter(INFECTION_YEAR_BIN!="NA")

#recasting as a sf dataframe
hwa_infect_years_bins_sf<- st_as_sf(hwa_infect_years_bins_sf)
hwa_infect_years_bins_sf #NAD 83

############


#Checking coordinate system--- want NAD83 
#State and county boundaries
st_crs(states.map) #good 
st_crs(counties_working) #good

#Range
st_crs(TSCAN.range) #need to set
st_crs(TSCAR.range) #need to set
#Setting range coordinate system to NAD83 (to match state maps)
TSCAN.range <- st_transform(TSCAN.range, st_crs(states.map))
st_crs(TSCAN.range) #good
TSCAR.range <- st_transform(TSCAR.range, st_crs(states.map))
st_crs(TSCAR.range) #good

#Hwa infection
st_crs(hwa_raster) #good 
st_crs(hwa_shape) #good
st_crs(hwa_infect_years_bins_sf) #good



#To Transforming for NAD83 geographic coordinate systme (degrees) to NAD83 Albers (meters) 
#county and state layers
#counties_working <- st_transform(counties_working, 5070)
#states.map<- st_transform(states.map, 5070)
#range data
#TSCAN.range <- st_transform(TSCAN.range, 5070)
#TSCAR.range <- st_transform(TSCAR.range, 5070)
#hwa infection
#hwa_infect_years_bins_sf <- st_transform(hwa_infect_years_bins_sf, 5070)
#hwa_shape <- st_transform(hwa_shape, 5070)


#Clipping range map to US 
TSCAN_clipped = st_intersection(TSCAN.range, states.map)
TSCAR_clipped = st_intersection(TSCAR.range, states.map)

#Clipping states.map to target states
states.map.target <- states.map %>%
  filter(STATEFP %in% counties_working$STATEFP)


#Mapping 

#Plotting just ranges
map.1<-ggplot()+
  geom_sf(data = states.map, fill = "white", linewidth=.5)+ # US map
  #geom_sf(data = counties_working, fill = "white", linewidth=.2, col="grey")+ # US map
  geom_sf(data = TSCAN_clipped, aes(col = "Tsuga canadensis"), fill="grey" ,alpha= 0.5, linewidth = .75) + # plots the outline as blue
 # geom_sf(data = TSCAR_clipped, aes(col = "Tsuga caroliniana"), fill="grey" ,alpha= 0.5, linewidth = .75) +# plots the outline as red
  scale_color_manual(name ="Species range", values = c("Tsuga canadensis" = "blue", "Tsuga caroliniana" = "red" ), guide="none")+
  labs(
    title = "A. Tsuga canadensis native range")+
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "bl", which_north = "true",                     #note: ignoring note about scale..says this b/c im in a geographic coordinate system NAD83 (degrees)- I tried NAD83 albers (in meters so technically no distortion at the poles) and the scale was the same but map looked crooked... stickign with this b/c i dont think scale is off that much (or at all)
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme_classic()
map.1 

#Plotting just HWA infection
map.2<-ggplot()+
  geom_sf(data = hwa_infect_years_bins_sf, aes(fill= INFECTION_YEAR_BIN))+
  scale_fill_viridis_d(name = "Decade of HWA Infection", option = "turbo", direction = -1, begin = 0.2, end = 0.8) +
  geom_sf(data = counties_working, fill = NA, linewidth=.5, col="grey")+ # US map
  geom_sf(data = states.map.target, fill = NA, linewidth=.5, col="black")+
  labs(
    title = "B. Hemlock Woolly Adelgid Infection")+
  annotation_scale(location = "bl") +
  #annotation_north_arrow(location = "bl", which_north = "true, pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering)+
  theme_classic()
map.2

#Merging two together 

map.combined<- ggarrange(map.1, map.2, ncol = 2)
map.combined  
  
#PLOTTING TSUGA RANGE AND INFECTION ALL ON ONE PLOT
#Plotting with CSV (best way)
map.3<-ggplot()+
  geom_sf(data = hwa_infect_years_bins_sf, aes(fill= INFECTION_YEAR_BIN))+
  geom_sf(data = counties_working, fill = NA, linewidth=.5, col="grey")+ 
  geom_sf(data = states.map.target, fill = NA, linewidth=.5, col="black")+
  scale_fill_viridis_d(name = "Decade of HWA Infection", option = "turbo", direction = -1, begin = 0.2, end = 0.8) +
  geom_sf(data = TSCAN_clipped, aes(col = "Tsuga canadensis"), fill="grey" ,alpha= 0.5, linewidth = .75) + # plots the outline as blue
  geom_sf(data = TSCAR_clipped, aes(col = "Tsuga caroliniana"), fill="grey" ,alpha= 0.5, linewidth = .75) +# plots the outline as red
  scale_color_manual(name ="Species range", values = c("Tsuga canadensis" = "blue", "Tsuga caroliniana" = "red" ))+
  theme_classic()
map.3


#Making a nice clean figure with T.canadaensis range, HWA spread, and 80%mort maps 
#Trying with patchwork
map_ranges<-(map.1 | map.2) + plot_layout(guides = "collect")+ plot_annotation(tag_levels = 'A') & theme(legend.position = 'right') 
map_ranges

test1<-(map_cumulative_mortality_1 | map_cumulative_mortality_1_subset_subset_subset) + plot_layout(guides = "collect") +  plot_annotation(tag_levels = 'A') & theme(legend.position = 'right')
test1

test_final<-test/test1 
test_final


#outputting graph
setwd("C:/Users/rschiafo/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Stuble Lab - Hemlock - Forest Health/Analysis/FIA Analysis/ROutput Figures")
ggsave("RangeMaps.png", plot = map_ranges, width = 24, height = 10, dpi = 600)



#Putting 80-90% plots on the HWA spread map 
#Plotting just HWA infection
map.4<-ggplot()+
  geom_sf(data = hwa_infect_years_bins_sf, aes(fill= INFECTION_YEAR_BIN))+
  scale_fill_viridis_d(name = "Decade of HWA Infection", option = "turbo", direction = -1, begin = 0.2, end = 0.8) +
  geom_sf(data = counties_working, fill = NA, linewidth=.5, col="grey")+ # US map
  geom_sf(data = states.map.target, fill = NA, linewidth=.5, col="black")+
  geom_sf(data = recent_cumulative_mortality_1_subset_subset_subset, aes(size = ORIG_COHORT_N), alpha = 0.8) +
  scale_size_continuous(range = c(1, 10),    # controls bubble size
                        breaks = c(1, 10, 20, 30, 40, 50, 60),
                        limits = c(0, 60),
                        name = "Original cohort size \n (# of trees)") + 
  labs(
    title = "Hemlock Woolly Adelgid Infection")+
  annotation_scale(location = "bl") +
  #annotation_north_arrow(location = "bl", which_north = "true, pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering)+
  theme_classic()
map.4

map_ranges_cohorts<-(map.1/map.4 | map.2) + plot_layout(guides = "collect")+ plot_annotation(tag_levels = 'A') & theme(legend.position = 'right') 
map_ranges_cohorts

#Plotting with shape file 
#ggplot()+
#  # geom_sf(data = states.map, fill = "white")+ # US map
#  geom_sf(data = counties_working, fill = "white")+ # US map
#  geom_sf(data = hwa_shape,
#          aes(fill = Infection_)) +
#  scale_fill_viridis_d(name = "Decade of HWA Infection", option = "turbo", direction = -1) +
#  geom_sf(data = TSCAN_clipped, col = "blue", fill="grey" ,alpha= 0.25, linewidth = .5) + # plots the outline as blue
#  geom_sf(data = TSCAR_clipped, col = "red", fill="grey" ,alpha= 0.25, linewidth = .5) # plots the outline as red
#
#
##Plotting with raster data
#ggplot()+
# # geom_sf(data = states.map, fill = "white")+ # US map
#  geom_sf(data = counties_working, fill = "white")+ # US map
#  geom_raster(data = hwa_raster_df, 
#  aes(x = x, y = y, fill = HWACounties_AllYears_All_PolygonToRaster)) +
#  scale_fill_viridis_d(name = "Decade of HWA Infection", option = "turbo", direction = -1) +
#  geom_sf(data = TSCAN_clipped, col = "blue", fill="grey" ,alpha= 0.5, linewidth = .5) + # plots the outline as blue
#  geom_sf(data = TSCAR_clipped, col = "red", fill="grey" ,alpha= 0.5, linewidth = .5) # plots the outline as red  

