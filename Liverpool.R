library(magrittr)
library(osmdata)
library(dodgr)
library(sf)
library(expss)
library(tmap)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(stringr)
library(leaflet)
library(tmaptools)
library(OpenStreetMap)
library(ggmap)
library(maptools)
library(spatstat)

#boundary data

liverpool <- st_read('data/boundaries/liverpool/england_lad_2011_clipped.shp')
liverpool <- liverpool%>% filter(str_detect(name, "Liverpool"))

liverpool_lsoa <- st_read('data/boundaries/liverpool/england_lsoa_2011_clipped.shp')
liverpool_lsoa <- liverpool_lsoa %>% filter(str_detect(name, "Liverpool"))

#extract point of interest and highway data for network analysis routing

bb <- c(-3.078232,53.316518,-2.743149,53.525207)#liverpool #bbfinder.com

assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

rm(list = ls())

osmdata <- opq(bbox = bb) %>%
  add_osm_feature(key = 'highway', value = c('primary', 'secondary', 'tertiary', 'residential','path','footway', 'unclassified','living_street', 'pedestrian')) %>% 
  osmdata_sf()

liverpool_nodes <- osmdata$osm_points[,"osm_id"]

liverpool_edges <- osmdata$osm_lines[,c("osm_id", "name", "highway","maxspeed", "oneway")]

#schools <- opq(bbox=bb) %>% 
  #add_osm_feature(key = 'amenity', value = c('school')) %>% 
  #osmdata_sf()

ff <- opq(bbox = bb) %>% 
  add_osm_feature(key = 'amenity', value = 'fast_food') %>% 
  osmdata_sf()

ff_points <- ff$osm_points[,c("osm_id", "name")]

poi <- st_read('data/poi/gis_osm_pois_a_free_1.shp')

schools <- poi %>% filter(fclass == "school")

schools_bng <- st_transform(schools, crs = 27700)

l_schools <- st_filter(schools_bng, liverpool, .pred = st_intersects)

ff_points <- st_transform(ff_points, crs = 27700)

ff_points <- st_filter(ff_points, liverpool, .pred = st_intersects)

l_schools <- st_transform(l_schools, crs = 4326)
ff_points <- st_transform(ff_points, crs = 4326)


#snap values to network

network_sp <- as(liverpool_edges, 'Spatial')

schools_sp <- as(l_schools, 'Spatial')

ff_sp <- as(ff_points, 'Spatial')

schools_snap <- maptools::snapPointsToLines(schools_sp, network_sp)

schools_snap <- st_as_sf(schools_snap)

ff_snap <- maptools::snapPointsToLines(ff_sp, network_sp)

ff_snap <- st_as_sf(ff_snap)

ff_snap$osm_id <- ff_points$osm_id
ff_snap$name <- ff_points$name

tmap_mode("view")

tm_shape(ff_snap)+
  tm_dots(col = "red")+
  tm_shape(schools_snap)+
  tm_dots(col = "green")+
  tm_shape(liverpool)+
  tm_fill(alpha = 0.8)

#create graph
graph <- weight_streetnet(liverpool_edges, wt_profile = "foot")

graph_connected <- graph[graph$component == 1,]

#create a matrix

sch_to_ff_calc <- dodgr_dists(graph_connected, from = st_coordinates(schools_snap),
                              to = st_coordinates(ff_snap), 
                              shortest = TRUE,
                              pairwise = FALSE,
                              quiet = FALSE)

sch_to_ff_times <- dodgr_times(graph_connected, from = st_coordinates(schools_snap),
                               to = st_coordinates(ff_snap), 
                               shortest = TRUE) #calculate fastest time


schools_snap$ff_within_400m <- count_row_if(lt(401), sch_to_ff_calc)
schools_snap$ff_within_800m <- count_row_if(lt(801), sch_to_ff_calc)  
schools_snap$mean_dist <- mean_row(sch_to_ff_calc)
schools_snap$average_dist_lt_800m <- mean_row_if(lt(801), sch_to_ff_calc)
schools_snap$min_dist <- min_row(sch_to_ff_calc)


schools_snap$lt_10mins <- count_row_if(lt(600), sch_to_ff_times) #10 mins = 600 seconds
schools_snap$lt_5mins <- count_row_if(lt(300), sch_to_ff_times) #5 mins = 300 seconds

school_data <- st_transform(schools_snap, crs = 27700)

ff_bng <- st_transform(ff_snap, crs = 27700)

#have a look

tm_shape(schools_snap)+
  tm_dots("lt_10mins",
          style = "jenks")

brewer.pal.info

tm_shape(school_data)+
  tm_dots("mean_dist",
          palette = "viridis",
          showNA = FALSE,
          colorNA = "white")+
  tm_shape(liverpool)+
  tm_polygons(alpha = 0.3)

imd <- st_read('data/imd/Indices_of_Multiple_Deprivation_IMD_2019.shp')

imd <- imd %>% filter(str_detect(lsoa11nm, "Liverpool"))

imd <- st_transform(imd, crs = 27700)

names(imd)

school_imd <- st_join(school_data,imd[,c("IMD_Decile", "lsoa11cd")])

school_lsoa <- st_join(imd[,c("IMD_Decile", "lsoa11cd")], school_data)


write.csv(school_imd,"data/exports//liverpool_data4.csv", row.names = FALSE)

#calculations

remove(mean)

mean_400 <- school_lsoa %>% 
  # Calculate total
  group_by(lsoa11cd) %>%
  summarise(mean_400 = mean(ff_within_400m, na.omit = TRUE)) %>%
  # Arrange in descending order by total
  arrange(desc(mean_400))

mean_800 <- school_lsoa %>% 
  # Calculate total
  group_by(lsoa11cd) %>%
  summarise(mean_800 = mean(ff_within_800m, na.rm = T)) %>% #change mean to sum
  # Arrange in descending order by total
  arrange(desc(mean_800))

mean_dist <- school_lsoa %>% 
  # Calculate total
  group_by(lsoa11cd) %>%
  summarise(mean_dist = mean(mean_dist, na.rm = T)) %>% #change mean to sum
  # Arrange in descending order by total
  arrange(desc(mean_dist))

lt800m <- school_lsoa %>% 
  # Calculate total
  group_by(lsoa11cd) %>%
  summarise(average_dist_lt_800m = mean(average_dist_lt_800m, na.rm = T)) %>% #change mean to sum
  # Arrange in descending order by total
  arrange(desc(average_dist_lt_800m))

lt10min <- school_lsoa %>% 
  # Calculate total
  group_by(lsoa11cd) %>%
  summarise(avg_lt_10min = mean(lt_10mins, na.rm = T)) %>% #change mean to sum
  # Arrange in descending order by total
  arrange(desc(avg_lt_10min))

lt5min <- school_lsoa %>% 
  # Calculate total
  group_by(lsoa11cd) %>%
  summarise(avg_lt_5min = mean(lt_5mins, na.rm = T)) %>% #change mean to sum
  # Arrange in descending order by total
  arrange(desc(avg_lt_5min))

top_10min <- school_data %>% filter(ff_within_800m>30)

qtm(top_10min)

#obesity

obesity <- read.csv(file = 'data/obesity/NCMP_data_Ward_update_2019.csv', header = TRUE)

wards <- st_read('data/boundaries/Wards__December_2015__Boundaries.shp')%>%  st_transform(., 27700)

qtm(wards)

#maps

#basemaps
osm <- get_map(bb, map.type = "toner-lite")
  
osm <-  read_osm(bb, type = "esri-topo")
osm <- st_transform(osm, crs = 27700)

stamen <- read_osm(bb, type = 'stamen-toner')
stamen <- st_transform(osm, crs = 27700)

tm_shape(osm)+
  tm_rgb()

#within800 <- school_imd %>% filter(ff_within_800m >0)

tm_shape(imd)+
  tm_polygons('IMD_Decile',
          title = "IMD Decile",
              alpha = 0.5,
              palette = 'YlGnBu')


  tm_borders()+
tm_shape(school_imd)+
  tm_bubbles(size = "ff_within_800m",
             col = "orange",
             border.col = "orange",
             title.size = "FF within 800m of a school")

tm_shape(imd)+
  tm_polygons('IMD_Decile',
              title = "IMD Decile",
              alpha = 0.5,
              palette = 'YlGnBu')+
  tm_borders()+
  tm_shape(school_imd)+
  tm_bubbles(size = "ff_within_400m",
             col = "orange",
             border.col = "orange",
             title.size = "FF within 400m of a school")+
  tm_compass(north = 0,
             position = c("right", "top"))

#mean800m

tm_shape(osm)+
  tm_rgb(alpha = 0.5)+
tm_shape(imd)+
  tm_polygons('IMD_Decile',
              title = "IMD Decile",
              alpha = 0.4,
              palette = 'YlGnBu')+
tm_shape(mean_800)+
  tm_bubbles("mean_800",
             col = "darkred",
             colorNA = "white",
             title.size = "Fast-food outlets <800m 
from a school (mean per LSOA)")+
  tm_compass(north = 0,
             position = c("right", "top"))+
  tm_layout(legend.bg.color = "white",
            legend.outside = TRUE,
            legend.frame = "black")+
  tm_scale_bar(position=c("left", "bottom"),
               breaks = c(0,1,2),
               text.size = 1)+
  tm_credits("(c) OpenStreetMap contrbutors", position=c("left", "bottom"))
  
  
#using school data
tm_shape(osm)+
  tm_rgb(alpha = 0.5)+
  tm_shape(imd)+
  tm_polygons('IMD_Decile',
              title = "IMD Decile",
              alpha = 0.5,
              palette = 'YlGnBu')+
  tm_shape(school_imd)+
  tm_bubbles("ff_within_800",
             col = "orange",
             colorNA = "white",
             title.size = "FF within 800m of a school")+
  tm_compass(north = 0,
             position = c("right", "top"))+
  tm_layout(legend.bg.color = "white")

#schools

UK_outline <- st_read('data/boundaries/gadm36_GBR_shp/gadm36_GBR_0.shp') %>% st_transform(., 27700)

newbb <- c(xmin=-296000, ymin=5408, xmax=655696, ymax=1000000)

UK_outlinecrop <- UK_outline$geometry %>%
  st_crop(., newbb)

Worldcities <- st_read('data/worldcities/World_Cities.shp') %>%  st_transform(., 27700)

Worldcities2 <- Worldcities %>%
  filter(CNTRY_NAME=='United Kingdom'&
           Worldcities$CITY_NAME=='London'|
           Worldcities$CITY_NAME=='Liverpool')

liverpoolbb <- st_bbox(liverpool_edges,crs = st_crs(school_data)) %>% 
  st_as_sfc()

map <- tm_shape(osm)+
  tm_rgb(alpha = 0.5)+
tm_shape(liverpool)+
  tm_borders(lwd = 1.5)+
tm_shape(school_data)+
  tm_dots(col = "blue",
          size = 0.1)+
tm_shape(ff_bng)+
  tm_dots(col = "red",
          size = 0.1)+
  tm_compass(north = 0,
             position = c("right", "top"))+
  tm_layout(legend.bg.color = "white",
            legend.outside = TRUE,
            legend.outside.position = "right",
            legend.frame = TRUE)+
  tm_scale_bar(position=c("left", "bottom"),
               breaks = c(0,1,2),
               text.size = 1)+
  tm_credits("(c) OpenStreetMap contrbutors", position=c("left", "bottom"))+
  tm_add_legend(type = 'symbol',
                col = c("blue", "red"),
                labels = c("Schools", "Fast-food outlets"))
  

inset <- tm_shape(UK_outlinecrop) + 
  tm_polygons(col = "white")+
  tm_layout(frame = TRUE,
            bg.color = "lightblue")+
tm_shape(liverpoolbb)+ 
  tm_borders(col = "blue", lwd = 3)+
tm_shape(Worldcities2) +
  tm_symbols(col = "green", scale = .3)+
  tm_text("CITY_NAME", xmod=0.5, ymod=-0.5, size = 0.7, fontface = "bold")

library(grid)
map
print(inset, vp = viewport(0.76, 0.24, width = 0.4, height = 0.45))

#schools within a 10 minutes walk

tm_shape(osm)+
  tm_rgb(alpha = 0.3)+
tm_shape(liverpool)+
  tm_borders(lwd = 1.5)+
tm_shape(lt10min)+
  tm_bubbles("avg_lt_10min",
             col = "darkgreen",
             title.size = "Fast-food outlets <10min walk
from a school (mean per LSOA)")+
  tm_compass(north = 0,
             position = c("right", "top"))+
  tm_layout(legend.bg.color = "white",
            legend.outside = TRUE,
            legend.outside.position = "right",
            legend.frame = TRUE)+
  tm_scale_bar(position=c("left", "bottom"),
               breaks = c(0,1,2),
               text.size = 1)+
  tm_credits("(c) OpenStreetMap contrbutors", position=c("left", "bottom"))

palette.colors()

