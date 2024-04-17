library(tidycensus)
library(tidyverse)
library(dplyr)
library(sf)
library(tmap)
library(stplanr)
library(geosphere)
library(spData)
library(ggplot2)
library(ggpubr)

# Divvy Data - July 2023 data was no good and October 2023 data not here yet
jan <- read.csv("~/GISC 37104/Thurs_TransportDivvy/data/202301-divvy-tripdata.csv")
apr <- read.csv("~/GISC 37104/Thurs_TransportDivvy/data/202304-divvy-tripdata.csv")
jul <- read.csv("~/GISC 37104/Thurs_TransportDivvy/data/202207-divvy-tripdata.csv")
oct <- read.csv("~/GISC 37104/Thurs_TransportDivvy/data/202210-divvy-tripdata.csv")

dim(jan)
str(jan)
head(jan)

#Community Areas
cas <- st_read("~/GISC 37104/Thurs_TransportDivvy/data/CommAreas.shp")

# replace blanks with NA values, rename shorter
jan <- jan %>% mutate_if(is.character, ~na_if(., ''))
apr <- apr %>% mutate_if(is.character, ~na_if(., ''))
jul <- jul %>% mutate_if(is.character, ~na_if(., ''))
oct <- oct %>% mutate_if(is.character, ~na_if(., ''))

# specify non-NA origins and destinations - dataset shrinks from 190301 to 148284 observations for january
jan_def <- jan %>% 
  drop_na(start_station_id) %>%
  drop_na(end_station_id)
apr_def <- apr %>% 
  drop_na(start_station_id) %>%
  drop_na(end_station_id)
jul_def <- jul %>% 
  drop_na(start_station_id) %>%
  drop_na(end_station_id)
oct_def <- oct %>% 
  drop_na(start_station_id) %>%
  drop_na(end_station_id)

# calculate distance with geosphere package and add it to the data
o_coords_jan <- data.frame(lon = jan_def$start_lng, lat = jan_def$start_lat)
d_coords_jan <- data.frame(lon = jan_def$end_lng, lat = jan_def$end_lat)
jan_def$dists <- distVincentySphere(o_coords_jan, d_coords_jan)

o_coords_apr <- data.frame(lon = apr_def$start_lng, lat = apr_def$start_lat)
d_coords_apr <- data.frame(lon = apr_def$end_lng, lat = apr_def$end_lat)
apr_def$dists <- distVincentySphere(o_coords_apr, d_coords_apr)

o_coords_jul <- data.frame(lon = jul_def$start_lng, lat = jul_def$start_lat)
d_coords_jul <- data.frame(lon = jul_def$end_lng, lat = jul_def$end_lat)
jul_def$dists <- distVincentySphere(o_coords_jul, d_coords_jul)

o_coords_oct <- data.frame(lon = oct_def$start_lng, lat = oct_def$start_lat)
d_coords_oct <- data.frame(lon = oct_def$end_lng, lat = oct_def$end_lat)
oct_def$dists <- distVincentySphere(o_coords_oct, d_coords_oct)

mean(jan_def$dists)
# plot to see how it looks
jan_dists <- ggplot(jan_def, aes(x = dists, y = after_stat(density))) +
  geom_histogram(breaks = c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000)) +
  geom_vline(xintercept = mean(jan_def$dists), color = "red", linetype = "dashed", size = 1) +
  labs(x = "Distance Traveled (m)", y = "Frequency") + 
  ggtitle("Divvy Distance Distribution by Month") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))
apr_dists <- ggplot(apr_def, aes(x = dists, y = after_stat(density))) +
  geom_histogram(breaks = c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000)) +
  geom_vline(xintercept = mean(apr_def$dists), color = "red", linetype = "dashed", size = 1) +
  labs(x = "Distance Traveled (m)", y = "Frequency")
jul_dists <- ggplot(jul_def, aes(x = dists, y = after_stat(density))) +
  geom_histogram(breaks = c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000)) +
  geom_vline(xintercept = mean(jul_def$dists), color = "red", linetype = "dashed", size = 1) +
  labs(x = "Distance Traveled (m)", y = "Frequency")
oct_dists <- ggplot(oct_def, aes(x = dists, y = after_stat(density))) +
  geom_histogram(breaks = c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000)) +
  geom_vline(xintercept = mean(oct_def$dists), color = "red", linetype = "dashed", size = 1) +
  labs(x = "Distance Traveled (m)", y = "Frequency")

ggarrange(jan_dists, apr_dists, jul_dists, oct_dists + rremove("x.text"), 
          labels = c("January", "April", "July", "October"),
          ncol = 1, nrow = 4)
  
# origins, spatialize
jan_o_spatial <- jan_def %>%
  select("ride_id", "start_station_id", "end_station_id","start_lat","start_lng","end_lat","end_lng","dists") %>%
  st_as_sf(coords=c("start_lng", "start_lat"), crs = 4326) # x = longitude, y = latitude
jan_o_spatial <- st_transform(jan_o_spatial,3435) 

# aggregate to community area
jan_agg <- aggregate(x=jan_o_spatial, by=cas, FUN=mean)
jan_agg$dists <- jan_agg$dists/1000

#apr
apr_o_spatial <- apr_def %>%
  select("ride_id", "start_station_id", "end_station_id","start_lat","start_lng","end_lat","end_lng","dists") %>%
  st_as_sf(coords=c("start_lng", "start_lat"), crs = 4326) # x = longitude, y = latitude
apr_o_spatial <- st_transform(apr_o_spatial,3435)

apr_agg <- aggregate(x=apr_o_spatial, by=cas, FUN=mean)
apr_agg$dists <- apr_agg$dists/1000

#jul
jul_o_spatial <- jul_def %>%
  select("ride_id", "start_station_id", "end_station_id","start_lat","start_lng","end_lat","end_lng","dists") %>%
  st_as_sf(coords=c("start_lng", "start_lat"), crs = 4326) # x = longitude, y = latitude
jul_o_spatial <- st_transform(jul_o_spatial,3435)

jul_agg <- aggregate(x=jul_o_spatial, by=cas, FUN=mean)
jul_agg$dists <- jul_agg$dists/1000

#oct
oct_o_spatial <- oct_def %>%
  select("ride_id", "start_station_id", "end_station_id","start_lat","start_lng","end_lat","end_lng","dists") %>%
  st_as_sf(coords=c("start_lng", "start_lat"), crs = 4326) # x = longitude, y = latitude
oct_o_spatial <- st_transform(oct_o_spatial,3435)

oct_agg <- aggregate(x=oct_o_spatial, by=cas, FUN=mean)
oct_agg$dists <- oct_agg$dists/1000

# create tmaps
jan_dists_ca <- tm_shape(jan_agg) + 
  tm_fill("dists", breaks = c(0,1,1.5,2,2.5,3,3.5,4,5),title="Distance (km)", title.position = "top") +
  tm_layout(title = "Mean Divvy Ride Distances: 01/2023")
apr_dists_ca <- tm_shape(apr_agg) + 
  tm_fill("dists", breaks = c(0,1,1.5,2,2.5,3,3.5,4,5),title="Distance (km)", title.position = "top") +
  tm_layout(title = "Mean Divvy Ride Distances: 04/2023")
jul_dists_ca <- tm_shape(jul_agg) + 
  tm_fill("dists", breaks = c(0,1,1.5,2,2.5,3,3.5,4,5),title="Distance (km)", title.position = "top") +
  tm_layout(title = "Mean Divvy Ride Distances: 07/2022")
oct_dists_ca <- tm_shape(oct_agg) + 
  tm_fill("dists", breaks = c(0,1,1.5,2,2.5,3,3.5,4,5),title="Distance (km)", title.position = "top") +
  tm_layout(title = "Mean Divvy Ride Distances: 10/2022")

# lay out maps
tmap_arrange(jan_dists_ca, apr_dists_ca, jul_dists_ca, oct_dists_ca, nrow = 2, ncol = 2)

# I tried to use od2line to plot lines that would show the direction of mean movement, but it said that the package wasn't available for the latest version of R.

# compute difference between high and low seasons
diff_agg <- jul_agg %>%
  select("ride_id")
diff_agg$season_diff <- jul_agg$dists-jan_agg$dists

# plot difference tmap
season_dists_ca <- tm_shape(diff_agg) + 
  tm_fill("season_diff") +
  tm_layout(title = "Difference in Mean Travel Distance: July-January")
season_dists_ca

# Keep only one instance of each origin station to create point dataset of stations (note minor noise in lat/long)
o_stations <- divvy_data %>% 
  drop_na(start_station_id) %>% # remove rows where start_station_id has null values
  distinct(start_station_id, .keep_all = TRUE)

# Drop columns that aren't relevant (because they pertain to individual rides)
colnames(o_stations)
o_stations <- o_stations %>% 
  subset(select=c("start_station_name", "start_station_id", "start_lat", "start_lng"))

# Convert to spatial using start_lat, start_long
o_spatial <- o_stations %>% 
  st_as_sf(coords=c("start_lng", "start_lat"), crs = 4326) # x = longitude, y = latitude

d_stations <- divvy_data %>% 
  drop_na(end_station_id) %>%
  distinct(end_station_id, .keep_all = TRUE)

d_spatial <- d_stations %>% 
  subset(select=c("end_station_name", "end_station_id", "end_lat", "end_lng")) %>% 
  st_as_sf(coords=c("end_lng","end_lat"), crs = 4326)



# flows_total <- od2line(flow = hp_trip_summary, zones = o_spatial)
# 
# jul_o_spatial$start_lat <- jul_def$start_lat
# jul_o_spatial$start_lng <- jul_def$start_lng
# 
# test_line <- data.frame(x=c(jul_agg$start_lng[1],jul_agg$end_lng[1]),y=c(jul_agg$start_lat[1],jul_agg$end_lat[1]))%>%
#   as.matrix()
# test_lines<- data.frame(x=c(jul_agg$start_lng,jul_agg$end_lng),y=c(jul_agg$start_lat,jul_agg$end_lat))%>%
#   drop_na(x) %>%
#   drop_na(y) %>%
#   as.matrix()
# test_geom <- st_line(test_line)
# plot(test_geom)
# sf_object <- st_as_sf(test_geom)
# str(test_geom)
# str(test_lines)
# 
# jan_joined <- jan_o_spatial %>%
#   st_join(cas, join = st_within, left = TRUE)
# str(jan_agg)
# str(cas)
