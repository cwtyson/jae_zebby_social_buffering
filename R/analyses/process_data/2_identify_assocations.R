## Count hourly associations 
library(spatsoc)
library(sf)
library(tidyverse)

## Telemetry data
dets <- readRDS('./outputs/telemetry/zebby_sn_dets_cleaned.RDS')

## Check counts
length(unique(dets$individual.local.identifier))

## Tag data
tags <- readRDS("./raw_data/2022_tags_processed.RDS")

## Tracks (from raw data)
tracks_df <- dets  %>% 
  
  ## Retain strong detections that are between two receivers
  filter(max_RSSI > -80) %>%
  
  sf::st_as_sf(coords = c("location.long", "location.lat"), crs = 4326) %>%
  sf::st_transform(3308) %>%
  dplyr::select(tag=individual.local.identifier,dt=timestamp) %>%
  dplyr::arrange(dt) %>%
  mutate(x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
         y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>%
  sf::st_drop_geometry() %>%
  
  ## Combine tag information
  dplyr::left_join(tags,by="tag")

## Set interaction distance threshold
dist_threshold = 60

## Weather data summary ########
wd <- readRDS("/Users/tyson/Documents/academia/institutions/WUR/research/australia/zebby_tracking/data/weather_data/processed_weather_data.RDS")

## Summarize wind by day
wd_sum <- wd %>% 
  mutate(day = format(floor_date(dt, unit = "day"), "%j"),
         hour = format(floor_date(dt, unit = "hour"), "%H")) %>% 
  group_by(day,hour) %>% 
  summarise(
    mean_wind = round((mean(wind_speed,na.rm = T))),
    mean_temp = round(mean(temp,na.rm = T)),
    .groups = "keep")

## Summarise by day/hour, get number of individuals
ssp <- tracks_df %>% 
  mutate(day = format(floor_date(dt, unit = "day"), "%j"),
         hour = format(floor_date(dt, unit = "hour"), "%H")) %>% 
  
  ## Count of individuals in each time stamp
  group_by(day,hour) %>% 
  mutate(inds = n_distinct(tag),
         group_id = cur_group_id()) 

## Convert to dt
tracks_dt <- data.table::setDT(ssp)

## Group times - simultaneous fixes
group_times(tracks_dt, 
            datetime = 'dt')

## Get distance between simultaneous fixes for all individuals - using 'maximum' distance to get all distances 
pair_distances_dt <- edge_dist(tracks_dt,
                               threshold = 4000,
                               id = "tag",
                               timegroup = "timegroup",
                               coords = c("x","y"),
                               fillNA = FALSE,
                               returnDist = TRUE,
                               splitBy = c("day", "hour"))

## Rearrange IDs
dyad_id(pair_distances_dt, id1 = 'ID1', id2 = 'ID2')

## Arrange
pair_distances_dt <- pair_distances_dt %>% 
  arrange(timegroup, dyadID) %>% 
  
  ## Label interactions
  mutate(int = distance<dist_threshold) %>% 
  
  ## Keep distinct dyadIDs in each observation period
  ungroup() %>% 
  distinct(day, hour, dyadID, timegroup, .keep_all = T) 

## Add back in coordinates
int_locs <- pair_distances_dt %>% 
  left_join(tracks_dt %>% 
              select(day,hour,section,timegroup,tag,x,y),
            by = join_by(day, hour, timegroup,"ID1" == "tag")) %>% 
  left_join(wd_sum,
            by = join_by(day, hour)) %>% 
  arrange(desc(int)) %>% 
  mutate(int = factor(int,levels=c(TRUE,FALSE)))

## In each hour, sum interactions between dyads
int_sum <- pair_distances_dt %>% 
  
  ## Keeping section
  group_by(day, hour, ID1, ID2,dyadID) %>% 
  mutate(
    
    ## Count of simultaneous detections within set distance (i.e. interaction)
    int_count = sum(int),
    
    ## Count of simultaneous detections overall 
    obs_count = length(int)) %>% 
  
  data.frame() %>% 
  
  ## Join weather data
  left_join(wd_sum, by = c("day","hour")) %>% 
  
  ## Reformat hour to numeric
  mutate(hour = as.numeric(hour))

saveRDS(int_sum,"./outputs/sna/hourly_association_counts.RDS")

