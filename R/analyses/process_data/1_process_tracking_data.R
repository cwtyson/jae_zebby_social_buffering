## Process tracking data for analyses. This 

## Housekeeping 
library(tidyverse)
library(sf)

## Process raw data #########

## Read in data
dets <- readRDS('./raw_data/zebby_sn_dets.RDS')

## 129 total
n_distinct(dets$tag)

## Convert to telemetry
dets_f <- dets %>% 
  sf::st_as_sf(coords = c("x_est", "y_est"), 
               crs = 3308) %>% 
  
  ## Transmute
  dplyr::transmute(individual.local.identifier = tag,
                   timestamp = force_tz(dt_r, tz = "Australia/Broken_Hill"),
                   location.long = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
                   location.lat = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2],
                   Argos.orientation = orientation, 
                   Argos.semi.major = semi_major,
                   Argos.semi.minor  = semi_minor,
                   max_RSSI,
  ) %>% 
  
  ## Remove duplicates
  distinct(individual.local.identifier, timestamp,.keep_all = T) 

## Coordinates of nodes
gp <- sf::read_sf("./raw_data/grid_points_20240916.GPX") %>%
  sf::st_transform(3308) %>% 
  transmute(grid_point  = name) %>% 
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>% 
  
  filter(grepl("Gp",grid_point)) %>% 
  mutate(grid_point = gsub("Gp ","",grid_point))

## Get polygon from nodes with 150 m buffer
gp_poly <- gp %>% 
  sf::st_buffer(dist = 150) %>% 
  sf::st_union()

## Remove points outside of the study area
dets_wi <- dets_f %>% 
  sf::st_filter(., gp_poly)

## Change filtered dets to data frame
dets_df <- dets_wi %>%
  sf::st_transform(4326) %>%
  mutate(location.long = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
         location.lat = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2]) %>% 
  sf::st_drop_geometry() %>% 
  data.frame()

## One individual has only 1 record - remove
dets_df2 <- dets_df %>% 
  filter(individual.local.identifier != "7834554B")

## Save
saveRDS(dets_df2, './outputs/telemetry/zebby_sn_dets_cleaned.RDS')
