## Summary of data for manuscript. Script also makes overview figures (Figure 1 and Figure 2)

## Housekeeping
library(tidyverse)

## Detection summary #######

## Tag data
tags <- readRDS("./raw_data/2022_tags_processed.RDS")

## Table (1 individual (male) was dropped due to malfunctioning tag)
table(tags$sex)

## Telemetry data
dets <- readRDS('./outputs/telemetry/zebby_sn_dets_cleaned.RDS') %>% 
  mutate(tag= individual.local.identifier)

## Filter detections
dets_f<- dets  %>% 
  
  ## Retain strong detections
  filter(max_RSSI > -80) %>%
  group_by(tag) %>% 
  summarise(dets=n())

## Time between detections
dets_day <- dets  %>% 
  
  ## Retain strong detections
  filter(max_RSSI > -80) %>%
  
  transmute(tag,
            day = floor_date(ymd_hms(timestamp),unit="day")) %>% 
  distinct(day,tag) %>% 
  group_by(day) %>% 
  mutate(inds = n())

## Time between detections
dets_sum_time <- dets  %>% 
  
  ## Retain strong detections
  filter(max_RSSI > -80) %>%
  
  transmute(tag,
            day = floor_date(ymd_hms(timestamp),unit="day"),
            timestamp = ymd_hms(timestamp)) %>% 
  group_by(tag,day) %>% 
  mutate(time_diff = lead(timestamp)-timestamp)

## Average localization interval
mean(dets_sum_time$time_diff,na.rm=T)

## Weather data #######
wd <- readRDS("./raw_data/processed_weather_data.RDS") %>% 
  mutate(dt = force_tz(dt,"Australia/Broken_Hill"))

## Filter
wdf <- wd %>% 
  filter(dt > min(dets$timestamp) & dt < max(dets$timestamp))

wd_sum <- wdf %>% 
  mutate(day = floor_date(dt,unit="day"),
         hour = floor_date(dt,unit="hour")) %>% 
  group_by(day,hour) %>% 
  summarise(mean_temp = mean(temp),
            mean_wind= mean(wind_speed))
mean(wd_sum$mean_temp)
range(wd_sum$mean_temp)
mean(wd_sum$mean_wind)
range(wd_sum$mean_wind)

## Interactions ######

## Interaction data
ints <- readRDS("./outputs/sna/hourly_association_counts.RDS")

nrow(ints) ## Number of simultaneous localizations
sum(inlts$int == TRUE)

## Summarize by dyadID
int_sum <- ints %>% 
  group_by(day,hour,dyadID) %>% 
  summarise(tot = n(),
            ints = sum(int),
            prop = ints/tot)
mean(int_sum$tot)
range(int_sum$tot)
mean(int_sum$ints)
range(int_sum$ints)


## Long term weather ######

## Weather data #######
wd <- readRDS("./raw_data/processed_weather_data.RDS") %>% 
  mutate(dt = force_tz(dt,"Australia/Broken_Hill"))

## Filter
wdf <- wd %>% 
  filter(dt > min(dets$timestamp) & dt < max(dets$timestamp))

wd_sum <- wdf %>% 
  mutate(day = floor_date(dt,unit="day"),
         hour = floor_date(dt,unit="hour")) %>% 
  group_by(day,hour) %>% 
  summarise(mean_temp = mean(temp),
            mean_wind= mean(wind_speed))
mean(wd_sum$mean_temp)
quantile(wd_sum$mean_temp, 0.05)
quantile(wd_sum$mean_temp, 0.95)

range(wd_sum$mean_temp)


mean(wd_sum$mean_wind)
quantile(wd_sum$mean_wind, 0.05)
quantile(wd_sum$mean_wind, 0.95)
range(wd_sum$mean_wind)



## Long term weather
ltw <- readxl::read_excel("./raw_data/long_term_weather.xlsx") %>% 
  janitor::clean_names() 

ltwp <- ltw %>% 
  dplyr::rename(year = year_month_day_hour_minutes_in_yyyy,
                month = mm,
                day = dd,
                hour = hh24) %>% 
  dplyr::transmute(dt = lubridate::ymd_h(paste(year,
                                               month,
                                               day,
                                               hour),
                                         tz = 'Australia/Broken_Hill'), 
                   temp = air_temperature_in_degrees_c,
                   wind_speed = wind_speed_in_km_h,
                   precip = precipitation_since_9am_local_time_in_mm) %>% 
  na.omit() 

## Filter for September
sep <- ltwp %>% 
  mutate(year = year(dt),
         month = month(dt),
         day = day(dt),
         hour = hour(dt)) %>% 
  filter(month == 9)   %>% 
  filter(hour >= 6 & hour <= 18) %>% 
  filter(year < 2022) %>% 
  group_by(year,month,day,hour) %>% 
  summarise(mean_t = mean(temp,na.omit = T),
            mean_w = mean(wind_speed,na.omit = T))


mean(sep$mean_t)
range(sep$mean_t)
mean(sep$mean_w)
range(sep$mean_w)

## Mean monthly daily wind and temperature
ltwm <- ltwp %>% 
  mutate(year = year(dt),
         month = month(dt,label=TRUE,abbr=FALSE),
         month2 = month(dt),
         day = day(dt),
         hour = hour(dt)) %>% 
  filter(hour >= 6 & hour <= 18) %>% 
  filter(year < 2022) %>% 
  group_by(year,month,month2,day,hour) %>% 
  summarise(mean_t = mean(temp,na.omit = T),
            mean_w = mean(wind_speed,na.omit = T))

## Plot
ggplot(ltwm) +
  geom_jitter(aes(x=month,
                  y = mean_w,
                  color=mean_t)) +
  geom_smooth(aes(x=month2,
                  y = mean_w),
              color="red") 
