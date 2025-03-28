## Analysis of zebra finch movement in relation to wind/temp
library(tidyverse)
library(brms)
library(DHARMa)
library(tidybayes)
library(bayesplot)
library(sjPlot)
# remotes::install_github('trinhdhk/sgtbrms')
library(sgtbrms)

## Process proximity estimates ###########

## Tag data
tags_p <- readRDS("./raw_data/2022_tags_processed.RDS") %>% 
  rename(group=section)

## Weather data
wd <- readRDS("./raw_data/processed_weather_data.RDS") %>% 
  mutate(day = format(floor_date(dt, unit = "day"), "%j"),
         hour = format(floor_date(dt, unit = "hour"), "%H")) %>% 
  group_by(day,hour) %>% 
  summarise(
    mean_wind = round((mean(wind_speed,na.rm = T))),
    mean_temp = round(mean(temp,na.rm = T)),
    .groups = "keep")


## Combine proximity estimates - set times for t
mod_data <- lapply(list.files("./outputs/proximity/matching_times/", full.names = T), readRDS) %>% 
  do.call(bind_rows,.) %>% 
  ## Remove empty values
  filter(high < Inf) %>% 
  filter(n > 0) %>% 
  na.omit() %>% 
  
  ## Add tag data
  left_join(tags_p %>% 
              transmute(ind = tag,
                        section = group),
            by = "ind")  %>% 
  
  ## Add tag data
  left_join(wd,
            by = c("day","hour")) %>% 
  arrange(day,hour) %>% 
  select(est,ind,partner,section,day,hour,mean_wind,mean_temp) %>% 
  
  ## Scale
  mutate(hour = as.numeric(scale(as.numeric(hour))),
         mean_temp = as.numeric(scale(mean_temp)),
         mean_wind = as.numeric(scale(mean_wind)))


## Model setup ########
mdl <- bf(est ~ hour + mean_wind*mean_temp + section + (1|ind) + (1|partner) + (1|day))
prior = default_prior(mdl, data = mod_data)


## Run model
prox_mod <- brm_sgt(
  
  ## Define model formula
  formula = mdl,

  ## Data
  data = mod_data,
  
  ## Cores
  cores = 4, 
  
  ## Prior
  prior = sgt_default_prior(),
  
  ## Sampling parameters
  iter = 20000,
  control = list(max_treedepth = 20),
  init = 0,
  
)

## Read in model
prox_mod <- readRDS("./outputs/models/proximity_brms.RDS")

## Checks
plot(prox_mod)
brms::rhat(prox_mod)
y = prox_mod$est
yrep_skew <- posterior_predict(prox_mod)

ggsave("./plots/ppc.jpg")

## Summary
summary(prox_mod, digits = 4)

ems <- emmeans::emmeans(prox_mod, ~section)
summary(pairs(ems), point.est = mean)

