## Run multiple brms models on density ~ temp/wind

## Housekeeping #####
library(bisonR)
library(tidyverse)
library(brms)
library(tidybayes)
library(modelr)

## Read in data  ########
gd_df <- readRDS("./outputs/sna/ewms.RDS") 

## Scale mean temp and mean wind
gd_df <- gd_df %>%
  group_by(samp) %>%
  mutate(hour = as.numeric(scale(hour)),
         mean_temp = as.numeric(scale(mean_temp)),
         mean_wind = as.numeric(scale(mean_wind)))


## Split into list
gd_df_lists <- split(gd_df, 
                     ~samp,
                     ~day_hr, drop=TRUE)

## Number of data points
n_distinct(gd_df_lists[[1]]$day_hr)

## Run brms_multiple on density ########

## Set priors
mdl_formula <- bf(density ~ poly(hour, 2) + mean_wind*mean_temp)
(prior = default_prior(mdl_formula, gd_df_lists[[1]]))

## Run brms multiple
dens_mods <- brm_multiple(
  
  ## Define model formula
  formula = mdl_formula,
  
  ## Iterations
  iter = 10000,
  
  ## Cores
  cores = 4,
  
  control = list(max_treedepth = 20),
  
  ## List of data frames
  data = gd_df_lists,
  
  ## Set priors
  prior = c(
    # prior(normal(0, 1), class = b, coef = "inds"),
    prior(normal(0, 1), class = b, coef = "mean_temp"),
    prior(normal(0, 1), class = b, coef = "mean_wind"),
    prior(normal(0, 1), class = b, coef = "mean_wind:mean_temp"),
    prior(normal(0, 1), class = b, coef = "polyhour21"),
    prior(normal(0, 1), class = b, coef = "polyhour22")
  )
  
)

saveRDS(dens_mods, "./outputs/models/density_brms.RDS")
