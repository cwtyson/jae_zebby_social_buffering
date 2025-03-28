## Inspect outputs from density and modularity - brms multiple models

## Housekeeping #####
library(bisonR)
library(tidyverse)
library(brms)
library(tidybayes)
library(modelr)


## Inspect outputs from density models #####

## Read in model
mod_mods <- readRDS("./outputs/models/modularity_brms.RDS")

rhat(dens_mods)

## Combine models
mod_mods_c <- combine_models(mod_mods)

## Posterior predictive checks
pp_check(mod_mods_c)

## Summary
mod_mods_c <- summary(mod_mods_c)

## Inspect outputs from modularity models #####

## Read in model
mod_mods <- readRDS("./outputs/models/modularity_brms.RDS")

rhat(dens_mods)

## Combine models
mod_mods_c <- combine_models(mod_mods)

## Posterior predictive checks
pp_check(mod_mods_c)

## Summary
mod_mods_c <- summary(mod_mods_c)