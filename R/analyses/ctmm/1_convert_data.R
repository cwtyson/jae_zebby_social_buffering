## Convert data to as.telemetry() for ctmm workflow

## Housekeeping 
library(tidyverse)
library(sf)

## Process raw data #########

## Read in data
dets <- readRDS('./outputs/telemetry/zebby_sn_dets_cleaned.RDS')

## Change to as telemetry
tm_df <- ctmm::as.telemetry(dets)

## Save
saveRDS(tm_df, './outputs/telemetry/zebby_as_telemetry.RDS')
