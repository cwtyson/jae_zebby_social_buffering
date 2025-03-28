## Calculate edge weight models (EWM) for hourly social networks (SN)

## Housekeeping 
library(bisonR)
library(rstan)
library(tidyverse)
library(brms)
library(igraph)

## EWMs ########

## Read in data
int_sum_all <- readRDS("./outputs/sna/hourly_association_counts.RDS")

## Tag data
tags <- readRDS("./raw_data/2022_tags_processed.RDS")

## For each hour, for each day:

## Look at number of dyads per day/hour
int_sum_complete <- int_sum_all %>%
  group_by(day,hour) %>%
  mutate(dyad_count = n_distinct(dyadID),
         inds = n_distinct(c(ID1,ID2)))

## Number of day/hours before filtering
n_distinct(paste(int_sum_complete$day))
unique(paste(int_sum_complete$hour))
n_distinct(paste(int_sum_complete$day,int_sum_complete$hour))

## Keep intervals with at least 100 dyads
int_sum <- int_sum_complete %>%
  
  filter(dyad_count >= 100) %>% 
  
  ## Distinct for day, hour
  distinct(day, hour, dyadID, .keep_all = T) %>% 
  
  select(day,hour,ID1,ID2,dyadID,int_count,obs_count,mean_wind,mean_temp)

## Number of day/hours after filtering
n_distinct(paste(int_sum$day))
unique(paste(int_sum$hour))
n_distinct(paste(int_sum$day,int_sum_complete$hour))

## Minimum non-zero edge weight
mnze <- int_sum_all %>% 
  mutate(mnze = int_count/obs_count) %>% 
  filter(mnze > 0) %>% 
  pull(mnze) %>% 
  min()

## Fit edge list models for each subset of data 

## Empty lists
edge_lists <- list()
gd_list <- list()
num_draws=400

for(day_f in unique(int_sum$day)){
  
  cat(day_f, "\n")
  
  # day_f = 262
  
  int_sum_d <- int_sum[int_sum$day == day_f,]
  
  for(hour_f in sort(unique(int_sum_d$hour))){
    
    cat(hour_f, "\n")
    
    # hour_f = unique(int_sum_d$hour)[1]
    
    ## Subset day and hour
    int_sum_d_h <- int_sum_d[int_sum_d$hour == hour_f,]
    
    day_hr_f = paste(day_f,"-",hour_f,sep="")
    
    ## Temp
    day_hr_f_mean_temp = int_sum_d_h %>%
      filter(day == day_f) %>%
      filter(hour == hour_f) %>% 
      pull(mean_temp) %>% 
      unique()
    
    ## Wind
    day_hr_f_mean_wind = int_sum_d_h %>%
      filter(day == day_f) %>%
      filter(hour == hour_f) %>% 
      pull(mean_wind) %>% 
      unique()
    
    ## Edge model 
    priors <- bisonR::get_default_priors("count_conjugate")
    
    priors$edge <- "gamma(0.1,0.1)"
    
    bisonR::prior_check(priors, "count_conjugate")
    
    fit_edge <- bison_model(
      (int_count | obs_count) ~ dyad(ID1, ID2),
      model_type = "count_conjugate",
      zero_inflated = T,
      priors=priors,
      iter_sampling = 1000,
      mc_cores = 4,
      iter_warmup = 1000,
      data=int_sum_d_h
    )

    ## Setting seed for plotting purposes
    edgelist_samples <- draw_edgelist_samples(fit_edge, num_draws)
    net <- igraph::graph_from_edgelist(as.matrix(edgelist_samples[,
                                                                  1:2]),
                                       directed = fit_edge$directed)
    
    ## Cluster modularity samples vector
    clmod_samples <- c()
    
    ## For each draw:
    for(i in 1:num_draws){
      
      ## Get weights on original scale
      weights = exp(edgelist_samples[,2+i])
      
      ## Set weights below minimum non-zero weight to 0
      weights_adj <- ifelse(weights < mnze, 0, weights)
      
      E(net)$weight <- weights_adj
      
      clmod <- modularity(cluster_louvain(net))
      
      clmod_samples <- c(clmod_samples,clmod)
      
    }
    
    ## Add edge model to list
    edge_lists[[paste(day_f,hour_f,sep="-")]] <- fit_edge
    
    ## Calculate global density
    gd_samples <- extract_metric(fit_edge, "global_density", num_draws = num_draws)
    cv_samples <- extract_metric(fit_edge, "global_cv", num_draws = num_draws)
    std_samples <- extract_metric(fit_edge, "global_std", num_draws = num_draws)
    diameter_samples <- extract_metric(fit_edge, "global_diameter", num_draws = num_draws)
    cluster_samples <- extract_metric(fit_edge, "global_clustering[0.2]", num_draws = num_draws)
    
    ## Add to list
    rep_list <- list(data.frame(day = day_f,
                                hour = hour_f,
                                dyads = length(unique(c(int_sum_d_h$dyadID))),
                                inds = length(unique(c(int_sum_d_h$ID1,int_sum_d_h$ID2))),
                                density = gd_samples,
                                modularity = clmod_samples,
                                samp = 1:num_draws))
    
    gd_list <- append(gd_list, rep_list)
    
  }
  
}

## Get weather data
wd_data_df <- int_sum %>%
  mutate(day_hr = paste(day,hour,sep="-")) %>%
  distinct(day_hr, .keep_all = T) %>% 
  ungroup() %>% 
  select(day_hr,hour,mean_wind,mean_temp)


## Reformat
gd_df <- do.call(bind_rows,gd_list) %>% 
  mutate(day_hr = paste(day,hour,sep="-")) %>% 
  select(-hour) %>% 
  arrange(day,samp) %>% 
  
  ## Join weather data
  left_join(wd_data_df, by = "day_hr") %>% 
  select(day_hr,day,hour,dyads,inds,samp,density, modularity,mean_wind,mean_temp) 

## Save data
saveRDS(gd_df, "./outputs/sna/ewms.RDS")

## Save data
saveRDS(edge_lists, "./outputs/sna/edge_lists.RDS")
