## Hourly pairwise proximity estimates 
library(ctmm)
library(tidyverse)
library(foreach)
cores = parallel::detectCores()
cl <- parallel::makeForkCluster(cores-1, outfile = "")
doParallel::registerDoParallel(cl)

# ## Detections
# dets <- readRDS('./data/zebby_sn_dets_cleaned.RDS') %>%
#   mutate(day = format(floor_date(timestamp, unit = "day"), "%j"),
#          hour = format(floor_date(timestamp, unit = "hour"), "%H")) %>%
#   group_by(individual.local.identifier, day, hour) %>%
#   mutate(n = n()) %>%
#   filter(n > 50) %>%
#   mutate(individual.local.identifier = paste(individual.local.identifier,day,hour,sep="-")) %>%
#   group_by(day,hour) %>%
#   mutate(inds= n_distinct(individual.local.identifier))
# 
# ## Telemetry
# tem <- as.telemetry(dets)
# 
# ## Name based on tags
# tag_names <- unique(dets$individual.local.identifier)
# 
# ## Name list
# names(tem) <- tag_names
# 
# ## Save
# saveRDS(tem, "./data/zebby_sn_telemetry_hour_day.RDS")

## Read in data
tem <- readRDS("./data/telemetry/zebby_sn_telemetry_hour_day.RDS")

## CTMMs
names <- gsub(".RDS", "", list.files("./outputs/ctmm_fits/"))
fits <- lapply(list.files("./outputs/ctmm_fits/", full.names = T),
               readRDS)

names(fits) <- names

## Combine
top_mod_list <- list()
for(i in 1:length(fits)){
  
  top_mod <- fits[[i]][[1]]
  
  top_mod_list[[top_mod@info$identity]] <- top_mod
  
}

## Tag data
tags <- readRDS("./data/tag/2022_tags_processed.RDS")

## Create combinations of pairs
combos <- data.frame(ind = tags$tag,
                     partner = tags$tag) %>%
  expand.grid() %>% 
  distinct(ind, partner) %>% 
  filter(ind != partner) %>% 
  left_join(tags,
            join_by(ind==tag)) %>% 
  rename(ind_section = section,
         ind_sex = sex) %>% 
  left_join(tags,
            join_by(partner==tag)) %>% 
  rename(partner_section = section,
         partner_sex = sex) %>% 
  filter(ind_section == partner_section) %>% 
  mutate(row = 1:n()) %>% 
  filter(ind_sex != "Juvenile",
         partner_sex != "Juvenile") %>% 
  arrange(desc(row))

## Get combos that are done
done_combos <- gsub(".RDS","",list.files("./outputs/proximity/"))

## Add to combos df
combos <- combos %>% 
  mutate(to_do = paste(ind,partner,sep="_"),
         to_do2 = paste(partner,ind,sep="_"))

## Remove combos that are done
combos_f <- combos %>% 
  filter(!(to_do %in% done_combos))  %>% 
  filter(!(to_do2 %in% done_combos))

## Data frame of names
tem_df <- data.frame(tag_all = stringr::str_split_i(names(tem), "-", i = 1),
                     day = stringr::str_split_i(names(tem), "-", i = 2),
                     hour = stringr::str_split_i(names(tem), "-", i = 3))

## Proximity function
prox_function <- function(i)
{
  
  start_time <- Sys.time()
  
  # i = 951
  
  # cat(i)
  
  rows <- combos[i,]
  
  tag = rows$ind
  partner = rows$partner
  
  ## Filter df of names based on tag and partner
  tem_df_f <- tem_df %>% 
    dplyr::filter(tag_all %in% c(tag,partner))
  
  ## If both present:
  if(length(unique(tem_df_f$tag_all)) == 2){
    
    ## Mod list sub
    top_mod_list_sub <- top_mod_list[c(tag,partner)]
    
    ## Get tags from tem
    tem_tags <- stringr::str_split_i(names(tem), "-", i = 1)
    
    ## Get index to keep
    tem_tags_f <- which(tem_tags %in% c(tag,partner))
    
    ## Filter tem
    tem_f = tem[tem_tags_f]
    
    ## Get times where both tags were present
    tem_times1 <- which(duplicated(gsub("^[^-]*-", "", names(tem_f)), fromLast = T))
    tem_times2 <- which(duplicated(gsub("^[^-]*-", "", names(tem_f))))
    tem_times3 <- c(tem_times1,tem_times2)
    
    ## Keep those intervals
    tem_f2 <- tem_f[tem_times3]
    
    ## Rename 
    tem_f2_renamed_names <- gsub("^[^-]*-", "", names(tem_f2))
    
    ## Rename list
    names(tem_f2) <- tem_f2_renamed_names
    
    ## Empty data frame
    prox_df <- data.frame()
    
    if(length(unique(names(tem_f2))) >= 1){
      
      ## For each value, get two elements in the list
      for(l in unique(names(tem_f2))){
        
        # l = unique(names(tem_f2))[1]
        
        ## Get telemetry data
        dets_f <- tem_f2[grepl(l, names(tem_f2))]
        
        ## Reorder
        dets_f <- dets_f[order(names(dets_f))]
        
        ## Reorder
        top_mod_list_sub <- top_mod_list_sub[order(names(top_mod_list_sub))]
        
        cat("Running:", names(top_mod_list_sub), "\n")
        
        ## Rename
        dets_f[[1]]@info$identity <- names(top_mod_list_sub)[1]
        dets_f[[2]]@info$identity <- names(top_mod_list_sub)[2]
        
        ## Get number of simultaneous dets
        dets_bind <- rbind(dets_f[[1]] %>% 
                             data.frame() %>% 
                             mutate(tag = dets_f[[1]]@info$identity),
                           dets_f[[2]] %>% 
                             data.frame() %>% 
                             mutate(tag = dets_f[[2]]@info$identity)) %>% 
          group_by(t) %>% 
          mutate(n = n()) %>% 
          filter(n == 2) 
        
        sim_dets <- nrow(dets_bind)
        
        t = sort(unique(dets_bind$t))
        
        ## Run proximity
        PROXIMITY <- proximity(CTMM = top_mod_list_sub,
                               data = dets_f,
                               GUESS = ctmm(error=TRUE),
                               t=t) 
      
        
        ## Proximity of pair
        prox_pair_df <- PROXIMITY %>%
          t() %>%
          data.frame() %>%
          dplyr::mutate(ind = names(top_mod_list_sub)[1],
                        partner = names(top_mod_list_sub)[2],
                        day = stringr::str_split_i(l, "-", i = 1),
                        hour = stringr::str_split_i(l, "-", i = 2),
                        n = sim_dets)
        
        prox_df <- bind_rows(prox_df, prox_pair_df)
        
      }
      
      ## Save
      saveRDS(prox_df,
              paste0("./outputs/proximity/t_set/",
                     tag,
                     "_",
                     partner,
                     ".RDS"))
      
      time_taken = Sys.time()-start_time
      cat("Finished after", time_taken, "\n")
      
    }
    
  } else{
    
    cat("Skipped:", tag,"-",partner, "\n")
    
  }

}

foreach(i=1:max(combos$row),
        .packages=c('ctmm','stringr'),
        .verbose = TRUE) %dopar%
  { prox_function(i) }
