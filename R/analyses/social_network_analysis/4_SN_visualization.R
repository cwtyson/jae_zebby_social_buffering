## Visualize example networks

## Housekeeping
library(tidyverse)
library(brms)
library(igraph)
library(tidygraph)
library(ggraph)
library(ggforce)
library(graphlayouts)
library(patchwork)

## Visualize modularity for two cases with low/high wind speed #######

## Two examplars: 267-12 (low mod) and 263-13 (high mod)

## Read in edge list from each day and make plots

## Edge list
el <- readRDS("./outputs/mods/edge_lists_2.RDS")

## Days to plot
#262-12 - low
#264-13 - high

## Number of draws - defined above
num_draws = 400

## Days with modularity to use as examples
days <- c("262-12","264-13")

## Index for draw with weights to plot
mod_index <- c(349,22)

## Check scores
mod_reps <- readRDS("./outputs/brms_data/brms_data_2.RDS") %>% 
  filter(day_hr == days[1] & samp == mod_index[1] | day_hr == days[2] & samp == mod_index[2])

## Low ######

## Subset 
el_low <- el[[days[1]]]

## Setting seed for consistent plotting 
set.seed(123)

## Edgelist samples for low modularity
edgelist_samples_law <- draw_edgelist_samples(el_low, num_draws)
low_net <- igraph::graph_from_edgelist(as.matrix(edgelist_samples_law[,
                                                                      1:2]),
                                       directed = FALSE)

## Get weights on original scale
weight_low = exp(edgelist_samples_law[,2+mod_index[1]])

## Set weights below minimum non-zero weight to 0
weights_low <- ifelse(weight_low < mnze, 0, weight_low)
E(low_net)$weight <- weights_low

## Calculate modularity
(clmod <- modularity(cluster_louvain(low_net)))

## Create graph
graph_df_low <- data.frame(as.matrix(edgelist_samples_law[,
                                                          1:2])) %>%
  rename(from = node_1,
         to = node_2) %>%
  mutate(weight = weights_low)

## Get node info
verts_df_low <- data.frame(from = c(graph_df_low$from,graph_df_low$to)) %>%
  left_join(tags, by = c("from" = "tag")) %>%
  select(id = from,
         section,
         sex) %>%
  distinct(id,.keep_all = T)

## Get graph
net_df_low <- igraph::graph_from_data_frame(
  d = graph_df_low,
  vertices = verts_df_low,
  directed = FALSE
)


## Louvain clustering groups
louv_low <- igraph::cluster_louvain(net_df_low, weights = E(net_df_low)$weight)
V(net_df_low)$membership <- louv_low$membership

## Membership df
membership_low_df <- data.frame(ind = louv_low$names,
                                community = louv_low$membership)

## Finally, adjust weights for plotting - cannot be 0
weights_adj_low <- ifelse(weights_low < mnze, mnze/1000, weights_low)
E(net_df_low)$weight <- weights_adj_low


## High ######

## Subset 
el_high <- el[[days[2]]]

## Setting seed for consistent plotting 
set.seed(123)

## Edgelist samples for high modularity
edgelist_samples_law <- draw_edgelist_samples(el_high, num_draws)
high_net <- igraph::graph_from_edgelist(as.matrix(edgelist_samples_law[,
                                                                       1:2]),
                                        directed = FALSE)

## Get weights on original scale
weight_high = exp(edgelist_samples_law[,2+mod_index[2]])

## Set weights behigh minimum non-zero weight to 0
weights_high <- ifelse(weight_high < mnze, 0, weight_high)
E(high_net)$weight <- weights_high

## Calculate modularity
(clmod <- modularity(cluster_louvain(high_net)))

## Create graph
graph_df_high <- data.frame(as.matrix(edgelist_samples_law[,
                                                           1:2])) %>%
  rename(from = node_1,
         to = node_2) %>%
  mutate(weight = weights_high)

## Get node info
verts_df_high <- data.frame(from = c(graph_df_high$from,graph_df_high$to)) %>%
  left_join(tags, by = c("from" = "tag")) %>%
  select(id = from,
         section,
         sex) %>%
  distinct(id,.keep_all = T)

## Get graph
net_df_high <- igraph::graph_from_data_frame(
  d = graph_df_high,
  vertices = verts_df_high,
  directed = FALSE
)


## Louvain clustering groups
louv_high <- igraph::cluster_louvain(net_df_high, weights = E(net_df_high)$weight)
V(net_df_high)$membership <- louv_high$membership

## Membership df
membership_high_df <- data.frame(ind = louv_high$names,
                                 community = louv_high$membership)

## Finally, adjust weights for plotting - cannot be 0
weights_adj_high <- ifelse(weights_high < mnze, mnze/1000, weights_high)
E(net_df_high)$weight <- weights_adj_high

















## Make plots

## Make tidy and give edge attribute if connected with member in own community
low_net_df_tidy <- tidygraph::as_tbl_graph(net_df_low) %>% 
  activate(nodes) %>% 
  rename(ind = name) %>% 
  activate(edges) %>% 
  mutate(ind = .N()$ind[from], 
         partner = .N()$ind[to]) %>% 
  left_join(membership_low_df,
            join_by(ind)) %>% 
  rename(ind_community = community) %>% 
  left_join(membership_low_df,
            join_by(partner == ind)) %>% 
  rename(partner_community = community) %>% 
  mutate(same_community = ifelse(ind_community==partner_community,"yes","no"),
         edge_color_community = as.character(ifelse(same_community == "yes", 
                                                    ind_community,
                                                    "out")))

## High
high_net_df_tidy <- tidygraph::as_tbl_graph(net_df_high) %>% 
  activate(nodes) %>% 
  rename(ind = name) %>% 
  activate(edges) %>% 
  mutate(ind = .N()$ind[from], 
         partner = .N()$ind[to]) %>% 
  left_join(membership_high_df,
            join_by(ind)) %>% 
  rename(ind_community = community) %>% 
  left_join(membership_high_df,
            join_by(partner == ind)) %>% 
  rename(partner_community = community) %>% 
  mutate(same_community = ifelse(ind_community==partner_community,"yes","no"),
         edge_color_community = as.character(ifelse(same_community == "yes", 
                                                    ind_community,
                                                    "out")))

## Edge scales 
edge_range_low <- low_net_df_tidy %>% 
  activate(edges) %>% 
  pull(weight) 

edge_range_high <- high_net_df_tidy %>% 
  activate(edges) %>% 
  pull(weight) 

edge_range <- range(c(edge_range_low,edge_range_high))
edge_scale <- data.frame(breaks = seq(from = edge_range[1],
                                      to = edge_range[2], 
                                      by = 1),
                         weights = seq(1,3,length.out = length( seq(from = edge_range[1],
                                                                    to = edge_range[2], 
                                                                    by = 1))))

## Color scales
communities_low <- length(unique(louv_low$membership))
communities_high <- length(unique(louv_high$membership))
color_scale = data.frame(breaks = c(1:max(c(communities_low, communities_high)),"out"),
                         color = c(wesanderson::wes_palette("Zissou1", 100, type = "continuous")[seq(1, 100, length.out = max(c(communities_low, communities_high)))],
                                   "black"))


## Reformat edge weights for plotting with layout to stress group membership


# # Cretae membership vector
# louv_low_m <- louv_low$membership
# names(louv_low_m) <- louv_low$names
# 
# ## Create new graph fro plotting
# low_net_df_plot <- low_net_df
# 
# ## Adjust weights given custom function to new graph
# E(low_net_df_plot)$weight=apply(get.edgelist(low_net_df_plot),1,weight.community,membership=louv_low_m,10,1)

set.seed(123)
(low_mod = ggraph(low_net_df_tidy,
                  # %>% 
                  #   activate(edges) %>% 
                  #   arrange(edge_color_community)
                  ,
                  layout = "kk") +
    
    # geom_edge_density(aes(fill = weights)) +
    geom_edge_link(aes(width = weight,
                       # color= edge_color_community,
                       alpha = weight),
                   show.legend = FALSE) +
    
    # geom_edge_density(aes(fill = same_community),
    #                   show.legend = FALSE,
    #                   data = low_net_df_tidy %>% 
    #                     activate(edges) %>% 
    #                     filter(same_community == "yes"))) +
    
    scale_edge_color_manual(name = "Community",
                            breaks = as.character(color_scale$breaks),
                            values = color_scale$color) +
    
    geom_node_point(aes(color = as.factor(membership),
                        shape = as.factor(section)), size = 5) +
    scale_shape(name = "Section") +
    scale_color_manual(name = "Community",
                       breaks = color_scale$community,
                       values = color_scale$color) +
    scale_edge_alpha(name = "Edge\nWeights",
                     # breaks = c(0:3),
                     # values = c(1:4)
    ) +
    # scale_edge_width(name = "Edge\nWeights",
    #                         # breaks = c(0:3),
    #                         # values = c(1:4) 
    # ) +
    theme_void() +
    theme(legend.position = "bottom"))


## Reformat edge weights for plotting with layout to stress group membership
louv_high_m <- louv_high$membership
names(louv_high_m) <- louv_high$names
E(net_df_high)$weight = apply(get.edgelist(net_df_high),1,weight.community,membership=louv_high_m,10,1)

high_net_df_tidy2 <- high_net_df_tidy %>%
  activate(edges) %>%
  mutate(
    # weight = apply(get.edgelist(net_df_high),1,weight.community,membership=louv_high_m,10,1)
  )

set.seed(123)
(high_mod = 
    
    # ggraph(high_net_df, 
    #        layout = "fr") +
    # 
    # geom_edge_link(aes(width = weights_adj,
    #                    # color= edge_color_community,
    #                    alpha = weights_adj),
    #                show.legend = FALSE) +
    
    
    ggraph(high_net_df_tidy %>%
             activate(edges) %>%
             arrange(edge_color_community),
           layout = "kk") +
    
    geom_edge_link(aes(width = weight,
                       color= edge_color_community,
                       alpha = weight),
                   show.legend = FALSE) +
    
    # geom_edge_density(aes(fill = same_community),
    #                   show.legend = FALSE,
    #                   data = low_net_df_tidy %>% 
    #                     activate(edges) %>% 
    #                     filter(same_community == "yes"))) +
    
    scale_edge_color_manual(name = "Community",
                            breaks = as.character(color_scale$breaks),
                            values = color_scale$color) +
    
    geom_node_point(aes(color = as.factor(membership),
                        shape = as.factor(section)), size = 5) +
    scale_shape(name = "Section") +
    scale_color_manual(name = "Community",
                       breaks = color_scale$community,
                       values = color_scale$color) +
    scale_edge_alpha(
      name = "Edge\nWeights",
      # breaks = c(0:3),
      # values = c(1:4)
    ) +
    # scale_edge_width(name = "Edge\nWeights",
    #                         # breaks = c(0:3),
    #                         # values = c(1:4) 
    # ) +
    theme_void() +
    theme(legend.position = "bottom"))

## Combine
wrap_plots(low_mod, high_mod) + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = c('A'),
                  tag_suffix = ')',
                  theme = list(legend.position = "bottom"))

ggsave("./plots/modularity_plots.jpg",
       scale=1,width = 9.85,height=7)

## Plot localizations from corresponding period #####

## Days with modularity to use as examples
days <- c("262-12","264-13")

## Locations
locs <- readRDS("./data/telemetry/spatsoc_tracks_dt.RDS")

## Read in data
int_sum_all <- readRDS("./outputs/brms_data/ex_df.RDS")

## Tag data
tags <- readRDS("./data/tag/2022_tags_processed.RDS")


## Low modularity example
low_ints <- int_sum_all %>% 
  left_join(tags,
            by = c("ID1" = "tag")) %>% 
  rename(ind_sex = sex,
         ind_section = section) %>% 
  left_join(tags,
            by = c("ID1" = "tag")) %>% 
  rename(partner_sex = sex,
         partner_section = section) %>% 
  
  mutate(day_hr = paste(day,hour,sep="-")) %>% 
  
  ## Low
  filter(day_hr %in% days[1]) %>% 
  
  group_by(dyadID) %>% 
  mutate(mean_int = mean(int_count/obs_count),
         w_mean_int = weighted.mean(int_count/obs_count, obs_count)) %>% 
  left_join(locs %>% 
              select(tag,
                     timegroup,
                     x,
                     y), 
            by = join_by(ID1 == tag,
                         timegroup)) %>%
  ungroup() %>% 
  select(timegroup,ID1,ID2,int,ind_sex,ind_section,partner_sex,partner_section,w_mean_int,
         ind_x=x,ind_y=y) %>% 
  left_join(locs %>% 
              select(tag,
                     timegroup,
                     partner_x = x,
                     partner_y = y), 
            by = join_by(ID2 == tag,
                         timegroup)) %>%
  
  left_join(locs %>% 
              select(tag,
                     timegroup,
                     x,
                     y), 
            by = join_by(ID1 == tag,
                         timegroup)) %>% 
  
  
  left_join(membership_low_df,
            join_by(ID1 == ind)) %>% 
  rename(ind_community = community) %>% 
  left_join(membership_low_df,
            join_by(ID2 == ind)) %>% 
  rename(partner_community = community) %>% 
  mutate(same_community = ifelse(ind_community==partner_community,"yes","no"))

centroid <- low_ints %>% 
  st_as_sf(coords= c("ind_x","ind_y"),
           crs=3308) %>% 
  
  st_transform(4326) %>% 
  mutate(pt = "all") %>% 
  aggregate(.,
            by = list(.$pt),
            function(x) x = x[1]) %>%
  st_centroid() %>% 
  mutate(center_x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
         center_y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2])


## Get maps
ggmap::register_google(key = "AIzaSyB7k_VksgEYRzFHte4vBgd2HUNXRrFA440")
fg_map <- ggmap::get_map(location = c(centroid$center_x, centroid$center_y),
                         maptype = "satellite",
                         zoom = 16)


low_mod_locs <- ggmap::ggmap(fg_map) +
  
  ## Non-Interactions
  geom_sf(aes(
    alpha=w_mean_int),
    alpha=0.2,
    color="white",
    data = low_ints %>% 
      filter(int == FALSE) %>% 
      st_as_sf(coords= c("ind_x","ind_y"),
               crs=3308) %>% 
      st_transform(4326),
    inherit.aes = FALSE) +
  
  ## Non-Interactions
  geom_sf(aes(
    alpha=w_mean_int),
    alpha=0.2,
    color="white",
    data = low_ints %>% 
      filter(int == FALSE) %>% 
      st_as_sf(coords= c("partner_x","partner_y"),
               crs=3308) %>% 
      st_transform(4326),
    inherit.aes = FALSE) +
  
  ## Interactions
  geom_sf(aes(
    alpha=w_mean_int,
    shape=ind_section,
    color=as.character(ind_community)),
    data = low_ints %>% 
      filter(int == TRUE) %>% 
      st_as_sf(coords= c("ind_x","ind_y"),
               crs=3308) %>% 
      st_transform(4326),
    inherit.aes = FALSE) +
  
  ## Interactions
  geom_sf(aes(
    alpha=w_mean_int,
    shape=ind_section,
    color=as.character(ind_community)),
    data = low_ints %>% 
      filter(int == TRUE) %>% 
      st_as_sf(coords= c("partner_x","partner_y"),
               crs=3308) %>% 
      st_transform(4326),
    inherit.aes = FALSE) +
  
  scale_color_manual(name = "Community",
                     breaks = color_scale$breaks,
                     values = color_scale$color) +
  theme_minimal()  +
  theme(legend.position = "none")   +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        strip.text = element_blank(),
        plot.margin = margin(2,2,2,2,"pt"),
        plot.title = element_text(hjust = 0.01, vjust = -7, color="white")) +
  labs(x = NULL, y= NULL) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.4,text_col="white",pad_y=unit(0.4, "cm"),text_cex=0.9)


## High modularity example
high_ints <- int_sum_all %>% 
  left_join(tags,
            by = c("ID1" = "tag")) %>% 
  rename(ind_sex = sex,
         ind_section = section) %>% 
  left_join(tags,
            by = c("ID1" = "tag")) %>% 
  rename(partner_sex = sex,
         partner_section = section) %>% 
  
  mutate(day_hr = paste(day,hour,sep="-")) %>% 
  
  ## High
  filter(day_hr %in% days[2]) %>% 
  
  group_by(dyadID) %>% 
  mutate(mean_int = mean(int_count/obs_count),
         w_mean_int = weighted.mean(int_count/obs_count, obs_count)) %>% 
  left_join(locs %>% 
              select(tag,
                     timegroup,
                     x,
                     y), 
            by = join_by(ID1 == tag,
                         timegroup)) %>%
  ungroup() %>% 
  select(timegroup,ID1,ID2,int,ind_sex,ind_section,partner_sex,partner_section,w_mean_int,
         ind_x=x,ind_y=y) %>% 
  left_join(locs %>% 
              select(tag,
                     timegroup,
                     partner_x = x,
                     partner_y = y), 
            by = join_by(ID2 == tag,
                         timegroup)) %>%
  
  left_join(locs %>% 
              select(tag,
                     timegroup,
                     x,
                     y), 
            by = join_by(ID1 == tag,
                         timegroup)) %>% 
  
  
  left_join(membership_high_df,
            join_by(ID1 == ind)) %>% 
  rename(ind_community = community) %>% 
  left_join(membership_high_df,
            join_by(ID2 == ind)) %>% 
  rename(partner_community = community) %>% 
  mutate(same_community = ifelse(ind_community==partner_community,"yes","no"))

## Centroid
centroid <- high_ints %>% 
  st_as_sf(coords= c("ind_x","ind_y"),
           crs=3308) %>% 
  
  st_transform(4326) %>% 
  mutate(pt = "all") %>% 
  aggregate(.,
            by = list(.$pt),
            function(x) x = x[1]) %>%
  st_centroid() %>% 
  mutate(center_x = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,1],
         center_y = as.matrix((sf::st_coordinates(.data$geometry)), ncol = 2)[,2])


## Get maps
ggmap::register_google(key = "AIzaSyB7k_VksgEYRzFHte4vBgd2HUNXRrFA440")
fg_map <- ggmap::get_map(location = c(centroid$center_x, centroid$center_y+0.001),
                         maptype = "satellite",
                         zoom = 16)


high_mod_locs <- ggmap::ggmap(fg_map) +
  
  ## Non-Interactions
  geom_sf(aes(
    alpha=w_mean_int),
    alpha=0.2,
    color="white",
    data = high_ints %>% 
      filter(int == FALSE) %>% 
      st_as_sf(coords= c("ind_x","ind_y"),
               crs=3308) %>% 
      st_transform(4326),
    inherit.aes = FALSE) +
  
  ## Non-Interactions
  geom_sf(aes(
    alpha=w_mean_int),
    alpha=0.2,
    color="white",
    data = high_ints %>% 
      filter(int == FALSE) %>% 
      st_as_sf(coords= c("partner_x","partner_y"),
               crs=3308) %>% 
      st_transform(4326),
    inherit.aes = FALSE) +
  
  ## Interactions
  geom_sf(aes(
    alpha=w_mean_int,
    shape=ind_section,
    color=as.character(ind_community)),
    data = high_ints %>% 
      filter(int == TRUE) %>% 
      st_as_sf(coords= c("ind_x","ind_y"),
               crs=3308) %>% 
      st_transform(4326),
    inherit.aes = FALSE) +
  
  ## Interactions
  geom_sf(aes(
    alpha=w_mean_int,
    shape=ind_section,
    color=as.character(ind_community)),
    data = high_ints %>% 
      filter(int == TRUE) %>% 
      st_as_sf(coords= c("partner_x","partner_y"),
               crs=3308) %>% 
      st_transform(4326),
    inherit.aes = FALSE) +
  
  scale_color_manual(name = "Community",
                     breaks = color_scale$breaks,
                     values = color_scale$color) +
  theme_minimal()  +
  theme(legend.position = "none") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        strip.text = element_blank(),
        plot.margin = margin(2,2,2,2,"pt"),
        plot.title = element_text(hjust = 0.01, vjust = -7, color="white")) +
  labs(x = NULL, y= NULL) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.4,text_col="white",pad_y=unit(0.4, "cm"),text_cex=0.9)


## Combine
wrap_plots(low_mod_locs, high_mod_locs, 
           low_mod, high_mod,  ncol = 2, nrow = 2) + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = c('A'),
                  tag_suffix = ')',
                  theme = list(legend.position = "bottom")
  )

ggsave("./plots/modularity_plots_w_locs.jpg",
       scale=1,width = 9.85,height=9.85)


