## Make plots and tables from models

## Housekeeping #####
library(tidyverse)
library(brms)
library(tidybayes)
library(modelr)
library(patchwork)
library(sjPlot)

theme_set(theme_classic())

## Plot density #######

## Read in model
dens_mods <- readRDS("./outputs/models/density_brms.RDS")

## Combine models
dens_mods_c <- combine_models(dens_mods)

## Read in raw data
gd_df <- readRDS("./outputs/sna/ewms.RDS") %>%
  
  ## Scale mean temp and mean wind
  group_by(samp) %>%
  mutate(hour = as.numeric(scale(hour)),
         mean_temp = as.numeric(scale(mean_temp)),
         mean_wind = as.numeric(scale(mean_wind)))

## Create subset to use for generating predited values - only need values to generate the range
gd_df_sub <- gd_df %>%
  slice_sample(prop = 0.02)

## New data for both density and modularity predictions
new_data =  expand.grid(
  hour = seq_range(gd_df_sub$hour, n = 101),
  mean_temp = seq_range(gd_df_sub$mean_temp, n = 101),
  mean_wind = seq_range(gd_df_sub$mean_wind, n = 101)) %>%
  mutate(across(everything(), ~ round(.x,1))) %>%
  distinct()

## Expectation of prediction draws
epr = dens_mods_c %>%
  epred_draws(newdata = new_data,
              ndraws = 30,
              re_formula = NA)

## Pivot expected predictions
epr_l_dens <- epr %>%
  ungroup() %>%
  select(hour,
         mean_temp,
         mean_wind,
         density = .epred) %>%
  pivot_longer(cols = c(-density),
               names_to = "predictor")

## Pivot observed data
gd_df_l_dens <- gd_df %>%
  ungroup() %>%
  select(hour,
         mean_temp,
         mean_wind,
         density) %>%
  pivot_longer(cols = c(-density),
               names_to = "predictor")

fill_breaks_df <- data.frame(breaks = c(0.5,0.8,.95),
                             color = wesanderson::wes_palette("Zissou1", 100, type = "continuous")[rev(c(5,40,80))])

# ## Plot labels
# labels<- c(
#   hour = "Hour of day",
#   mean_temp = "Mean hourly\ntemperature (\u00B0C)",
#   mean_wind = "Mean hourly\nwind speed (km/h)"
# )

## Plot labels
labels<- c(
  hour = "Hour of day",
  mean_temp = "Mean hourly\ntemperature",
  mean_wind = "Mean hourly\nwind speed"
)


## Plot predictions with observed data
(dens_plot  <- ggplot(epr_l_dens) +
    geom_jitter(aes(x= value,
                    y=density),
                data = gd_df_l_dens,
                alpha=0.05,
                width = 0.1) +
    stat_lineribbon(aes(x = value,
                        y = density),
                    .width = c(0.5,0.8,.95),
                    alpha = 0.6) +
    labs(x = "Z-transformed value",
         y = "Predicted density index") +
    theme_classic(base_size = 18) +
    # scale_color_continuous() +
    facet_grid(~predictor, 
               labeller = labeller(
                 predictor = labels
               )) +
    scale_fill_manual(name = "Credible\ninterval\nlevel",
                      breaks = fill_breaks_df$breaks,
                      values = fill_breaks_df$color) +
    theme(legend.position = "none"))  

## Plot modularity #######

## Read in model
mod_mods <- readRDS("./outputs/models/modularity_brms.RDS")

## Combine models
mod_mods_c <- combine_models(mod_mods)

## Expectation of prediction draws
epr = mod_mods_c %>%
  epred_draws(newdata = new_data,
              ndraws = 10,
              re_formula = NA)

## Pivot expected predictions
epr_l_mod<- epr %>%
  ungroup() %>%
  select(hour,
         mean_temp,
         mean_wind,
         modularity = .epred) %>%
  pivot_longer(cols = c(-modularity),
               names_to = "predictor")

## Pivot observed data
gd_df_l_mod <- gd_df %>%
  ungroup() %>%
  select(hour,
         mean_temp,
         mean_wind,
         modularity) %>%
  pivot_longer(cols = c(-modularity),
               names_to = "predictor")

## Plot predictions with observed data
(mod_plot  <- ggplot(epr_l_mod) +
    geom_jitter(aes(x= value,
                    y=modularity),
                data = gd_df_l_mod,
                alpha=0.05,
                width = 0.2) +
    stat_lineribbon(aes(x = value,
                        y = modularity),
                    .width = c(0.5,0.8,.95),
                    alpha = 0.6) +
    labs(x = "Z-transformed value",
         y = "Predicted modularity index") +
    theme_classic(base_size = 18) +
    # scale_color_continuous() +
    facet_grid(~predictor, 
               labeller = labeller(
                 predictor = labels
               )) +
    scale_fill_manual(name = "Credible\ninterval\nlevel",
                      breaks = fill_breaks_df$breaks,
                      values = fill_breaks_df$color)) 

## Combine plots
plot_list <- list(dens_plot, mod_plot)
(sn__metric_plots <- patchwork::wrap_plots(plot_list, 1,2,
                                           guides = "keep") +
    plot_annotation(tag_levels = 'A'))

ggsave(plot=sn__metric_plots,
       filename="./plots/sn_metrics.jpg",
       width = 17,
       height=10)

## Model tables ######

## Table from models
mod_table <- tab_model(dens_mods_c, mod_mods_c,
                       pred.labels = c("Intercept",
                                       "Hour",
                                       "Hour^2",
                                       "Wind speed",
                                       "Temperature",
                                       "Wind speed * Temperature"),
                       dv.labels = c("Density", "Modularity")
)


