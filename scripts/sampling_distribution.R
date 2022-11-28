library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# load sampling data
load("data/2022-09-24_collection_env_var.rda")

# reshape to lat long
plot_df <- sf_df_env_ex %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  dplyr::arrange(ce)

# Get Hawaii map
hi <- sf::st_read("data/GIS/cb_2018_us_state_500k") %>%
  dplyr::filter(NAME == "Hawaii")
hibox = c(xmin = -159.87, ymin = 18.86, xmax = -154.76, ymax = 22.31) # make crop box
hi_crop <- sf::st_crop(hi, hibox) # crop all the small islands


hi_clust_map <- ggplot() + geom_sf(data = hi_crop, size = 0.25, fill = "light grey") +
  #geom_segment(data = group_map2_join, aes(x = long, y = lat, xend = x2, yend = y2), colour = 'black', size = 0.15) +
  #geom_label_repel(data = sf_df_env_ex, 
   #                aes( x=lon, y=lat, fill = ce), # data point size
    #               size = 2.5, # font size in the text labels
     #              point.padding = 0, # additional padding around each point
      #             min.segment.length = 0, # draw all line segments
       #            max.iter = 1e5, # stop after 1 second, or after 100,000 iterations
        #           box.padding = 0.3 # additional padding around each text label
  #) +
  geom_point(data = plot_df,
             aes(x=lon, y=lat, fill = as.character(ce)), shape =21, stroke = 0.25, alpha = 0.5) +
  ggthemes::theme_map()+
  #scale_size("isotype count", breaks = c(1, 5, 10, 25), labels = c('1','2-5','6-10','11-25')) +
  #scale_fill_manual("", values = isl_cols, guide = 'none') +
  #scale_color_manual("", values = isl_cols, guide = 'none') +
  #continuous_size_manual("distinct isotypes", values = c()) +
  theme(text = element_text(size=12), legend.position = "left") +
  labs(fill = "occurrence") +
  ggsn::scalebar(hi_crop, dist = 100, dist_unit = "km",
                 transform = TRUE, model = "WGS84", st.size = 3, border.size = 0.25, height = 0.01,  location = "bottomleft")

hi_clust_map
ggsave(hi_clust_map, filename = "plots/sampling_distribution.png", width = 7.5, height= 7.5)
