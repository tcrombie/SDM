library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))
 
#=====================================================#
# Cleaning raster data for SDM
#=====================================================#
# rasters from Giambelluca, T.W., Shuai, X., Barnes, M.L., Alliss, R.J. Evapotranspiration of Hawai ’i. (2014)
tair_ann_raster <- raster::raster("data/GIS/Tair_month_raster/tair_ann/hdr.adf")
names(tair_ann_raster) <- "tair_ann" # rename
soil_moisture_raster <- raster::raster("data/GIS/SoilMoisture_month_raster/sl_mst_ann/hdr.adf")
names(soil_moisture_raster) <- "soil_moist_ann" # rename
tsurf_ann_raster <- raster::raster("data/GIS/TSurf_ann_hr_raster/tsurf_ann_01/hdr.adf")
names(tsurf_ann_raster) <- "tsurf_ann" # rename
lai_ann_raster <- raster::raster("data/GIS/LAI_month_raster/lai_ann/hdr.adf")
names(lai_ann_raster) <- "lai_ann" # rename
# get rainfall mm annual. The projection, extent, and resolution match the reference raster, good!
rainfall_ann_mm_raster <- raster::raster("data/GIS/StateRFGrids_mm/staterf_mmann/hdr.adf")
names(rainfall_ann_mm_raster) <- "rain_mm_ann" # rename

# load lc and hs from Jacobi, J.D., Price, J.P., Fortini, L.B., Gon, S.M., Berkowitz, P. Baseline land cover.(2017)
lc_raster <- raster::raster("data/GIS/CAH_LandCover/CAH_LandCover.tif")
habstatus_raster <- raster::raster("data/GIS/CAH_HabStatus/CAH_HabStatus.tif") 

# For land cover and habitat status we need to set the correct projection following https://datacarpentry.org/r-raster-vector-geospatial/03-raster-reproject-in-r/
# syntax is projectRaster(RasterObject, crs = CRSToReprojectTo)
lc_rp <- raster::projectRaster(lc_raster, crs = raster::crs(tair_ann_raster),
                               res = raster::res(tair_ann_raster)) # reproject ls raster to reference. Takes a long time!
lc_rp_rs <- raster::resample(lc_rp, tair_ann_raster, method="ngb") # resample lc raster to reference to match extent
names(lc_rp_rs) <- "land_cover" # rename

hs_rp <- raster::projectRaster(lc_raster, crs = raster::crs(tair_ann_raster),
                               res = raster::res(tair_ann_raster)) # reproject hs raster to reference
hs_rp_rs <- raster::resample(hs_rp, tair_ann_raster, method="ngb") # resample to reference to match extent
names(hs_rp_rs) <- "habitat_status" # rename

# load elevation tiles from SRTM data https://srtm.csi.cgiar.org/srtmdata/. We will need to merge these.
el1 <- raster::raster("data/GIS/srtm_05_08/srtm_05_08.tif")
el2 <- raster::raster("data/GIS/srtm_05_09/srtm_05_09.tif")
el3 <- raster::raster("data/GIS/srtm_06_09/srtm_06_09.tif")
# make a list of tiles
x <- list(el1, el2, el3)
# name elements in the list
names(x) <- c("x", "y", "z")
# add filename and overwrite
x$filename <- 'data/GIS/elevation_merged.tif'
x$overwrite <- TRUE
# execute raster::merge with el_list object and save to elevation_merged.tif
el_merged <- do.call(raster::merge, x)

# In order to generate a clean data structure for modeling and combining all grids into one stack of grid layers for our study area (this facilitates subsequent analyses) we first resample all grids with deviating resolution and extent (here el_merged, lc_raster, habstatus_raster) with reference to a master grid (here tair_ann_raster).
# Two methods are available within the resample() command to adjust the resolution, namely nearest neighbor (method=“ngb”) and bilinear (method=“bilinear”). The first is fast and picks the value at the central location of the new grid from the old grid with no interpolation. This method is usually recommended for categorical data.The bilinear method is much slower, as it interpolates from all neighboring cells to calculate the new cell values. It is usually preferred for continuous data.
el_merged_rs <- raster::resample(el_merged, tair_ann_raster, method="bilinear")
names(el_merged_rs) <- "elevation_m" # rename

# Now lets stack all the env variables neatly
env_rs <- raster::stack(tair_ann_raster, soil_moisture_raster, tsurf_ann_raster, lai_ann_raster, rainfall_ann_mm_raster, el_merged_rs) #, lc_rp_rs, hs_rp_rs

# export the raster stack object to improve storage for github.
#save(env_rs, file = "data/processed_environmental_variable_raster_stack.rda")

# example plotting with ggplot
raster_plot_tair_ann <- ggplot2::ggplot() +
  ggplot2::geom_raster(data = raster::as.data.frame(tair_ann_raster, xy = TRUE), 
              ggplot2::aes(x = x, y = y, fill = tair_ann)) + 
  ggplot2::scale_fill_gradientn(name = "tair_ann", colors = terrain.colors(10)) + 
  ggplot2::coord_quickmap()

# example plotting with raster
raster::plot(env_rs[[1:6]], col=rainbow(100,start=.0,end=.8))

# load env variables
#load(file = "data/processed_environmental_variable_raster_stack.rda")
#========================================================#
# resample collection data
#========================================================#
# load sampling data
load(file = "data/hawaii_sampling/2021-11-11_2021JuneHawaiifulcrum.rds")
sd_1 <- data_out %>%
  dplyr::select(project:strain_name, lat = collection_latitude, long = collection_longitude)
rm(data_out)

sd_2 <- readRDS(file = "data/hawaii_sampling/2021-11-12_2021OctoberHawaii_fulcrum.rds") %>%
  dplyr::select(project:strain_name, lat = collection_latitude, long = collection_longitude)

sd_3 <- data.table::fread("data/hawaii_sampling/MolEcol_data.csv") %>%
  dplyr::select(project:strain_name, -isotype, lat = collection_latitude, long = collection_longitude)

# bind the sampling data
sd <- rbind(sd_1, sd_2, sd_3) %>%
  dplyr::filter(!is.na(lat)) %>%
  dplyr::mutate(id = paste0(project, c_label)) %>%
  dplyr::mutate(ce = case_when(!is.na(species_id) & species_id %in% c("Caenorhabditis elegans") ~ 1,
                               !is.na(species_id) & !(species_id %in% c("Caenorhabditis elegans")) ~ 0,
                               TRUE ~ 0)) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(ce = ifelse(sum(ce) >= 1, 1, 0)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(id, .keep_all = T) %>%
  dplyr::select(-strain_name, -species_id, -s_label)

# Create data frame of only longitude and latitude values
#coords <- sd %>%
 # dplyr::select(long, lat)
# Make a spatial points dataframe
#sp_df <- sp::SpatialPointsDataFrame(coords = coords,
 #                                   data = sd,  
  #                                  proj4string = raster::crs(tair_ann_raster))

# OR? Make sf object: https://www.jessesadler.com/post/gis-with-r-intro/
sd_sf <- sf::st_as_sf(sd, coords = c("long", "lat"), crs = raster::crs(tair_ann_raster))

# extract env values from the rasterStack and join them with the spatial dataframe
sf_df_env_ex <- sd_sf %>%
  dplyr::bind_cols(as.data.frame(raster::extract(env_rs, sd_sf)))

# export this
#save(sf_df_env_ex, file = "data/2022-03-15_collection_env_var.rda")

#======================================================#
# assess pariwise corelations and multicollinearity
#======================================================#
# load the raster stack. Need to watch out that raster layers are available from memory with load. not working yet. need to run code above to generate raster stack
#load(file = "data/processed_environmental_variable_raster_stack.rda")

# load the full sampling dataframe
#load("data/2022-03-15_collection_env_var.rda")

# drop geometry and remove NAs
stats_df <- sf_df_env_ex %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>% # extract the lon and lat from geometry
  sf::st_drop_geometry(.) %>% # remove the geometry
  tidyr::drop_na(.) # drop rows with NAs

# make a pairwise correlation plot with ecospat package
ecospat::ecospat.cor.plot(stats_df %>% dplyr::select(5:10))

# need to convert stats_df to dataframe from tibble. WOW! need to drop temp variables, keep elevation, lai, and rainfall
vif1 <- usdm::vif(as.data.frame(stats_df %>% dplyr::select(5:10)))
vif2 <- usdm::vif(as.data.frame(stats_df %>% dplyr::select(lai_ann, rain_mm_ann, elevation_m)))
usdm::vifcor(as.data.frame(stats_df %>% dplyr::select(5:10)), th = 0.7) 

# other variables could be included in this analysis, just working with GIS data for now.
#======================================================#
# Regression models
#======================================================#
glm1 <- glm(ce ~ 1 + elevation_m + lai_ann + rain_mm_ann, data = stats_df, family = "binomial")
anova(glm1)
stats::AIC(glm1)

# add quadratic terms, want a 
glm2 <- glm(ce ~  poly(elevation_m, 2) + lai_ann + rain_mm_ann,
            data = stats_df, family = "binomial")
anova(glm2)
stats::AIC(glm2)

glm_small <- glm(ce ~ 1 + poly(elevation_m, 2),
                 data = stats_df, family = "binomial")
stats::AIC(glm_small)

#=========================================================#
# compare models with VIF pruned env vars
#=========================================================#
glmStart <- stats::glm(ce ~ 1, data = stats_df, family = "binomial")
glm.formula <- stats::formula("ce ~ poly(elevation_m,2) + poly(rain_mm_ann,2) + lai_ann + elevation_m:rain_mm_ann + elevation_m:lai_ann + rain_mm_ann:lai_ann")
glmModAIC <- MASS::stepAIC(glmStart, glm.formula, data = stats_df, direction = "forward", trace = FALSE, k = 2, control = glm.control(maxit = 100))
anova(glmModAIC)
stats::AIC(glmModAIC)
# elevation is best predictor, then lai and rainfall. Interaction terms help but not much. 
# select best model and predict for every cell in raster grid

# plot the response curves
rp <- biomod2::response.plot2(models = c("glm1", "glm2", "glmModAIC"), 
                     Data = as.data.frame(stats_df %>% dplyr::select(lai_ann, rain_mm_ann, elevation_m)),
                     show.variables = c("lai_ann", "rain_mm_ann", "elevation_m"),
                     fixed.var.metric = "mean",
                     plot = FALSE,
                     use.formal.names = TRUE)

gg.rp <- ggplot2::ggplot(rp, aes(x = expl.val, y = pred.val, lty = pred.name)) +
  geom_line() +
  labs(y = "prob of occurance", x = "", lty = "") +
  theme_bw() +
  facet_grid(~expl.name, scales = "free_x")
gg.rp

cowplot::ggsave2(gg.rp, filename = "plots/model_response_cruves.pdf", width = 7.5, height = 5)


# make a raster layer with new values
r0 <- raster::predict(env_rs, model = glm1, progress='text', type = "response")
r1 <- raster::predict(env_rs, model = glmModAIC, progress='text', type = "response")
r2 <- raster::predict(env_rs[[4:6]], model = glm2, progress='text', type = "response")
# plot  predictions from raster with ggplot
# convert to a df for plotting in two steps, First, to a SpatialPointsDataFrame then to a dataframe.
r0_pts <- raster::rasterToPoints(r0, spatial = TRUE)
r1_pts <- raster::rasterToPoints(r1, spatial = TRUE)
r2_pts <- raster::rasterToPoints(r2, spatial = TRUE)
r0_df  <- data.frame(r0_pts)
r1_df  <- data.frame(r1_pts)
r2_df  <- data.frame(r2_pts)

# plot r1df with ggplot
glm1_pred_plot <- ggplot2::ggplot() +
  ggplot2::geom_raster(data = r0_df , aes(x = x, y = y, fill = layer)) + 
  ggplot2::ggtitle(glue::glue("glm1: ce ~ 1 + lai_ann + rain_mm_ann + elevation_m\nAIC:{round(stats::AIC(glm1), 0)}")) +
  labs(fill = "occ. prob.") +
  ggthemes::theme_map() +
  viridis::scale_fill_viridis() +
  ggplot2::coord_quickmap()
glm1_pred_plot

# plot r1df with ggplot
glmModAIC_pred_plot <- ggplot2::ggplot() +
  ggplot2::geom_raster(data = r1_df , aes(x = x, y = y, fill = layer)) + 
  ggplot2::ggtitle(glue::glue("glmModAIC: ce ~ poly(elevation_m, 2) + poly(rain_mm_ann, 2) +
                            lai_ann + lai_ann:rain_mm_ann + lai_ann:elevation_m\nAIC:{round(stats::AIC(glmModAIC), 0)}")) +
  labs(fill = "occ. prob.") +
  ggthemes::theme_map() +
  viridis::scale_fill_viridis() +
  ggplot2::coord_quickmap()

# plot r2df with ggplot
glm2_pred_plot <- ggplot2::ggplot() +
  ggplot2::geom_raster(data = data.frame(raster::rasterToPoints(r2, spatial = TRUE)), aes(x = x, y = y, fill = layer)) + 
  viridis::scale_fill_viridis() +
  ggplot2::labs(title = glue::glue("glm2 ce ~ poly(elevation_m, 2) + lai_ann + rain_mm_ann\nAIC:{round(stats::AIC(glm2), 0)}")) +
  labs(fill = "occ. prob.") +
  ggthemes::theme_map() +
  ggplot2::coord_quickmap()

cowplot::ggsave2(glm1_pred_plot, filename = "plots/glm1_pred_plot.png", width = 7.5, height = 5)
cowplot::ggsave2(glm2_pred_plot, filename = "plots/glm2_pred_plot.png", width = 7.5, height = 5)
cowplot::ggsave2(glmModAIC_pred_plot, filename = "plots/glmModAIC_pred_plot.png", width = 7.5, height = 5)

#=========================================================#
# compare models with ALL env vars NEEDS COMPLETION
#=========================================================#
glmStart <- stats::glm(ce ~ 1, data = stats_df, family = "binomial")
glm.formula <- stats::formula("ce ~ poly(tair_ann,2)+ poly(elevation_m,2) + poly(tair_ann, 2) + poly(rain_mm_ann,2) 
                              + tair_ann + soil_moist_ann + tsurf_ann lai_ann rain_mm_ann elevation_m 
                              + lai_ann:rain_mm_ann + lai_ann:elevation_m + rain_mm_ann:elevation_m")
glmModAIC <- MASS::stepAIC(glmStart, glm.formula, data = stats_df, direction = "both", trace = FALSE, k = 2, control = glm.control(maxit = 100))
anova(glmModAIC)
stats::AIC(glmModAIC)

#=====================================================#
# better tools
#=====================================================#

# install sdmTMB package with C++ compiler in Xcode 
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)

# look at the sdmTMB package here: https://pbs-assess.github.io/sdmTMB/index.html

#===================================================#
# old tutorial
#====================================================#
# 
# # load example data
# mammals_data <- read.csv("data/tutorial/mammals_and_bioclim_table.csv")
# 
# # A simple envelope model built with all data, and 97.5 and 95 percentiles of each variable
# pred_BIOCLIM <- biomod2::sre(Response = mammals_data$VulpesVulpes,
#                                  Explanatory = mammals_data[, c("bio3", "bio7", "bio11","bio12")],
#                                  NewData = mammals_data[, c("bio3", "bio7", "bio11","bio12")],
#                                  Quant = 0)
# pred_BIOCLIM_025 <- biomod2::sre(Response = mammals_data$VulpesVulpes,
#                                  Explanatory = mammals_data[, c("bio3", "bio7", "bio11","bio12")],
#                                  NewData = mammals_data[, c("bio3", "bio7", "bio11","bio12")],
#                                  Quant = 0.025)
# pred_BIOCLIM_05 <- biomod2::sre(Response = mammals_data$VulpesVulpes,
#                                  Explanatory = mammals_data[, c("bio3", "bio7", "bio11","bio12")],
#                                  NewData = mammals_data[, c("bio3", "bio7", "bio11","bio12")],
#                                  Quant = 0.05)
# 
# # Filtering environmental variables
# # 1) Assess pairwise correlation between variables prior to use in any model fitting
# data <- data.table::fread("data/tutorial/bioclim_table.csv")
# # make a pairwise correlation plot with ecospat package
# ecospat::ecospat.cor.plot(data[,3:7])
# 
# # 2) Identifying Reducing multicollinearity problems. "sometimes a hidden correlation structure that is not clearly visible in the pairwise correlation analysis.
# # It can only be detected with a variance inflation factor (VIF) analysis (Harrell, 2001; Guisan et al., 2002, 2006b; Hair et al., 2006)
# # VIF estimates the severity of the effect of multicollinearity, by measuring the extent to which variance in a regression increases due to collinearity compared to when uncorrelated variables are used.
# # VIF tests are recom- mended, especially when numerous variables are added to a regression, as they detect the variables’ linear correlation structure."
# # Use the vif function from usdm package. Typically values from 5 to 10 are considered as critical for multi-variable correlation. Some authors suggest that VIF values of up to 20 can be accepted, but we (Guisan 2017) do not recommend going above 10.
# usdm::vif(data[,3:7])
# # bio4 has a very high VIF. We will remove it and re-run.
# usdm::vif(data %>% dplyr::select(-bio4, -X_WGS84, -Y_WGS84))
# # can use vifcor function too
# usdm::vifcor(data %>% dplyr::select(-X_WGS84, -Y_WGS84), th = 0.7)
# 
# # 3) Variabel transformation if necessary. The bio12 vaiable is heavily left skewed. We can transform to reduce correlation.
# cor(data$bio12,data$bio7)
# # log transformation. Add small value to avoid error.
# cor(log(data$bio12+.001),data$bio7)
# 
# #====================================================#
# # Regression modeling
# #====================================================#
# # for presence-absence data use "binomial" family.
# glm1 <- stats::glm(VulpesVulpes ~ 1 + bio3 + bio7 + bio11 + bio12, data = mammals_data, family = "binomial")
# glm2 <- stats::glm(VulpesVulpes ~ 1 + poly(bio3, 2) + poly(bio7, 2) + poly(bio11, 2) + poly(bio12, 2), data = mammals_data, family = "binomial")
# 
# # ploting the models with dismo package "level.plot" function. This does not require absence data and any envrionmental variable can be used. However, it is important to filter env variables to remove highly correlated pairs or variables that fail coliniarity tests
# par(mfrow = c(2, 2))
# biomod2::level.plot(mammals_data$VulpesVulpes, XY = mammals_data[, c("X_WGS84", "Y_WGS84")], color.gradient = "grey", cex = 0.3,
#              show.scale = F, title = "Original data")
# biomod2::level.plot(pred_BIOCLIM, XY = mammals_data[, c("X_WGS84", "Y_WGS84")], color.gradient = "grey", cex = 0.3,
#              show.scale = F, title = "BIOCLIM 100%")
# biomod2::level.plot(pred_BIOCLIM_025, XY = mammals_data[, c("X_WGS84", "Y_WGS84")], color.gradient = "grey", cex = 0.3,
#              show.scale = F, title = "BIOCLIM 97.5%")
# 
# 
# # regression based approach
# 
# 
# #================================================#
# # downsampling occurance data with gridsample
# #================================================#
# # I have cells with a huge number of samples. These should be downsampled to avoid bias.
# 
# #1 make a multi-layer raster object for all climate variables. This means they are all 
# 
#==================================================#
# old prediction code
#================================================#
# predict for newdata. I can't get the values out of the rasters env_rs[[1:5]]!! Is this a rename issue from above?
#xy <- raster::as.data.frame(raster::rasterToPoints(env_rs[[6]], spatial = F))
# #coords <- xy[,1:2]
# 
# # get a dataframe of cell points from the raster stack
# p_df <- raster::as.data.frame(raster::rasterToPoints(env_rs, na.rm = , spatial = F)) # working quickly and correctly with 
# # p_df <- raster::as.data.frame(raster::rasterToPoints(env_rs[[6]], na.rm = , spatial = F))
# 
# # make predictions with model for all cells in raster layer
# p <- as.data.frame(stats::predict.glm(object = glmModAIC, newdata = p_df, type = "response")) %>%
#   dplyr::bind_cols(p_df) %>%
#   dplyr::select(prob = 1, everything())
# 
# p_sf <- sf::st_as_sf(p, coords = c("x", "y"), crs = raster::crs(env_rs[[6]]))
