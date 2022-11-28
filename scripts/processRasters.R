library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

#=================================================================================#
# Read rasters from http://evapotranspiration.geography.hawaii.edu/downloads.html
#=================================================================================#
# pull data dictionary
dic <- data.table::fread("data/GIS_data_dictionary.csv")

# get folders containing evapotranspiration data
evap_dirs <- dic %>%
  dplyr::filter(source == "http://evapotranspiration.geography.hawaii.edu/downloads.html" & extent == "evap") %>% # filter to the evap data sets with similar extent
  dplyr::mutate(evap_path1 = paste0("data/GIS/", folder)) %>%
  dplyr::pull(evap_path1)

# make a cool function to get all the rasters
getRasters <- function(dirs){
  # make a big list
  rasters <- NULL
  for(i in unique(dirs)){
    # get the list of files in the directories
    file_list <- list.files(i, recursive = T)
    
    # get the subdirs with hdr.adf files and get the relative path
    df.i <- tibble::tibble(hdr = stringr::str_detect(file_list, pattern = "hdr.adf"),
                           file = file_list,
                           dir = i) %>%
      dplyr::filter(hdr == T) %>%
      dplyr::mutate(var = stringr::str_replace(file, pattern = "/hdr.adf", replacement = ""),
                    path = paste0(dir, "/", file))
    
    # get the var names
    var_names <- df.i$var
    
    # make a list with var names
    var.list <- list[1:length(df.i$path)]
    names(var.list) <- df.i$var
    
    # say something
    message(glue::glue("looking in {i} for rasters"))
    # pull the rasters in the directories and 
    for (j in 1:length(df.i$path)){
      # slice the data
      df.j <- df.i %>%
        dplyr::slice(j)
      # get the raster
      raster.ij <- raster::raster(glue::glue("{df.j$path}"))
      # name it
      names(raster.ij) <- glue::glue("{df.j$var}")
      # putin the list
      var.list[[j]] <- raster.ij
      # say something
      message(glue::glue("--- I found this one {df.j$var}"))
    }
    rasters[[i]] <- var.list
  }
  # unlist all those rasters
  unlisted_rasters <- unlist(rasters)
  # make a raster stack
  raster_stack <- raster::stack(unlisted_rasters)
  # return it
  return(raster_stack)
}  

# Use it ;) 
my.env.rasters <- getRasters(evap_dirs)

#======================================#
# Get habitat status
#======================================#
# get a reference raster 
ref <- raster::raster("data/GIS/Tair_month_raster/tair_ann/hdr.adf")
names(ref) <- "ref_raster" # rename

# load lc and hs from Jacobi, J.D., Price, J.P., Fortini, L.B., Gon, S.M., Berkowitz, P. Baseline land cover.(2017)
lc_raster <- raster::raster("data/GIS/CAH_LandCover/CAH_LandCover.tif")
habstatus_raster <- raster::raster("data/GIS/CAH_HabStatus/CAH_HabStatus.tif") 

# For land cover and habitat status we need to set the correct projection following https://datacarpentry.org/r-raster-vector-geospatial/03-raster-reproject-in-r/
# syntax is projectRaster(RasterObject, crs = CRSToReprojectTo)
lc_rp <- raster::projectRaster(lc_raster, crs = raster::crs(ref),
                               res = raster::res(ref)) # reproject ls raster to reference. Takes a long time!
lc_rp_rs <- raster::resample(lc_rp, ref, method="ngb") # resample lc raster to reference to match extent
names(lc_rp_rs) <- "land_cover" # rename

hs_rp <- raster::projectRaster(lc_raster, crs = raster::crs(ref),
                               res = raster::res(ref)) # reproject hs raster to reference
hs_rp_rs <- raster::resample(hs_rp, ref, method="ngb") # resample to reference to match extent
names(hs_rp_rs) <- "habitat_status" # rename

#======================================#
# Get elevation rasters
#======================================#
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
el_merged_rs <- raster::resample(el_merged, ref, method="bilinear")
names(el_merged_rs) <- "elevation_m" # rename

#===========================================#
# Merge all the rasters and export the stack
#===========================================#
env_rs <- raster::stack(my.env.rasters, el_merged_rs, lc_rp_rs, hs_rp_rs) #, lc_rp_rs, hs_rp_rs
# example plotting with raster
raster::plot(env_rs[[455:458]]) # hab status needs a key for categorical vars

#===========================================#
# Extract values for sampling data
#===========================================#
## load sampling data
load(file = "data/hawaii_sampling/2021-11-11_2021JuneHawaiifulcrum.rds")
sd_1 <- data_out %>%
  dplyr::select(project:strain_name, lat = collection_latitude, long = collection_longitude,
                ambient_humidity,
                substrate,
                ambient_temp = proc_ambient_temperature,
                substrate_temp = proc_substrate_temperature,
                collection_time = collection_local_time,
                collection_date = collection_date_UTC)
rm(data_out)

sd_2 <- readRDS(file = "data/hawaii_sampling/2021-11-12_2021OctoberHawaii_fulcrum.rds") %>%
  dplyr::select(project:strain_name, lat = collection_latitude, long = collection_longitude,
                ambient_humidity,
                substrate,
                ambient_temp = proc_ambient_temperature,
                substrate_temp = proc_substrate_temperature,
                collection_time = collection_local_time,
                collection_date = collection_date_UTC)

sd_3 <- data.table::fread("data/hawaii_sampling/MolEcol_data.csv") %>%
  dplyr::select(project:strain_name, -isotype, lat = collection_latitude, long = collection_longitude,
                ambient_humidity,
                substrate,
                ambient_temp = ambient_temperature,
                substrate_temp = substrate_temperature,
                collection_time = collection_local_time,
                collection_date = collection_date_UTC)

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

# OR? Make sf object: https://www.jessesadler.com/post/gis-with-r-intro/
sd_sf <- sf::st_as_sf(sd, coords = c("long", "lat"), crs = raster::crs(ref))

# extract env values from the rasterStack and join them with the spatial dataframe
sf_df_env_ex <- sd_sf %>%
  dplyr::bind_cols(as.data.frame(raster::extract(env_rs, sd_sf)))

#=====================================================#
# Save the data
#=====================================================#
# drop geometry and remove NAs
stats_df <- sf_df_env_ex %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>% # extract the lon and lat from geometry
  sf::st_drop_geometry(.) %>% # remove the geometry
  tidyr::drop_na(.) %>% # drop rows with NAs
  dplyr::rename_with(stringr::str_replace, pattern = "data\\.GIS\\..*\\.", replacement = "", 
                     matches("data\\.GIS\\..*\\.")) %>%
  dplyr::select(project,
                c_label,
                id,
                ce,
                substrate,
                ambient_humidity,
                ambient_temp,
                substrate_temp,
                collection_time,
                collection_date,
                everything())

# get the date
today <- format(Sys.Date(), "%Y%m%d")

# output the stats_df file
save(stats_df, file = glue::glue("data/{today}_stats_df.Rdata"))