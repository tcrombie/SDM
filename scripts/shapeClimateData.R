library(tidyverse)
library(lubridate)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# load data from processRasters.R
load("data/20221012_stats_df.Rdata")

## load sampling data to extract Caenorhabditis and nematode postive values
load(file = "data/hawaii_sampling/2021-11-11_2021JuneHawaiifulcrum.rds")
sd_1 <- data_out %>%
  dplyr::select(project:strain_name, lat = collection_latitude, long = collection_longitude,
                ambient_humidity,
                substrate,
                ambient_temp = proc_ambient_temperature,
                substrate_temp = proc_substrate_temperature,
                collection_time = collection_local_time,
                collection_date = collection_date_UTC,
                worms_on_sample)

sd_2 <- readRDS(file = "data/hawaii_sampling/2021-11-12_2021OctoberHawaii_fulcrum.rds") %>%
  dplyr::select(project:strain_name, lat = collection_latitude, long = collection_longitude,
                ambient_humidity,
                substrate,
                ambient_temp = proc_ambient_temperature,
                substrate_temp = proc_substrate_temperature,
                collection_time = collection_local_time,
                collection_date = collection_date_UTC,
                worms_on_sample)

sd_3 <- data.table::fread("data/hawaii_sampling/MolEcol_data.csv") %>%
  dplyr::select(project:strain_name, -isotype, lat = collection_latitude, long = collection_longitude,
                ambient_humidity,
                substrate,
                ambient_temp = ambient_temperature,
                substrate_temp = substrate_temperature,
                collection_time = collection_local_time,
                collection_date = collection_date_UTC,
                worms_on_sample)

# bind the sampling data
sd <- rbind(sd_1, sd_2, sd_3) %>%
  dplyr::filter(!is.na(lat)) %>%
  dplyr::mutate(n = case_when(worms_on_sample == "" ~ 1,
                                             worms_on_sample == "Yes" ~ 1,
                                             worms_on_sample == "No" ~ 0,
                                             worms_on_sample == "Tracks" ~ 0,
                                             TRUE ~ -10000)) %>%
  dplyr::mutate(id = paste0(project, c_label)) %>%
  dplyr::mutate(ce = case_when(!is.na(species_id) & species_id %in% c("Caenorhabditis elegans") ~ 1,
                               !is.na(species_id) & !(species_id %in% c("Caenorhabditis elegans")) ~ 0,
                               TRUE ~ -10000),
                c = case_when(stringr::str_detect(species_id, pattern = "Caenorhabditis") == T ~ 1,
                              stringr::str_detect(species_id, pattern = "Caenorhabditis") == F ~ 0,
                              TRUE ~ -10000)) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(ce = ifelse(sum(ce) >= 1, 1, 0),
                c = ifelse(sum(c) >= 1, 1, 0)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(id, .keep_all = T) %>%
  dplyr::select(project, c_label, id, n, c, ce)

# bind the environmental data
merge_df <- left_join(stats_df, sd) %>%
  dplyr::select(project, c_label, id, n, c, ce, everything())

time_matched_df <- merge_df %>%
  dplyr::select(-contains(c('_ann'))) %>% #remove annual values
  tidyr::pivot_longer(cols = aet_mm_apr:wce_mm_sep) %>%
  dplyr::mutate(col_month = lubridate::month(collection_date),
                col_month_lag1 = lubridate::month(collection_date %m-% months(1)),
                col_month_lag2 = lubridate::month(collection_date %m-% months(2)),
                col_month_next1 = lubridate::month(collection_date %m+% months(1))) %>% 
  dplyr::mutate(env_par_month = stringr::str_extract(name, pattern = "([^_]+$)"),
                env_name = stringr::str_extract(name, pattern = "(.*)(?=_)"),
                env_par_month2 = case_when(env_par_month == "jan" ~ 1,
                                       env_par_month == "feb" ~ 2,
                                       env_par_month == "mar" ~ 3,
                                       env_par_month == "apr" ~ 4,
                                       env_par_month == "may" ~ 5,
                                       env_par_month == "jun" ~ 6,
                                       env_par_month == "jul" ~ 7,
                                       env_par_month == "aug" ~ 8,
                                       env_par_month == "sep" ~ 9,
                                       env_par_month == "oct" ~ 10,
                                       env_par_month == "nov" ~ 11,
                                       env_par_month == "dec" ~ 12)) %>%
  dplyr::mutate(value_month_of_col = ifelse(col_month == env_par_month2, value, NA_real_),
                value_1_month_prev = ifelse(col_month_lag1 == env_par_month2, value, NA_real_),
                value_2_month_prev = ifelse(col_month_lag2 == env_par_month2, value, NA_real_),
                value_1_month_after = ifelse(col_month_next1 == env_par_month2, value, NA_real_)) %>%
  dplyr::select(-name, -value, -env_par_month2, -env_par_month) %>%
  tidyr::pivot_longer(cols = value_month_of_col:value_1_month_after) %>%
  dplyr::filter(!is.na(value)) %>%
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  dplyr::group_by(id, env_name) %>%
  dplyr::mutate(value_avg_around_col_month = sum(value_month_of_col, value_1_month_after, value_1_month_prev)/3) %>%
  dplyr::select(-value_1_month_after) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_longer(cols = value_2_month_prev:value_avg_around_col_month) %>%
  dplyr::mutate(name = stringr::str_replace(name, pattern = "value", replacement = ""),
                name = paste0(env_name,name)) %>%
  dplyr::select(-env_name, -lon:-col_month_next1, -land_cover, -habitat_status) %>%
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  dplyr::arrange(collection_date, collection_time)
  
# observe it
#options(scipen=999)
#glimpse(time_matched_df)

# save it
save(time_matched_df, file = "data/20221028_time_matched_df.Rda")

