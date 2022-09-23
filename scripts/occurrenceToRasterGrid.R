library(tidyverse)


# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# load the raster stack object to improve storage for github.
load("data/processed_environmental_variable_raster_stack.rda")

# load the full data with ce abundance
load("data/2022-03-15_collection_env_var.rda")

#===========================================================================#
# get binomial occurrence and and sampling effort values for each raster cell
#===========================================================================#
# get ce present absent for each unique raster cell sampled. 
uniq_cell_dat <- sf_df_env_ex %>%
  dplyr::group_by(tair_ann,soil_moist_ann,tsurf_ann,lai_ann,rain_mm_ann,elevation_m,land_cover,habitat_status) %>% # find unique raster cells
  dplyr::mutate(ce_cell = sum(ce), # find the number of ce found
                cell_sample_n = n(), # find the number of samples in cell
                cell_fraction_ce = ce_cell/cell_sample_n, # get fraction ce positive in cell
                ce_cell = ifelse(ce_cell > 0, 1, 0), # convert to binomial ce positive within cell
                cell_id = cur_group_id()) %>% # get cell group index (random order)
  dplyr::distinct(cell_id, .keep_all = T) %>% # just keep unique cells
  dplyr::ungroup()

# show regression of positive cells by sampling effort
binomial_dat <- uniq_cell_dat %>%
  dplyr::group_by(cell_sample_n) %>%
  dplyr::mutate(succ = sum(ce_cell),
                fail = n()-succ) %>%
  dplyr::distinct(cell_sample_n, .keep_all = T) %>%
  dplyr::select()

# plot fraction ce by sample effort
occur_x_sampEff_p <- ggplot(data = uniq_cell_dat, aes(x = cell_sample_n, y = ce_cell)) + #cell_fraction_ce) +
  geom_point(shape = 21, alpha = 0.5) +
  geom_smooth(size = 0.5, color = "red",
              method="glm",
              method.args=list(family="binomial"),
              formula = y ~ x) +
  theme_bw() +
  annotate("text", label = 'glm(occurrence ~ sampling effort, family = "binomial")', x = 125, y = 0.15) +
  labs(x = "Number of samples in raster cell", y = "Occurrence in raster cell", title = "Occurrence probability scales with sampling effort")
cowplot::ggsave2(occur_x_sampEff_p, filename = "plots/occurrence_probability_by_smpling_effort_in_raster_cell.png", width = 5, height = 5)

# plot distribution of sampling effort in all raster cells
sampEff_dist_p <- ggplot(data = uniq_cell_dat) +
  aes(x = cell_sample_n, fill = as.character(ce_cell)) +
  geom_histogram(bins = 100) +
  #facet_wrap(~ce_cell, scales = "free_y") +
  labs(x = "Number of samples in raster cell", y = "Count", fill = "Occurrence", title = glue::glue("Sampling effort distribution among {nrow(uniq_cell_dat)} raster cells")) +
  theme_bw()
sampEff_dist_p

cowplot::ggsave2(occur_x_sampEff_p, filename = "plots/occurrence_probability_by_smpling_effort_in_raster_cell.png", width = 5, height = 5)

