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
occur_x_sampEff_p
cowplot::ggsave2(occur_x_sampEff_p, filename = "plots/occurrence_probability_by_smpling_effort_in_raster_cell.png", width = 5, height = 5)

# plot distribution of sampling effort in all raster cells
sampEff_dist_p <- ggplot(data = uniq_cell_dat) +
  aes(x = cell_sample_n, fill = as.character(ce_cell)) +
  geom_histogram(bins = 100) +
  #facet_wrap(~ce_cell, scales = "free_y") +
  labs(x = "Number of samples in raster cell", y = "Count", fill = "Occurrence", title = glue::glue("Sampling effort distribution among {nrow(uniq_cell_dat)} raster cells")) +
  theme_bw()
sampEff_dist_p
cowplot::ggsave2(sampEff_dist_p, filename = "plots/sampling_effort_distribution.png", width = 5, height = 5)

#======================================================#
# Show balance by raster cell vs individual collections
#======================================================#
# make a df
bal_dat <-tibble::tibble(type = c("unique_samples","unique_cells"),
               n_neg = c(table(sf_df_env_ex$ce)[[1]], table(uniq_cell_dat$ce_cell)[[1]]),
               n_pos = c(table(sf_df_env_ex$ce)[[2]], table(uniq_cell_dat$ce_cell)[[2]])) %>%
  dplyr::mutate(frac_neg = n_neg/(n_neg + n_pos),
                frac_pos = 1-frac_neg) %>%
  tidyr::pivot_longer(cols = starts_with("n_") ,names_to = "condition", values_to = "n") %>%
  dplyr::mutate(condition = ifelse(condition == "n_neg", "0", "1"))

# plot the percentage of occurrence for each type 
bal_p <- ggplot(bal_dat) +
  aes(fill=condition, y=n, x=factor(type, levels = c("unique_samples", "unique_cells"))) + 
  geom_bar(position="fill", stat="identity") +
  labs(x = "", y = "fraction of all samples in group", fill = "Occurrence", title = "Occurrence within unique raster cells\nimproves balance") +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.04748659, 0.08533654, 0.25, 0.50, 0.75, 1.00), labels = scales::number_format(accuracy = 0.01)) +
  #geom_hline(yintercept = 0.04748659, linetype = 2, size = 0.25) +
  #geom_hline(yintercept = 0.08533654) +
  annotate("text", label = glue::glue('n={bal_dat %>%
                                      dplyr::filter(type == "unique_samples" & condition == "0") %>%
                                      dplyr::pull(n)}'), x = 1, y = 0.5, size = 3) +
  annotate("text", label = glue::glue('n={bal_dat %>%
                                      dplyr::filter(type == "unique_samples" & condition == "1") %>%
                                      dplyr::pull(n)}'), x = 1, y = 0.025, size = 3) +
  annotate("text", label = glue::glue('n={bal_dat %>%
                                      dplyr::filter(type == "unique_cells" & condition == "0") %>%
                                      dplyr::pull(n)}'), x = 2, y = 0.5, size = 3) +
  annotate("text", label = glue::glue('n={bal_dat %>%
                                      dplyr::filter(type == "unique_cells" & condition == "1") %>%
                                      dplyr::pull(n)}'), x = 2, y = 0.05, size = 3) +
  theme(panel.grid.minor = element_blank())
  
cowplot::ggsave2(bal_p, filename = "plots/Sampling_balance_by_occurrence_type.png", width = 5, height = 5)
