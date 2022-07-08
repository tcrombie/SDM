library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# load stats dataframe
load("data/stats_df.Rdata")

# pull in PC data
pc_df <- ###
  
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
