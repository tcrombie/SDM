library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# pull in PC data from XXX.R
pc_df <- data.table::fread("data/PCA_stats_df.csv")
  
#=========================================================#
# compare models with VIF pruned env vars
#=========================================================#
glmStart <- stats::glm(ce ~ 1, data = pc_df, family = "binomial")
glm.formula <- stats::formula("ce ~ poly(pc1,2) + poly(pc2,2) + poly(pc3,2) + pc1:pc2 + pc1:pc3 + pc2:pc3")
#glm.formula <- stats::formula("ce ~ poly(elevation_m,2) + poly(rain_mm_ann,2) + lai_ann + elevation_m:rain_mm_ann + elevation_m:lai_ann + rain_mm_ann:lai_ann")
glmModAIC <- MASS::stepAIC(glmStart, glm.formula, data = pc_df, direction = "both", trace = T, k = 2, control = glm.control(maxit = 100))
anova(glmModAIC)
stats::AIC(glmModAIC)
# select best model and predict for every cell in raster grid


#=========================================================#
# try pcaLogisticR function from MethyIT package
#=========================================================#
#devtools::install_git("https://github.com/genomaths/MethylIT.git")


formula <- ce ~ tair_ann + soil_moist_ann + tsurf_ann + lai_ann + rain_mm_ann + elevation_m
#formula <- Species ~ Petal.Length + Sepal.Length + Petal.Width
pca.logistic <- MethylIT::pcaLogisticR(formula = formula,
                                       data = pc_df, n.pc = 3, scale = TRUE,
                                       center = TRUE, max.pc = 3)


#pca.logistic <- MethylIT::pcaLogisticR(formula = formula,
                             data = data, n.pc = 2, scale = TRUE,
                             center = TRUE, max.pc = 2)
set.seed(123)
newdata <- iris[sample.int(150, 40), 1:4]
newdata.prediction <- predict(pca.logistic, newdata, type = "all")
