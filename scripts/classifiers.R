library(tidyverse)
library(ggfortify)
library(logisticPCA)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# load data
load("data/stats_df.Rdata")

#get just the environmental variables 
env_var <- stats_df[,c(5:12)]

#get the components 
pca_env <- prcomp(env_var,scale=TRUE)
autoplot(pca_env)

# use classifier

#what is the percentage of positive finds? 

pos_find <- sum(stats_df$ce)/length(stats_df$ce)

#4.7% of the searches result in finding c elegans 

neg_find <- 1-pos_find

#95.3% of the searches result in not finding c elegans 

#this means that we would need the accruacy to be > 95.3 for this to mean anything. 

library(logistf)


fm=ce ~ tair_ann+soil_moist_ann+tsurf_ann+lai_ann+rain_mm_ann+elevation_m+lon+lat
#fit1 <- logistf(data=stats_df,fm,firth=TRUE,pl=FALSE)
fit1 <- logisticPCA(data=stats_df,fm,firth=TRUE,pl=FALSE)
summary(fit1)
output<-fit1$predict
stats_df$pred_fm <- output > 0.5
stats_df$pred_fm <- as.integer(as.logical(stats_df$pred_fm))
stats_df$comp <- stats_df$ce + stats_df$pred_fm
mis_class<-length(which(stats_df$comp == 1))/nrow(stats_df)
accuracy <- 1-mis_class
