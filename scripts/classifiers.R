.libPaths("/opt/homebrew/lib/R/4.1/site-library")
library(tidyverse)
library(ggfortify)
library(logisticPCA)
library(logistf)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# load data
load("data/stats_df.Rdata")

#get just the environmental variables 
env_var <- stats_df[,c(5:12)]

#run PCA and plot PC1 and PC2
pca_env <- prcomp(env_var,scale=TRUE)
autoplot(pca_env)

#collect the PCs 

pc1 <- pca_env$x[,1]
pc2 <- pca_env$x[,2]
pc3 <- pca_env$x[,3]

stats_df$pc1 <-pc1
stats_df$pc2 <-pc2
stats_df$pc3 <- pc3

#save new dataframe as csv 

#write.csv(stats_df,"./data/PCA_stats_df.csv", row.names = FALSE)

# use classifier

#what is the percentage of positive finds? 

pos_find <- sum(stats_df$ce)/length(stats_df$ce)

#4.7% of the searches result in finding c elegans 

neg_find <- 1-pos_find

#95.3% of the searches result in not finding c elegans 

#this means that we would need the accruacy to be > 95.3 for this to mean anything. 

#fm=ce ~ tair_ann+soil_moist_ann+tsurf_ann+lai_ann+rain_mm_ann+elevation_m+lon+lat
fm <- ce ~ pc1 + pc2 + pc3
fit1 <- logistf(data=stats_df,fm,firth=TRUE)
#fit1 <- glm(fm,data=stats_df,family = 'binomial')

summary(fit1)
output<-fit1$predict #the problem is... max(output ~ 0.2)
stats_df$output <- output
stats_df$pred_fm <- output > 0.1
stats_df$pred_fm <- as.integer(as.logical(stats_df$pred_fm))
stats_df$comp <- stats_df$ce + stats_df$pred_fm
mis_class<-length(which(stats_df$comp == 1))/nrow(stats_df)
accuracy <- 1-mis_class

#write.csv(stats_df,"./data/lr_pca_stats_df.csv", row.names = FALSE)

