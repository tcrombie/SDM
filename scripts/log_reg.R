#basic logistic regression on Erik's worm data set 

#based on https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/

#set path for libraries 
.libPaths("/opt/homebrew/lib/R/4.1/site-library")

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

#load libraries 
library(tidyverse)
library(caTools)
#library(psc1) #cannot load this package but was supposed to get McFadden R^2 

#load in data 
load("data/stats_df.Rdata")

#split data into test and train 
#resource: https://www.statology.org/train-test-split-r/
set.seed(1) #for reproducability 

sample <- sample.split(stats_df$ce,SplitRatio = 0.7)
train <- subset(stats_df, sample == TRUE)
test <- subset(stats_df, sample == FALSE)

#check dimensions
nrow(train)/nrow(stats_df)
nrow(test)/nrow(stats_df)

#logistic regression model 
model <- glm(ce ~ tair_ann+soil_moist_ann+tsurf_ann+lai_ann+rain_mm_ann+elevation_m+lon+lat,family=binomial(link='logit'),data=train)
summary(model)
anova(model,test="Chisq")
#pR2(model) McFadden R^2 

data_test <- subset(test,select=c(5,6,7,8,9,10,11,12)) #get only environmental predictors in test data set 

#results 
fitted.results <- predict(model,newdata = data_test,type = "response")
#newdata is just the variables you want to predict, so in our case: tair_ann+soil_moist_ann+tsurf_ann+lai_ann+rain_mm_ann+elevation_m+lon+lat
#checked it here: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/predict.lm 

true_label <- test$ce

#for loop here. 
color_vec = c()
for (i in 1:length(true_label)) {
  check_tmp = true_label[i]
  if (check_tmp== 1) {
  color_vec[i] = "#DA9306"  }
  else {
    color_vec[i] = rgb(20/260, 107/260, 240/260, 0.1) }
}

#combine output probabilities, true labels, and color vector for plotting 
pred_vs_true <- cbind(fitted.results,true_label)
pred_vs_true <- data.frame(pred_vs_true)
pred_vs_true$color <- color_vec

plot(1:length(pred_vs_true$fitted.results),pred_vs_true$fitted.results,xlab="Test Data", ylab="Output Probabilities",pch = 1, col = pred_vs_true$color)

#the issue here is that there is no "good place" to draw the line that divides positive searches (gold) and negative searches (blue)

#logistic model with the above regressors does a bad job at classification. 





