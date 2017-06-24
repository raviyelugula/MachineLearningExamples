##### CART - Classification

################# DATA COLLECTION ####################################
##### 1. Read train and test data sets
##### 2. Check of data types and NA's -- if needed change the data type or replace NA's
##### 3. Check Target rate -- if skewed, apply over/under sampling tech if needed.
################ CART MODEL ON train data #############################
##### 4. Apply CART -- "rpart" is used here.
##### 5. Colourful graphical design of CART model -- "rattle" is used.
##### 6. Print CP plot and check where xerror gets increase make that as prune point -- "rpart"
##### 7. Prune tree based on cp value (post pruning)
############### MODEL performance on train date ########################
##### 8. ConfusionMatrix for train data -- "caret" is used.
##### 9. Decile the data and form RANK Order table -- "data.table" is used
############## Apply MODEL on test data ################################
##### 10. Predict target on test data based on pruned model.
############## MODEL Performance on test data ##########################
##### 11. Calculate Confusion Matrix on test data results
##### 12. Calculate Decile and RANK Order table for test data results and compare.

## Let us first set the working directory path
setwd ("C:/Users/ravin/Documents/GitHub/MachineLearningExamples/CART/datafile/")
getwd()

## Data Import
raw_train_data <- read.table("DEV_SAMPLE.csv", sep = ",", header = T)
raw_test_data <- read.table("HOLDOUT_SAMPLE.csv", sep = ",", header = T)

names(raw_train_data)
head(raw_train_data,3)
c(traindata=nrow(raw_train_data),testdata=nrow(raw_test_data))
str(raw_train_data)

## NA_Count check
NA_Count = sapply(raw_train_data, function(y) sum(length(which(is.na(y)))))
NA_Count

## Class check
Col_Class=sapply(raw_train_data,class)
Col_Class

## Target Rate -- % of positives classes
TargetRate_Train=(sum(raw_train_data$Target)/nrow(raw_train_data))*100
print(noquote(paste0('Target Rate in Training data = ',TargetRate_Train)))

## CART Model
# install.packages("rpart")
library(rpart)
CART_model1 <- rpart(formula = Target ~ ., 
                     data = raw_train_data[,-1], ## removing ID column, cause that doesn't help in classification
                     method = "class", 
                     minsplit=100, ## to split a node, node should have atleast minsplit no of observations
                                  ## 3 times minbucket, in general
                     minbucket = 10, ## after spliting atleast minbucket no of obs should present in all leafs to make the split valid
                                     ## 2-3% of observations in the provided train/test data set, in general
                     cp = 0, ## zero makes the tree grow to its full length -- Post prune it based on CP Values and xerror increase point
                     xval = 5 ## Cross validation parameter -- xval numbers splits are done on data 
                              ## all splits, except 1 will find best gini/any splitting algo based node, 
                              ## this is done xval times and most selected column is considered to be splitting column 
)
CART_model1

## install.packages("rattle")
## install.packages('rpart.plot')
## install.packages("RcolorBrewer") library(RColorBrewer)
library(rattle)
library(rpart.plot)
fancyRpartPlot(CART_model1)

## to find how the tree performs
printcp(CART_model1)
plotcp(CART_model1)

## Pruning Code
CP_Prune_CART_model1 = prune(CART_model1, cp= 0.0013 ,"CP") ## post pruning 
printcp(CP_Prune_CART_model1)
fancyRpartPlot(CP_Prune_CART_model1, uniform=TRUE,  main="Pruned Classification Tree")

## Confusion Matrix for Train data
train_data_results = raw_train_data
train_data_results$CART_Predict_class = predict(CP_Prune_CART_model1, newdata = raw_train_data , type = 'class')
train_data_results$CART_Predict_prob = predict(CP_Prune_CART_model1, newdata = raw_train_data , type = 'prob')

CM_train_table=table(Actual = train_data_results$Target, prediction = train_data_results$CART_Predict_class)
addmargins(CM_train_table)
CM_train_table/nrow(train_data_results)*100

library(lattice)
library(caret)
CM_train=confusionMatrix(CM_train_table)

## deciling code
decile = function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}
rank = function(x){
  rank <- x[, list(
    cnt = length(Target), 
    cnt_resp = sum(Target), 
    cnt_non_resp = sum(Target == 0)) , 
    by=deciles][order(-deciles)]
  rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
  rank$cum_resp <- cumsum(rank$cnt_resp)
  rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
  rank$cum_perct_resp <- round(rank$cum_resp * 100 / sum(rank$cnt_resp),2);
  rank$cum_perct_non_resp <- round(rank$cum_non_resp * 100 / sum(rank$cnt_non_resp),2);
  rank$ks <- abs(rank$cum_perct_resp - rank$cum_perct_non_resp);
  View(rank)
  return(rank)
  
}
class(train_data_results$CART_Predict_prob)
train_data_results$deciles <- decile(train_data_results$CART_Predict_prob[,2])
View(train_data_results)

## Ranking code
##install.packages("data.table")
library(data.table)
train_temp = data.table(train_data_results)
rank(train_temp)

####### Predicting on hold out data
## "vector", "prob", "class", "matrix"
test_data_results = raw_test_data
test_data_results$CART_Predict_class = predict(CP_Prune_CART_model1, newdata = raw_test_data , type = 'class')
test_data_results$CART_Predict_prob = predict(CP_Prune_CART_model1, newdata = raw_test_data , type = 'prob')

head(test_data_results)
nrow(test_data_results)

## Confusion matrix on test data
CM_test_table=table(Actual = test_data_results$Target, prediction = test_data_results$CART_Predict_class)
addmargins(CM_test_table)
CM_test_table/nrow(test_data_results)*100

library(lattice)
library(caret)
CM_test=confusionMatrix(CM_test_table)

class(test_data_results$CART_Predict_prob)
test_data_results$deciles <- decile(test_data_results$CART_Predict_prob[,2])
View(test_data_results)

## Ranking code
##install.packages("data.table")
library(data.table)
test_temp = data.table(test_data_results)
rank(test_temp)

