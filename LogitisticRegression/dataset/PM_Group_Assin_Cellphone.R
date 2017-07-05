### Logistic Regression

##################### DATA Collection #############################
### 1. Data Collection
### 2. Check if NA's and data type changes are necessary
### 3. Check Target response rate for skewness detection
##################### DATA Split ##################################
### 4. Split the dataset to TRAIN, TEST
##################### Logistic Regression #########################
### 5. Apply Logistic regression model
#################### Significance on Train data ###################
### 6. Check Overall test significance (chisqr test) -- "lmtest"
### 7. Check McFadden R2 or Psudo R2 -- "pscl" -- 40+ excellent,15-30 good, 10-15 Ok model -- "pscl"
### 8. Check Individual Significance 
### 9. Check Odds/Probabilities and then decide the signifiance.
### 10. Check ROC plot -- "Deducer"
#################### Apply on Test data ############################
### 11. Predict target class on Train data, Cut it and Check confusion matrix -- "caret"
### 12. Predit target class on Test data based on this model. Calculate Confusion Matrix
################### Smoothing if Skewed data set ###################
### 13. Apply ROSE/Over/Under/Both Smoothing on train data -- "ROSE"
### 14. Repeat Model build, pR2, Overall Sig, Individaul Sig, Odds, Predict, Cutoff, Confusion Matrix, ROC on ROC_Smoothened_Traindata
### 15. Compare the Sensitivity, Specificity, Precision of all the models build and choose the best among. -- "tidyr"



## Setting my directories
setwd('C:/Users/ravin/Documents/GitHub/MachineLearningExamples/LogitisticRegression/dataset/')
getwd()

## reading XLSX data to dataframe
# install.packages('XLConnect')
# library(XLConnect)
# Wb = loadWorkbook("Cellphone-1.xlsx")
# Raw_data = readWorksheet(Wb, sheet = "Data", header = TRUE)
Raw_data = read.csv('Cellphone-1.csv',header=T)  ## incase if XLConnect is not working - convert the file to csv.
head(Raw_data)
names(Raw_data)

## Checking if there are any NA's in the given data set
NA_Count = sapply(Raw_data, function(y) sum(length(which(is.na(y)))))
NA_Count

## Checking all the data types in the data set
Col_Class = sapply(Raw_data,class)
Col_Class

## Target Response rate
table(as.factor(Raw_data$Churn))
table(as.factor(Raw_data$Churn))/length(Raw_data$Churn) ## skewed data - Unbalanced to 0's - 85%

## this give same random combinations when ever we re-run the same code
set.seed(10) 

## Data split to 70% training and 30% testing
indexes = sample(1:nrow(Raw_data), size=0.3*nrow(Raw_data))
test_DF = Raw_data[indexes,]
train_DF = Raw_data[-indexes,]

head(test_DF)
head(train_DF)
table(train_DF$Churn)
length(train_DF$Churn)


## Logistic regression 
logit_model = glm(Churn~., data=train_DF, family = binomial )
plot(logit_model)


## Overall test significance 
# install.packages('lmtest')
library(lmtest) ## maximun likelyhood test test

lrtest(logit_model) ## overall Chisqr test is also significant

## MCFadden R2 calculation
## install.packages('pscl')
library(pscl)

pR2(logit_model) ## psudo R2 / mcfadden r2

## Indivial Coefficients
summary(logit_model)
confint(logit_model)
exp(coef(logit_model))

## Odds & Probabilities
odds = exp(coefficients(logit_model))
prob = odds/(1+odds)
odds
prob


## trainset prediction & Cut Off calculation
Prediction = fitted(logit_model)
summary(Prediction)

cutdf = data.frame(
  Cutoff=numeric(), 
  Error=numeric(), 
  stringsAsFactors=FALSE) 
tempVector = seq(from=0.15,to=0.95,by=0.05)
for(i in tempVector){
  cutof = floor(Prediction+i)
  a=table(Actual =train_DF$Churn, Predicted = cutof)
  E=a[1,2] + a[2,1]
  cutdf = rbind(cutdf,data.frame(Cutoff=i,Error=E))
}

CutOFF=cutdf$Cutoff[cutdf$Error==min(cutdf$Error)]
CutOFF
plot(x=cutdf[,1],y=cutdf[,2])


CutoffTrain = floor(Prediction+CutOFF)
addmargins(table(Actual =train_DF$Churn, Predicted = CutoffTrain))
table(Actual =train_DF$Churn, Predicted = CutoffTrain)/length(train_DF$Churn)

## ROC curve
# install.packages('Deducer')
# install.packages('JavaGD')
# install.packages('JGR')
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_131') # for 64-bit version
library(rJava)
library(Deducer)

rocplot(logit_model)

## test data prediction & odd & ROC
Test_prediction <- predict( logit_model, newdata = test_DF ,type = "response" )
summary(Test_prediction)

Test_CutOFF = CutOFF
CutoffTest = floor(Test_prediction+Test_CutOFF)
addmargins(table(Actual =test_DF$Churn, Predicted = CutoffTest))
table(Actual =test_DF$Churn, Predicted = CutoffTest)/length(test_DF$Churn)

library(caret)
confusionMatrix_original = confusionMatrix(CutoffTest,test_DF$Churn)


############### Imbalanced smoothing - ROSE - AV ################################
# install.packages('ROSE')
# install.packages('DMwR')

library(ROSE)
traindata_rose <- ROSE(Churn ~ ., data = train_DF, seed = 1)$data
table(traindata_rose$Churn)
length(traindata_rose$Churn)

logit_model_ROSE = glm(Churn~., data=traindata_rose, family = binomial )
lrtest(logit_model_ROSE)
pR2(logit_model_ROSE) 
summary(logit_model_ROSE)
confint(logit_model_ROSE)
exp(coef(logit_model_ROSE))

odds_ROSE = exp(coefficients(logit_model_ROSE))
prob_ROSE = odds_ROSE/(1+odds_ROSE)
odds_ROSE
prob_ROSE

Prediction_ROSE = fitted(logit_model_ROSE)
summary(Prediction_ROSE)

cutdf_ROSE = data.frame(
  Cutoff=numeric(), 
  Error=numeric(), 
  stringsAsFactors=FALSE) 

tempVector = seq(from=0.15,to=0.95,by=0.05)
for(i in tempVector){
  cutof = floor(Prediction_ROSE+i)
  a=table(Actual =traindata_rose$Churn, Predicted = cutof)
  E=a[1,2] + a[2,1]
  cutdf_ROSE = rbind(cutdf_ROSE,data.frame(Cutoff=i,Error=E))
}

CutOFF_ROSE=cutdf_ROSE$Cutoff[cutdf_ROSE$Error==min(cutdf_ROSE$Error)]
CutOFF_ROSE
plot(x=cutdf_ROSE[,1],y=cutdf_ROSE[,2])

CutoffTrain_ROSE = floor(Prediction_ROSE+CutOFF_ROSE)
addmargins(table(Actual =traindata_rose$Churn, Predicted = CutoffTrain_ROSE))
table(Actual =traindata_rose$Churn, Predicted = CutoffTrain_ROSE)/length(train_DF$Churn)

rocplot(logit_model_ROSE)

## test data prediction & odd & ROC
Test_prediction_ROSE <- predict( logit_model_ROSE, newdata = test_DF ,type = "response" )
summary(Test_prediction_ROSE)

Test_CutOFF_ROSE = CutOFF_ROSE
CutoffTest_ROSE = floor(Test_prediction_ROSE+Test_CutOFF_ROSE)
addmargins(table(Actual =test_DF$Churn, Predicted = CutoffTest_ROSE))
table(Actual =test_DF$Churn, Predicted = CutoffTest_ROSE)/length(test_DF$Churn)

library(caret)
confusionMatrix_ROSE = confusionMatrix(CutoffTest_ROSE,test_DF$Churn)


############### Imbalanced smoothing - Oversampling - AV ################################
library(ROSE)

length(train_DF$Churn)
train_DF_over <- ovun.sample(Churn ~ ., data = train_DF, 
                             method = "over",N = 3500,seed = 10)$data #3967 would make 50-50
table(train_DF_over$Churn)
length(train_DF_over$Churn)

logit_model_Oversampling = glm(Churn~., data=train_DF_over, family = binomial )
lrtest(logit_model_Oversampling)
pR2(logit_model_Oversampling) 
summary(logit_model_Oversampling)
confint(logit_model_Oversampling)
exp(coef(logit_model_Oversampling))

odds_Oversampling = exp(coefficients(logit_model_Oversampling))
prob_Oversampling = odds_Oversampling/(1+odds_Oversampling)
odds_Oversampling
prob_Oversampling

Prediction_Oversampling = fitted(logit_model_Oversampling)
summary(Prediction_Oversampling)

cutdf_Oversampling = data.frame(
  Cutoff=numeric(), 
  Error=numeric(), 
  stringsAsFactors=FALSE) 

tempVector = seq(from=0.15,to=0.95,by=0.05)
for(i in tempVector){
  cutof = floor(Prediction_Oversampling+i)
  a=table(Actual =train_DF_over$Churn, Predicted = cutof)
  E=a[1,2] + a[2,1]
  cutdf_Oversampling = rbind(cutdf_Oversampling,data.frame(Cutoff=i,Error=E))
}

CutOFF_Oversampling=cutdf_Oversampling$Cutoff[cutdf_Oversampling$Error==min(cutdf_Oversampling$Error)]
CutOFF_Oversampling
plot(x=cutdf_Oversampling[,1],y=cutdf_Oversampling[,2])

CutoffTrain_Oversampling = floor(Prediction_Oversampling+CutOFF_Oversampling)
addmargins(table(Actual =train_DF_over$Churn, Predicted = CutoffTrain_Oversampling))
table(Actual =train_DF_over$Churn, Predicted = CutoffTrain_Oversampling)/length(train_DF_over$Churn)

rocplot(logit_model_Oversampling)

## test data prediction & odd & ROC
Test_prediction_Oversampling <- predict( logit_model_Oversampling, newdata = test_DF ,type = "response" )
summary(Test_prediction_Oversampling)

Test_CutOFF_Oversampling = CutOFF_Oversampling
CutoffTest_Oversampling = floor(Test_prediction_Oversampling+Test_CutOFF_Oversampling)
addmargins(table(Actual =test_DF$Churn, Predicted = CutoffTest_Oversampling))
table(Actual =test_DF$Churn, Predicted = CutoffTest_Oversampling)/length(test_DF$Churn)

library(caret)
confusionMatrix_over = confusionMatrix(CutoffTest_Oversampling,test_DF$Churn)

############### Imbalanced smoothing - Undersampling - AV ################################
library(ROSE)

length(train_DF$Churn)
train_DF_under <- ovun.sample(Churn ~ ., data = train_DF, 
                              method = "under",N = 750,seed = 10)$data #694 would make 50-50
table(train_DF_under$Churn)
length(train_DF_under$Churn)

logit_model_undersampling = glm(Churn~., data=train_DF_under, family = binomial )
lrtest(logit_model_undersampling)
pR2(logit_model_undersampling) 
summary(logit_model_undersampling)
confint(logit_model_undersampling)
exp(coef(logit_model_undersampling))

odds_undersampling = exp(coefficients(logit_model_undersampling))
prob_undersampling = odds_undersampling/(1+odds_undersampling)
odds_undersampling
prob_undersampling

Prediction_undersampling = fitted(logit_model_undersampling)
summary(Prediction_undersampling)

cutdf_undersampling = data.frame(
  Cutoff=numeric(), 
  Error=numeric(), 
  stringsAsFactors=FALSE) 

tempVector = seq(from=0.15,to=0.95,by=0.05)
for(i in tempVector){
  cutof = floor(Prediction_undersampling+i)
  a=table(Actual =train_DF_under$Churn, Predicted = cutof)
  E=a[1,2] + a[2,1]
  cutdf_undersampling = rbind(cutdf_undersampling,data.frame(Cutoff=i,Error=E))
}

CutOFF_undersampling=cutdf_undersampling$Cutoff[cutdf_undersampling$Error==min(cutdf_undersampling$Error)]
CutOFF_undersampling
plot(x=cutdf_undersampling[,1],y=cutdf_undersampling[,2])

CutoffTrain_undersampling = floor(Prediction_undersampling+CutOFF_undersampling)
addmargins(table(Actual =train_DF_under$Churn, Predicted = CutoffTrain_undersampling))
table(Actual =train_DF_under$Churn, Predicted = CutoffTrain_undersampling)/length(train_DF_under$Churn)

rocplot(logit_model_undersampling)

## test data prediction & odd & ROC
Test_prediction_undersampling <- predict( logit_model_undersampling, newdata = test_DF ,type = "response" )
summary(Test_prediction_undersampling)

Test_CutOFF_undersampling = CutOFF_undersampling
CutoffTest_undersampling = floor(Test_prediction_undersampling+Test_CutOFF_undersampling)
addmargins(table(Actual =test_DF$Churn, Predicted = CutoffTest_undersampling))
table(Actual =test_DF$Churn, Predicted = CutoffTest_undersampling)/length(test_DF$Churn)

library(caret)
confusionMatrix_under = confusionMatrix(CutoffTest_undersampling,test_DF$Churn)


############### Imbalanced smoothing - bothsampling - AV ################################
library(ROSE)

length(train_DF$Churn)
train_DF_both <- ovun.sample(Churn ~ ., data = train_DF, 
                             method = "both", p=0.5,N=2334,seed = 10)$data #694 would make 50-50
table(train_DF_both$Churn)
length(train_DF_both$Churn)

logit_model_bothsampling = glm(Churn~., data=train_DF_both, family = binomial )
lrtest(logit_model_bothsampling)
pR2(logit_model_bothsampling) 
summary(logit_model_bothsampling)
confint(logit_model_bothsampling)
exp(coef(logit_model_bothsampling))

odds_bothsampling = exp(coefficients(logit_model_bothsampling))
prob_bothsampling = odds_bothsampling/(1+odds_bothsampling)
odds_bothsampling
prob_bothsampling

Prediction_bothsampling = fitted(logit_model_bothsampling)
summary(Prediction_bothsampling)

cutdf_bothsampling = data.frame(
  Cutoff=numeric(), 
  Error=numeric(), 
  stringsAsFactors=FALSE) 

tempVector = seq(from=0.15,to=0.95,by=0.05)
for(i in tempVector){
  cutof = floor(Prediction_bothsampling+i)
  a=table(Actual =train_DF_both$Churn, Predicted = cutof)
  E=a[1,2] + a[2,1]
  cutdf_bothsampling = rbind(cutdf_bothsampling,data.frame(Cutoff=i,Error=E))
}

CutOFF_bothsampling=cutdf_bothsampling$Cutoff[cutdf_bothsampling$Error==min(cutdf_bothsampling$Error)]
CutOFF_bothsampling
plot(x=cutdf_bothsampling[,1],y=cutdf_bothsampling[,2])

CutoffTrain_bothsampling = floor(Prediction_bothsampling+CutOFF_bothsampling)
addmargins(table(Actual =train_DF_both$Churn, Predicted = CutoffTrain_bothsampling))
table(Actual =train_DF_both$Churn, Predicted = CutoffTrain_bothsampling)/length(train_DF_both$Churn)

rocplot(logit_model_bothsampling)

## test data prediction & odd & ROC
Test_prediction_bothsampling <- predict( logit_model_bothsampling, newdata = test_DF ,type = "response" )
summary(Test_prediction_bothsampling)

Test_CutOFF_bothsampling = CutOFF_bothsampling
CutoffTest_bothsampling = floor(Test_prediction_bothsampling+Test_CutOFF_bothsampling)
addmargins(table(Actual =test_DF$Churn, Predicted = CutoffTest_bothsampling))
table(Actual =test_DF$Churn, Predicted = CutoffTest_bothsampling)/length(test_DF$Churn)

# library(caret)
confusionMatrix_both = confusionMatrix(CutoffTest_bothsampling,test_DF$Churn)


##################### Comparision in UI ############################

models = list(original = logit_model,
              under = logit_model_undersampling,
              over = logit_model_Oversampling,
              both = logit_model_bothsampling,
              ROSE = logit_model_ROSE
)

library(dplyr)
comparison = data.frame(model = names(models),
                        Sensitivity = rep(NA, length(models)),
                        Specificity = rep(NA, length(models)),
                        Precision = rep(NA, length(models)),
                        Recall = rep(NA, length(models)),
                        F1 = rep(NA, length(models)))

for (name in names(models)) {
  model <- get(paste0("confusionMatrix_", name))
  
  comparison[comparison$model == name, ] <- filter(comparison, model == name) %>%
    mutate(Sensitivity = model$byClass["Sensitivity"],
           Specificity = model$byClass["Specificity"],
           Precision = model$byClass["Precision"],
           Recall = model$byClass["Recall"],
           F1 = model$byClass["F1"])
}

## install.packages("tidyr")
library(tidyr)
comparison %>%
  gather(x, y, Sensitivity:F1) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 3)+ 
  xlab('Model Performance parameters')+
  ylab(' ')









