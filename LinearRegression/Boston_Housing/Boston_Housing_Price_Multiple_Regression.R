### Problem : Boston housing price prediction
### Source  : http://lib.stat.cmu.edu/datasets/boston
### Method  : Multiple Regression.

## defining directories
setwd('G:/BHAV/')
getwd()

## reading the full data 
data= read.csv('Boston.csv', header = T)

pairs(data)  ## Scatter plot with each and every other variable
             ## trying to figure out correlation from image

## Cleaning the data - making Charles River as FACTOR Variable
data$CHAS = as.factor(data$CHAS)
sapply(data,class)

summary(data)

## Splitting the data into Test and 
indexes = sample(1:nrow(data), size=0.2*nrow(data))
test_DF = data[indexes,]
train_DF = data[-indexes,]

## Building the model on TRAINING DATA
Model = lm(train_DF$MEDV~.,data = train_DF)

## Analysing the model
summary(Model) ##Overall -- Adjusted R2 is 0.7326 which is decently good.


## Second model build on removing the non significant values

# Model2 = lm(train_DF$MEDV~train_DF$ï..CRIM+train_DF$ZN+train_DF$CHAS
#             +train_DF$NOX+train_DF$RM+train_DF$DIS+train_DF$RAD
#             +train_DF$TAX+train_DF$PTRATIO+train_DF$B+train_DF$LSTAT,
#             data= train_DF)
# 
# summary(Model2)  ## Adj R2 is 0.7339


coefficients(Model) # model coefficients
confint(Model, level=0.95) # CIs for model parameters 
fitted(Model) # predicted values
residuals(Model) # residuals
anova(Model) # anova table 
vcov(Model) # covariance matrix for model parameters 
influence(Model) # regression diagnostics

# diagnostic plots 
plot(Model)


# Applying model to train data set
test_DF_Prediction = predict(Model,test_DF)
test_DF$Predicton = test_DF_Prediction

# Calculating the Root Mean sqr Error and mean Abs Percentage error
RMSE = sqrt(sum((test_DF$MEDV- test_DF$Predicton)^2)/length(test_DF$MEDV))
MAPE = sum(abs((test_DF$MEDV- test_DF$Predicton)/test_DF$MEDV))*(100/length(test_DF$MEDV))



write.csv(test_DF,"testdata_after_pred.csv")

