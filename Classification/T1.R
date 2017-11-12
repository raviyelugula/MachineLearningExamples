# seperating into two kinds outliers and normal data ----
T1_traindata = subset(traindata, UUL_flag == 1 | DR_flag == 1)
T1_testdata = subset(testdata, UUL_flag == 1 | DR_flag == 1)

## T1 data modeling
# Missing data checking ----
Missing_data_Check(T1_traindata)

# Splitting the training data (SET1: build and tune the model) ~ (SET1: test the model) ----
set.seed(123)
split = sample.split(T1_traindata$Dependents, SplitRatio = 0.75)
T1_traindata_Train = subset(T1_traindata, split == TRUE)
T1_traindata_Test = subset(T1_traindata, split == FALSE)

# Missing data handling ----
T1_traindata_Test = Missing_data_handling(T1_traindata_Test)
Missing_data_Check(T1_traindata_Test)
T1_traindata_Train = Missing_data_handling(T1_traindata_Train)
Missing_data_Check(T1_traindata_Train)
# Target variable ratio check ----
Target_Ratio_Check(T1_traindata_Test) #93:7 
Target_Ratio_Check(T1_traindata_Train) #92:8
# SMOTE for treating imbalance data set ----
Two_D_View(T1_traindata_Test)
T1_traindata_Test_SMOTEd = SMOTE_fitting(T1_traindata_Test,600,300) #72:28
Two_D_View(T1_traindata_Train)
T1_traindata_Train_SMOTEd = SMOTE_fitting(T1_traindata_Train,600,300) #72:28

# SMOTE has oversampled the major class area too - so trying boundary SMOTE ----
Two_D_View(T1_traindata_Test)
T1_traindata_Test_BS = Boderline_SMOTE_fitting(T1_traindata_Test,8) #73:27

Two_D_View(T1_traindata_Train)
T1_traindata_Train_BS = Boderline_SMOTE_fitting(T1_traindata_Train,15) #71:29
rm(list = c('T1_traindata_Test_SMOTEd','T1_traindata_Train_SMOTEd')) # Removing as SMOTE has overfitted the majored regions too

# Building a Scaled data set for classification models ----
T1_traindata_Test_BS_Scaled = Scaling(T1_traindata_Test_BS)
T1_traindata_Train_BS_Scaled = Scaling(T1_traindata_Train_BS)
# Logistic regression -- Specificity: Train - 72.88 K-fold Train - 69.27 Test - 80.58 ----
T1_LR = glm( formula = DLQs~., 
             family = binomial,
             data = T1_traindata_Train_BS_Scaled)
prob_pred = predict(T1_LR, type = 'response', newdata = T1_traindata_Train_BS_Scaled[-1])
y_pred = ifelse(prob_pred > 0.4, 1, 0)
CM = table(T1_traindata_Train_BS_Scaled[,1],y_pred)
LR_Speci_Train = CM[4]/(CM[4]+CM[2])
round(LR_Speci_Train*100,2)
LR_Acc = (CM[4]+CM[1])/(CM[4]+CM[1]+CM[2]+CM[3])
LR_Acc

require(lmtest)
lrtest(T1_LR) # overall test i significant
require(pscl)
pR2(T1_LR) # 18 - ok model by McFadden R2

set.seed(1234)
folds = createFolds(T1_traindata_Train_BS_Scaled$DLQs, k = 10)
cv = lapply(folds, function(x) {
  training_fold = T1_traindata_Train_BS_Scaled[-x, ]
  test_fold = T1_traindata_Train_BS_Scaled[x, ]
  T1_LR_KF = glm( formula = DLQs~., 
                  family = binomial,
                  data = training_fold)
  prob_pred = predict(T1_LR_KF, type = 'response', newdata = test_fold[-1])
  y_pred = ifelse(prob_pred > 0.4, 1, 0)
  CM = table(test_fold[,1],y_pred)
  temp = CM[4]/(CM[4]+CM[2])
  return(temp)
})
LR_Speci_KF = mean(as.numeric(cv))
round(LR_Speci_KF*100,2)

prob_pred = predict(T1_LR, type = 'response', newdata = T1_traindata_Test_BS_Scaled[-1])
y_pred = ifelse(prob_pred > 0.4, 1, 0)
CM = table(T1_traindata_Test_BS_Scaled[,1],y_pred)
LR_Speci_Test = CM[4]/(CM[4]+CM[2])
round(LR_Speci_Test*100,2)

# KNN Classification -- Specificity:Train - xxxxx K-fold Train - 71.25 Test 41.73  ---- 
caret_tune = train(form = DLQs~ ., data = T1_traindata_Train_BS_Scaled, method = 'knn')
caret_tune
caret_tune$bestTune # caret to tune for k value

y_pred = knn(train =T1_traindata_Train_BS_Scaled[,-1],
             test =T1_traindata_Test_BS_Scaled[,-1],
             cl = T1_traindata_Train_BS_Scaled[, 1],
             k = 5,
             prob = TRUE)
CM = table(T1_traindata_Test_BS_Scaled[,1],y_pred)
Knn_Speci_Test = CM[4]/(CM[4]+CM[2])
Knn_Speci_Test

set.seed(1234)
folds = createFolds(T1_traindata_Train_BS_Scaled$DLQs, k = 10)
cv = lapply(folds, function(x) {
  training_fold = T1_traindata_Train_BS_Scaled[-x, ]
  test_fold = T1_traindata_Train_BS_Scaled[x, ]
  y_pred = knn(train =training_fold[,-1],
               test =test_fold[,-1],
               cl = training_fold[, 1],
               k = 5,
               prob = TRUE)
  CM = table(test_fold[,1],y_pred)
  temp = CM[4]/(CM[4]+CM[2])
  return(temp)
})
Knn_Speci_KF = mean(as.numeric(cv)) #overfitted

# SVM Classification -- Specificity:Train - 63.4  K-fold Train - 59.097 Test 46.76  ---- 
set.seed(1234)
caret_tune = train(form = DLQs~ ., data = T1_traindata_Train_BS_Scaled, method = 'svmLinearWeights')
caret_tune
caret_tune$bestTune # caret to tune for cost and weight value - cost is 1 which is default
set.seed(1234)
tune_svm_kernal = tune(svm, DLQs~ ., data = T1_traindata_Train_BS_Scaled,
                       kernal = 'radial',
                       ranges = list(cost = c(0.1,0.4,0.8,1,3,5,10,50,100), # penalising factor for missclassification, high c => low bias, high viariance, default is 1
                                     gamma = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2,4))) # smoothening the boundary shape sharpness, low gama => pointy bounday, low bias, high variance, default 1/dimensions
summary(tune_svm_kernal) # tuned parameters says cost 50 and gamma 0.9
set.seed(1234)
tune_svm_kernal = tune(svm, DLQs~ ., data = T1_traindata_Train_BS_Scaled,
                       kernal = 'sigmoid',
                       ranges = list(cost = c(0.1,0.4,0.8,1,3,5,10,50,100), 
                                     gamma = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2,4))) 
summary(tune_svm_kernal) # tuned parameters says cost 50 and gamma 0.9
set.seed(1234)
tune_svm_kernal = tune(svm, DLQs~ ., data = T1_traindata_Train_BS_Scaled,
                       kernal = 'polynomial',
                       ranges = list(ccost = c(0.1,0.4,0.8,1,3,5,10,50,100),
                                     gamma = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2,4),
                                     degree = c(2,3,4,5,6)))
summary(tune_svm_kernal) # tuned parameters says cost 0.1  and gamma 4 and degree 2

for(svmType in c('C-classification','nu-classification')){
  for(svmKernal in c('linear','polynomial','radial','sigmoid')){
    set.seed(1234)
    folds = createFolds(T1_traindata_Train_BS_Scaled$DLQs, k = 10)
    cv = lapply(folds, function(x) {
      training_fold = T1_traindata_Train_BS_Scaled[-x, ]
      test_fold = T1_traindata_Train_BS_Scaled[x, ]
      if(svmKernal == 'radial'){
        T1_SVM = svm(formula = DLQs ~ .,
                     data = training_fold,
                     type = 'C-classification',
                     kernel = svmKernal, cost = 50,gamma = 0.9)
        y_pred = predict(T1_SVM, newdata = test_fold[-1])
      }else if(svmKernal=='sigmoid'){
        T1_SVM = svm(formula = DLQs ~ .,
                     data = training_fold,
                     type = 'C-classification',
                     kernel = svmKernal, cost = 50 ,gamma = 0.9)
        y_pred = predict(T1_SVM, newdata = test_fold[-1])
      }else if(svmKernal=='polynomial'){
        T1_SVM = svm(formula = DLQs ~ .,
                     data = training_fold,
                     type = 'C-classification',
                     kernel = svmKernal, cost =0.1 ,gamma = 4 ,degre = 2)
        y_pred = predict(T1_SVM, newdata = test_fold[-1])
      }else{
        T1_SVM = svm(formula = DLQs ~ .,
                     data = training_fold,
                     type = 'C-classification',
                     kernel = svmKernal, cost =1)
        y_pred = predict(T1_SVM, newdata = test_fold[-1])
      }
      CM = table(test_fold[,1],y_pred)
      temp = CM[4]/(CM[4]+CM[2])
      return(temp)
    })
    specificity_SVM = round(mean(as.numeric(cv)),5)*100
    print.noquote(paste0(svmKernal,'-kernal ',svmType,' has K-fold specificity of ',specificity_SVM))
  }
} # choose radial kernal with C-Classification as it has highest 59.097

T1_SVM = svm(formula = DLQs ~ .,
             data = T1_traindata_Train_BS_Scaled,
             type = 'C-classification',
             kernel = 'radial', cost= 50, gamma= 9)
y_pred = predict(T1_SVM, newdata = T1_traindata_Train_BS_Scaled[-1])
CM = table(T1_traindata_Train_BS_Scaled[,1],y_pred)
SVM_Speci_Train = CM[4]/(CM[4]+CM[2])

y_pred = predict(T1_SVM, newdata = T1_traindata_Test_BS_Scaled[-1])
CM = table(T1_traindata_Test_BS_Scaled[,1],y_pred)
SVM_Speci_Test = CM[4]/(CM[4]+CM[2]) # dropped in Test, overfitted, but will consider for Test set, as k-fold is close to train_test

# Naive Bayes -- Specificity:Train - 91.83 K-fold Train - 81.785 Test 5.7   ----
T1_NB = naiveBayes(x = T1_traindata_Train_BS_Scaled[-1],
                   y = T1_traindata_Train_BS_Scaled$DLQs)

y_pred = predict(T1_NB, newdata = T1_traindata_Train_BS_Scaled[-1])
CM = table(T1_traindata_Train_BS_Scaled[,1],y_pred)
NB_Speci_Train = CM[4]/(CM[4]+CM[2])

set.seed(1234)
folds = createFolds(T1_traindata_Train_BS_Scaled$DLQs, k = 10)
cv = lapply(folds, function(x) {
  training_fold = T1_traindata_Train_BS_Scaled[-x, ]
  test_fold = T1_traindata_Train_BS_Scaled[x, ]
  T1_NB = naiveBayes(x = training_fold[-1],
                     y = training_fold$DLQs)
  y_pred = predict(T1_NB, newdata = test_fold[-1])
  CM = table(test_fold[,1],y_pred)
  temp = CM[4]/(CM[4]+CM[2])
  return(temp)
})
NB_Speci_KF = round(mean(as.numeric(cv)),5)*100 

y_pred = predict(T1_NB, newdata = T1_traindata_Test_BS_Scaled[-1])
CM = table(T1_traindata_Test_BS_Scaled[,1],y_pred)
NB_Speci_Test = CM[4]/(CM[4]+CM[2]) # overfitted

# CART -- Specificity:Train - 81.67 K-fold Train - 77.441  Test 77.7    --------
caret_tune = train(form = DLQs~ ., data = T1_traindata_Train_BS_Scaled, method = 'rpart')
caret_tune
caret_tune$bestTune # CP - Tunning 0.06535947712

T1_CART_temp = rpart(formula = DLQs ~ ., 
                     data = T1_traindata_Train_BS_Scaled, 
                     method = "class", 
                     minsplit= 25, 
                     cp = 0, 
                     xval = 7)
printcp(T1_CART_temp)
plotcp(T1_CART_temp)
T1_CART = prune(T1_CART_temp, cp= 0.06535947712 ,"CP")
y_pred = predict(T1_CART, newdata = T1_traindata_Train_BS_Scaled[-1], type='class')
CM = table(T1_traindata_Train_BS_Scaled[,1],y_pred)
CART_Speci_Train = CM[4]/(CM[4]+CM[2])

set.seed(1234)
folds = createFolds(T1_traindata_Train_BS_Scaled$DLQs, k = 10)
cv = lapply(folds, function(x) {
  training_fold = T1_traindata_Train_BS_Scaled[-x, ]
  test_fold = T1_traindata_Train_BS_Scaled[x, ]
  T1_CART_temp = rpart(formula = DLQs ~ ., 
                       data = training_fold, 
                       method = "class", 
                       minsplit= 25, 
                       cp = 0.06535947712, 
                       xval = 7)
  y_pred = predict(T1_CART_temp, newdata = test_fold[-1], type='class')
  CM = table(test_fold[,1],y_pred)
  temp = CM[4]/(CM[4]+CM[2])
  return(temp)
})
CART_Speci_KF = round(mean(as.numeric(cv)),5)*100

y_pred = predict(T1_CART, newdata = T1_traindata_Test_BS_Scaled[-1],type='class')
CM = table(T1_traindata_Test_BS_Scaled[,1],y_pred)
CART_Speci_Test = CM[4]/(CM[4]+CM[2]) # moves to build the test solution

# Random Forest -- Specificity:Train - 88.8  K-fold Train - 83.67 Test 19.42 ------
set.seed(1234)
T1_RF = randomForest(DLQs ~ ., data = T1_traindata_Train_BS_Scaled, 
                     ntree=500, mtry = 2, nodesize = 20,
                     importance=TRUE)
plot(T1_RF) ## 150 tree from OOB
caret_tune = train(form = DLQs~ ., data = T1_traindata_Train_BS_Scaled, method = 'rf')
caret_tune
caret_tune$bestTune # mtry - Tunning 

set.seed(1234)
T1_RF = tuneRF(x = T1_traindata_Train_BS_Scaled[,-1], 
               y=T1_traindata_Train_BS_Scaled$DLQs,
               mtryStart = 2,
               ntreeTry=150, 
               stepFactor = 1, ## 1st try 2 variables, next 4 , next 5 , next 6 MtryStart*Stepfactor 
               improve = 0.001, ## delta OOB 
               trace=TRUE, 
               plot = TRUE,
               doBest = TRUE,
               nodesize = 20, 
               importance=TRUE
) # random Forest tuning also lead to mtry = 2

y_pred = predict(T1_RF, newdata = T1_traindata_Train_BS_Scaled[-1], type='class')
CM = table(T1_traindata_Train_BS_Scaled[,1],y_pred)
RF_Speci_Train = CM[4]/(CM[4]+CM[2])

set.seed(1234)
folds = createFolds(T1_traindata_Train_BS_Scaled$DLQs, k = 10)
cv = lapply(folds, function(x) {
  training_fold = T1_traindata_Train_BS_Scaled[-x, ]
  test_fold = T1_traindata_Train_BS_Scaled[x, ]
  T1_RF_temp = randomForest(DLQs ~ ., data = training_fold, 
                            ntree=150, mtry = 2, nodesize = 20)
  y_pred = predict(T1_RF_temp, newdata = test_fold[-1], type='class')
  CM = table(test_fold[,1],y_pred)
  temp = CM[4]/(CM[4]+CM[2])
  return(temp)
})
RF_Speci_KF = round(mean(as.numeric(cv)),5)*100

y_pred = predict(T1_RF, newdata = T1_traindata_Test_BS_Scaled[-1], type='class')
CM = table(T1_traindata_Test_BS_Scaled[,1],y_pred)
RF_Speci_Test = CM[4]/(CM[4]+CM[2]) # overfitted

# ANN -- Specificity:Train - 83.96 K-fold Train - xxxxxx Test 100 ----
training_set_scaled_ANN = T1_traindata_Train_BS_Scaled
training_set_scaled_ANN$DLQs = as.numeric(as.character(training_set_scaled_ANN$DLQs))
test_set_scaled_ANN = T1_traindata_Test_BS_Scaled
test_set_scaled_ANN$DLQs = as.numeric(as.character(test_set_scaled_ANN$DLQs))

n = names(training_set_scaled_ANN)
long_formula = as.formula(paste("DLQs ~", paste(n[!n %in% "DLQs"], collapse = " + ")))
set.seed(123)
T1_ANN = neuralnet(formula = long_formula,
                   data = training_set_scaled_ANN,
                   hidden = c(4),
                   err.fct = "sse",
                   linear.output = FALSE,
                   lifesign = "full",
                   lifesign.step = 1,
                   threshold = 0.05,
                   stepmax = 100000)
plot(T1_ANN)
y_pred = ifelse(T1_ANN$net.result[[1]] >= 0.5,1,0)
CM = table(training_set_scaled_ANN[,1],y_pred)
ANN_Speci_Train = CM[4]/(CM[4]+CM[2])
ANN_Speci_Train

y_pred = compute(T1_ANN,test_set_scaled_ANN[,-1])
y_pred = ifelse(y_pred$net.result >= 0.5,1,0)
CM = table(test_set_scaled_ANN[,1],y_pred)
ANN_Speci_Test = CM[4]/(CM[4]+CM[2])
ANN_Speci_Test # underfitted

# Test data prep LR: 49.07 KNN: 71.29 SVM:3.7 NB: 39.81 CART: 25 RF: 67.59 ANN: 36.11 ----
Missing_data_Check(T1_testdata)
data_C = subset(T1_testdata,!is.na(Dependents))
data_M = subset(T1_testdata,is.na(Dependents))
dependents = knn(train = scale(data_C[,c(2,3,4)]),
                 test = as.matrix(cbind(scale(data_M[2]), data_M[3], scale(data_M[4]))),
                 cl = as.factor(data_C$Dependents),
                 k = 9,
                 prob = F)
data_M$Dependents = dependents
T1_testdata = rbind(data_C,data_M)
rm(list = c('data_C','data_M'))
Missing_data_Check(T1_testdata)
T1_testdata$Dependents = as.numeric(T1_testdata$Dependents)
Two_D_View(T1_testdata)
T1_testdata_BS = SMOTE_fitting(T1_testdata,500,300)
T1_testdata_BS = T1_testdata_BS[,c(1:5)]
T1_testdata_BS_Scaled = Scaling(T1_testdata_BS)

T1_traindata_Complete_BS_Scaled = rbind(T1_traindata_Train_BS_Scaled, 
                                        T1_traindata_Test_BS_Scaled)

T1_LR = glm( formula = DLQs~.,
             family = binomial,
             data = T1_traindata_Complete_BS_Scaled)
prob_pred = predict(T1_LR, type = 'response', newdata = T1_testdata_BS_Scaled[-1])
y_pred = ifelse(prob_pred > 0.4, 1, 0)
CM = table(T1_testdata_BS_Scaled$DLQs,y_pred)
LR_Speci_Hold = CM[4]/(CM[4]+CM[2])
round(LR_Speci_Hold*100,2) # 49.07

y_pred = knn(train =T1_traindata_Complete_BS_Scaled[,-1],
             test =T1_testdata_BS_Scaled[,-1],
             cl = T1_traindata_Complete_BS_Scaled[, 1],
             k = 5,
             prob = TRUE)
CM = table(T1_testdata_BS_Scaled[,1],y_pred)
Knn_Speci_Hold = CM[4]/(CM[4]+CM[2])
round(Knn_Speci_Hold*100,2) # 71.3

T1_SVM = svm(formula = DLQs ~ .,
             data = T1_traindata_Complete_BS_Scaled,
             type = 'C-classification',
             kernel = 'radial', cost= 50, gamma= 9)
y_pred = predict(T1_SVM, newdata = T1_testdata_BS_Scaled[-1])
CM = table(T1_testdata_BS_Scaled[,1],y_pred)
SVM_Speci_Hold = CM[4]/(CM[4]+CM[2]) #3.7

T1_NB = naiveBayes(x = T1_traindata_Complete_BS_Scaled[-1],
                   y = T1_traindata_Complete_BS_Scaled$DLQs)
y_pred = predict(T1_NB, newdata = T1_testdata_BS_Scaled[-1])
CM = table(T1_testdata_BS_Scaled[,1],y_pred)
NB_Speci_Hold = CM[4]/(CM[4]+CM[2]) #39.81

T1_CART = rpart(formula = DLQs ~ .,
                     data = T1_traindata_Complete_BS_Scaled,
                     method = "class",
                     minsplit= 25,
                     cp = 0.06535947712,
                     xval = 7)
y_pred = predict(T1_CART, newdata = T1_testdata_BS_Scaled[-1],type='class')
CM = table(T1_testdata_BS_Scaled[,1],y_pred)
CART_Speci_Hold = CM[4]/(CM[4]+CM[2]) #25

T1_RF = randomForest(DLQs ~ ., data = T1_traindata_Complete_BS_Scaled,
                     ntree=150, mtry = 2, nodesize = 20,
                     importance=TRUE)
y_pred = predict(T1_RF, newdata = T1_testdata_BS_Scaled[-1], type='class')
CM = table(T1_testdata_BS_Scaled[,1],y_pred)
RF_Speci_Hold = CM[4]/(CM[4]+CM[2]) # 67.59

training_set_scaled_ANN = T1_traindata_Complete_BS_Scaled
training_set_scaled_ANN$DLQs = as.numeric(as.character(training_set_scaled_ANN$DLQs))
test_set_scaled_ANN = T1_testdata_BS_Scaled
test_set_scaled_ANN$DLQs = as.numeric(as.character(test_set_scaled_ANN$DLQs))

n = names(training_set_scaled_ANN)
long_formula = as.formula(paste("DLQs ~", paste(n[!n %in% "DLQs"], collapse = " + ")))
set.seed(123)
T1_ANN = neuralnet(formula = long_formula,
                   data = training_set_scaled_ANN,
                   hidden = c(4),
                   err.fct = "sse",
                   linear.output = FALSE,
                   lifesign = "full",
                   lifesign.step = 1,
                   threshold = 0.05,
                   stepmax = 100000)
y_pred = compute(T1_ANN,test_set_scaled_ANN[,-1])
y_pred = ifelse(y_pred$net.result >= 0.5,1,0)
CM = table(test_set_scaled_ANN[,1],y_pred)
ANN_Speci_Hold = CM[4]/(CM[4]+CM[2])
ANN_Speci_Hold # 36.11








