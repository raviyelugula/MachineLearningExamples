require(readxl) ## read Excel Files
require(dplyr) ## data manupulation
require(usdm) ## VIF 
require(ggplot2) ## Visualization
require(caTools) ## split
require(class) ## KNN
require(DMwR) ## SMOTE
require(smotefamily) ## BoderLine SMOTE
require(caret) ## K-Fold, tuning
require(e1071) ## SVM
require(rpart) ## CART - Decision Tree
require(randomForest) ## RandomForest
require(neuralnet) ## ANN

# reading the data
excel_sheets(path = 'training.xlsx')
traindata = read_excel(path = 'training.xlsx', sheet = 'training')

# rename cols, new features, data type adjustments
colnames(traindata) = c('RowID','DLQs','Utlz_UnsecLines','DebtRatio',
                        'Credit_Loans','Dependents')
traindata = traindata %>% 
            dplyr::select(DLQs,Utlz_UnsecLines,DebtRatio,Credit_Loans,Dependents) 
traindata$UUL_flag = ifelse(traindata$Utlz_UnsecLines>1,1,0)
traindata$DR_flag = ifelse(traindata$DebtRatio>1,1,0)
sapply(traindata,class)
traindata$Dependents = ifelse(traindata$Dependents =='NA',NA,traindata$Dependents)
traindata$Dependents = as.numeric(traindata$Dependents)
traindata[,c(1,6,7)] = data.frame(lapply(traindata[,c(1,6,7)],as.factor))

# spliting into two kinds outliers and normal data
T1_traindata = subset(traindata, UUL_flag == 1 | DR_flag == 1)
T2_traindata = subset(traindata, UUL_flag == 0 & DR_flag == 0)

# Missing data check
Missing_data_Check <- function(data_set){
  NA_Count = sapply(data_set,function(y) sum(length(which(is.na(y))))) 
  Null_Count = sapply(data_set,function(y) sum(length(which(is.null(y)))))
  Length0_Count = sapply(data_set,function(y) sum(length(which(length(y)==0))))
  Empty_Count = sapply(data_set,function(y) sum(length(which(y==''))))
  Total_NonData = NA_Count+Null_Count+Length0_Count+Empty_Count
  return( Total_NonData )
}
Missing_data_Check(T2_traindata)
Missing_data_Check(T1_traindata)

# Handling missing data 
vif(data.frame(T2_traindata[,c(2:4)]))
T2_traindata_C = subset(T2_traindata,!is.na(Dependents))
T2_traindata_M = subset(T2_traindata,is.na(Dependents))

set.seed(123)
split = sample.split(T2_traindata_C$Dependents, SplitRatio = 0.75)
T2_traindata_C_Tr = subset(T2_traindata_C, split == TRUE)
T2_traindata_C_Te = subset(T2_traindata_C, split == FALSE)
dependents = knn(train = scale(T2_traindata_C_Tr[,c(2,3,4)]),
             test = scale(T2_traindata_C_Te[,c(2,3,4)]),
             cl = as.factor(T2_traindata_C_Tr$Dependents),
             k = 9,
             prob = F)
print(length(which(T2_traindata_C_Te$Dependents == dependents))/length(dependents))
model = lm(Dependents~Utlz_UnsecLines+DebtRatio+Credit_Loans,
           data=T2_traindata_C_Tr)
summary(model)
dependents = round(predict(model,T2_traindata_C_Te)) ## LR is not working as it gives all as 1
rm(list = c('T2_traindata_C_Te','T2_traindata_C_Tr'))

dependents = knn(train = scale(T2_traindata_C[,c(2,3,4)]),
                 test = scale(T2_traindata_M[,c(2,3,4)]),
                 cl = as.factor(T2_traindata_C$Dependents),
                 k = 9,
                 prob = F)
T2_traindata_M$Dependents = dependents
T2_traindata = rbind(T2_traindata_C,T2_traindata_M)
rm(list = c('T2_traindata_C','T2_traindata_M'))
Missing_data_Check(T2_traindata)

vif(data.frame(T1_traindata[,c(2:4)]))
T1_traindata_C = subset(T1_traindata,!is.na(Dependents))
T1_traindata_M = subset(T1_traindata,is.na(Dependents))

set.seed(123)
split = sample.split(T1_traindata_C$Dependents, SplitRatio = 0.75)
T1_traindata_C_Tr = subset(T1_traindata_C, split == TRUE)
T1_traindata_C_Te = subset(T1_traindata_C, split == FALSE)
dependents = knn(train = scale(T1_traindata_C_Tr[,c(2,3,4)]),
                 test = scale(T1_traindata_C_Te[,c(2,3,4)]),
                 cl = as.factor(T1_traindata_C_Tr$Dependents),
                 k = 9,
                 prob = F)
print(length(which(T1_traindata_C_Te$Dependents == dependents))/length(dependents))
model = lm(Dependents~Utlz_UnsecLines+DebtRatio+Credit_Loans,
           data=T1_traindata_C_Tr)
summary(model)
dependents = round(predict(model,T1_traindata_C_Te))
print(length(which(T1_traindata_C_Te$Dependents == dependents))/length(dependents))
rm(list = c('T1_traindata_C_Te','T1_traindata_C_Tr'))

dependents = knn(train = scale(T1_traindata_C[,c(2,3,4)]),
                 test = scale(T1_traindata_M[,c(2,3,4)]),
                 cl = as.factor(T1_traindata_C$Dependents),
                 k = 9,
                 prob = F)
T1_traindata_M$Dependents = dependents
T1_traindata = rbind(T1_traindata_C,T1_traindata_M)
rm(list = c('T1_traindata_C','T1_traindata_M'))
Missing_data_Check(T1_traindata)

## Model building for T2 - 94:6 target varibale ratio
T2_traindata = T2_traindata[,1:5]
T2_traindata$Dependents = as.numeric(T2_traindata$Dependents)

ggplot(data = T2_traindata)+
  geom_point(aes(x = Utlz_UnsecLines, y = DebtRatio,
                 #shape = as.factor(Dependents), size = Credit_Loans, 
                 color = DLQs))
# SMOTE for treating imbalance data set 94:6 ratio T- variable
T2_traindata_SMOTE = SMOTE(DLQs~Utlz_UnsecLines+DebtRatio+
                             Credit_Loans+Dependents,as.data.frame(T2_traindata),
                           perc.over = 600,perc.under = 300)
table(T2_traindata$DLQs)
table(T2_traindata_SMOTE$DLQs)
table(T2_traindata_SMOTE$DLQs)/length(T2_traindata_SMOTE$DLQs)

ggplot(data = T2_traindata_SMOTE)+
  geom_point(aes(x = Utlz_UnsecLines, y = DebtRatio,
                 #shape = as.factor(Dependents), size = Credit_Loans, 
                 color = DLQs))

# SMOTE has oversampled the major class area too - so trying boundary SMOTE
T2_traindata_SMOTE_B = BLSMOTE(as.data.frame(T2_traindata[2:5]),as.numeric(T2_traindata$DLQs),
                               K=4,C=3,dupSize=25,method =c("type1"))
table(T2_traindata_SMOTE_B$data$class)/length(T2_traindata_SMOTE_B$data$class)
table(T2_traindata$DLQs)/length(T2_traindata$DLQs)
T2_traindata_SMOTE_BS = T2_traindata_SMOTE_B$data
T2_traindata_SMOTE_BS$DLQs = ifelse(T2_traindata_SMOTE_BS$class == 1, 0, 1)
T2_traindata_SMOTE_BS = T2_traindata_SMOTE_BS[,c(6,1,2,3,4)]
T2_traindata_SMOTE_BS$DLQs = as.factor(T2_traindata_SMOTE_BS$DLQs)

ggplot(data = T2_traindata_SMOTE_BS)+
  geom_point(aes(x = Utlz_UnsecLines, y = DebtRatio,
                 #shape = as.factor(Dependents), size = Credit_Loans, 
                 color = DLQs))

split = sample.split(T2_traindata_SMOTE_BS$DLQs, SplitRatio = 0.75)
training_set = subset(T2_traindata_SMOTE_BS, split == TRUE)
test_set = subset(T2_traindata_SMOTE_BS, split == FALSE)
T2_traindata_SMOTE_BS_scaled = T2_traindata_SMOTE_BS
T2_traindata_SMOTE_BS_scaled[-1] = scale(T2_traindata_SMOTE_BS_scaled[-1])
training_set_scaled = training_set
training_set_scaled[-1] = scale(training_set_scaled[-1])
test_set_scaled = test_set
test_set_scaled[-1] = scale(test_set_scaled[-1])

# Logistic regression -- Specificity: Train - 62.591 K-fold Train - 63.004 Test - 60.869 ----
T2_LR = glm( formula = DLQs~., 
             family = binomial,
             data = training_set)
prob_pred = predict(T2_LR, type = 'response', newdata = training_set[-1])
y_pred = ifelse(prob_pred > 0.55, 1, 0)
CM = table(training_set[,1],y_pred)
LR_Speci_Train = CM[4]/(CM[4]+CM[2])
LR_Speci_Train

require(lmtest)
lrtest(T2_LR) # overall test i significant
require(pscl)
pR2(T2_LR) # 35 - very good McFadden R2

set.seed(1234)
folds = createFolds(training_set$DLQs, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  T2_LR_KF = glm( formula = DLQs~., 
                    family = binomial,
                    data = training_fold)
  prob_pred = predict(T2_LR_KF, type = 'response', newdata = test_fold[-1])
  y_pred = ifelse(prob_pred > 0.55, 1, 0)
  CM = table(test_fold[,1],y_pred)
  temp = CM[4]/(CM[4]+CM[2])
  return(temp)
})
LR_Speci_KF = mean(as.numeric(cv))

prob_pred = predict(T2_LR, type = 'response', newdata = test_set[-1])
y_pred = ifelse(prob_pred > 0.55, 1, 0)
CM = table(test_set[,1],y_pred)
LR_Speci_Test = CM[4]/(CM[4]+CM[2])
LR_Speci_Test

# KNN Classification -- Specificity:Train - xxxxx K-fold Train - 87.363 Test 85.714  ---- 
caret_tune = train(form = DLQs~ ., data = training_set_scaled, method = 'knn')
caret_tune
caret_tune$bestTune # caret to tune for k value

y_pred = knn(train =training_set_scaled[,-1],
             test =test_set_scaled[,-1],
             cl = training_set_scaled[, 1],
             k = 5,
             prob = TRUE)
CM = table(test_set_scaled[,1],y_pred)
Knn_Speci_Test = CM[4]/(CM[4]+CM[2])
Knn_Speci_Test

set.seed(1234)
folds = createFolds(training_set_scaled$DLQs, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set_scaled[-x, ]
  test_fold = training_set_scaled[x, ]
  y_pred = knn(train =training_fold[,-1],
               test =test_fold[,-1],
               cl = training_fold[, 1],
               k = 5,
               prob = TRUE)
  CM = table(test_fold[,1],y_pred)
  temp = CM[4]/(CM[4]+CM[2])
  return(temp)
})
Knn_Speci_KF = mean(as.numeric(cv))

# SVM Classification -- Specificity:Train - 90.881 K-fold Train - 86.116 Test 87.267  ---- 
caret_tune = train(form = DLQs~ ., data = training_set_scaled, method = 'svmLinearWeights')
caret_tune
caret_tune$bestTune # caret to tune for cost and weight value - cost is 1 which is default
tune_svm_kernal = tune(svm, DLQs~ ., data = training_set_scaled,
                       kernal = 'radial',
                       ranges = list(cost = c(0.1,0.4,0.8,1,3,5,10,50,100), # penalising factor for missclassification, high c => low bias, high viariance, default is 1
                                     gamma = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2,4))) # smoothening the boundary shape sharpness, low gama => pointy bounday, low bias, high variance, default 1/dimensions
summary(tune_svm_kernal) # tuned parameters says cost 3 and gamma 4
tune_svm_kernal = tune(svm, DLQs~ ., data = training_set_scaled,
                       kernal = 'sigmoid',
                       ranges = list(cost = c(0.1,0.4,0.8,1,3,5,10,50,100), 
                                     gamma = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2,4))) 
summary(tune_svm_kernal) # tuned parameters says cost 1 and gamma 4
tune_svm_kernal = tune(svm, DLQs~ ., data = training_set_scaled,
                       kernal = 'polynomial',
                       ranges = list(ccost = c(0.1,0.4,0.8,1,3,5,10,50,100),
                                     gamma = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2,4),
                                     degree = c(2,3,4,5,6)))
summary(tune_svm_kernal) # tuned parameters says cost 0.1 and gamma 4 and degree 3

for(svmType in c('C-classification','nu-classification')){
  for(svmKernal in c('linear','polynomial','radial','sigmoid')){
    set.seed(1234)
    folds = createFolds(training_set_scaled$DLQs, k = 10)
    cv = lapply(folds, function(x) {
      training_fold = training_set_scaled[-x, ]
      test_fold = training_set_scaled[x, ]
      if(svmKernal == 'radial'){
        T2_SVM = svm(formula = DLQs ~ .,
                     data = training_fold,
                     type = 'C-classification',
                     kernel = svmKernal, cost = 3,gamma = 4)
        y_pred = predict(T2_SVM, newdata = test_fold[-1])
      }else if(svmKernal=='sigmoid'){
        T2_SVM = svm(formula = DLQs ~ .,
                     data = training_fold,
                     type = 'C-classification',
                     kernel = svmKernal, cost = 1,gamma = 4)
        y_pred = predict(T2_SVM, newdata = test_fold[-1])
      }else{
        T2_SVM = svm(formula = DLQs ~ .,
                     data = training_fold,
                     type = 'C-classification',
                     kernel = svmKernal)
        y_pred = predict(T2_SVM, newdata = test_fold[-1])
      }
      CM = table(test_fold[,1],y_pred)
      temp = CM[4]/(CM[4]+CM[2])
      return(temp)
    })
    specificity_SVM = round(mean(as.numeric(cv)),5)*100
    print.noquote(paste0(svmKernal,'-kernal ',svmType,' has K-fold specificity of ',specificity_SVM))
  }
} # choose radial kernal with C-Classification as it has highest 86.116

# [1] linear-kernal C-classification has K-fold specificity of 80.112
# [1] polynomial-kernal C-classification has K-fold specificity of 60.208
# [1] radial-kernal C-classification has K-fold specificity of 86.116
# [1] sigmoid-kernal C-classification has K-fold specificity of 40.947
# [1] linear-kernal nu-classification has K-fold specificity of 80.112
# [1] polynomial-kernal nu-classification has K-fold specificity of 60.208
# [1] radial-kernal nu-classification has K-fold specificity of 86.116
# [1] sigmoid-kernal nu-classification has K-fold specificity of 40.947
T2_SVM = svm(formula = DLQs ~ .,
             data = training_set_scaled,
             type = 'C-classification',
             kernel = 'radial', cost= 3, gamma= 4)
y_pred = predict(T2_SVM, newdata = training_set_scaled[-1])
CM = table(training_set_scaled[,1],y_pred)
SVM_Speci_Train = CM[4]/(CM[4]+CM[2])

y_pred = predict(T2_SVM, newdata = test_set_scaled[-1])
CM = table(test_set_scaled[,1],y_pred)
SVM_Speci_Test = CM[4]/(CM[4]+CM[2])

# Naive Bayes -- Specificity:Train - 81.347 K-fold Train - 81.557 Test 83.851   ----
T2_NB = naiveBayes(x = training_set[-1],
                   y = training_set_scaled$DLQs)

y_pred = predict(T2_NB, newdata = training_set_scaled[-1])
CM = table(training_set_scaled[,1],y_pred)
NB_Speci_Train = CM[4]/(CM[4]+CM[2])

set.seed(1234)
folds = createFolds(training_set_scaled$DLQs, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set_scaled[-x, ]
  test_fold = training_set_scaled[x, ]
  T2_NB = naiveBayes(x = training_fold[-1],
                     y = training_fold$DLQs)
  y_pred = predict(T2_SVM, newdata = test_fold[-1])
  CM = table(test_fold[,1],y_pred)
  temp = CM[4]/(CM[4]+CM[2])
  return(temp)
})
NB_Speci_KF = round(mean(as.numeric(cv)),5)*100

y_pred = predict(T2_NB, newdata = test_set_scaled[-1])
CM = table(test_set_scaled[,1],y_pred)
NB_Speci_Test = CM[4]/(CM[4]+CM[2])

# CART -- Specificity:Train - 71.813 K-fold Train - 75.866 Test 72.360    --------
caret_tune = train(form = DLQs~ ., data = training_set_scaled, method = 'rpart')
caret_tune
caret_tune$bestTune # CP - Tunning 

T2_CART_temp = rpart(formula = DLQs ~ ., 
                     data = training_set_scaled, 
                     method = "class", 
                     minsplit= 225, 
                     cp = 0, 
                     xval = 7)
printcp(T2_CART_temp)
plotcp(T2_CART_temp)
T2_CART = prune(T2_CART_temp, cp= 0.05284974 ,"CP")
y_pred = predict(T2_CART, newdata = training_set_scaled[-1], type='class')
CM = table(training_set_scaled[,1],y_pred)
CART_Speci_Train = CM[4]/(CM[4]+CM[2])

set.seed(1234)
folds = createFolds(training_set_scaled$DLQs, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set_scaled[-x, ]
  test_fold = training_set_scaled[x, ]
  T2_CART_temp = rpart(formula = DLQs ~ ., 
                       data = training_fold, 
                       method = "class", 
                       minsplit= 225, 
                       cp = 0.05284974, 
                       xval = 7)
  y_pred = predict(T2_CART_temp, newdata = test_fold[-1], type='class')
  CM = table(test_fold[,1],y_pred)
  temp = CM[4]/(CM[4]+CM[2])
  return(temp)
})
CART_Speci_KF = round(mean(as.numeric(cv)),5)*100

y_pred = predict(T2_CART, newdata = test_set_scaled[-1],type='class')
CM = table(test_set_scaled[,1],y_pred)
CART_Speci_Test = CM[4]/(CM[4]+CM[2])

# Random Forest -- Specificity:Train - 86.736 K-fold Train - 82.279 Test 83.851 ------
set.seed(1234)
T2_RF = randomForest(DLQs ~ ., data = training_set_scaled, 
                   ntree=500, mtry = 2, nodesize = 40,
                   importance=TRUE)
plot(T2_RF) ## 250 tree from OOB
caret_tune = train(form = DLQs~ ., data = training_set_scaled, method = 'rf')
caret_tune
caret_tune$bestTune # mtry - Tunning 

set.seed(1234)
T2_RF = tuneRF(x = training_set_scaled[,-1], 
              y=training_set_scaled$DLQs,
              mtryStart = 2,
              ntreeTry=250, 
              stepFactor = 1, ## 1st try 3 variables, next 6 , next 12 , next 24 MtryStart*Stepfactor 
              improve = 0.001, ## delta OOB 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 20, 
              importance=TRUE
) # random Forest tuning also lead to mtry = 2

y_pred = predict(T2_RF, newdata = training_set_scaled[-1], type='class')
CM = table(training_set_scaled[,1],y_pred)
RF_Speci_Train = CM[4]/(CM[4]+CM[2])

set.seed(1234)
folds = createFolds(training_set_scaled$DLQs, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set_scaled[-x, ]
  test_fold = training_set_scaled[x, ]
  T2_RF_temp = randomForest(DLQs ~ ., data = training_fold, 
                       ntree=500, mtry = 2, nodesize = 40)
  y_pred = predict(T2_RF_temp, newdata = test_fold[-1], type='class')
  CM = table(test_fold[,1],y_pred)
  temp = CM[4]/(CM[4]+CM[2])
  return(temp)
})
RF_Speci_KF = round(mean(as.numeric(cv)),5)*100

y_pred = predict(T2_RF, newdata = test_set_scaled[-1], type='class')
CM = table(test_set_scaled[,1],y_pred)
RF_Speci_Test = CM[4]/(CM[4]+CM[2])

# ANN -- Specificity:Train - 87.254 K-fold Train - xxxxxx Test 85.093 ----
training_set_scaled_ANN = training_set_scaled
training_set_scaled_ANN$DLQs = as.numeric(as.character(training_set_scaled_ANN$DLQs))
test_set_scaled_ANN = test_set_scaled
test_set_scaled_ANN$DLQs = as.numeric(as.character(test_set_scaled_ANN$DLQs))

n = names(training_set_scaled_ANN)
long_formula = as.formula(paste("DLQs ~", paste(n[!n %in% "DLQs"], collapse = " + ")))
set.seed(123)
T2_ANN = neuralnet(formula = long_formula,
                         data = training_set_scaled_ANN,
                         hidden = c(5,5),
                         err.fct = "sse",
                         linear.output = FALSE,
                         lifesign = "full",
                         lifesign.step = 1,
                         threshold = 0.05,
                         stepmax = 100000)
plot(T2_ANN)
y_pred = ifelse(T2_ANN$net.result[[1]] >= 0.5,1,0)
CM = table(training_set_scaled_ANN[,1],y_pred)
ANN_Speci_Train = CM[4]/(CM[4]+CM[2])
ANN_Speci_Train

y_pred = compute(T2_ANN,test_set_scaled_ANN[,-1])
y_pred = ifelse(y_pred$net.result >= 0.5,1,0)
CM = table(test_set_scaled_ANN[,1],y_pred)
ANN_Speci_Test = CM[4]/(CM[4]+CM[2])
ANN_Speci_Test

# Test Data preparation-----
# reading the data
excel_sheets(path = 'test.xlsx')
testdata = read_excel(path = 'test.xlsx', sheet = 'test')

# rename cols, new features, data type adjustments
colnames(testdata) = c('RowID','DLQs','Utlz_UnsecLines','DebtRatio',
                       'Credit_Loans','Dependents')
testdata = testdata %>% 
  dplyr::select(DLQs,Utlz_UnsecLines,DebtRatio,Credit_Loans,Dependents) 
testdata$UUL_flag = ifelse(testdata$Utlz_UnsecLines>1,1,0)
testdata$DR_flag = ifelse(testdata$DebtRatio>1,1,0)
sapply(testdata,class)
testdata$Dependents = ifelse(testdata$Dependents =='NA',NA,testdata$Dependents)
testdata$Dependents = as.numeric(testdata$Dependents)
testdata[,c(1,6,7)] = data.frame(lapply(testdata[,c(1,6,7)],as.factor))

# spliting into two kinds outliers and normal data
T1_testdata = subset(testdata, UUL_flag == 1 | DR_flag == 1)
T2_testdata = subset(testdata, UUL_flag == 0 & DR_flag == 0)

# Check missing values
Missing_data_Check(T2_testdata)
Missing_data_Check(T1_testdata)

# Handling missing data 
vif(data.frame(T2_testdata[,c(2:4)]))
T2_testdata_C = subset(T2_testdata,!is.na(Dependents))
T2_testdata_M = subset(T2_testdata,is.na(Dependents))

dependents = knn(train = scale(T2_testdata_C[,c(2,3,4)]),
                 test = as.matrix(cbind(scale(T2_testdata_M[2]), T2_testdata_M[3], scale(T2_testdata_M[4]))),
                 cl = as.factor(T2_testdata_C$Dependents),
                 k = 3,
                 prob = F)
T2_testdata_M$Dependents = dependents
T2_testdata = rbind(T2_testdata_C,T2_testdata_M)
rm(list = c('T2_testdata_C','T2_testdata_M'))
Missing_data_Check(T2_testdata)

vif(data.frame(T1_testdata[,c(2:4)]))
T1_testdata_C = subset(T1_testdata,!is.na(Dependents))
T1_testdata_M = subset(T1_testdata,is.na(Dependents))

dependents = knn(train = scale(T1_testdata_C[,c(2,3,4)]),
                 test = as.matrix(cbind(scale(T1_testdata_M[2]), T1_testdata_M[3], scale(T1_testdata_M[4]))),
                 cl = as.factor(T1_testdata_C$Dependents),
                 k = 9,
                 prob = F)
T1_testdata_M$Dependents = dependents
T1_testdata = rbind(T1_testdata_C,T1_testdata_M)
rm(list = c('T1_testdata_C','T1_testdata_M'))
Missing_data_Check(T1_testdata)

## Model building for T2 - 94:6 target varibale ratio
T2_testdata = T2_testdata[,1:5]
T2_testdata$Dependents = as.numeric(T2_testdata$Dependents)

ggplot(data = T2_testdata)+
  geom_point(aes(x = Utlz_UnsecLines, y = DebtRatio,
                 #shape = as.factor(Dependents), size = Credit_Loans, 
                 color = DLQs))
# SMOTE has oversampled the major class area too - so trying boundary SMOTE
T2_testdata_SMOTE_B = BLSMOTE(as.data.frame(T2_testdata[2:5]),as.numeric(T2_testdata$DLQs),
                              K=4,C=3,dupSize=15,method =c("type1"))
table(T2_testdata_SMOTE_B$data$class)/length(T2_testdata_SMOTE_B$data$class)
table(T2_testdata$DLQs)/length(T2_testdata$DLQs)
T2_testdata_SMOTE_BS = T2_testdata_SMOTE_B$data
T2_testdata_SMOTE_BS$DLQs = ifelse(T2_testdata_SMOTE_BS$class == 1, 0, 1)
T2_testdata_SMOTE_BS = T2_testdata_SMOTE_BS[,c(6,1,2,3,4)]
T2_testdata_SMOTE_BS$DLQs = as.factor(T2_testdata_SMOTE_BS$DLQs)

ggplot(data = T2_testdata_SMOTE_BS)+
  geom_point(aes(x = Utlz_UnsecLines, y = DebtRatio,
                 #shape = as.factor(Dependents), size = Credit_Loans, 
                 color = DLQs))

T2_testdata_Scale = T2_testdata_SMOTE_BS
T2_testdata_Scale[,-1] = scale(T2_testdata_Scale[,-1]) 

# Model Testing againist Test data LR: 48.889 KNN: 24.444  ----

T2_LR = glm( formula = DLQs~., 
             family = binomial,
             data = T2_traindata_SMOTE_BS_scaled)
prob_pred = predict(T2_LR, type = 'response', newdata = T2_testdata_Scale[-1])
y_pred = ifelse(prob_pred > 0.55, 1, 0)
CM = table(T2_testdata_Scale$DLQs,y_pred)
LR_Speci_Hold = CM[4]/(CM[4]+CM[2])
LR_Speci_Hold 

y_pred = knn(train =T2_traindata_SMOTE_BS_scaled[,-1],
             test =T2_testdata_Scale[,-1],
             cl = T2_traindata_SMOTE_BS_scaled[, 1],
             k = 5,
             prob = TRUE)
CM = table(T2_testdata_Scale$DLQs,y_pred)
Knn_Speci_Hold = CM[4]/(CM[4]+CM[2])
Knn_Speci_Hold

T2_SVM = svm(formula = DLQs ~ .,
             data = T2_traindata_SMOTE_BS_scaled,
             type = 'C-classification',
             kernel = 'radial', cost= 3, gamma= 4)
y_pred = predict(T2_SVM, newdata = T2_testdata_Scale[-1])
CM = table(T2_testdata_Scale$DLQs,y_pred)
SVM_Speci_Hold = CM[4]/(CM[4]+CM[2])
SVM_Speci_Hold

T2_NB = naiveBayes(x = T2_traindata_SMOTE_BS_scaled[-1],
                   y = T2_traindata_SMOTE_BS_scaled$DLQs)

y_pred = predict(T2_NB, newdata = T2_testdata_Scale[-1])
CM = table(T2_testdata_Scale$DLQs,y_pred)
NB_Speci_Hold = CM[4]/(CM[4]+CM[2])
NB_Speci_Hold























