require(dplyr)
require(ggplot2)
require(readxl)
require(dummies)
require(randomForest)
require(caTools)
require(caret)
require(usdm)
require(caret)
require(rpart)
require(e1071)
require(caret)

excel_sheets('Telco Churn.xlsx')
rawdata = read_excel('Telco Churn.xlsx','WA_Fn-UseC_-Telco-Customer-Chur')
sapply(rawdata,class)
data_factors = rawdata
factor_col_names = c("gender","SeniorCitizen","Partner","Dependents"
                     ,"PhoneService","MultipleLines","InternetService" 
                     ,"OnlineSecurity","OnlineBackup","DeviceProtection"
                     ,"TechSupport","StreamingTV","StreamingMovies" 
                     ,"Contract","PaperlessBilling","PaymentMethod","Churn")
data_factors[factor_col_names] = lapply(data_factors[factor_col_names],factor)
str(data_factors)

Missing_data_Check <- function(data_set){
  NA_Count = sapply(data_set,function(y) sum(length(which(is.na(y))))) 
  Null_Count = sapply(data_set,function(y) sum(length(which(is.null(y)))))
  Length0_Count = sapply(data_set,function(y) sum(length(which(length(y)==0))))
  Empty_Count = sapply(data_set,function(y) sum(length(which(y==''))))
  Total_NonData = NA_Count+Null_Count+Length0_Count+Empty_Count
  return( Total_NonData )
}
Missing_data_Check(data_factors)
data_factors[which(is.na(data_factors$TotalCharges)),'TotalCharges'] = mean(data_factors$TotalCharges,na.rm = T)
data_factors = data_factors[-1]

sapply(data_factors,class)
# data_factors$Churn = as.factor(ifelse(data_factors$Churn=='Yes',1,0))

set.seed(123)
split = sample.split(data_factors$Churn, SplitRatio = 0.75)
data_factors_tr = subset(data_factors, split == TRUE)
data_factors_te = subset(data_factors, split == FALSE)

mtry_opt_value = train(form = Churn~ .,
                       data = data_factors_tr, method = 'rf')
RF_model = randomForest(Churn ~ ., 
                        data = data_factors_tr, 
                        ntree=200, mtry = 2, nodesize = 20,
                        importance=TRUE)
featureImp_df = RF_model$importance

caret_tune = train(form = Churn ~ ., data = data_factors_tr, method = 'rpart')
caret_tune
caret_tune$bestTune # CP - Tunning 
cart_model = rpart(Churn ~ ., 
                   data = data_factors_tr,
                   method= 'class',
                   minsplit = 100,
                   cp=0.008321446)
cart_model$variable.importance

featureImp_df[order(featureImp_df[,4]),]
sort(cart_model$variable.importance)


# names(data_factors_tr[,c('tenure','TotalCharges','Contract','InternetService',
#                          'MonthlyCharges','TechSupport','OnlineSecurity','Churn')])
# data_factors_tr_imp = data_factors_tr[,c('tenure','TotalCharges','Contract','InternetService',
#                                          'MonthlyCharges','TechSupport','OnlineSecurity','Churn')]
# temp = data_factors_tr[,c('tenure','TotalCharges','Contract','InternetService',
#                           'MonthlyCharges','TechSupport','OnlineSecurity')]
# dummy_df = dummy.data.frame(as.data.frame(temp))
# dummy_df = cbind(dummy_df,Churn=data_factors_tr$Churn)
# vif(dummy_df[,c(3:4)])
# vif(dummy_df[,c(6,7)])
# vif(dummy_df[,c(10:11)])
# vif(dummy_df[,c(13:14)])
# vif(dummy_df[,c(1:4,6,7,9:11,13,14)])
# vif(dummy_df[,c(1,2,9)])
# 
# dummy_df = dummy_df[-2]

# log_model_imp= glm(formula = Churn~.,  
#                    family = binomial,
#                    data = dummy_df)
# summary(log_model_imp)

log_model_imp= glm(formula = Churn~tenure+Contract+InternetService
                    +MonthlyCharges+TechSupport+OnlineSecurity,
                   family = binomial,
                   data = data_factors_tr[,c('tenure','Contract','InternetService',
                                             'MonthlyCharges','TechSupport','OnlineSecurity','Churn')])
summary(log_model_imp)
prob_pred = predict(log_model_imp, type = 'response',
                    newdata = data_factors_te[,c('tenure','Contract','InternetService',
                                                  'MonthlyCharges','TechSupport','OnlineSecurity')])
y_pred = ifelse(prob_pred > 0.4, 1, 0)
CM = table(data_factors_te$Churn,y_pred)
log_model_imp_spec = CM[4]/(CM[4]+CM[2])
round(log_model_imp_spec*100,2)
log_model_imp_acc =( CM[1]+CM[4])/(CM[1]+CM[4]+CM[2]+CM[3])
round(log_model_imp_acc*100,2)


##################### Naive Bayes all features
NB_model = naiveBayes(x = data_factors_tr[-20],
                   y = data_factors_tr$Churn)

y_pred = predict(NB_model, newdata = data_factors_te[-20])
CM = table(data_factors_te$Churn,y_pred)
NB_Specificity = CM[4]/(CM[4]+CM[2])
NB_Acc = (CM[1]+CM[4])/(CM[1]+CM[4]+CM[2]+CM[3])

#################### Naive Bayes imp features
NB_model = naiveBayes(x = data_factors_tr[,c('tenure','Contract','InternetService',
                                             'MonthlyCharges','TechSupport','OnlineSecurity')],
                      y = data_factors_tr$Churn)

y_pred = predict(NB_model, newdata = data_factors_te[,c('tenure','Contract','InternetService',
                                                        'MonthlyCharges','TechSupport','OnlineSecurity')])
CM = table(data_factors_te$Churn,y_pred)
NB_Specificity_imp = CM[4]/(CM[4]+CM[2])
NB_Acc_imp = (CM[1]+CM[4])/(CM[1]+CM[4]+CM[2]+CM[3])

################### RF all features
RF_model = randomForest(Churn ~ ., 
                        data = data_factors_tr, 
                        ntree=200, mtry = 2, nodesize = 20,
                        importance=TRUE) # mtry 2 is optimal value - found from caret 
y_pred = predict(RF_model, newdata = data_factors_te[-20], type='class')
CM = table(data_factors_te$Churn,y_pred)
RF_Specificity = CM[4]/(CM[4]+CM[2])
RF_Acc = (CM[1]+CM[4])/(CM[1]+CM[4]+CM[2]+CM[3])
################# RF Imp features
RF_model = randomForest(Churn ~ ., 
                        data = data_factors_tr[,c('tenure','Contract','InternetService',
                                                  'MonthlyCharges','TechSupport','OnlineSecurity','Churn')], 
                        ntree=200, mtry = 2, nodesize = 20,
                        importance=TRUE) # mtry 2 is optimal value - found from caret 
y_pred = predict(RF_model, newdata = data_factors_te[,c('tenure','Contract','InternetService',
                                                        'MonthlyCharges','TechSupport','OnlineSecurity')], type='class')
CM = table(data_factors_te$Churn,y_pred)
RF_Specificity_imp = CM[4]/(CM[4]+CM[2])
RF_Acc_imp = (CM[1]+CM[4])/(CM[1]+CM[4]+CM[2]+CM[3])

################## SVM radial all features
tune_svm_kernal = tune(svm, Churn~ ., data = data_factors_tr,
                       kernal = 'radial',
                       ranges = list(cost = c(0.1,0.4,0.8,1,3,5,10,50,100), # penalising factor for missclassification, high c => low bias, high viariance, default is 1
                                     gamma = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2,4))) # smoothening the boundary shape sharpness, low gama => pointy bounday, low bias, high variance, default 1/dimensions

SVM_model = svm(formula = Churn ~ .,
             data = data_factors_tr,
             type = 'C-classification',
             kernel = 'radial', cost= c, gamma= g)
y_pred = predict(SVM_model, newdata = data_factors_te[-20])
CM = table(data_factors_te$Churn,y_pred)
SVM_Specificity = CM[4]/(CM[4]+CM[2])
SVM_Acc = (CM[1]+CM[4])/(CM[1]+CM[4]+CM[2]+CM[3])

######### SVM imp features 
tune_svm_kernal = tune(svm, Churn~ ., data = data_factors_tr[,c('tenure','Contract','InternetService',
                                                                'MonthlyCharges','TechSupport','OnlineSecurity','Churn')],
                       kernal = 'radial',
                       ranges = list(cost = c(0.1,0.4,0.8,1,3,5,10,50,100), # penalising factor for missclassification, high c => low bias, high viariance, default is 1
                                     gamma = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2,4))) # smoothening the boundary shape sharpness, low gama => pointy bounday, low bias, high variance, default 1/dimensions

SVM_model = svm(formula = Churn ~ .,
                data = data_factors_tr[,c('tenure','Contract','InternetService',
                                          'MonthlyCharges','TechSupport','OnlineSecurity','Churn')],
                type = 'C-classification',
                kernel = 'radial', cost= c, gamma= g)
y_pred = predict(SVM_model, newdata = data_factors_te[,c('tenure','Contract','InternetService',
                                                         'MonthlyCharges','TechSupport','OnlineSecurity')])
CM = table(data_factors_te$Churn,y_pred)
SVM_Specificity_imp = CM[4]/(CM[4]+CM[2])
SVM_Acc_imp = (CM[1]+CM[4])/(CM[1]+CM[4]+CM[2]+CM[3])



