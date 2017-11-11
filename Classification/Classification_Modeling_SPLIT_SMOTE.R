## Process followed. Considered the Test data as future data to see how our best model with stands to unknown data.
## The Train data is used to build, tune, compare the models and give the best to production (handle the test data)
## So the train data is splited, handle the missing data, SMOTE it, SCALE it and then feature it, in the same order before building the models.

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
require(gridExtra) ## Multiple plots in single pannel

# reading the data ----
excel_sheets(path = 'training.xlsx')
traindata = read_excel(path = 'training.xlsx', sheet = 'training')
excel_sheets('test.xlsx')
testdata = read_excel(path = 'test.xlsx', sheet = 'test')

# rename cols, new features, data type adjustments ----
Data_Building <- function(data){
  colnames(data) = c('RowID','DLQs','Utlz_UnsecLines','DebtRatio',
                          'Credit_Loans','Dependents')
  data = data %>% 
    dplyr::select(DLQs,Utlz_UnsecLines,DebtRatio,Credit_Loans,Dependents) 
  data$UUL_flag = ifelse(data$Utlz_UnsecLines>1,1,0)
  data$DR_flag = ifelse(data$DebtRatio>1,1,0)
  message('Original data formats:')
  print(sapply(data,class))
  data$Dependents = ifelse(data$Dependents =='NA',NA,data$Dependents)
  data$Dependents = as.numeric(data$Dependents)
  data[,c(1,6,7)] = data.frame(lapply(data[,c(1,6,7)],as.factor))
  message('Final data formats:')
  print(sapply(data,class))
  return(data)
}
traindata = Data_Building(traindata)
testdata = Data_Building(testdata)
# seperating into two kinds outliers and normal data ----
T1_traindata = subset(traindata, UUL_flag == 1 | DR_flag == 1)
T2_traindata = subset(traindata, UUL_flag == 0 & DR_flag == 0)

T1_testdata = subset(testdata, UUL_flag == 1 | DR_flag == 1)
T2_testdata = subset(testdata, UUL_flag == 0 & DR_flag == 0)

## T2 data modeling
# Missing data checking ----
Missing_data_Check <- function(data_set){
  NA_Count = sapply(data_set,function(y) sum(length(which(is.na(y))))) 
  Null_Count = sapply(data_set,function(y) sum(length(which(is.null(y)))))
  Length0_Count = sapply(data_set,function(y) sum(length(which(length(y)==0))))
  Empty_Count = sapply(data_set,function(y) sum(length(which(y==''))))
  Total_NonData = NA_Count+Null_Count+Length0_Count+Empty_Count
  return( Total_NonData )
}
Missing_data_Check(T2_traindata)

# Splitting the training data (SET1: build and tune the model) ~ (SET2: test the model) ----
set.seed(123)
split = sample.split(T2_traindata$Dependents, SplitRatio = 0.75)
T2_traindata_Train = subset(T2_traindata, split == TRUE)
T2_traindata_Test = subset(T2_traindata, split == FALSE)

# Missing data handling ----
Missing_data_handling <- function(data){
  print(vif(data.frame(data[,c(2:4)])))
  data_C = subset(data,!is.na(Dependents))
  data_M = subset(data,is.na(Dependents))
  
  set.seed(123)
  split = sample.split(data_C$Dependents, SplitRatio = 0.75)
  data_C_Tr = subset(data_C, split == TRUE)
  data_C_Te = subset(data_C, split == FALSE)
  dependents = knn(train = scale(data_C_Tr[,c(2,3,4)]),
                   test = scale(data_C_Te[,c(2,3,4)]),
                   cl = as.factor(data_C_Tr$Dependents),
                   k = 9,
                   prob = F)
  message(paste0('KNN Accuracy: ',round(length(which(data_C_Te$Dependents == dependents))/length(dependents),2)))
  model = lm(Dependents~Utlz_UnsecLines+DebtRatio+Credit_Loans,
             data=data_C_Tr)
  dependents = round(predict(model,data_C_Te)) ## LR is not working as it gives all as 1
  message(paste0('Linear regression Accuracy: ',round(length(which(data_C_Te$Dependents == dependents))/length(dependents),2)))
  rm(list = c('data_C_Te','data_C_Tr'))
  set.seed(1234)
  dependents = knn(train = scale(data_C[,c(2,3,4)]),
                   test = scale(data_M[,c(2,3,4)]),
                   cl = as.factor(data_C$Dependents),
                   k = 9,
                   prob = F)
  data_M$Dependents = dependents
  data = rbind(data_C,data_M)
  rm(list = c('data_C','data_M'))
  message('Missing data in each column after handling:')
  message(paste0(Missing_data_Check(data)))
  data$Dependents = as.numeric(data$Dependents)
  return(data)
}
T2_traindata_Test = Missing_data_handling(T2_traindata_Test)
T2_traindata_Train = Missing_data_handling(T2_traindata_Train)

# Target variable ratio check ----
Two_D_View <- function(data){
  P1 = ggplot(data = data)+
    geom_point(aes(x = Utlz_UnsecLines, y = DebtRatio,
                   color = DLQs),show.legend = F)
  P2 = ggplot(data = data)+
    geom_point(aes(x = Dependents, y = Credit_Loans,
                    color = DLQs),show.legend = F)
  P3 = ggplot(data = data)+
    geom_point(aes(x = Utlz_UnsecLines, y = Credit_Loans,
                   color = DLQs),show.legend = F)
  P4 = ggplot(data = data)+
    geom_point(aes(x = Dependents, y = DebtRatio,
                    color = DLQs),show.legend = F)
  grid.arrange(P1, P2, P3,P4, ncol = 2, nrow = 2)
}
Target_Ratio_Check <- function(data){
  data$Dependents = as.numeric(data$Dependents)
  message('Target ratio split:')
  print(table(data$DLQs))
  message('Target ratio:')
  print(round(table(data$DLQs)[1]/sum(table(data$DLQs)),2))
  Two_D_View(data)
}
Target_Ratio_Check(T2_traindata_Test) #95:5 
Target_Ratio_Check(T2_traindata_Train) #94:6
# SMOTE for treating imbalance data set ----
SMOTE_fitting <- function(data,o,u){
  data_SMOTE = DMwR::SMOTE(DLQs~Utlz_UnsecLines+DebtRatio+Credit_Loans+Dependents,
                     as.data.frame(data),perc.over = o,perc.under = u)
  message('Original data ratio:')
  print(round(table(data$DLQs)/length(data$DLQs),2))
  message('SMOTEd data ratio:')
  print(table(data_SMOTE$DLQs)/length(data_SMOTE$DLQs))
  message('SMOTEd data split')
  print(table(data_SMOTE$DLQs))
  Two_D_View(data_SMOTE)
  return(data_SMOTE)
}
Two_D_View(T2_traindata_Test)
T2_traindata_Test_SMOTEd = SMOTE_fitting(T2_traindata_Test,600,300)
Two_D_View(T2_traindata_Train)
T2_traindata_Train_SMOTEd = SMOTE_fitting(T2_traindata_Train,600,300)

# SMOTE has oversampled the major class area too - so trying boundary SMOTE ----
Boderline_SMOTE_fitting <- function(data,i){
  set.seed(1234)
  data_SMOTE_B = BLSMOTE(as.data.frame(data[2:5]),as.numeric(data$DLQs),
                                 K=4,C=3,dupSize=i,method =c("type1"))
  message('Boarderline SMOTE data Target variable ratio:')
  print(round(table(data_SMOTE_B$data$class)/length(data_SMOTE_B$data$class),2))
  message('Original data set Target ratio:')
  print(round(table(data$DLQs)/length(data$DLQs),2))
  data_SMOTE_BS = data_SMOTE_B$data
  data_SMOTE_BS$DLQs = ifelse(data_SMOTE_BS$class == 1, 0, 1)
  data_SMOTE_BS = data_SMOTE_BS[,c(6,1,2,3,4)]
  data_SMOTE_BS$DLQs = as.factor(data_SMOTE_BS$DLQs)
  Two_D_View(data_SMOTE_BS)
  return(data_SMOTE_BS)
}
Two_D_View(T2_traindata_Test)
T2_traindata_Test_BS = Boderline_SMOTE_fitting(T2_traindata_Test,25) #70:30

Two_D_View(T2_traindata_Train)
T2_traindata_Train_BS = Boderline_SMOTE_fitting(T2_traindata_Train,25) #70:30
rm(list = c('T2_traindata_Test_SMOTEd','T2_traindata_Train_SMOTEd')) # Removing as SMOTE has overfitted the majored regions too

# Building a Scaled data set for classification models ----
Scaling <- function(data){
  data_scaled = data
  data_scaled[-1] = scale(data_scaled[-1])
  return(data_scaled)
}
T2_traindata_Test_BS_Scaled = Scaling(T2_traindata_Test_BS)
T2_traindata_Train_BS_Scaled = Scaling(T2_traindata_Train_BS)
# Logistic regression -- Specificity: Train - 78.14 K-fold Train - 77.78 Test - 86.29 ----
T2_LR = glm( formula = DLQs~., 
             family = binomial,
             data = T2_traindata_Train_BS_Scaled)
prob_pred = predict(T2_LR, type = 'response', newdata = T2_traindata_Train_BS_Scaled[-1])
y_pred = ifelse(prob_pred > 0.55, 1, 0)
CM = table(T2_traindata_Train_BS_Scaled[,1],y_pred)
LR_Speci_Train = CM[4]/(CM[4]+CM[2])
round(LR_Speci_Train*100,2)

require(lmtest)
lrtest(T2_LR) # overall test i significant
require(pscl)
pR2(T2_LR) # 35 - very good McFadden R2

set.seed(1234)
folds = createFolds(T2_traindata_Train_BS_Scaled$DLQs, k = 10)
cv = lapply(folds, function(x) {
  training_fold = T2_traindata_Train_BS_Scaled[-x, ]
  test_fold = T2_traindata_Train_BS_Scaled[x, ]
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
round(LR_Speci_KF*100,2)

prob_pred = predict(T2_LR, type = 'response', newdata = T2_traindata_Test_BS_Scaled[-1])
y_pred = ifelse(prob_pred > 0.55, 1, 0)
CM = table(T2_traindata_Test_BS_Scaled[,1],y_pred)
LR_Speci_Test = CM[4]/(CM[4]+CM[2])
round(LR_Speci_Test*100,2)

# Test data prep LR: 62.33 ----
Missing_data_Check(T2_testdata)
data_C = subset(T2_testdata,!is.na(Dependents))
data_M = subset(T2_testdata,is.na(Dependents))
dependents = knn(train = scale(data_C[,c(2,3,4)]),
                 test = as.matrix(cbind(scale(data_M[2]), data_M[3], scale(data_M[4]))),
                 cl = as.factor(data_C$Dependents),
                 k = 9,
                 prob = F)
data_M$Dependents = dependents
T2_testdata = rbind(data_C,data_M)
rm(list = c('data_C','data_M'))
Missing_data_Check(T2_testdata)
T2_testdata$Dependents = as.numeric(T2_testdata$Dependents)
Two_D_View(T2_testdata)
T2_testdata_BS = Boderline_SMOTE_fitting(T2_testdata,15)
T2_testdata_BS_Scaled = Scaling(T2_testdata_BS)

prob_pred = predict(T2_LR, type = 'response', newdata = T2_testdata_BS_Scaled[-1])
y_pred = ifelse(prob_pred > 0.55, 1, 0)
CM = table(T2_testdata_BS_Scaled$DLQs,y_pred)
LR_Speci_Hold = CM[4]/(CM[4]+CM[2])
round(LR_Speci_Hold*100,2) 


