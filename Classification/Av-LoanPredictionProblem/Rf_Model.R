### User defined functions -----
Missing_data_Check <- function(data_set){
  NA_Count = sapply(data_set,function(y) sum(length(which(is.na(y))))) 
  Null_Count = sapply(data_set,function(y) sum(length(which(is.null(y)))))
  Length0_Count = sapply(data_set,function(y) sum(length(which(length(y)==0))))
  Empty_Count = sapply(data_set,function(y) sum(length(which(y==''))))
  Total_NonData = NA_Count+Null_Count+Length0_Count+Empty_Count
  return( Total_NonData )
}

factorizing <- function(current_dataset,flag){
  current_dataset$Gender = factor(current_dataset$Gender, levels = c('Female','Male'), labels = c(0,1))
  current_dataset$Married = factor(current_dataset$Married, levels = c('No','Yes'), labels = c(0,1))
  current_dataset$Dependents = factor(current_dataset$Dependents, levels = c('0','1','2','3+'), labels = c(0,1,2,3))
  current_dataset$Education = factor(current_dataset$Education, levels = c('Graduate','Not Graduate'), labels = c(0,1))
  current_dataset$Self_Employed = factor(current_dataset$Self_Employed, levels = c('No','Yes'), labels = c(0,1))
  current_dataset$Property_Area = factor(current_dataset$Property_Area, levels = c('Rural','Semiurban','Urban'), labels = c(0,1,2))
  if(flag==1){
    current_dataset$Loan_Status = factor(current_dataset$Loan_Status, levels = c("N","Y"), labels = c(0,1))
  }
  return(current_dataset)
}


### Feature engineering ----
# data load
data_set = read.csv('train.csv', header = T)
test_data_set = read.csv('test.csv',header =T)
# Check any loan ids are dulplicated
which((table(data_set$Loan_ID)>2)==T)
which((table(test_data_set$Loan_ID)>2)==T)
# Remove Primary Key i.e. LoanID
data_set=data_set[,-1]
#test_data_set=test_data_set[,-1]
# Cheking Missing Values
Missing_data_Check(data_set)
Missing_data_Check(test_data_set)
# Missing data treatment 
data_set_No_Missing_Values=data_set[!apply(data_set, 1, function(x) any(x=="" | is.na(x))),] 
temp = table(data_set_No_Missing_Values$Gender)
data_set[data_set$Gender=='',]$Gender = names(temp[which(temp==max(temp))])
temp = table(data_set_No_Missing_Values$Married)
data_set[data_set$Married=='',]$Married = names(temp[which(temp==max(temp))])
temp = table(data_set_No_Missing_Values$Dependents)
data_set[data_set$Dependents=='',]$Dependents = names(temp[which(temp==max(temp))])
temp = table(data_set_No_Missing_Values$Self_Employed)
data_set[data_set$Self_Employed=='',]$Self_Employed =names(temp[which(temp==max(temp))])
data_set[is.na(data_set$LoanAmount),]$LoanAmount = median(data_set_No_Missing_Values$LoanAmount,na.rm = T)
data_set[is.na(data_set$Loan_Amount_Term),]$Loan_Amount_Term = median(data_set_No_Missing_Values$Loan_Amount_Term,na.rm = T)
data_set[is.na(data_set$Credit_History),]$Credit_History = median(data_set_No_Missing_Values$Credit_History,na.rm = T)
Missing_data_Check(data_set)

test_data_set_No_Missing_Values=test_data_set[!apply(test_data_set, 1, function(x) any(x=="" | is.na(x))),] 
temp = table(test_data_set_No_Missing_Values$Gender)
test_data_set[test_data_set$Gender=='',]$Gender = names(temp[which(temp==max(temp))])
temp = table(test_data_set_No_Missing_Values$Dependents)
test_data_set[test_data_set$Dependents=='',]$Dependents = names(temp[which(temp==max(temp))])
temp = table(test_data_set_No_Missing_Values$Self_Employed)
test_data_set[test_data_set$Self_Employed=='',]$Self_Employed =names(temp[which(temp==max(temp))])
test_data_set[is.na(test_data_set$LoanAmount),]$LoanAmount = median(test_data_set_No_Missing_Values$LoanAmount,na.rm = T)
test_data_set[is.na(test_data_set$Loan_Amount_Term),]$Loan_Amount_Term = median(test_data_set_No_Missing_Values$Loan_Amount_Term,na.rm = T)
test_data_set[is.na(test_data_set$Credit_History),]$Credit_History = median(test_data_set_No_Missing_Values$Credit_History,na.rm = T)
Missing_data_Check(test_data_set)
# labeling the factors
data_set=factorizing(data_set,1)
test_data_set=factorizing(test_data_set,0)
# removing temp objects
rm(list = c("data_set_No_Missing_Values","test_data_set_No_Missing_Values","temp"))

### Spliting the data set into dev and holdout ------
require(caTools)
set.seed(123)
splitvector = sample.split(data_set$Loan_Status, SplitRatio = 0.75)
dev_dataset = subset(data_set, splitvector==T)
holdout_dataset = subset(data_set, splitvector==F)
# removing tmep variables
rm(splitvector)
#### Random Forest building -------
require(randomForest)
# Dense Forest attempt
set.seed(123)
RF_Model = randomForest(x = dev_dataset[-12],
                        y = dev_dataset$Loan_Status,
                        data = dev_dataset,
                        ntree =  5000,
                        mtry =  4,
                        nodesize = 15)
RF_Model
plot(RF_Model, main = "High number of trees vs OOB error") # 20.22% OOB
# Optimized ntrees
set.seed(123)
RF_Model = randomForest(x = dev_dataset[-12],
                        y = dev_dataset$Loan_Status,
                        data = dev_dataset,
                        ntree =  300,
                        mtry =  4,
                        nodesize = 15)
RF_Model
plot(RF_Model, main = "optimized ntrees vs OOB error")  # 20.43% OOB
# optimizing mtry
set.seed(123)
tuned_RF_model = tuneRF(x = dev_dataset[-12],
                        y = dev_dataset$Loan_Status,
                        data = dev_dataset,
                        mtryStart = 8,
                        stepFactor = 1.5,
                        improve = 0.00001,
                        ntreeTry = 300,
                        nodesize = 15,
                        doBest = T, trace = T, plot = T,importance = T)
tuned_RF_model #tune_RF - mtry 2 & 20% OOB
require(caret)
set.seed(123)
grid_RF_tune = train(x = dev_dataset[-12],
                     y = dev_dataset$Loan_Status,
                     method = 'rf')
grid_RF_tune #gridsearch gives 2
set.seed(123)
RF_Model = randomForest(x = dev_dataset[-12],
                        y = dev_dataset$Loan_Status,
                        data = dev_dataset,
                        ntree =  300,
                        mtry =  2,
                        nodesize = 15)
RF_Model      # 19.57% OOB
plot(RF_Model, main = "optimized ntrees,mtry vs OOB error")
# optimizing nodesize
temp_dev_acc = numeric()
temp_nodesize = numeric()
temp_hold_acc = numeric()
j = 1
for (i in seq(from = 1, to = 150 , by = 3)){
  set.seed(123)
  RF_model = randomForest(x = dev_dataset[-12],
                          y = dev_dataset$Loan_Status,
                          data = dev_dataset,
                          ntree =  300,
                          mtry =  2,
                          nodesize = i)
  if(RF_model$confusion[1]!=460 & RF_model$confusion[2]!=460){
    temp_dev_acc[j] = (RF_model$confusion[1]+RF_model$confusion[4])/460
    temp_nodesize[j] = i
    holdout_prediction = predict(RF_model, newdata = holdout_dataset, type ='class')
    table = table(holdout_dataset$Loan_Status,holdout_prediction)
    temp_hold_acc[j] = (table[1]+table[4])/154
    j = j+1
  }
}
nodesize_df = data.frame(x=temp_nodesize,y1=temp_dev_acc,y2=temp_hold_acc) # nodesize 46
# Final model
RF_Model = randomForest(x = dev_dataset[-12],
                        y = dev_dataset$Loan_Status,
                        data = dev_dataset,
                        ntree =  300,
                        mtry =  2,
                        nodesize = 46)
RF_Model # 20% OOB 
plot(RF_Model, main = "optimized ntrees,mtry & nodesize vs OOB error")
# Full data based model 
RF_Model = randomForest(x = data_set[-12],
                        y = data_set$Loan_Status,
                        data = data_set,
                        ntree =  300,
                        mtry =  2,
                        nodesize = 46)
RF_Model  # 19.22 OOB 
# test data solution generation 
test_predict_rf = predict(RF_Model, newdata = test_data_set, type ='response')
test_predict_rf = ifelse(test_predict_rf==1,"Y","N")
test_dataframe = data.frame(Loan_ID = test_data_set$Loan_ID,Loan_Status = test_predict_rf)
write.csv(test_dataframe,'RF_Solution.csv',row.names = F)
### Final score : 77.778% -----
# Confusion matrix:
#   0   1 class.error
# 0 83 109  0.56770833
# 1  9 413  0.02132701
