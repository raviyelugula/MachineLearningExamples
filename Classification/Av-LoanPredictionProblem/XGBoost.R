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
# Converting factors to numerics 
sapply(data_set,class)
names(data_set)
temp = data.frame(model.matrix(~ Gender - 1, data = data_set))
data_set = data.frame(data_set, temp[-1])
data_set = data_set[-1]
temp = data.frame(model.matrix(~ Married - 1, data = data_set))
data_set = data.frame(data_set, temp[-1])
data_set = data_set[-1]
temp = data.frame(model.matrix(~ Dependents - 1, data = data_set))
data_set = data.frame(data_set, temp[-1])
data_set = data_set[-1]
temp = data.frame(model.matrix(~ Education - 1, data = data_set))
data_set = data.frame(data_set, temp[-1])
data_set = data_set[-1]
temp = data.frame(model.matrix(~ Self_Employed - 1, data = data_set))
data_set = data.frame(data_set, temp[-1])
data_set = data_set[-1]
temp = data.frame(model.matrix(~ Property_Area - 1, data = data_set))
data_set = data.frame(data_set, temp[-1])
data_set = data_set[-6]
data_set$Loan_Status = as.numeric(as.character(factor(data_set$Loan_Status,
                                                      levels = c(0,1),
                                                      labels = c(0,1))))

names(test_data_set)
temp = data.frame(model.matrix(~ Gender - 1, data = test_data_set))
test_data_set = data.frame(test_data_set, temp[-1])
test_data_set = test_data_set[-2]
temp = data.frame(model.matrix(~ Married - 1, data = test_data_set))
test_data_set = data.frame(test_data_set, temp[-1])
test_data_set = test_data_set[-2]
temp = data.frame(model.matrix(~ Dependents - 1, data = test_data_set))
test_data_set = data.frame(test_data_set, temp[-1])
test_data_set = test_data_set[-2]
temp = data.frame(model.matrix(~ Education - 1, data = test_data_set))
test_data_set = data.frame(test_data_set, temp[-1])
test_data_set = test_data_set[-2]
temp = data.frame(model.matrix(~ Self_Employed - 1, data = test_data_set))
test_data_set = data.frame(test_data_set, temp[-1])
test_data_set = test_data_set[-2]
temp = data.frame(model.matrix(~ Property_Area - 1, data = test_data_set))
test_data_set = data.frame(test_data_set, temp[-1])
test_data_set = test_data_set[-7]

# Scaling all elements
for(i in 1:15){
  if(i!=6){
    data_set[i] = scale(data_set[i])
  }
}

for(i in 1:15){
  if(i!=1){
    test_data_set[i] = scale(test_data_set[i])
  }
}
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
### XG-Boost -----[]
require(xgboost)
XGBoost_Model = xgboost(data = as.matrix(dev_dataset[-6]),
                     label = dev_dataset$Loan_Status,
                     nrounds = 10)
dev_prediction = predict(XGBoost_Model, newdata = as.matrix(dev_dataset[-6]))
dev_prediction_cutoff_dataframe = data.frame(dev_prediction,dev_dataset$Loan_Status)
dev_prediction = ifelse(dev_prediction >= 0.6,1,0)
acurracy_table=addmargins(table(dev_dataset$Loan_Status,dev_prediction)) 
acurracy_table
acurracy = (acurracy_table[1]+acurracy_table[5])/acurracy_table[9]
acurracy #96.08%
# holdout data test
hold_prediction = predict(XGBoost_Model, newdata = as.matrix(holdout_dataset[-6]), type = "prob")
hold_prediction_cutoff_dataframe = data.frame(hold_prediction,holdout_dataset$Loan_Status)
hold_prediction = ifelse(hold_prediction >= 0.6,1,0)
acurracy_table=addmargins(table(holdout_dataset$Loan_Status,hold_prediction)) 
acurracy_table
acurracy = (acurracy_table[1]+acurracy_table[5])/acurracy_table[9]
acurracy #80.08%
# complete data XGBoost model building
XGBoost_Model = xgboost(data = as.matrix(data_set[-6]),
                        label = data_set$Loan_Status,
                        nrounds = 10)
# testdata results
test_predict = predict(XGBoost_Model, newdata = as.matrix(test_data_set[-1]), type ='prob')
test_predict = ifelse(test_predict >=0.6,1,0)
test_predict = ifelse(test_predict==1,"Y","N")
test_dataframe = data.frame(Loan_ID = test_data_set$Loan_ID,Loan_Status = test_predict)
write.csv(test_dataframe,'XGBoost_Solution.csv',row.names = F)
### Final score : 68.75% -----



