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
  current_dataset$Credit_History = factor(current_dataset$Credit_History, levels = c('0','1'), labels = c(0,1))
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
rm(list = c("temp","data_set_No_Missing_Values","test_data_set_No_Missing_Values"))

temp=data_set[c(which(data_set$CoapplicantIncome>15000),
                which(data_set$ApplicantIncome>40000)),]
data_set$ApplicantIncome = ifelse(data_set$ApplicantIncome >40000, 40000,data_set$ApplicantIncome)
data_set$CoapplicantIncome = ifelse(data_set$CoapplicantIncome >15000,15000,data_set$CoapplicantIncome)
test_data_set$ApplicantIncome = ifelse(test_data_set$ApplicantIncome >40000, 40000,test_data_set$ApplicantIncome)
test_data_set$CoapplicantIncome = ifelse(test_data_set$CoapplicantIncome >15000,15000,test_data_set$CoapplicantIncome)

# labeling the factors
data_set=factorizing(data_set,1)
test_data_set=factorizing(test_data_set,0)
### Spliting the data set into dev and holdout ------
require(caTools)
set.seed(123)
splitvector = sample.split(data_set$Loan_Status, SplitRatio = 0.75)
dev_dataset = subset(data_set, splitvector==T)
holdout_dataset = subset(data_set, splitvector==F)
# removing tmep variables
rm(splitvector)
###  SVM - Kernal Non Linear ----
require(e1071)
svm_kernal_model = svm(Loan_Status~., 
                       data = dev_dataset,
                       type ='C-classification',
                       kernal = 'radial')
# dev data prediction - 80.65
dev_predict_svm = predict(svm_kernal_model, newdata = dev_dataset[-12], type ='response')
accuracy_table=addmargins(table(actual = dev_dataset$Loan_Status,predicted = dev_predict_svm))
accuracy_table
paste0("dev dataset accuracy with SVM radial model : ",round((accuracy_table[1,1]+accuracy_table[2,2])/accuracy_table[3,3]*100,2))
# holdout data prediction - 83.12
holdout_dataset_predict_svm = predict(svm_kernal_model, newdata = holdout_dataset[-12], type ='response')
accuracy_table=addmargins(table(actual = holdout_dataset$Loan_Status,predicted = holdout_dataset_predict_svm))
accuracy_table
paste0("holdout dataset accuracy with SVM radial model : ",round((accuracy_table[1,1]+accuracy_table[2,2])/accuracy_table[3,3]*100,2))
# complete data model - 81.6
svm_kernal_model = svm(Loan_Status~., 
                       data = data_set,
                       type ='C-classification',
                       kernal = 'radial')
train_predict_svm = predict(svm_kernal_model, newdata = data_set[-12], type ='response')
accuracy_table=addmargins(table(actual = data_set$Loan_Status,predicted = train_predict_svm))
accuracy_table
paste0("train dataset accuracy with SVM radial model : ",round((accuracy_table[1,1]+accuracy_table[2,2])/accuracy_table[3,3]*100,2))
# test data solution generation 
test_predict_svm = predict(svm_kernal_model, newdata = test_data_set, type ='response')
test_predict_svm = ifelse(test_predict_svm==1,"Y","N")
test_dataframe = data.frame(Loan_ID = test_data_set$Loan_ID,
                            Loan_Status = test_predict_svm)
write.csv(test_dataframe,'SVM_Solution_2.csv',row.names = F)
### Final score : 77.083% -----







