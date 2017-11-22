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
### Logistic Regression ----
# model building
LogR_Model = glm(Loan_Status~., 
                 data = dev_dataset,
                 family = binomial)
# model significance
require(lmtest)
lrtest(LogR_Model)
# McFadden value
require(pscl)
pR2(LogR_Model)
# cutoff calculation
dev_prediction = fitted(LogR_Model)
cutoff_dataframe = data.frame(
  Cutoff=numeric(), 
  Error=numeric(), 
  stringsAsFactors=FALSE) 
tempVector = seq(from=0.15,to=0.95,by=0.05)
for(i in tempVector){
  dev_prediction_cutoff = floor(dev_prediction+i)
  acurracy_table=table(Actual =dev_dataset$Loan_Status, Predicted = dev_prediction_cutoff)
  error=acurracy_table[1,2] + acurracy_table[2,1]
  cutoff_dataframe = rbind(cutoff_dataframe,data.frame(Cutoff=i,Error=error))
}
LR_Cutoff=cutoff_dataframe$Cutoff[cutoff_dataframe$Error==min(cutoff_dataframe$Error)]
LR_Cutoff
plot(x=cutoff_dataframe[,1],y=cutoff_dataframe[,2],type = 'b')
dev_prediction =floor(dev_prediction+LR_Cutoff)
acurracy_table=addmargins(table(Actual =dev_dataset$Loan_Status, Predicted = dev_prediction))
acurracy_table
paste0("development accuracy in LR Model: ",round((acurracy_table[1,1]+acurracy_table[2,2])/acurracy_table[3,3]*100,2))
# testing against holdoutdata
holdout_prediction =floor(predict(LogR_Model,holdout_dataset[-12],type ='response')+LR_Cutoff)
acurracy_table=addmargins(table(Actual =holdout_dataset$Loan_Status, Predicted = holdout_prediction))
acurracy_table
paste0("holdout accuracy in LR Model: ",round((acurracy_table[1,1]+acurracy_table[2,2])/acurracy_table[3,3]*100,2))
# removing temp objects
rm(list=c("cutoff_dataframe","acurracy_table","dev_prediction",
          "dev_prediction_cutoff","error","i","tempVector"))
# model with complete data 
LogR_Model = glm(Loan_Status~., 
                 data = data_set,
                 family = binomial)
train_prediction =floor(predict(LogR_Model,data_set[-12],type ='response')+LR_Cutoff)
acurracy_table=addmargins(table(Actual =data_set$Loan_Status, Predicted = train_prediction))
acurracy_table
paste0("development accuracy in LR Model: ",round((acurracy_table[1,1]+acurracy_table[2,2])/acurracy_table[3,3]*100,2))
# test set prediction
test_prediction =floor(predict(LogR_Model,test_data_set,type ='response')+LR_Cutoff)
test_prediction = ifelse(test_prediction==1,"Y","N")
test_dataframe = data.frame(Loan_ID = test_data_set$Loan_ID,
                            Loan_Status = test_prediction)
write.csv(test_dataframe,'LR_Solution.csv',row.names = F)


### Final score : 78.472% -----
# > acurracy_table
# Predicted
# Actual   0   1 Sum
# 0    91 101 192
# 1    18 404 422
# Sum 109 505 614



