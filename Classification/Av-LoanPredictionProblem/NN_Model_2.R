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
# Feature engineering
sapply(data_set,class)
sapply(test_data_set,class)
data_set$ApplicantIncome = ifelse(data_set$ApplicantIncome <=2876, "low",
                                  ifelse(data_set$ApplicantIncome <= 3813, "medium",
                                         ifelse(data_set$ApplicantIncome <= 5795 ,"high",
                                                ifelse(data_set$ApplicantIncome <= 81000,"very_high","Error"))))
data_set$ApplicantIncome = factor(data_set$ApplicantIncome, 
                                  levels = c("low","medium","high","very_high"),
                                  labels = c(0,1,2,3))
data_set$CoapplicantIncome = ifelse(data_set$CoapplicantIncome <= 0, "low",
                                    ifelse(data_set$CoapplicantIncome <= 1189, "medium",
                                           ifelse(data_set$CoapplicantIncome <= 2297, "high",
                                                  ifelse(data_set$CoapplicantIncome <= 41667,"very_high","Error"))))
data_set$CoapplicantIncome = factor(data_set$CoapplicantIncome, 
                                    levels = c("low","medium","high","very_high"),
                                    labels = c(0,1,2,3))
test_data_set$ApplicantIncome = ifelse(test_data_set$ApplicantIncome <=2876, "low",
                                       ifelse(test_data_set$ApplicantIncome <= 3813, "medium",
                                              ifelse(test_data_set$ApplicantIncome <= 5795 ,"high",
                                                     ifelse(test_data_set$ApplicantIncome <= 81000,"very_high","Error"))))
test_data_set$ApplicantIncome = factor(test_data_set$ApplicantIncome, 
                                       levels = c("low","medium","high","very_high"),
                                       labels = c(0,1,2,3))
test_data_set$CoapplicantIncome = ifelse(test_data_set$CoapplicantIncome <= 0, "low",
                                         ifelse(test_data_set$CoapplicantIncome <= 1189, "medium",
                                                ifelse(test_data_set$CoapplicantIncome <= 2297, "high",
                                                       ifelse(test_data_set$CoapplicantIncome <= 41667,"very_high","Error"))))
test_data_set$CoapplicantIncome = factor(test_data_set$CoapplicantIncome, 
                                         levels = c("low","medium","high","very_high"),
                                         labels = c(0,1,2,3))
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
temp = data.frame(model.matrix(~ ApplicantIncome - 1, data = data_set))
data_set = data.frame(data_set, temp[-1])
data_set = data_set[-1]
temp = data.frame(model.matrix(~ CoapplicantIncome - 1, data = data_set))
data_set = data.frame(data_set, temp[-1])
data_set = data_set[-1]
temp = data.frame(model.matrix(~ Credit_History - 1, data = data_set))
data_set = data.frame(data_set, temp[-1])
data_set = data_set[-3]
temp = data.frame(model.matrix(~ Property_Area - 1, data = data_set))
data_set = data.frame(data_set, temp[-1])
data_set = data_set[-3]
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
temp = data.frame(model.matrix(~ ApplicantIncome - 1, data = test_data_set))
test_data_set = data.frame(test_data_set, temp[-1])
test_data_set = test_data_set[-2]
temp = data.frame(model.matrix(~ CoapplicantIncome - 1, data = test_data_set))
test_data_set = data.frame(test_data_set, temp[-1])
test_data_set = test_data_set[-2]
temp = data.frame(model.matrix(~ Credit_History - 1, data = test_data_set))
test_data_set = data.frame(test_data_set, temp[-1])
test_data_set = test_data_set[-4]
temp = data.frame(model.matrix(~ Property_Area - 1, data = test_data_set))
test_data_set = data.frame(test_data_set, temp[-1])
test_data_set = test_data_set[-4]
# Scaling all elements
for(i in 1:19){
  if(i!=3){
    data_set[i] = scale(data_set[i])
  }
}

for(i in 1:19){
  if(i!=1){
    test_data_set[i] = scale(test_data_set[i])
  }
}
# removing temp objects
rm(list = c("i","temp"))
### Spliting the data set into dev and holdout ------
require(caTools)
set.seed(123)
splitvector = sample.split(data_set$Loan_Status, SplitRatio = 0.75)
dev_dataset = subset(data_set, splitvector==T)
holdout_dataset = subset(data_set, splitvector==F)
# removing tmep variables
rm(splitvector)
### Nueral Network Model ----
require(neuralnet)
n = names(dev_dataset)
long_formula = as.formula(paste("Loan_Status ~", paste(n[!n %in% "Loan_Status"], collapse = " + ")))
set.seed(123)
NN_Model = neuralnet(formula = long_formula,
                     data = dev_dataset,
                     hidden = 20,
                     err.fct = "sse",
                     linear.output = FALSE,
                     lifesign = "full",
                     lifesign.step = 1,
                     threshold = 0.01,
                     stepmax = 20000)
plot(NN_Model)
dev_prediction = NN_Model$net.result[[1]]
dev_prediction_cutoff_dataframe = data.frame(dev_prediction,dev_dataset$Loan_Status)
dev_prediction = ifelse(dev_prediction>=0.5,1,0)
acurracy_table=addmargins(table(dev_dataset$Loan_Status,dev_prediction)) 
acurracy_table
acurracy = (acurracy_table[1]+acurracy_table[5])/acurracy_table[9]
acurracy # 98.26%
# holding data 
hold_prediction = compute(NN_Model,holdout_dataset[-6])
hold_prediction = hold_prediction$net.result
hold_prediction_cutoff_dataframe = data.frame(hold_prediction,holdout_dataset$Loan_Status)
hold_prediction = ifelse(hold_prediction>=0.5,1,0)
acurracy_table=addmargins(table(holdout_dataset$Loan_Status,hold_prediction)) 
acurracy_table
acurracy = (acurracy_table[1]+acurracy_table[5])/acurracy_table[9]
acurracy # 75.97%

# complete data NN model
set.seed(123)
NN_Model = neuralnet(formula = long_formula,
                     data = data_set,
                     hidden = 20,
                     err.fct = "sse",
                     linear.output = FALSE,
                     lifesign = "full",
                     lifesign.step = 1,
                     threshold = 0.01,
                     stepmax = 20000)
# testing data 
test_prediction = compute(NN_Model,test_data_set[-1])
test_prediction = test_prediction$net.result
test_prediction = ifelse(test_prediction>=0.5,1,0)
test_prediction = ifelse(test_prediction==1,"Y","N")
test_dataframe = data.frame(Loan_ID = test_data_set$Loan_ID,Loan_Status = test_prediction)
write.csv(test_dataframe,'NN_Solution_2.csv',row.names = F)

### Final score : 65.972% -----


















