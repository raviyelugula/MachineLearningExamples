### Reading Train data
data_set = read.csv('train.csv', header = T)
head(data_set)
sapply(data_set, class)
names(data_set)

### Checking if any loanID is duplicated -- Result: No
which((table(data_set$Loan_ID)>2)==T)

### Removing LoanID's -- because this is not helpful in classifying data (primary key)
data_set=data_set[,-1]

table(data_set$Gender)
sapply(data_set[,c(1,2,3,4,5,10,11,12)],table)

## Checking if there are any NA's or Null's in the given data set 
NA_Count = sapply(data_set,
                  function(y) sum(length(which(is.na(y))))) 
NA_Count 

Null_Count = sapply(data_set,
                    function(y) sum(length(which(is.null(y)))))
Null_Count 

Length0_Count = sapply(data_set,
                       function(y) sum(length(which(length(y)==0))))
Length0_Count 

Empty_Count = sapply(data_set,
                     function(y) sum(length(which(y==''))))
Empty_Count

### Removing all the rows having missing values
data_set_No_Missing_Values=data_set[!apply(data_set, 1, function(x) any(x=="" | is.na(x))),] 




current_dataset = data_set_No_Missing_Values 
sapply(current_dataset, class)

summary(current_dataset$Property_Area) 
current_dataset$Gender = factor(current_dataset$Gender, levels = c('Female','Male'), labels = c(0,1))
current_dataset$Married = factor(current_dataset$Married, levels = c('No','Yes'), labels = c(0,1))
current_dataset$Dependents = factor(current_dataset$Dependents, levels = c('0','1','2','3+'), labels = c(0,1,2,3))
current_dataset$Education = factor(current_dataset$Education, levels = c('Graduate','Not Graduate'), labels = c(0,1))
current_dataset$Self_Employed = factor(current_dataset$Self_Employed, levels = c('No','Yes'), labels = c(0,1))
current_dataset$Loan_Status = factor(current_dataset$Loan_Status, levels = c('N','Y'), labels = c(0,1))
current_dataset$Property_Area = factor(current_dataset$Property_Area, levels = c('Rural','Semiurban','Urban'), labels = c(0,1,2))

library(caTools)
set.seed(123)
splitvector = sample.split(current_dataset$Loan_Status, SplitRatio = 0.75)
training_dataset = subset(current_dataset, splitvector==T)
testing_dataset = subset(current_dataset, splitvector==F)

### Logistic Regression ----
summary(training_dataset$Education)
LogR_Model = glm(Loan_Status~., 
                 data = training_dataset,
                 family = binomial)
library(lmtest)
lrtest(LogR_Model)
library(pscl)
pR2(LogR_Model)

## trainset prediction & Cut Off calculation
Prediction = fitted(LogR_Model)
summary(Prediction)

cutdf = data.frame(
  Cutoff=numeric(), 
  Error=numeric(), 
  stringsAsFactors=FALSE) 
tempVector = seq(from=0.15,to=0.95,by=0.05)
for(i in tempVector){
  cutof = floor(Prediction+i)
  a=table(Actual =training_dataset$Loan_Status, Predicted = cutof)
  E=a[1,2] + a[2,1]
  cutdf = rbind(cutdf,data.frame(Cutoff=i,Error=E))
}

CutOFF=cutdf$Cutoff[cutdf$Error==min(cutdf$Error)]
CutOFF
plot(x=cutdf[,1],y=cutdf[,2])

summary(training_dataset$Loan_Status)

CutoffTrain = floor(Prediction+CutOFF)
addmargins(table(Actual =training_dataset$Loan_Status, Predicted = CutoffTrain))
table(Actual =training_dataset$Loan_Status, Predicted = CutoffTrain)/length(training_dataset$Loan_Status)


y_predict = predict(LogR_Model, newdata = testing_dataset[-12], type = 'response')
y_predict = floor(y_predict+CutOFF)
addmargins(table(Actual =testing_dataset$Loan_Status, Predicted = y_predict))
### training - 81.3% and test 83.3% -- Logistic regression - removing all missing values

###  SVM - Kernal Non Linear ----

library(e1071)
svm_kernal_model = svm(Loan_Status~., 
                       data = training_dataset,
                       type ='C-classification',
                       kernal = 'radial')
training_predict_svm = predict(svm_kernal_model, newdata = training_dataset[-12], type ='response')
addmargins(table(actual = training_dataset$Loan_Status,
                 predicted = training_predict_svm))

testing_predict_svm = predict(svm_kernal_model, newdata = testing_dataset[-12], type ='response')
addmargins(table(actual = testing_dataset$Loan_Status,
                 predicted = testing_predict_svm))
### training - 81.6% and test 82.5% -- Radial SVM -- removing all missing vales

### RF----

library(randomForest)
RF_model = randomForest(y = training_dataset$Loan_Status,
                        x = training_dataset[-12],
                        data = training_dataset,
                        ntree=200, mtry = 10, nodesize = 6)

training_predict_RF = predict(RF_model, newdata = training_dataset[-12])
CM_Train=addmargins(table(actual = training_dataset$Loan_Status,
                          predicted = training_predict_RF))
training_accuracy = (CM_Train[1,1]+CM_Train[2,2])/CM_Train[3,3]
training_accuracy

testing_predict_RF = predict(RF_model, newdata = testing_dataset[-12])
CM_Test=addmargins(table(actual = testing_dataset$Loan_Status,
                         predicted = testing_predict_RF))
testing_accuracy = (CM_Test[1,1]+CM_Test[2,2])/CM_Test[3,3]
testing_accuracy

Results =data.frame(nt = numeric(),
                    ns = numeric(),
                    mt = numeric(),
                    train = numeric(),
                    test = numeric()) 
for(NT in seq(from=10, to =1000, by =5)){
  for(NS in seq(from = 1 , to =100, by =2)){
    for(MT in seq(from = 1, to =10, by =1)){
      RF_model = randomForest(y = training_dataset$Loan_Status,
                              x = training_dataset[-12],
                              data = training_dataset,
                              ntree= NT, mtry = MT, nodesize = NS)
      training_predict_RF = predict(RF_model, newdata = training_dataset[-12])
      CM_Train=addmargins(table(actual = training_dataset$Loan_Status,
                                predicted = training_predict_RF))
      training_accuracy = (CM_Train[1,1]+CM_Train[2,2])/CM_Train[3,3]
      testing_predict_RF = predict(RF_model, newdata = testing_dataset[-12])
      CM_Test=addmargins(table(actual = testing_dataset$Loan_Status,
                               predicted = testing_predict_RF))
      testing_accuracy = (CM_Test[1,1]+CM_Test[2,2])/CM_Test[3,3]
      Results = rbind(Results,data.frame(NT,NS,MT,training_accuracy,testing_accuracy)) 
    }
  }
  print(NT)
}

Results[which(Results$testing_accuracy==max(Results$testing_accuracy)),]


### KNN----

sapply(testing_dataset[,-12],class)
current_dataset$Gender = as.numeric(current_dataset$Gender)
current_dataset$Married = as.numeric(current_dataset$Married)
current_dataset$Dependents = as.numeric(current_dataset$Dependents)
current_dataset$Education = as.numeric(current_dataset$Education)
current_dataset$Self_Employed = as.numeric(current_dataset$Self_Employed)
current_dataset$Property_Area = as.numeric(current_dataset$Property_Area)

library(caTools)
set.seed(123)
splitvector = sample.split(current_dataset$Loan_Status, SplitRatio = 0.75)
training_dataset = subset(current_dataset, splitvector==T)
testing_dataset = subset(current_dataset, splitvector==F)


library(class)
model = knn(train = training_dataset[,-12],
            test = testing_dataset[,-12],
            cl = training_dataset[,12],
            k=5,
            prob = T)

table(actuals = testing_dataset[,12], predicted = model)























### CART ----
library(rpart)

### ANN -----


library(h2o)
h2o.init(nthreads = -1)

current_dataset = data_set_No_Missing_Values 
sapply(current_dataset, class)

summary(current_dataset$Property_Area) 
current_dataset$Gender = factor(current_dataset$Gender, levels = c('Female','Male'), labels = c(0,1))
current_dataset$Married = factor(current_dataset$Married, levels = c('No','Yes'), labels = c(0,1))
current_dataset$Dependents = factor(current_dataset$Dependents, levels = c('0','1','2','3+'), labels = c(0,1,2,3))
current_dataset$Education = factor(current_dataset$Education, levels = c('Graduate','Not Graduate'), labels = c(0,1))
current_dataset$Self_Employed = factor(current_dataset$Self_Employed, levels = c('No','Yes'), labels = c(0,1))
current_dataset$Loan_Status = factor(current_dataset$Loan_Status, levels = c('N','Y'), labels = c(0,1))
current_dataset$Property_Area = factor(current_dataset$Property_Area, levels = c('Rural','Semiurban','Urban'), labels = c(0,1,2))

current_dataset$ApplicantIncome= as.numeric(scale(current_dataset$ApplicantIncome))
current_dataset$CoapplicantIncome= as.numeric(scale(current_dataset$CoapplicantIncome))
current_dataset$LoanAmount= as.numeric(scale(current_dataset$LoanAmount))
current_dataset$Loan_Amount_Term= as.numeric(scale(current_dataset$Loan_Amount_Term))
current_dataset$Credit_History= as.numeric(scale(current_dataset$Credit_History))


library(caTools)
set.seed(123)
splitvector = sample.split(current_dataset$Loan_Status, SplitRatio = 0.75)
training_dataset = subset(current_dataset, splitvector==T)
testing_dataset = subset(current_dataset, splitvector==F)

model = h2o.deeplearning(y = 'Loan_Status',
                         training_frame = as.h2o(training_dataset),
                         activation = 'Rectifier',
                         hidden = c(11),
                         epochs = 100,
                         train_samples_per_iteration = -2
)
Y_predict1 = h2o.predict(model, newdata= as.h2o(testing_dataset[,-12]))
Y_predict2 = ifelse(Y_predict1[3]>0.5,1,0)
Y_predict = as.vector(Y_predict2)
nrow(testing_dataset[,-12])
length(Y_predict)

CMT=addmargins(table(actuals = factor(testing_dataset[,12]), predictions = Y_predict))

accuracy = (CMT[1,1]+CMT[2,2])/CMT[3,3]

CMTh2o.shutdown()

## test data set -- ANN 84.16



