## Data Load ----
data_set = read.csv('HR_Employee_Attrition_Data.csv', header = T)

table(sapply(data_set,class))
factor_feildIDs=which(sapply(data_set, class) == "factor")
integer_feildIDs=which(sapply(data_set, class) == "integer")
## Data Split ----
library(caTools)
set.seed(123)
split_vector = sample.split(data_set$Attrition,SplitRatio = 0.7)
Dev_Original = subset(data_set, split_vector==T)
HoldOut_Original = subset(data_set, split_vector ==F)
Dev_data = Dev_Original
HoldOut_data = HoldOut_Original
## Visual representation of each variable contribution to Attrition ----
require(ggplot2)
# bar plots for all the factor variables
for( i in factor_feildIDs){
  name = paste(names(Dev_data[i]),'.png', sep = '')
  png(filename =paste0('plots/',name), width = 800, height = 600, units = 'px')
  print(ggplot( Dev_data,
                aes(x=Dev_data[,i],
                    fill =Dev_data$Attrition))+
          xlab(names(Dev_data[i]))+
          ylab('Frequency')+
          geom_bar(position = 'dodge')+
          guides(fill = guide_legend('Attrition')))
  dev.off()
  print.noquote(names(Dev_data[i]))
  print.noquote(summary(Dev_data[,i]))
}
# single category variable in categorical type variables 
for(i in factor_feildIDs){
  if(length(levels(Dev_data[,i]))==1){
    print.noquote(paste0(names(Dev_data[i]),' has only 1 category and its field id is ',i))
  }
}
# single valued variable in interger type
for( i in integer_feildIDs){
  if(max(Dev_data[,i])==min(Dev_data[,i])){
    print.noquote(paste0('All the values of ',names(Dev_data[i]),' are : ',
                         max(Dev_data[,i]),' and its ID is : ',i ))
  }
}
# correlation plots for all interger variables
require(gclus)
cor_int_data1 = Dev_data[c(head(integer_feildIDs,13))]
cor_int_data2 = Dev_data[c(tail(integer_feildIDs,13))]
png(filename = 'plots/Correlation_1.png', width = 1600, height = 1200, units = 'px')
cpairs(data = cor_int_data1,
       panel.colors = dmat.color(abs(cor(cor_int_data1))),
       gap =0.5)
dev.off()
png(filename = 'plots/Correlation_2.png', width = 1600, height = 1200, units = 'px')
cpairs(data = cor_int_data2,
       panel.colors = dmat.color(abs(cor(cor_int_data2))),
       gap =0.5)
dev.off() 
rm(list = c("cor_int_data1",
            "cor_int_data2","i","name"))
## Missing data Check -----
Missing_data_Check <- function(data_set){
  NA_Count = sapply(data_set,function(y) sum(length(which(is.na(y))))) 
  Null_Count = sapply(data_set,function(y) sum(length(which(is.null(y)))))
  Length0_Count = sapply(data_set,function(y) sum(length(which(length(y)==0))))
  Empty_Count = sapply(data_set,function(y) sum(length(which(y==''))))
  Total_NonData = NA_Count+Null_Count+Length0_Count+Empty_Count
  return( Total_NonData )
}

if(length(which(Missing_data_Check(Dev_data)>0))==0){
  print("No Missing data")
}else{
  which(Missing_data_Check(Dev_data)>0)
}

## Removing non-useful data ----
Dev_data = Dev_data[,-c(9,10,22,27)] # removing 'Over18','EmployeeCount','StandardHours','Employeenumber'
HoldOut_data = HoldOut_data[,-c(9,10,22,27)]
## Random Forset on Holdout data & Tunning  ----
library(randomForest)
# Dense Forest
set.seed(123)
RF_model = randomForest(x = Dev_data[-2],
                        y = Dev_data$Attrition,
                        data_set = Dev_data,
                        ntree =  5000,  # large number because we want to build as many trees as possible
                        mtry =  6,
                        nodesize = 40)
RF_model
plot(RF_model, main = "High number of trees vs OOB error")
# Adjusted forest based on previous plot
set.seed(123)
RF_model = randomForest(x = Dev_data[-2],
                        y = Dev_data$Attrition,
                        data_set = Dev_data,
                        ntree =  500,
                        mtry =  6,
                        nodesize = 40)
RF_model
plot(RF_model, main = "Adjusted tree number vs OOB error")
# Adjusting mtry - 2 techinques - tureRF and gridsearch
set.seed(321)
tuned_RF_model = tuneRF(x = Dev_data[-2],
                        y = Dev_data$Attrition,
                        data_set = Dev_data,
                        mtryStart = 2,
                        stepFactor = 1.5,
                        improve = 0.001,
                        ntreeTry = 500,
                        nodesize = 40,
                        doBest = T, trace = T, plot = T,importance = T)
tuned_RF_model #tune_RF - 19
library(caret)
set.seed(123)
grid_RF_tune = train(x = Dev_data[-2],
                     y = Dev_data$Attrition,
                     method = 'rf')
grid_RF_tune #gridsearch gives 16

temp_dev_acc = numeric()
temp_nodesize = numeric()
temp_hold_acc = numeric()
j = 1
for (i in seq(from = 1, to = 90 , by = 3)){
  set.seed(123)
  RF_model = randomForest(x = Dev_data[-2],
                          y = Dev_data$Attrition,
                          data_set = Dev_data,
                          ntree =  500,
                          mtry =  19,
                          nodesize = i)
  if(RF_model$confusion[1]!=2058 & RF_model$confusion[2]!=2058){
    temp_dev_acc[j] = (RF_model$confusion[1]+RF_model$confusion[4])/2058
    temp_nodesize[j] = i
    holdout_prediction = predict(RF_model, newdata = HoldOut_data, type ='class')
    table = table(HoldOut_data$Attrition,holdout_prediction)
    temp_hold_acc[j] = (table[1]+table[4])/882
    j = j+1
  }
}
nodesize_df = data.frame(x=temp_nodesize,y1=temp_dev_acc,y2=temp_hold_acc)
library(ggplot2)
#png('plots/node_size.png',width = 800, height = 600, units = 'px')
ggplot() +
  geom_line(data = nodesize_df,
            aes(nodesize_df$x,nodesize_df$y1),col = 'red')+
  # geom_line(data = nodesize_df,
  #           aes(nodesize_df$x,nodesize_df$y2),col = 'green')+
  xlab('node size')+
  ylab('accuracy')+
  ggtitle('ntree is 500 & mtry is 19')
#dev.off()
# final Random forest ------
set.seed(123)
RF_model = randomForest(x = Dev_data[-2],
                        y = Dev_data$Attrition,
                        data_set = Dev_data,
                        ntree =  500,
                        mtry =  19,
                        nodesize = 2)
RF_model
plot(RF_model)
rm(list = c("nodesize_df","holdout_prediction","i","j",
            "temp_nodesize","temp_dev_acc","temp_hold_acc","table"))
Dev_Original$RF_Prob = predict(RF_model,newdata = Dev_data, type='prob')
Dev_Original$RF_Prob =Dev_Original$RF_Prob[,2]
Holdout_prediction_Prob = predict(RF_model,newdata = HoldOut_data, type='prob')
Holdout_prediction_class =predict(RF_model,newdata = HoldOut_data, type='class')
HoldOut_data$RF_prob =Holdout_prediction_Prob[,2]
HoldOut_data$RF_class = Holdout_prediction_class
HoldOut_Original$RF_prob = HoldOut_data$RF_prob
HoldOut_Original$RF_Class = Holdout_prediction_class

# Classification 
Confusion_Matrix_RF=addmargins(table(actual = HoldOut_data$Attrition, Prediction = HoldOut_data$RF_class))
Confusion_Matrix_RF
Accuracy_RF=(Confusion_Matrix_RF[1]+Confusion_Matrix_RF[5])/Confusion_Matrix_RF[9]*100
Accuracy_RF #96.82 %

# KS Ranking 
decile <- function(x){
  deciles = vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
    ifelse(x<deciles[2], 2,
    ifelse(x<deciles[3], 3,
    ifelse(x<deciles[4], 4,
    ifelse(x<deciles[5], 5,
    ifelse(x<deciles[6], 6,
    ifelse(x<deciles[7], 7,
    ifelse(x<deciles[8], 8,
    ifelse(x<deciles[9], 9, 10 ))))))))))
}
HoldOut_data$decile = decile(Holdout_prediction_Prob[,2])
require(data.table)
require(scales)
HoldOut_data$Attrition_Numeric = ifelse(HoldOut_data$Attrition=="No",0,1)

Ranking <-function(tmp_DT){
  rank = tmp_DT[, list(
    cnt = length(Attrition_Numeric), 
    cnt_resp = sum(Attrition_Numeric), 
    cnt_non_resp = sum(Attrition_Numeric == 0)) , 
    by=decile][order(-decile)]
  rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
  rank$cum_resp <- cumsum(rank$cnt_resp)
  rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
  rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
  rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
  rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
  rank$rrate <- percent(rank$rrate)
  rank$cum_rel_resp <- percent(rank$cum_rel_resp)
  rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)
  
  return(rank)
}

Holdout_RF_Ranking = Ranking(data.table(HoldOut_data))

Dev_data$Attrition_Numeric = ifelse(Dev_data$Attrition=="No",0,1)
Dev_data_Prob = predict(RF_model,newdata = Dev_data, type='prob')
Dev_data_class =predict(RF_model,newdata = Dev_data, type='class')
Dev_data$RF_prob =Dev_data_Prob[,2]
Dev_data$decile = decile(Dev_data_Prob[,2])
Dev_data$RF_class = Dev_data_class
Dev_RF_Ranking = Ranking(data.table((Dev_data)))

View(Holdout_RF_Ranking)
View(Dev_RF_Ranking)
hist(HoldOut_data$RF_prob)
hist(Dev_data$RF_prob)
rm(list = c("Dev_data_Prob","Holdout_prediction_Prob","Dev_data_class",
            "Holdout_prediction_class"))

## NueralNet -----
Integer_dataset= data_set[,c(integer_feildIDs,factor_feildIDs)]
Integer_dataset = Integer_dataset[,-c(5,6,18,34)] # removing 'Over18','EmployeeCount','EmployeeNumber','StandardHours'

temp <- data.frame(model.matrix(~ BusinessTravel - 1, data = Integer_dataset))
Integer_dataset <- data.frame(Integer_dataset, temp[-1])
temp <- data.frame(model.matrix(~ Department - 1, data = Integer_dataset))
Integer_dataset <- data.frame(Integer_dataset, temp[-1])
temp <- data.frame(model.matrix(~ EducationField - 1, data = Integer_dataset))
Integer_dataset <- data.frame(Integer_dataset, temp[-1])
temp <- data.frame(model.matrix(~ Gender - 1, data = Integer_dataset))
Integer_dataset <- data.frame(Integer_dataset, temp[-1])
temp <- data.frame(model.matrix(~ JobRole - 1, data = Integer_dataset))
Integer_dataset <- data.frame(Integer_dataset, temp[-1])
temp <- data.frame(model.matrix(~ MaritalStatus - 1, data = Integer_dataset))
Integer_dataset <- data.frame(Integer_dataset, temp[-1])
temp <- data.frame(model.matrix(~ OverTime - 1, data = Integer_dataset))
Integer_dataset <- data.frame(Integer_dataset, temp[-1])
names(Integer_dataset)

Integer_dataset =Integer_dataset[,-c(25:31)]
names(Integer_dataset)


Integer_dataset$Attrition = as.numeric(as.character(factor(Integer_dataset$Attrition,
                                                           levels = c("No","Yes"),
                                                           labels = c(0,1))))
which(sapply(Integer_dataset, class) == "factor")
require(caret)
set.seed(123)
split_vector = sample.split(Integer_dataset$Attrition,SplitRatio = 0.7)
dev_data_int = subset(Integer_dataset, split_vector ==T)
hold_data_int = subset(Integer_dataset, split_vector ==F)


for(i in 1:45){
  if(i!=24){
    dev_data_int[i] = scale(dev_data_int[i])
  }
}

require(neuralnet)
n = names(dev_data_int)
long_formula = as.formula(paste("Attrition ~", paste(n[!n %in% "Attrition"], collapse = " + ")))
set.seed(123)
NN_model_int = neuralnet(formula = long_formula,
                         data = dev_data_int,
                         hidden = 10,
                         err.fct = "sse",
                         linear.output = FALSE,
                         lifesign = "full",
                         lifesign.step = 1,
                         threshold = 0.01,
                         stepmax = 4000)
plot(NN_model_int)
dev_NN_pred = NN_model_int$net.result[[1]]
dev_df = data.frame(dev_NN_pred,dev_data_int$Attrition)
Dev_Original$NN_prob =dev_NN_pred
dev_NN_pred = ifelse(dev_NN_pred>=0.5,1,0)
table = addmargins(table(dev_data_int$Attrition,dev_NN_pred)) # ~98.4%
table
dev_NN_acc = (table[1]+table[5])/table[9]
dev_NN_acc

for(i in 1:45){
  if(i!=24){
    hold_data_int[i] = scale(hold_data_int[i])
  }
}

holdout_NN_pred = compute(NN_model_int,hold_data_int[,-24])
holdout_NN_pred = holdout_NN_pred$net.result
hold_df = data.frame(holdout_NN_pred,hold_data_int$Attrition)
HoldOut_Original$NN_Pred = holdout_NN_pred
holdout_NN_pred = ifelse(holdout_NN_pred>=0.5,1,0)
table =addmargins(table(hold_data_int$Attrition,holdout_NN_pred)) # ~94.5
table
hold_NN_acc = (table[1]+table[5])/table[9]
hold_NN_acc

# Ensembling  -------
Dev_Original$Attrition = ifelse(Dev_Original$Attrition=="No",0,1)
Dev_Original$RF_Class = ifelse(Dev_data$RF_class=="No",0,1)
Dev_Original$NN_Class = dev_NN_pred

Dev_Original$Ensemble_prod = (Dev_Original$RF_Prob + Dev_Original$NN_prob)/2
Dev_Original$Class = ifelse(Dev_Original$Ensemble_prod>=0.3,1,0)

table =addmargins(table(Dev_Original$Attrition,Dev_Original$Class)) # 99.8 %
table
dev_Ensemble_acc = (table[1]+table[5])/table[9]
dev_Ensemble_acc

HoldOut_Original$Attrition = ifelse(HoldOut_Original$Attrition=="No",0,1)
HoldOut_Original$RF_Class = ifelse(HoldOut_Original$RF_Class=="No",0,1)
HoldOut_Original$NN_Class = holdout_NN_pred

HoldOut_Original$Ensemble_prod = (HoldOut_Original$RF_prob+HoldOut_Original$NN_Pred)/2
HoldOut_Original$Class = ifelse(HoldOut_Original$Ensemble_prod>=0.3,1,0)

table =addmargins(table(HoldOut_Original$Attrition,HoldOut_Original$Class)) # ~96.03
table
hold_Ensemble_acc = (table[1]+table[5])/table[9]
hold_Ensemble_acc

