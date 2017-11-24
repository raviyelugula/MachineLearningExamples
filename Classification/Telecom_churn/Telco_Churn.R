require(dplyr)
require(ggplot2)
require(readxl)
require(dummies)
require(randomForest)
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

# for( i in factor_col_names[1:16]){
#   print(ggplot(data_factors,
#                 aes(x=i,
#                     fill =Churn))+
#           xlab(names(data_factors[i]))+
#           ylab('Frequency')+
#           geom_bar(position = 'dodge')+
#           guides(fill = guide_legend('Churn')))
# }

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
mtry_opt_value = train(form = Churn~ ., 
                       data = data_factors, method = 'rf')
RF_model = randomForest(Churn ~ ., 
                        data = data_factors, 
                        ntree=200, mtry = 5, nodesize = 20,
                        importance=TRUE)

featureImp_df = RF_model$importance
colnames(featureImp_df)
featureImp_df[order(featureImp_df[,4]),]





