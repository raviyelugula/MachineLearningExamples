require(dplyr)
require(ggplot2)
require(readxl)

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





