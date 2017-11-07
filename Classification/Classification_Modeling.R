require(readxl) ## read Excel Files
require(dplyr) ## data manupulation
require(usdm) ## VIF 
require(ggplot2) ## Visualization
require(caTools) ## split
require(class) ## KNN
require(DMwR) ## SMOTE

# reading the data
excel_sheets(path = 'training.xlsx')
traindata = read_excel(path = 'training.xlsx', sheet = 'training')

# rename cols, new features, data type adjustments
colnames(traindata) = c('RowID','DLQs','Utlz_UnsecLines','DebtRatio',
                        'Credit_Loans','Dependents')
traindata = traindata %>% 
            dplyr::select(DLQs,Utlz_UnsecLines,DebtRatio,Credit_Loans,Dependents) 
traindata$UUL_flag = ifelse(traindata$Utlz_UnsecLines>1,1,0)
traindata$DR_flag = ifelse(traindata$DebtRatio>1,1,0)
sapply(traindata,class)
traindata$Dependents = ifelse(traindata$Dependents =='NA',NA,traindata$Dependents)
traindata$Dependents = as.numeric(traindata$Dependents)
traindata[,c(1,6,7)] = data.frame(lapply(traindata[,c(1,6,7)],as.factor))

# spliting into two kinds outliers and normal data
T1_traindata = subset(traindata, UUL_flag == 1 | DR_flag == 1)
T2_traindata = subset(traindata, UUL_flag == 0 & DR_flag == 0)

# Missing data check
Missing_data_Check <- function(data_set){
  NA_Count = sapply(data_set,function(y) sum(length(which(is.na(y))))) 
  Null_Count = sapply(data_set,function(y) sum(length(which(is.null(y)))))
  Length0_Count = sapply(data_set,function(y) sum(length(which(length(y)==0))))
  Empty_Count = sapply(data_set,function(y) sum(length(which(y==''))))
  Total_NonData = NA_Count+Null_Count+Length0_Count+Empty_Count
  return( Total_NonData )
}
Missing_data_Check(T2_traindata)
Missing_data_Check(T1_traindata)

# Handling missing data 
vif(data.frame(T2_traindata[,c(2:4)]))
T2_traindata_C = subset(T2_traindata,!is.na(Dependents))
T2_traindata_M = subset(T2_traindata,is.na(Dependents))

set.seed(123)
split = sample.split(T2_traindata_C$Dependents, SplitRatio = 0.75)
T2_traindata_C_Tr = subset(T2_traindata_C, split == TRUE)
T2_traindata_C_Te = subset(T2_traindata_C, split == FALSE)
dependents = knn(train = scale(T2_traindata_C_Tr[,c(2,3,4)]),
             test = scale(T2_traindata_C_Te[,c(2,3,4)]),
             cl = as.factor(T2_traindata_C_Tr$Dependents),
             k = 9,
             prob = F)
print(length(which(T2_traindata_C_Te$Dependents == dependents))/length(dependents))
model = lm(Dependents~Utlz_UnsecLines+DebtRatio+Credit_Loans,
           data=T2_traindata_C_Tr)
summary(model)
dependents = round(predict(model,T2_traindata_C_Te)) ## LR is not working as it gives all as 1
rm(list = c('T2_traindata_C_Te','T2_traindata_C_Tr'))

dependents = knn(train = scale(T2_traindata_C[,c(2,3,4)]),
                 test = scale(T2_traindata_M[,c(2,3,4)]),
                 cl = as.factor(T2_traindata_C$Dependents),
                 k = 9,
                 prob = F)
T2_traindata_M$Dependents = dependents
T2_traindata = rbind(T2_traindata_C,T2_traindata_M)
rm(list = c('T2_traindata_C','T2_traindata_M'))
Missing_data_Check(T2_traindata)

vif(data.frame(T1_traindata[,c(2:4)]))
T1_traindata_C = subset(T1_traindata,!is.na(Dependents))
T1_traindata_M = subset(T1_traindata,is.na(Dependents))

set.seed(123)
split = sample.split(T1_traindata_C$Dependents, SplitRatio = 0.75)
T1_traindata_C_Tr = subset(T1_traindata_C, split == TRUE)
T1_traindata_C_Te = subset(T1_traindata_C, split == FALSE)
dependents = knn(train = scale(T1_traindata_C_Tr[,c(2,3,4)]),
                 test = scale(T1_traindata_C_Te[,c(2,3,4)]),
                 cl = as.factor(T1_traindata_C_Tr$Dependents),
                 k = 9,
                 prob = F)
print(length(which(T1_traindata_C_Te$Dependents == dependents))/length(dependents))
model = lm(Dependents~Utlz_UnsecLines+DebtRatio+Credit_Loans,
           data=T1_traindata_C_Tr)
summary(model)
dependents = round(predict(model,T1_traindata_C_Te))
print(length(which(T1_traindata_C_Te$Dependents == dependents))/length(dependents))
rm(list = c('T1_traindata_C_Te','T1_traindata_C_Tr'))

dependents = knn(train = scale(T1_traindata_C[,c(2,3,4)]),
                 test = scale(T1_traindata_M[,c(2,3,4)]),
                 cl = as.factor(T1_traindata_C$Dependents),
                 k = 9,
                 prob = F)
T1_traindata_M$Dependents = dependents
T1_traindata = rbind(T1_traindata_C,T1_traindata_M)
rm(list = c('T1_traindata_C','T1_traindata_M'))
Missing_data_Check(T1_traindata)

# Model building for T2 - 94:6 target varibale
T2_traindata = T2_traindata[,1:5]
T2_traindata$Dependents = as.numeric(T2_traindata$Dependents)

ggplot(data = T2_traindata)+
  geom_point(aes(x = Utlz_UnsecLines, y = DebtRatio,
                 #shape = as.factor(Dependents), size = Credit_Loans, 
                 color = DLQs))

T2_traindata_SMOTE = SMOTE(DLQs~Utlz_UnsecLines+DebtRatio+
                             Credit_Loans+Dependents,as.data.frame(T2_traindata),
                           perc.over = 600,perc.under = 300)
table(T2_traindata$DLQs)
table(T2_traindata_SMOTE$DLQs)
table(T2_traindata_SMOTE$DLQs)/length(T2_traindata_SMOTE$DLQs)

ggplot(data = T2_traindata_SMOTE)+
  geom_point(aes(x = Utlz_UnsecLines, y = DebtRatio,
                 #shape = as.factor(Dependents), size = Credit_Loans, 
                 color = DLQs))





