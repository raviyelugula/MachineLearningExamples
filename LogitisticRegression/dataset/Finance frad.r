setwd("C:/Users/ravindranadh.y/Downloads/off campus assignment")
require(readxl)
excel_sheets('training.xlsx')
train_data = read_excel(path = 'training.xlsx',sheet = 'training')
train_data = train_data[-1]
train_data1 = train_data
train_data1$NumberOfDependents = ifelse(train_data1$NumberOfDependents == 'NA',
                                        NA,
                                        train_data1$NumberOfDependents)
train_data1$NumberOfDependents = as.numeric(train_data1$NumberOfDependents)
sapply(train_data1,class)
names(train_data1)
length(which(train_data1$DebtRatio > 1))
length(which(train_data1$RevolvingUtilizationOfUnsecuredLines > 1))
length(which(train_data1$DebtRatio > 1 | train_data1$RevolvingUtilizationOfUnsecuredLines > 1))

boxplot(train_data1$DebtRatio)
boxplot(train_data1$RevolvingUtilizationOfUnsecuredLines)

train_data_outLier=subset(train_data1,train_data1$DebtRatio > 1 | train_data1$RevolvingUtilizationOfUnsecuredLines > 1)
train_data_In=subset(train_data1,train_data1$DebtRatio <= 1 & train_data1$RevolvingUtilizationOfUnsecuredLines <= 1)
sum(train_data_outLier$SeriousDlqin2yrs)
length(train_data_outLier$SeriousDlqin2yrs)
sum(train_data_outLier$SeriousDlqin2yrs)/length(train_data_outLier$SeriousDlqin2yrs)*100

sum(train_data_In$SeriousDlqin2yrs)
length(train_data_In$SeriousDlqin2yrs)
sum(train_data_In$SeriousDlqin2yrs)/length(train_data_In$SeriousDlqin2yrs)*100

boxplot(train_data_In$DebtRatio)

sapply(train_data_In,class)
train_data_In$SeriousDlqin2yrs = as.factor(train_data_In$SeriousDlqin2yrs)



model = glm(SeriousDlqin2yrs~.,data = train_data_In,
            family = binomial)
model$fitted.values


