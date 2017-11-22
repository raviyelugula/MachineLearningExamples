RF_solution = read.csv('RF_Solution.csv',header =T)
LR_Solution = read.csv('NN_Solution.csv',header = T)
SVM_Solution = read.csv('SVM_Solution.csv',header = T)

Ensemble_Solution = cbind(RF_solution,
                          LR_S=LR_Solution$Loan_Status,
                          SVM_S=SVM_Solution$Loan_Status)
names(Ensemble_Solution)= c("Loan_ID","RF_S","LR_S","SVM_S")
Ensemble_Solution$RF_S = ifelse(Ensemble_Solution$RF_S == "Y",1,0)
Ensemble_Solution$LR_S = ifelse(Ensemble_Solution$LR_S == "Y",1,0)
Ensemble_Solution$SVM_S = ifelse(Ensemble_Solution$SVM_S == "Y",1,0)
Ensemble_Solution$Total = Ensemble_Solution$RF_S+Ensemble_Solution$LR_S+Ensemble_Solution$SVM_S
Ensemble_Solution$Loan_Status = ifelse(Ensemble_Solution$Total >=2,"Y","N")
write.csv(Ensemble_Solution[,c(1,6)],'Ensemble_Solution.csv',row.names = F)

### Final score :0.77778 ------


