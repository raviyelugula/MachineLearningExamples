
### Reading Train data
data_set = read.csv('train.csv', header = T, stringsAsFactors = F)
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
NA_Count = sapply(data_set, function(y) sum(length(which(is.na(y))))) 
NA_Count 
Null_Count = sapply(data_set, function(y) sum(length(which(is.null(y)))))
Null_Count 
Length0_Count = sapply(data_set, function(y) sum(length(which(length(y)==0))))
Length0_Count 
Empty_Count = sapply(data_set, function(y) sum(length(which(y==''))))
Empty_Count
Total_Miss_count = NA_Count+Null_Count+Length0_Count+Empty_Count
### Removing all the rows having missing values
data_set_No_Missing_Values=data_set[!apply(data_set, 1, function(x) any(x=="" | is.na(x))),] 

Total_Miss_count

















