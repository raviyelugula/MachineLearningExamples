require(ggplot2) # For Diamonds dataset
diamonds
# CH1 ----
# Fit lm model using 10-fold CV: model
model <- train(
  price~., data = diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)
# Fit lm model using 5 x 5-fold CV: model
# One of the awesome things about the train() function in caret is 
# how easy it is to run very different models or methods of cross-validation 
# just by tweaking a few simple arguments to the function call. 
# For example, you could repeat your entire cross-validation procedure 5 times
# for greater confidence in your estimates of the model's 
# out-of-sample accuracy, e.g.:
model <- train(
  medv ~ ., Boston,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 5,
    repeats = 5, verboseIter = TRUE
  )
)
#CH2 ----
require(mlbench)
data(Sonar)
Sonar
# Shuffle row indices: rows # Randomly order data: Sonar
# Identify row to split on: split # Create train & test
rows = sample(nrow(Sonar)) 
Sonar = Sonar[rows,]
split <- round(nrow(Sonar) * 0.6)
train = Sonar[c(1:split),]
test = Sonar[c((split+1):nrow(Sonar)),]
model = glm(Class~.,data = train, family="binomial")
p = predict(model,newdata= test , type="response")
p_class = ifelse(p > 0.5, "M","R")
confusionMatrix(p_class, test$Class)
p_class = ifelse( p>0.10, "M","R" )
confusionMatrix(p_class, test[["Class"]])
p_class = ifelse( p>0.02, "M","R" )
confusionMatrix(p_class, test[["Class"]])
p = predict(model, newdata = test, type= "response")
require(caTools)
caTools::colAUC(p, test[["Class"]], plotROC= T)
# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = T, # IMPORTANT!
  verboseIter = TRUE
)
# Train glm with custom trainControl: model
model = train(Class~., data = Sonar, method="glm", trControl = myControl)
# Print model to console
model

#CH3 ----
require(rattle.data)
data(wine)
# Fit random forest: model
model <- train(
  quality ~.,
  tuneLength = 1,
  data = wine, method = 'ranger',
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
# Print model to console
model
# Fit random forest: model Wider Model Space
model <- train(
  quality~.,
  tuneLength = 3,
  data = wine, method = 'ranger',
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
# Print model to console
model
# Fit random forest: model - Custom Choosing Grids
model <- train(
  quality~.,
  tuneGrid = data.frame(mtry = c(2,3,7) ),
  data = wine, method = 'ranger',
  trControl = trainControl(method = "cv", number = 5 , verboseIter = TRUE)
)
# Print model to console
model
# Create custom trainControl: myControl
myControl <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = T, # IMPORTANT!
  verboseIter = TRUE
)
# Fit glmnet model: model
model <- train(
  y~., data = overfit,
  method = "glmnet",
  trControl = myControl
)
# Print model to console
model
# Train glmnet with custom trainControl and tuning: model
model <- train(
  y~., data = overfit,
  tuneGrid = expand.grid(alpha=0:1, lambda = seq(0.0001,1,length=20)),
  method = "glmnet",
  trControl = myControl
)
# Print model to console
model

# CH4 ----
# Apply median imputation: model
model <- train(
  x = breast_cancer_x, y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "medianImpute"
)
# Print model to console
model
require(mlbench)
data("BreastCancer")
breast_cancer_x = BreastCancer[,-c(1,11)]
breast_cancer_y = BreastCancer[,c(11)]
# Apply KNN imputation: model2
model2 <- train(
  x = breast_cancer_x, y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "knnImpute"
)
# Print model to console
model2
# Fit glm with median imputation: model1
model1 <- train(
  x = breast_cancer_x, y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = c("medianImpute")
)
# Print model1
model1
require(caret)
data(BloodBrain)
dim(bbbDescr)
bloodbrain_x = bbbDescr[,-c(3,60)]
# Identify near zero variance predictors: remove_cols
remove_cols <- nearZeroVar(bloodbrain_x, names = TRUE, 
                           freqCut = 2, uniqueCut = 20)
# Get all column names from bloodbrain_x: all_cols
all_cols = names(bloodbrain_x)
# Remove from data: bloodbrain_x_small
bloodbrain_x_small <- bloodbrain_x[ , setdiff(all_cols, remove_cols)]
# Fit model on reduced data: model
model <- train(x = bloodbrain_x_small, y = bloodbrain_y, method = "glm")
# Print model to console
model
# Fit glm model using PCA: model
model <- train(
  x = bloodbrain_x, y = bloodbrain_y,
  method = 'glm', preProcess = "pca"
)
# Print model to console
model

# CH5 ----
# Create custom indices: myFolds
myFolds <- createFolds(churn_y, k = 5)
# Create reusable trainControl object: myControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)
# Fit glmnet model: model_glmnet
model_glmnet <- train(
  x = churn_x, y = churn_y,
  metric = "ROC",
  method = 'glmnet',
  trControl = myControl
)
plot(model_glmnet$finalModel)
# Fit random forest: model_rf
model_rf <- train(
  x = churn_x, y = churn_y,
  metric = "ROC",
  method = 'ranger',
  trControl = myControl
)
# Create model_list
model_list <- list(item1 = model_glmnet, item2 = model_rf)
# Pass model_list to resamples(): resamples
resamples = resamples(model_list)
# Summarize the results
summary(resamples)
# Create bwplot
bwplot(resamples , metric="ROC")
# Create xyplot
xyplot(resamples, metric='ROC')
# Create ensemble model: stack
stack <- caretStack(model_list, method='glm')
# Look at summary
summary(stack)

#### Code snippet for string search

setwd( "G:/GL/Machine learning")
#We will merge 2 Excel Files
# Election 2012 and 2017
#However, the Constituency names may have changed


UP.2017.unmatched<-read.csv(file.choose(), header=T)
UP.2012.all<-read.csv(file.choose(), header=T)

UP.2017.unmatched$AC<-UP.2017.unmatched$EC.2017
UP.2012.all$AC<-UP.2012.all$AC_NAME
#install.packages('stringdist')
library(stringdist)
#We will only use LV,DL,JACCARD and JARROW

distance.methods<-c('lv','dl','jaccard','jw')
dist.methods<-list()
for(m in 1:length(distance.methods))
{
  dist.name.enh<-matrix(NA, ncol = length(UP.2017.unmatched$AC),nrow = length(UP.2012.all$AC))
  for(i in 1:length(UP.2017.unmatched$AC)) {
    for(j in 1:length(UP.2012.all$AC)) { 
      dist.name.enh[j,i]<-stringdist(tolower(UP.2017.unmatched[i,]$AC),tolower(UP.2012.all[j,]$AC),
                                     method = distance.methods[m])      
      #adist.enhance(UP.Match[i,]$name,UP_Crime[j,]$name)
    }  
  }
  dist.methods[[distance.methods[m]]]<-dist.name.enh
}

match.s1.s2.enh<-NULL
for(m in 1:length(dist.methods))
{
  
  dist.matrix.1<-as.matrix(dist.methods[[distance.methods[m]]])
  min.name.enh<-apply(dist.matrix.1, 1, base::min)
  for(i in 1:nrow(dist.matrix.1))
  {
    s2.i<-match(min.name.enh[i],dist.matrix.1[i,])
    s1.i<-i
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=UP.2017.unmatched[s2.i,]$AC, 
                                      s1name=UP.2012.all[s1.i,]$AC, adist=min.name.enh[i],
                                      method=distance.methods[m]),match.s1.s2.enh)
  }
}

library(reshape2)
matched.names.matrix.17with12<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix.17with12)
#Looks like jw does well

write.csv(matched.names.matrix.17with12,"matched.name.matrix.17with12.csv")







