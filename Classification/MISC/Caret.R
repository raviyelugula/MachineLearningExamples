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







