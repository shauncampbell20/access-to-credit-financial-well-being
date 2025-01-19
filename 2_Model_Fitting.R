#### Shaun Campbell
#### COSC 6520 Project 1
#### Part 2: Model Fitting

# Clear working directory
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load required packages
library(dplyr)
library(caret)
library(data.table)
library(pROC)
library(gains)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROSE)
library(foreach)
library(doParallel)

# Read in prepared data set and drop index. Ensure action_taken is a factor
df <- fread(file.path('./data', 'data.csv'), header=TRUE)
df <- df[,-c('V1')]
df$action_taken <- as.factor(df$action_taken)

# Train, validation, test split
set.seed(1)
trainInd <- createDataPartition(df$action_taken, p=0.8, list=FALSE)
train <- df[trainInd, ]
val <- df[-trainInd, ]
set.seed(1)
valInd <- createDataPartition(val$action_taken, p=0.5, list=FALSE)
test <- val[-valInd, ]
val <- val[valInd, ]

# Under sample the training data to contain a balanced number of each outcome of the target variable
# Ensure the factor levels of the balanced train set match that of the original data set
train_balanced <- ovun.sample(action_taken~., data=train, method="under", p=0.5, seed=1)$data
train_balanced$action_taken <- factor(train_balanced$action_taken, levels=levels(train$action_taken))

# Verify that balanced train data, validation, and train are roughly 50/25/25
nrow(train_balanced)/(nrow(train_balanced)+nrow(test)+nrow(val))

# Create numeric target variables to be used later
val$y <- as.numeric(as.character(val$action_taken))
test$y <- as.numeric(as.character(test$action_taken))

##########################
####### NAIVE BAYES ######
##########################

# Set control for 5-fold cross validation and seed for reproducible results
# Fit naive Bayes model on balanced train data 
# Model is saved to disk 
control <- trainControl(method = "cv", number=5) 
set.seed(1)
nb_fit <- train(action_taken ~ .-dti1-census_region, data=train_balanced, method = "naive_bayes", trControl = control)
saveRDS(nb_fit, file.path('./models', 'nb_model.rds'))

# Predict class and probabilities on the validation data
nb_class <- predict(nb_fit, newdata = val)
nb_Class_prob <- predict(nb_fit, newdata = val, type='prob')

# Confusion matrix and performance measures for the validation set
cm <- confusionMatrix(nb_class, val$action_taken, positive = '1')
cm
cm[["byClass"]]

# Evaluate using the underlying ratio of denied applications for the cutoff
# Doing so pretty much exclusively predicts approved
table(train$action_taken)
nb_class2 <- as.factor(ifelse(nb_Class_prob[,2]>0.125, '1', '0'))
cm <- confusionMatrix(nb_class2, val$action_taken, positive = '1')
cm
cm[["byClass"]]

# Create ROC curve and calculate AUC
nb_roc <- roc(val$y, nb_Class_prob[,2])
plot.roc(nb_roc)
auc(nb_roc)

# Create gains table for NB model
# Plot cumulative lift chart
gains_table <- gains(val$y, nb_Class_prob[,2])
gains_table
gains_table$mean.resp/mean(val$y)
plot(c(0, gains_table$cume.pct.of.total*sum(val$y))~c(0, gains_table$cume.obs), xlab = "# of cases", ylab = "Cumulative", main="Cumulative Lift Chart", type="l")
lines(c(0, sum(val$y))~c(0, dim(val)[1]), col="red", lty=2)

# Plot decile-wise lift chart for NB model
barplot(gains_table$mean.resp/mean(val$y), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0,3), main="Decile-Wise Lift Chart")

######################################
####### CLASSIFICATION TREE ##########
######################################

# Set seed and grow the full tree
set.seed(1)
full_tree <- rpart(action_taken ~ .-dti1-census_region, 
                   data = train_balanced, 
                   method = "class", 
                   cp = 0, 
                   minsplit = 2, 
                   minbucket = 1)

# View the CP table and find cp for the best-pruned tree
# Different values for cp are also tested here on the validation set
# Model is saved to disk 
treecp <- full_tree$cptable
treecp
treecp[which(treecp[,4]-treecp[,5] <= min(treecp[,4])),]
pruned_tree <- prune(full_tree, cp = .00002152836)
saveRDS(pruned_tree, file.path('./models', 'tree_model.rds'))

# Plot the pruned tree
prp(pruned_tree, 
    type = 1, 
    extra = 1, 
    under = TRUE)

# Predict class and probabilities on the validation data
predicted_class <- predict(pruned_tree, val, type = "class")
predicted_prob <- predict(pruned_tree, val, type= 'prob')

# Confusion matrix and performance measures for the validation set
cm <- confusionMatrix(predicted_class, val$action_taken, positive = "1")
cm
cm[["byClass"]]

# Evaluate using the underlying ratio of denied applications for the cutoff
cm <- confusionMatrix(as.factor(ifelse(predicted_prob[,2]>.125,'1','0')), val$action_taken, positive = "1")
cm
cm[["byClass"]]

# Create ROC curve and calculate AUC
dt_roc <- roc(val$y, predicted_prob[,2])
plot.roc(dt_roc, col="red", add=TRUE)
auc(dt_roc)

# Create gains table for classification tree model
# Plot cumulative lift chart
gains_table <- gains(val$y, predicted_prob[,2])
gains_table
gains_table$mean.resp/mean(val$y)
plot(c(0, gains_table$cume.pct.of.total*sum(val$y))~c(0, gains_table$cume.obs), xlab = "# of cases", ylab = "Cumulative", main="Cumulative Lift Chart", type="l")
lines(c(0, sum(val$y))~c(0, dim(val)[1]), col="red", lty=2)

# Plot decile-wise lift chart for classification tree model
barplot(gains_table$mean.resp/mean(val$y), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0,3), main="Decile-Wise Lift Chart")

#################################
####### RANDOM FOREST ###########
#################################

# Set cores to half available
# Make and register cluster
cores <- parallel::detectCores()/2
cl <- makeCluster(cores)
registerDoParallel(cl)

# Create bagging function 
# Ntree = 100/number of cores, mtry is tested at different levels but 3 ends up being best 
rf_model <- foreach(ntree = rep(100/cores, cores), .combine = combine, .packages = "randomForest") %dopar% {
  randomForest(action_taken ~ MSA+census_division+race_ethnicity+gender+age+income1+down_payment1+married+military, data = train_balanced, ntree = ntree, mtry = 3, importance = TRUE)
}

# Stop the cluster and return to sequential processing
stopCluster(cl)
registerDoSEQ() 

# Save model to disk
saveRDS(rf_model, file.path('./models', 'rf_model.rds'))

# Predict class and probabilities on the validation data
predicted_class <- predict(rf_model, val)
predicted_prob <- predict(rf_model, val, type='prob')

# Confusion matrix and performance measures for the validation set
# Cutoff of 0.125 is evaluated and performs best
cm <- confusionMatrix(as.factor(ifelse(predicted_prob[,2]>0.125,1,0)), val$action_taken, positive = '1')
cm
cm[["byClass"]]

# Create ROC curve and calculate AUC
rf_roc <- roc(val$y, predicted_prob[,2])
plot.roc(rf_roc)
auc(rf_roc)

# Create gains table for bagging model
# Plot cumulative lift chart
gains_table <- gains(val$y, predicted_prob[,2])
gains_table
plot(c(0, gains_table$cume.pct.of.total*sum(val$y))~c(0, gains_table$cume.obs), xlab = "# of cases", ylab = "Cumulative", main="Cumulative Lift Chart", type="l")
lines(c(0, sum(val$y))~c(0, dim(val)[1]), col="red", lty=2)

# Plot decile-wise lift chart for bagging model
barplot(gains_table$mean.resp/mean(val$y), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0,3), main="Decile-Wise Lift Chart")

###########################
####### BAGGING ###########
###########################

# Set cores to half available
# Make and register cluster
cores <- parallel::detectCores()/2
cl <- makeCluster(cores)
registerDoParallel(cl)

# Create bagging function 
# Ntree = 100/number of cores, mtry = 9 because there are 9 predictor variables 
bag_parallel <- function(seed) {
  set.seed(seed)
  model <- randomForest(action_taken ~ ., data = train_balanced[,c('action_taken','MSA','census_division','race_ethnicity','gender','age','income1','down_payment1','married','military')]
                        , ntree = 100/cores, mtry = 9, importance = TRUE)
  return(model)
}

# Run the parallel bagging
models <- foreach(seed = 1:10, .combine = combine, .packages = "randomForest") %dopar% {
  bag_parallel(seed)
}

# Combine the model and view it to make sure it looks right then 
# stop the cluster and return to sequential processing
bag_model <- combine(models)
bag_model
stopCluster(cl)
registerDoSEQ() 

# Predict class and probabilities on the validation data
predicted_class <- predict(bag_model, val)
predicted_prob <- predict(bag_model, val, type='prob')

# Confusion matrix and performance measures for the validation set
cm <- confusionMatrix(predicted_class, val$action_taken, positive = '1')
cm
cm[["byClass"]]

# Create ROC curve and calculate AUC
rf_roc <- roc(val$y, predicted_prob[,2])
plot.roc(rf_roc, col='green', add=TRUE)
auc(rf_roc)

# Create gains table for bagging model
# Plot cumulative lift chart
gains_table <- gains(val$y, predicted_prob[,2])
gains_table
plot(c(0, gains_table$cume.pct.of.total*sum(val$y))~c(0, gains_table$cume.obs), xlab = "# of cases", ylab = "Cumulative", main="Cumulative Lift Chart", type="l")
lines(c(0, sum(val$y))~c(0, dim(val)[1]), col="red", lty=2)

# Plot decile-wise lift chart for bagging model
barplot(gains_table$mean.resp/mean(val$y), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0,3), main="Decile-Wise Lift Chart")

############################
####### FINAL MODELS #######
############################

# Read saved models from disc
rf_model <- readRDS(file.path('./models', 'rf_model.rds'))
tree_model <- readRDS(file.path('./models', 'tree_model.rds'))
nb_model <- readRDS(file.path('./models', 'nb_model.rds'))

# Predict classes and probabilities on test set using the different models
rf_class <- predict(rf_model, test, type='class')
rf_prob <- predict(rf_model, test, type='prob')
tree_class <- predict(tree_model, test, type='class')
tree_prob <- predict(tree_model, test, type='prob')
nb_class <- predict(nb_model, test)
nb_prob <- predict(nb_model, test, type='prob')

# Create confusion matrixes for each model 
rf_cm <- confusionMatrix(as.factor(ifelse(rf_prob[,2] >= 0.125, 1,0)), test$action_taken, positive='1')
tree_cm <- confusionMatrix(tree_class, test$action_taken, positive = '1')
nb_cm <- confusionMatrix(nb_class, test$action_taken, positive = '1')

# View the confusion matrices and metrics
rf_cm
rf_cm[['byClass']]
tree_cm
tree_cm[['byClass']]
nb_cm
nb_cm[['byClass']]

# Create ROC objects for each model
rf_roc <- roc(test$y, rf_prob[,2])
tree_roc <- roc(test$y, tree_prob[,2])
nb_roc <- roc(test$y, nb_prob[,2])

# Display ROC curves all on one graph
plot.roc(nb_roc,  main='ROC Curves', cex.lab=2, cex.main = 2, cex.axis=2)
plot.roc(rf_roc, col='red',add=TRUE)
plot.roc(tree_roc, col="blue",lty='dotted',  add=TRUE)
legend(0.65, .4, legend=c("Naive Bayes", "Classification Tree", "Random Forest"),  
       fill = c("black","blue","red"), lty = c("dotted","dotted","solid"), cex=1.4 )

# View AUCs for each model
auc(nb_roc)
auc(tree_roc)
auc(rf_roc)

# Create gains table and plot lift charts for the models
nb_gains <- gains(test$y, nb_prob[,2])
tree_gains <- gains(test$y, tree_prob[,2])
rf_gains <- gains(test$y, rf_prob[,2])
plot(c(0, nb_gains$cume.pct.of.total*sum(test$y))~c(0, nb_gains$cume.obs), xlab = "# of cases", ylab = "Cumulative", main="Cumulative Lift Chart", type="l", col='black', lty='dotted')
lines(c(0, sum(test$y))~c(0, dim(test)[1]), col="black", lty=1)
lines(c(0, rf_gains$cume.pct.of.total*sum(test$y))~c(0, rf_gains$cume.obs), xlab = "# of cases", ylab = "Cumulative", main="Cumulative Lift Chart", type="l", col='red')
lines(c(0, tree_gains$cume.pct.of.total*sum(test$y))~c(0, tree_gains$cume.obs), xlab = "# of cases", ylab = "Cumulative", main="Cumulative Lift Chart", type="l", col='blue', lty='dotted')
legend(2e+05,1.5e+5, legend=c("Naive Bayes", "Classification Tree", "Random Forest"),  
       fill = c("black","blue","red"), lty = c("dotted","dotted","solid"), cex=1.4 )

# Plot the decile-wise lift chart for the random forest model
barplot(rf_gains$mean.resp/mean(test$y), names.arg=rf_gains$depth, xlab="Percentile", ylab="Lift", ylim=c(0,1.5), main="Decile-Wise Lift Chart", cex.lab=1.4, cex.main = 2, cex.axis=1.5)
abline(h=1, col='red', lty=2)
