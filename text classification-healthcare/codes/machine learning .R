rm(list = ls())
##Setting the working directory.

getwd()
setwd("C:/Users/Architect_shwet/Documents")
list.files()



load('First_step.dat')
load('Second_step.dat')
load('Third_step.dat')
load('Fourth_step.dat')

library(caTools)
sampling <- function(master_data, set_seed = 123, samp.ratio= 0.075, train.ratio= 0.75){
  set.seed(seed <- set_seed) #sets the seed
  samp_split <- sample.split(master_data[,ncol(master_data)], samp.ratio)
  sample <- subset(master_data, samp_split == T) #sample from master
  
  # training and testing 
  smpl = sample.split(sample[,ncol(sample)], train.ratio)
  x_train <- subset(sample, smpl == T ) #training set
  x_test  <- subset(sample, smpl == F ) #testing set
  y_train <- x_train[,ncol(x_train)]
  y_test <- x_test[,ncol(x_test)]
  x_train[,ncol(x_train)] = NULL
  x_test[,ncol(x_test)] = NULL
  train_test <- list(x_train,x_test,y_train,y_test)
  return(train_test)
}

#Train and Test sets from the sample with seed = 555 to classify categories

train_test_cat <- sampling(master_data_cat,555)

#Train and Test sets from the sample with seed = 555 to classify sub categories

train_test_sub <- sampling(master_data_sub,555)

#Unpacking the train and test sets for clasifying categories
dim(train_test_cat)

x_train_cat = train_test_cat[[1]]
x_test_cat = train_test_cat[[2]]
y_train_cat = train_test_cat[[3]]
y_test_cat = train_test_cat[[4]]
rm(train_test_cat)
dim(x_train_cat)
dim(x_test_cat)
#Unpacking the train and test sets for clasifying sub categories

x_train_sub = train_test_sub[[1]]
x_test_sub = train_test_sub[[2]]
y_train_sub = train_test_sub[[3]]
y_test_sub = train_test_sub[[4]]
dim(x_train_sub)
dim(x_test_sub)

rm(train_test_sub)

## Create an H2O cloud 
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  max_mem_size = "2G")    ## specify the memory size for the H2O cloud
h2o.removeAll() # Clean slate - just in case the cluster was already running

#check h2o cluster status
h2o.init()

set.seed(123)

#categories

#Combining the predictors and target variables
p_cat = cbind(x_train_cat,y_train_cat)

# loading data to h2o clusters
h_train_cat = as.h2o(p_cat)

# creating predictor and target indices
x_cat = 1:ncol(p_cat)-1
y_cat = ncol(p_cat)

# Number of CV folds (to generate level-one data for stacking)
nfolds <- 5

# Train & Cross-validate a GBM
my_gbm_cat <- h2o.gbm(x = x_cat,y = y_cat,training_frame = h_train_cat,distribution = "bernoulli",max_depth = 3,
                  min_rows = 2,learn_rate = 0.2,nfolds = nfolds,fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,seed = 1)

# Train & Cross-validate a RF
my_rf_cat <- h2o.randomForest(x = x_cat,y = y_cat,training_frame = h_train_cat,nfolds = nfolds,fold_assignment = "Modulo",
                          ntrees=1000,keep_cross_validation_predictions = TRUE,seed = 1)

# Train & Cross-validate a DNN
my_dl_cat <- h2o.deeplearning(x = x_cat,y = y_cat,training_frame = h_train_cat,l1 = 0.001,l2 = 0.001,
                          hidden = c(200, 200, 200),nfolds = nfolds,fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,seed = 1)

# Train & Cross-validate a (shallow) XGB-GBM
my_xgb1_cat <- h2o.xgboost(x = x_cat,y = y_cat,training_frame = h_train_cat,distribution = "bernoulli",
                       ntrees = 50,max_depth = 3,min_rows = 2,learn_rate = 0.2,nfolds = nfolds,
                       fold_assignment = "Modulo",keep_cross_validation_predictions = TRUE,seed = 1)


# Train & Cross-validate another (deeper) XGB-GBM
my_xgb2_cat <- h2o.xgboost(x = x_cat, y = y_cat, training_frame = h_train_cat, distribution = "bernoulli",
                       ntrees = 50,max_depth = 8,min_rows = 1,learn_rate = 0.1,sample_rate = 0.7,
                       col_sample_rate = 0.9,nfolds = nfolds,fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,seed = 1)


# Train a stacked ensemble using the H2O and XGBoost models from above
base_models_cat <- list(my_gbm_cat@model_id, my_rf_cat@model_id, my_dl_cat@model_id,  
                    my_xgb1_cat@model_id, my_xgb2_cat@model_id)

ensemble_cat <- h2o.stackedEnsemble(x = x_cat, y = y_cat, training_frame = h_train_cat,
                                base_models = base_models_cat_)


set.seed(123)

q_cat = cbind(x_test_cat,y_test_cat)
h_test_cat = as.h2o(q_cat)

#predicting the gbm model
pred_cat_gbm <- as.data.frame(h2o.predict(my_gbm_cat, h_test_cat))
summary(pred_cat_gbm)
library(caret)
confusionMatrix(pred_cat_gbm $predict,y_test_cat)
h2o.varimp(my_gbm_cat)

#predicting the rf model
pred_cat_rf <- as.data.frame(h2o.predict(my_rf_cat, h_test_cat))
summary(pred_cat_rf)
library(caret)
confusionMatrix(pred_cat_rf$predict,y_test_cat)
h2o.varimp(my_rf_cat)

#predicting the dl model
pred_cat_dl <- as.data.frame(h2o.predict(my_dl_cat, h_test_cat))
summary(pred_cat_dl)
library(caret)
confusionMatrix(pred_cat_dl$predict,y_test_cat)
h2o.varimp(my_dl_cat)

#predicting the xgb1 model
pred_cat_xgb1 <- as.data.frame(h2o.predict(my_xgb1_cat, h_test_cat))
summary(pred_cat_xgb1)
library(caret)
confusionMatrix(pred_cat_xgb1$predict,y_test_cat)
h2o.varimp(my_xgb1_cat)

#predicting the xgb2 model
pred_cat_xgb2 <- as.data.frame(h2o.predict(my_xgb2_cat, h_test_cat))
summary(pred_cat_xgb2)
library(caret)
confusionMatrix(pred_cat_xgb2$predict,y_test_cat)
h2o.varimp(my_xgb2_cat)

#####predicting the stacked model
pred_cat <- as.data.frame(h2o.predict(ensemble_cat, h_test_cat))
summary(pred_cat)
library(caret)
confusionMatrix(pred_cat$predict,y_test_cat)
h2o.varimp(ensemble_cat)




#sub categories


#Combining the predictors and target variables
p_sub = cbind(x_train_sub,y_train_sub)

# loading data to h2o clusters
h_train_sub = as.h2o(p_sub)

# creating predictor and target indices
x_sub = 1:ncol(p_sub)-1
y_sub = ncol(p_sub)

# Number of CV folds (to generate level-one data for stacking)
nfolds <- 5

# Train & Cross-validate a GBM
my_gbm_sub <- h2o.gbm(x = x_sub,y = y_sub,training_frame = h_train_sub,distribution = "bernoulli",max_depth = 3,
                  min_rows = 2,learn_rate = 0.2,nfolds = nfolds,fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,seed = 1)

# Train & Cross-validate a RF
my_rf_sub <- h2o.randomForest(x = x_sub,y = y_sub,training_frame = h_train_sub,nfolds = nfolds,fold_assignment = "Modulo",
                          ntrees=1000,keep_cross_validation_predictions = TRUE,seed = 1)

# Train & Cross-validate a DNN
my_dl_sub <- h2o.deeplearning(x = x_sub,y = y_sub,training_frame = h_train_sub,l1 = 0.001,l2 = 0.001,
                          hidden = c(200, 200, 200),nfolds = nfolds,fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,seed = 1)

# Train & Cross-validate a (shallow) XGB-GBM
my_xgb1_sub <- h2o.xgboost(x = x_sub,y = y_sub,training_frame = h_train_sub,distribution = "bernoulli",
                       ntrees = 50,max_depth = 3,min_rows = 2,learn_rate = 0.2,nfolds = nfolds,
                       fold_assignment = "Modulo",keep_cross_validation_predictions = TRUE,seed = 1)


# Train & Cross-validate another (deeper) XGB-GBM
my_xgb2_sub <- h2o.xgboost(x = x_sub, y = y_sub, training_frame = h_train_sub, distribution = "bernoulli",
                       ntrees = 50,max_depth = 8,min_rows = 1,learn_rate = 0.1,sample_rate = 0.7,
                       col_sample_rate = 0.9,nfolds = nfolds,fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,seed = 1)


# Train a stacked ensemble using the H2O and XGBoost models from above
base_models_sub <- list(my_gbm_sub@model_id, my_rf_sub@model_id, my_dl_sub@model_id,  
                    my_xgb1_sub@model_id, my_xgb_sub2@model_id)

ensemble_sub <- h2o.stackedEnsemble(x = x_sub, y = y_sub, training_frame = h_train_sub,
                                base_models = base_models_sub)


set.seed(123)

q_sub = cbind(x_test_sub,y_test_sub)
h_test_sub = as.h2o(q_sub)

#predicting the gbm model 
pred_sub_gbm <- as.data.frame(h2o.predict(my_gbm_sub, h_test_sub))
summary(pred_sub_gbm)
library(caret)
confusionMatrix(pred_sub_gbm$predict,y_test_sub)
h2o.varimp(my_gbm_sub)

#predicting the rf model 
pred_sub_rf <- as.data.frame(h2o.predict(my_rf_sub, h_test_sub))
summary(pred_sub_rf)
library(caret)
confusionMatrix(pred_sub_rf$predict,y_test_sub)
h2o.varimp(my_rf_sub)

#predicting the dl model 
pred_sub_dl <- as.data.frame(h2o.predict(my_dl_sub, h_test_sub))
summary(pred_sub_dl)
library(caret)
confusionMatrix(pred_sub_dl$predict,y_test_sub)
h2o.varimp(my_dl_sub)

#predicting the xgb1 model 
pred_sub_xgb1 <- as.data.frame(h2o.predict(my_xgb1_sub, h_test_sub))
summary(pred_sub_xgb1)
library(caret)
confusionMatrix(pred_sub_xgb1$predict,y_test_sub)
h2o.varimp(my_xgb1_sub)

#predicting the xgb2 model
pred_sub_xgb2 <- as.data.frame(h2o.predict(my_xgb2_sub, h_test_sub))
summary(pred_sub_xgb2)
library(caret)
confusionMatrix(pred_sub_xgb2$predict,y_test_sub)
h2o.varimp(my_xgb2_sub)

###predicting the stacked model
pred_sub <- as.data.frame(h2o.predict(ensemble_sub, h_test_sub))
summary(pred_sub)
library(caret)
confusionMatrix(pred_sub$predict,y_test_sub)
h2o.varimp(ensemble_sub)






library(e1071)
x_cat = cbind(x_train_cat,y_train_cat)

fit_cat = svm(y_train_cat ~., data = x_cat)
summary(fit_cat)

#SVM model for classifying sub categories
x_sub = cbind(x_train_sub,y_train_sub)

fit_sub = svm(y_train_sub~.,data = x_sub)
summary(fit_sub)



#SVM Evaluation of categories

pred_cat = predict(fit_cat,x_test_cat)
summary(pred_cat)
library(caret)
confusionMatrix(pred_cat,y_test_cat)
perf_val <- performance(pred_cat,"auc")
# Plot the ROC curve
plot(perf_val, col = "green", lwd = 1.5)




pred_sub <- predict(fit_sub, x_test_sub)
summary(pred_sub)
library(caret)
confusionMatrix(pred_sub, y_test_sub)





















