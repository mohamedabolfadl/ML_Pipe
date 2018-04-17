




library(h2o)
h2o.init()
h2o.removeAll()


#---- Use only if you want it in second level
N_feats <- 20
importantFeatures = read_csv(file = "C:/Users/m00760171/Desktop/Templates/trunk/res/feature_importance.csv")
featVect <- importantFeatures$X1
featVect <- featVect[1:N_feats]

#------------ Importing the dataset ------------  
key <- "OFF"
train <- h2o.importFile(paste0("C:/Users/m00760171/Desktop/Templates/trunk/data/normal/data_",key,"_",key,"_",key,".csv"))
#train <- h2o.importFile(paste0("C:/Users/m00760171/Desktop/Templates/trunk/data/smote/data_",key,"_",key,"_",key,"_SMOTE.csv"))
test <- h2o.importFile(paste0("C:/Users/m00760171/Desktop/Templates/trunk/test/normal/data_",key,"_",key,"_",key,".csv"))

dl_layers =    c(round(seq(ncol(train)-2,4,length=7)))


# Identify predictors and response
y <- "churn"
x <- setdiff(names(train), y)
# After finding most important features
#x=featVect


# For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

# Number of CV folds (to generate level-one data for stacking)
nfolds <- 5

# LR1
my_lr1 <- h2o.glm(x = x,
                  y = y,
                  training_frame = train,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                   family='binomial',
                  seed = 123)
# LR2
#my_lr2 <- h2o.glm(x = x,
#                  y = y,
#                  training_frame = train,
#                  nfolds = nfolds,
#                  fold_assignment = "Modulo",
#                  keep_cross_validation_predictions = TRUE,
#                  family='binomial',
#                  lambda=0.001,
#                  seed = 123)

# DL1
#my_dl1 <- h2o.deeplearning(
 # training_frame=train, 
  #x=x, 
  #y=y, 
  #hidden=c(128,128,128),          ## more hidden layers -> more complex interactions
  #epochs=100,                      ## to keep it short enough
  #rate=0.01, 
  #rate_annealing=2e-6,            
  #momentum_start=0.2,             ## manually tuned momentum
  #momentum_stable=0.4, 
  #momentum_ramp=1e7, 
  #l1=1e-5,                        ## add some L1/L2 regularization
  #l2=1e-5,
  #max_w2=10,                       ## helps stability for Rectifier
  #nfolds = nfolds,
  #fold_assignment = "Modulo",
  #keep_cross_validation_predictions = TRUE,
  #seed=123
  #) 

# DL2
#my_dl2 <- h2o.deeplearning(
#  training_frame=train, 
#  x=x, 
#  y=y, 
#  hidden=dl_layers,          ## more hidden layers -> more complex interactions
#  epochs=100,                      ## to keep it short enough
#  l1=1e-5,                        ## add some L1/L2 regularization
#  l2=1e-5,
#  nfolds = nfolds,
#  fold_assignment = "Modulo",
#  keep_cross_validation_predictions = TRUE,
#  seed=123
#) 


# GBM 1
my_gbm1 <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train,
                  distribution = "bernoulli",
                  max_depth = 6,
                  min_rows = 1,
                  sample_rate = 0.8,
                  col_sample_rate = 0.8,
                  col_sample_rate_per_tree = 0.8,
                  learn_rate = 0.1,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 123)
# GBM 2
my_gbm2 <- h2o.gbm(x = x,
                   y = y,
                   training_frame = train,
                   distribution = "bernoulli",
                   ntrees = 200,
                   max_depth = 6,
                   learn_rate = 0.1,
                   nfolds = nfolds,
                   fold_assignment = "Modulo",
                   keep_cross_validation_predictions = TRUE,
                   seed = 123)

# RF
my_rf1 <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train,
                          ntrees = 50,
                          nfolds = nfolds,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 123)



# Train a stacked ensemble using the GBM and RF above
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                model_id = "my_ensemble_binomial",
                                base_models = list(my_gbm1@model_id, my_gbm2@model_id,my_rf1@model_id))
#                                base_models = list(my_lr1@model_id,my_lr2@model_id,my_dl2@model_id,my_gbm1@model_id, my_gbm2@model_id,my_rf1@model_id))



# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)

# Compare to base learner performance on the test set
perf_lr_test1 <- h2o.performance(my_lr1, newdata = test)
#perf_lr_test2 <- h2o.performance(my_lr2, newdata = test)
#perf_dl_test2 <- h2o.performance(my_dl2, newdata = test)
perf_gbm_test1 <- h2o.performance(my_gbm1, newdata = test)
perf_gbm_test2 <- h2o.performance(my_gbm2, newdata = test)
perf_rf_test <- h2o.performance(my_rf1, newdata = test)
#baselearner_best_auc_test <- max( h2o.auc(perf_lr_test1),h2o.auc(perf_lr_test2),h2o.auc(perf_dl_test2) ,h2o.auc(perf_gbm_test1),h2o.auc(perf_gbm_test2), h2o.auc(perf_rf_test))
baselearner_best_auc_test <- max( h2o.auc(perf_lr_test1),h2o.auc(perf_gbm_test1),h2o.auc(perf_gbm_test2), h2o.auc(perf_rf_test))
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))
