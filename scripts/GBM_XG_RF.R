

#  Script to implement GBM, RF and XG

# XGBoost read:
# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/


library(h2o)
h2o.init()
h2o.removeAll()


N_feats <- 20
targetName<- "churn"

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
y <- targetName
#x <- setdiff(names(train), y)
# After finding most important features
x=featVect


# For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

# Number of CV folds (to generate level-one data for stacking)
nfolds <- 5




# GBM 1
my_gbm1 <- h2o.gbm(x = x,
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


library(xgboost)
library(readr)
library(caret)
library (ROCR)


df_train <- read_csv(file=paste0("C:/Users/m00760171/Desktop/Templates/trunk/data/normal/data_",key,"_",key,"_",key,".csv"))
df_test <- read_csv(file=paste0("C:/Users/m00760171/Desktop/Templates/trunk/test/normal/data_",key,"_",key,"_",key,".csv"))

xgb <- xgboost(data = as.matrix(df_train[,1:(ncol(df_train)-1)]), 
               #label =df_train[[targetName]],
               label =df_train$churn,
               #eta = 0.5,
               #objective = "binary:logistic",
               #max_depth = 6, 
               nround=10
               #objective = "binary:logistic",
               #num_class = 2
)



y_pred <- predict(xgb,  data.matrix(df_test[,1:(ncol(df_test)-1)]))
xgbpred <- ifelse (y_pred > 0.5,1,0)
confusionMatrix (xgbpred, df_test[[targetName]])




y <- df_test[[targetName]] # logical array of positive / negative cases
predictions <- y_pred # array of predictions

pred <- prediction(predictions, y);

# Recall-Precision curve             
RP.perf <- performance(pred, "prec", "rec");

plot (RP.perf);

# ROC curve
ROC.perf <- performance(pred, "tpr", "fpr");
plot (ROC.perf);

# ROC area under the curve
auc.tmp <- performance(pred,"auc");
auc <- as.numeric(auc.tmp@y.values)



#view variable importance plot
 mat <- xgb.importance (feature_names = colnames(df_train),model = xgb)
 xgb.plot.importance (importance_matrix = mat[1:20]) 

