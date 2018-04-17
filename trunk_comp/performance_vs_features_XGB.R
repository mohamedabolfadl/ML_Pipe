

# Script to plot the accuracy/AUC/F-1 score vs number of features added
rm(list=ls())
#library(h2o)
#h2o.init()
#h2o.removeAll()
#------------ Importing the dataset ------------  
#train <- h2o.importFile("C:/Users/m00760171/Desktop/Templates/trunk/data/normal/data_CORR_CORR_CORR.csv")
#train <- h2o.importFile("C:/Users/m00760171/Desktop/Templates/trunk/data/normal/data_OFF_OFF_OFF.csv")

#train <- read.csv(file="C:/Users/m00760171/Desktop/Templates/trunk/data/normal/data_CORR_CORR_CORR.csv")
train <- read.csv(file="C:/Users/m00760171/Desktop/trunk_comp/data/normal/data_OFF_OFF_OFF.csv")
train <- train[sample(nrow(train)),]

# Identify predictors and response
y <- "btarget"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train[,y] <- 0.5+(train[,y]*0.5)

# Get rid of suspicous features
train[,c("Time_date_DateInt","Time_date_Day","Time_date_YearDayCount", "Time_date_Month")]<-NULL



# Number of CV folds (to generate level-one data for stacking)
nfolds <- 5

# Adding cv
#dataset$cv_i<-seq(1:nrow(dataset))%%k
N_target<-ncol(train)

library(xgboost)
#library(ModelMetrics)
##----------- XGB


classifier = xgboost(data = as.matrix(train[-N_target]), label = train$btarget,  nrounds = 328, eval_metric = 'auc',   max_depth = 10, eta = 0.1, subsample=0.8,colsample_bytree=0.8,  nthread = 8,  objective = "binary:logistic", verbose = FALSE)

varimp <- xgb.importance(x, model = classifier)
# Nice graph

#print(varimp[,1])

varimp$Gain<-NULL
varimp$Cover<-NULL
varimp$Frequency<-NULL


print(varimp)






#Nvars<-nrow(varimp)
Nvars<-nrow(varimp)
i<-13
accuracy_vec <- numeric(Nvars)
auc_vec <- numeric(Nvars)
f1_vec <- numeric(Nvars)
sel_feats<-c()
varimp_vec<-varimp$Feature
train[] <- lapply(train, as.numeric)

while(i<(Nvars+1))
{

  #names(train)==varimp[1:i]
  
  
  #  sel_feats<-as.matrix(varimp[1:i+1,1])
  #train_short <- as.data.frame(as.matrix(as.numeric(train[,sel_feats])))
  #names(train_short)<-sel_feats
  #train_short[,sel_feats]<-as.numeric(train_short[,sel_feats])
  if(i>10){
  cv <- xgb.cv(data = as.matrix( train[,varimp_vec[1:i]]) , label = train$btarget, nrounds = 328, nthread = 8, nfold = nfolds, metrics = "auc",  max_depth = 10, eta = 0.1, subsample=0.8,colsample_bytree=0.8, objective = "binary:logistic", verbose = FALSE)
  }else{
    cv <- xgb.cv(data = as.matrix( train[,varimp_vec[1:i]]) , label = train$btarget, nrounds = 328, nthread = 8, nfold = nfolds, metrics = "auc",  max_depth = 10, eta = 0.1, subsample=0.8, objective = "binary:logistic", verbose = FALSE)
  }
  mean_test_auc<-cv$evaluation_log$test_auc_mean
  
  
  auc_vec[i] <-mean_test_auc[length(mean_test_auc)]
  print(paste(i,"/",Nvars," AUC",auc_vec[i]))
  
  
  
  
  i<-i+1
}

auc_vec[1:31]<-c(0.5468068, 0.6028206, 0.693378, 0.7194486, 0.7332416, 0.748796, 0.7751428, 0.7824444, 0.7896242, 0.7924034, 0.778166, 0.7788328, 0.7784872,0.7883794, 0.792205, 0.7971244, 0.7967124, 0.803993, 0.8033892, 0.7954102, 0.7969538, 0.80085, 0.8046388, 0.8022376, 0.8009166, 0.8007048, 0.7999226,0.8006904,  0.7987244, 0.7990946, 0.7963072)   


varimp$auc<-0
#varimp[1:Nvars,"auc"]<-auc_vec
varimp[1:31,"auc"]<-auc_vec


max(auc_vec)



png(paste("figs/auc_vs_features_XGB.png"))
plot(auc_vec[1:31])
dev.off()


write.csv(varimp, file = "performance_vs_number_of_features_XGB.csv",row.names = FALSE)
