
# Script to tune NN
rm(list = ls())


library(h2o)
h2o.init(nthreads = 8)

# Total features selected
N<-39
# k-fold 
k<-5


# Read data
key <- "OFF"
targetName<-"churn"
train_h2o <- h2o.importFile(paste0("C:/Users/m00760171/Desktop/Templates/trunk/data/normal/data_",key,"_",key,"_",key,".csv"))

# Select a subset of the features if needed
feats<-names(train_h2o)
feats<-feats[1:N]
train_h2o<-train_h2o[,c(feats,targetName)]

# Identify predictors and response
y <- "churn"
x <- setdiff(names(train_h2o), y)

# Getting a dataframe to store the CV predictions
dataset_with_cv<-as.data.frame(train_h2o)


# For binary classification, response should be a factor
dataset_with_cv$cv_i<-seq(1:nrow(dataset_with_cv))%%k
tmp<-dataset_with_cv[,targetName]
dataset_with_cv[,targetName]<-NULL
dataset_with_cv[,targetName]<-tmp


# Bringing it back to h2o format
train_h2o<-as.h2o(dataset_with_cv)
train_h2o[,y] <- as.factor(train_h2o[,y])

# Search grid
params <- expand.grid(
  epochs = c(30,50,100),
  l_1_2 = c(0,1e-4),
  layers = c(3,6,8)
)

# Initialize AUC with zeros
params$AUC<-0

# Removing all features apart from cv_i and target
dataset_with_cv[, setdiff(names(dataset_with_cv),c("cv_i",targetName)) ]<-NULL

j<-0

while(j<nrow(params))
{
  
  print(params[j+1,])
  
  Nfeats<-ncol(train_h2o)-1
  
  midLayers<-rev(round((Nfeats)*seq(1:params[1+j,"layers"])/(params[1+j,"layers"]+1)))
  print(midLayers)
  
  # CV train RF
  my_dl <- h2o.deeplearning(
     training_frame=train_h2o, 
    x=x, 
    y=y, 
    hidden=midLayers,   
    epochs=params[1+j,"epochs"],             
    l1=params[1+j,"l_1_2"],
    l2=params[1+j,"l_1_2"],
    nfolds = k,
    fold_assignment = "Modulo",
    keep_cross_validation_predictions = TRUE,
    seed=123
    ) 
  
  # Getting the CV predictions
  nfolds <- my_dl@parameters$nfolds
  predlist <- sapply(1:nfolds, function(v) h2o.getFrame(my_dl@model$cross_validation_predictions[[v]]$name)$p1, simplify = FALSE)
  cvpred_sparse <- h2o.cbind(predlist)  
  pred <- apply(cvpred_sparse, 1, sum) 
  
  # Storing the predictions
  dataset_with_cv[,paste0("CV_NN_",j)]<-as.matrix(pred)
  
  # Storing the AUC
  mean_auc<-mean(sapply(sapply(my_dl@model$cross_validation_models, `[[`, "name"), function(x) { h2o.auc(h2o.getModel(x), valid=TRUE) }))
  params[j+1,"AUC"]<-mean_auc
  
  print(mean_auc)
  print("-----------------------------------")
  
  j<-j+1
  
}

tmp<-dataset_with_cv[,targetName]
dataset_with_cv[,targetName]<-NULL
dataset_with_cv[,targetName]<-tmp

write.csv(dataset_with_cv, file = "scripts/preds/NN_preds.csv",row.names = FALSE)
write.csv(params, file = "scripts/preds/NN_params.csv",row.names = FALSE)







