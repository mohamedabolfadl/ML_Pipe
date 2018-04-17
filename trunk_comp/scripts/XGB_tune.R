rm(list = ls())
library(xgboost)
library(ModelMetrics)
library(readr)

# Read data
key <- "OFF"
targetName<-"btarget"
dataset <- read_csv(file=paste0("C:/Users/m00760171/Desktop/trunk_comp/data/normal/data_",key,"_",key,"_",key,".csv"))

dataset<-dataset[,c("williamsAD","adx","EMA_200","TRIX_signal","VHF","atr","TDI_tdi","volatility","chaikinVolatility","runPercentRank","EMA_100","ultimateOscillator","SAR","Pbands_up","Pbands_dn","runVar","btarget")]
dataset <- dataset[sample(nrow(dataset)),]

dataset[,targetName] <- 0.5+(dataset[,targetName]*0.5)

#feats<-setdiff(names(dataset),targetName)

#dataset<-dataset[, c(feats,targetName)]


N_target<-ncol(dataset)
k<-5

params <- expand.grid(
  eta=c(0.1,0.01),
  nrounds=c(300,350,400),                  
  max.depth=c(8,10,15),                  
  colsample_bytree = c(0.7,0.8)
)
params$AUC<-0
dataset$cv_i<-seq(1:nrow(dataset))%%k
tmp<-dataset[,targetName]
dataset[,targetName]<-NULL
dataset[,targetName]<-tmp


dataset_with_cv <- dataset

dataset_with_cv[, setdiff(names(dataset_with_cv),c("cv_i",targetName)) ]<-NULL



j<-0

max_auc<-0

while(j<nrow(params))
{
  
  
  i<-0
  auc_cum<-0
  res<-numeric(k)
  while (i<k)
  {
    
    test<-dataset[dataset$cv_i==i , setdiff(names(dataset),"cv_i")   ]
    train <-dataset[dataset$cv_i!=i , setdiff(names(dataset),"cv_i") ]
    
    classifier = xgboost(data = as.matrix(train[-N_target]), label = train$btarget, colsample_bytree = params[1+j,"colsample_bytree"], nrounds = params[1+j,"nrounds"], eval_metric = 'auc', max.depth = params[1+j,"max.depth"], eta = params[1+j,"eta"],  nthread = 8,  objective = "binary:logistic", verbose = FALSE)
    
    
    y_pred = predict(classifier, newdata = as.matrix(test[-N_target]))
    
    dataset_with_cv[dataset_with_cv$cv_i==i,paste0("CV_XGB_",j)]<-y_pred
    
    auc_cum<-auc_cum+auc(test$btarget, y_pred)
    
    i<-i+1
  }
  params[j+1,"AUC"]<-auc_cum/k 
  if((auc_cum/k) > max_auc)
    {
    print(params[j+1,])
    print(auc_cum/k)
  print("-----------------------------------")
  max_auc<-auc_cum/k
}
  j<-j+1
  
}

tmp<-dataset_with_cv[,targetName]
dataset_with_cv[,targetName]<-NULL
dataset_with_cv[,targetName]<-tmp

write.csv(dataset_with_cv, file = "scripts/preds/XGB_preds.csv",row.names = FALSE)
write.csv(params, file = "scripts/preds/XGB_params.csv",row.names = FALSE)


