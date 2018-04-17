rm(list = ls())
library(class)
library(readr)
library(ModelMetrics)
N<-39
feats<-c("date_end_DateInt","date_renewal_DateInt","margin_gross_pow_ele",
         "date_activ_DateInt",
         "date_modif_prod_DateInt",
         "margin_net_pow_ele",
         "net_margin",
         "forecast_cons_12m",
         "pow_max",
         "forecast_meter_rent_12m",
         "imp_cons",
         "forecast_cons_year",
         "cons_12m",
         "price_p1_var",
         "forecast_price_energy_p1",
         "cons_last_month",
         "num_years_antig",
         "forecast_price_energy_p2",
         "price_p1_fix",
         "price_p2_var",
         "nb_prod_act",
         "price_p3_var",
         "forecast_price_pow_p1",
         "cons_gas_12m",
         "origin_up_lxidpiddsbxsbosboudacockeimpuepw",
         "channel_sales_foosdfpfkusacimwkcsosbicdxkicaua",
         "channel_sales_usilxuppasemubllopkaafesmlibmsdf",
         "has_gas_f",
         "has_gas_t",
         "origin_up_kamkkxfxxuwbdslkwifmmcsiusiuosws",
         "price_p3_fix",
         "origin_up_ldkssxwpmemidmecebumciepifcamkci",
         "price_p2_fix",
         "channel_sales_lmkebamcaaclubfxadlmueccxoimlema",
         "channel_sales_ewpakwlliwisiwduibdlfmalxowmwpci",
         "forecast_discount_energy",
         "channel_sales_sddiedcslfslkckwlfkdpoeeailfpeds",
         "channel_sales_epumfxlbckeskwekxbiuasklxalciiuu",
         "origin_up_usapbepcfoloekilkwsdiboslwaxobdp"
)

feats<-feats[1:N]

# Read data
key <- "OFF"
targetName<-"churn"
dataset <- read_csv(file=paste0("C:/Users/m00760171/Desktop/Templates/trunk/data/normal/data_",key,"_",key,"_",key,".csv"))
#df_test <- read_csv(file=paste0("C:/Users/m00760171/Desktop/Templates/trunk/test/normal/data_",key,"_",key,"_",key,".csv"))

dataset<-dataset[, c(feats,targetName)]
#df_test<-df_test[, c(feats,targetName)]

N_target<-ncol(dataset)
k<-5

params <- expand.grid(
  k=c(1,3,5,7,9)
)
params$AUC<-0
dataset$cv_i<-seq(1:nrow(dataset))%%k
tmp<-dataset[,targetName]
dataset[,targetName]<-NULL
dataset[,targetName]<-tmp


dataset_with_cv <- dataset

dataset_with_cv[, setdiff(names(dataset_with_cv),c("cv_i",targetName)) ]<-NULL



j<-0

while(j<nrow(params))
{
  
  print(params[j+1,])
  i<-0
  auc_cum<-0
  res<-numeric(k)
  while (i<k)
  {
    
    test<-dataset[dataset$cv_i==i , setdiff(names(dataset),"cv_i")   ]
    train <-dataset[dataset$cv_i!=i , setdiff(names(dataset),"cv_i") ]
    N_resp<-ncol(test)
    
    train[-N_resp] = scale(train[-N_resp])
    test[-N_resp] = scale(test[-N_resp])
    #test[test==NA]<-0
    test[is.na(test)]<-0
    train[is.na(train)]<-0
    
    #[, -N_resp]
    # Fitting K-NN to the Training set and Predicting the Test set results
    y_pred = knn(train = train[, -N_resp],
                 test = test[, -N_resp],
                 cl = as.numeric(train$churn),
                 k = params[1+j,"k"],
                 prob = TRUE)
    
    
    #classifier = xgboost(data = as.matrix(train[-N_target]), label = train$churn, nrounds = params[1+j,"nrounds"], eval_metric = 'auc', max.depth = params[1+j,"max.depth"], eta = params[1+j,"eta"],  nthread = 8,  objective = "binary:logistic", verbose = FALSE)
    #classifier = knn(train = train, test=test, k = params[1+j,"k"], cl=factor(train$churn), prob = TRUE)
    #y_pred = classifier
    
    dataset_with_cv[dataset_with_cv$cv_i==i,paste0("CV_KNN_",j)]<-y_pred
   
    auc_cum<-auc_cum+auc(test$churn, y_pred)
    
    i<-i+1
  }
  params[j+1,"AUC"]<-auc_cum/k 
  print(auc_cum/k)
  print("-----------------------------------")
  
  j<-j+1
  
}

tmp<-dataset_with_cv[,targetName]
dataset_with_cv[,targetName]<-NULL
dataset_with_cv[,targetName]<-tmp

write.csv(dataset_with_cv, file = "scripts/preds/KNN_preds.csv",row.names = FALSE)
write.csv(params, file = "scripts/preds/KNN_params.csv",row.names = FALSE)


