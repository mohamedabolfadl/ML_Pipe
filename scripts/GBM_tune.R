rm(list = ls())





library(h2o)
h2o.init(nthreads = 8)



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

#feats<-feats[1:N]

# Read data
key <- "OFF"
targetName<-"churn"

train_h2o <- h2o.importFile(paste0("C:/Users/m00760171/Desktop/Templates/trunk/data/normal/data_",key,"_",key,"_",key,".csv"))

feats<-names(train_h2o)
feats<-feats[1:N]
train_h2o<-train_h2o[,c(feats,targetName)]

# Identify predictors and response
y <- "churn"
x <- setdiff(names(train_h2o), y)

k<-5



dataset_with_cv<-as.data.frame(train_h2o)


# For binary classification, response should be a factor
dataset_with_cv$cv_i<-seq(1:nrow(dataset_with_cv))%%k
tmp<-dataset_with_cv[,targetName]
dataset_with_cv[,targetName]<-NULL
dataset_with_cv[,targetName]<-tmp

train_h2o<-as.h2o(dataset_with_cv)

train_h2o[,y] <- as.factor(train_h2o[,y])



N_target<-ncol(dataset_with_cv)


params <- expand.grid(
  ntrees = c(25,100,125),
  max_depth = c(3,5,9),
  col_sample_rate = c(0.8,1),
  learn_rate = c(0.1,0.05)
)
params$AUC<-0





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
    
    train<-train_h2o[train_h2o$cv_i!=i , setdiff(names(train_h2o),"cv_i")   ]
    test <-train_h2o[train_h2o$cv_i==i , setdiff(names(train_h2o),"cv_i") ]
    
    #train[,y] <- as.factor(train[,y])
    #test[,y] <- as.factor(test[,y])
    
    #classifier = xgboost(data = as.matrix(train[-N_target]), label = train$churn, nrounds = params[1+j,"nrounds"], eval_metric = 'auc', max.depth = params[1+j,"max.depth"], eta = params[1+j,"eta"],  nthread = 8,  objective = "binary:logistic", verbose = FALSE)
    my_gbm <- h2o.gbm(x = x,
                       y = y,
                       training_frame = train,
                       distribution = "bernoulli",
                       ntrees = params[1+j,"ntrees"],
                       max_depth = params[1+j,"max_depth"],
                       col_sample_rate = params[1+j,"col_sample_rate"],
                       learn_rate = params[1+j,"learn_rate"],
                       seed = 123)

    perf_gbm <- h2o.performance(my_gbm, newdata = test)
    
    y_pred = h2o.predict(my_gbm, newdata = test)
    
    #y_pred = predict(classifier, newdata = as.matrix(test[-N_target]))
    
    dataset_with_cv[dataset_with_cv$cv_i==i,paste0("CV_GBM_",j)]<-as.matrix(y_pred$p1)
    
    auc_cum<-auc_cum+h2o.auc(perf_gbm)
    
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

write.csv(dataset_with_cv, file = "scripts/preds/GBM_preds.csv",row.names = FALSE)
write.csv(params, file = "scripts/preds/GBM_params.csv",row.names = FALSE)


