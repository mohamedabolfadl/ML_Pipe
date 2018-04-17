
# Script to tune the XGBoost parameters
# based on reply in :
# https://stats.stackexchange.com/questions/171043/how-to-tune-hyperparameters-of-xgboost-trees


library(readr)
library(caret)
library(xgboost)
library(dplyr)
library(tidyr)
library(reshape2)
library(ModelMetrics)
library(pROC)

savePlot <- function(myPlot,figName) {
  #pdf(fileName)
  png(figName)
  print(myPlot)
  dev.off()
}


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
df_train <- read_csv(file=paste0("C:/Users/m00760171/Desktop/Templates/trunk/data/normal/data_",key,"_",key,"_",key,".csv"))
df_test <- read_csv(file=paste0("C:/Users/m00760171/Desktop/Templates/trunk/test/normal/data_",key,"_",key,"_",key,".csv"))

df_train<-df_train[, c(feats,targetName)]
df_test<-df_test[, c(feats,targetName)]


# setdiff(names(df_train),names(df_test))

library(caTools)
set.seed(123)
split = sample.split(df_train$churn, SplitRatio = 0.8)
training_set = subset(df_train, split == TRUE)
test_set = subset(df_train, split == FALSE)

df_train<-training_set
# Search grid
xgb_grid_1 = expand.grid(
  nrounds = c(50,80),
  eta = c(0.1,0.01),
  max_depth = c(5),
  colsample_bytree = c(0.85),
#  min_child_weight = 1,
  gamma = c(10),
  subsample=c(0.85)
)

# Initialize metrics
xgb_grid_1$AUC_CV<-0
xgb_grid_1$AUC_test_MM<-0
xgb_grid_1$AUC_test_inset<-0

N<-nrow(xgb_grid_1)
i<-1
while(i<N+1)
{
  print(paste0("-------------------",100.0*(i/N),"% ----------------------------------"))
# xgboost fitting with arbitrary parameters
xgb_params_1 = list(
  objective = "binary:logistic",                                               # binary classification
  eta = xgb_grid_1[i,"eta"],                                                                  # learning rate
  max_depth = xgb_grid_1[i,"max_depth"],                                                               # max tree depth
  gamma = xgb_grid_1[i,"gamma"],
    eval_metric = "auc"                                                          # evaluation/loss metric
)



xgb_1 = xgboost( data =  as.matrix(df_train[,1:(ncol(df_train)-1)]),
                 label =df_train$churn,
                 eta = xgb_grid_1[i,"eta"], 
                 max_depth = xgb_grid_1[i,"max_depth"],  
                 nrounds=xgb_grid_1[i,"nrounds"],
                 subsample = xgb_grid_1[i,"colsample_bytree"],
                 colsample_bytree =xgb_grid_1[i,"subsample"],
                 seed = 1,
                 eval_metric = "auc",
                 objective = "binary:logistic",
                 gamma=xgb_grid_1[i,"gamma"],
                 nthread = 8,
                 early_stop_round = 10 ,
                 stratified = TRUE,
                 prediction=TRUE,
                 verbose = FALSE
)

# cross-validate xgboost to get the accurate measure of error
xgb_cv_1 = xgb.cv(
                  data =  as.matrix(df_train[,1:(ncol(df_train)-1)]),
                  label = df_train$churn,
                  eta = xgb_grid_1[i,"eta"], 
                  max_depth = xgb_grid_1[i,"max_depth"],  
                  nrounds=xgb_grid_1[i,"nrounds"],
                  subsample = xgb_grid_1[i,"colsample_bytree"],
                  colsample_bytree =xgb_grid_1[i,"subsample"],
                  seed = 1,
                  eval_metric = "auc",
                  objective = "binary:logistic",
                  gamma=xgb_grid_1[i,"gamma"],
                  nthread = 8,
                  stratified = TRUE,
                  prediction=TRUE,
                  nfold = 5,                                                   # number of folds in K-fold
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  print.every.n = 0, 
                  early_stop_round = 10,
                  verbose = FALSE
)



if(FALSE)
  {
xgb_1 = xgboost( data =  data.matrix(df_train[,1:(ncol(df_train)-1)]),
                 label =df_train$churn,
                 eta = 0.1, 
                 max_depth = 10,  
                 nrounds=100,
                 subsample = 0.85,
                 colsample_bytree =0.85,
                 seed = 1,
                 eval_metric = "auc",
                 objective = "binary:logistic",
                 gamma=10,
                 nthread = 2,
                 early_stop_round = 10 ,
                 stratified = TRUE,
                 prediction=TRUE,
                 verbose = FALSE
)
}




y_pred <- predict(xgb_1,  as.matrix(df_test[,1:(ncol(df_test)-1)]))

y_pred_in_set <- predict(xgb_1,  as.matrix(test_set[,1:(ncol(df_test)-1)]))

xgb_grid_1[i,"AUC_test_MM"]<-ModelMetrics::auc(df_test[[targetName]],y_pred)
xgb_grid_1[i,"AUC_test_inset"]<-ModelMetrics::auc(test_set[[targetName]],y_pred_in_set)



ModelMetrics::confusionMatrix(df_test$churn,y_pred)

# plot the AUC for the training and testing samples
res<-xgb_cv_1$evaluation_log %>%select(-contains("std")) 

xgb_grid_1[i,"AUC_CV"]<-res[nrow(res),"test_auc_mean"]


#%>% mutate(IterationNum = 1:n()) 
#res$IterationNum<-NULL
d <- melt(res, id.vars="iter")
d$AUC<-d$value
d$value<-NULL

myPlot<-ggplot(d, aes(iter,AUC, col=variable)) +geom_point() +stat_smooth()  

savePlot(myPlot,paste0("figs/",i,".png"))
i<-i+1
}





if(FALSE)
{

# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = 100,
  eta = c(0.01,0.1),
  max_depth = c(2,10),
  colsample_bytree = 1,
  min_child_weight = 1,
  gamma = 1,
  subsample=1
)
   # subsample
# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 2,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE ,                                                          # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

df_train$churn<-as.factor(df_train$churn)
feature.names=names(df_train)
for (f in feature.names) {
  if (class(df_train[[f]])=="factor") {
    print(f)
    levels <- unique(c(df_train[[f]]))
    df_train[[f]] <- factor(df_train[[f]],
#                            labels=make.names(levels))
                            labels=c("a","b"))
    #labels=make.names(levels))
  }
  
}


# train the model for each parameter combination in the grid, 
#   using CV to evaluate
xgb_train_1 = train(
 #x=as.matrix(df_train[,1:(ncol(df_train)-1)]),
 x=as.matrix(df_train[,1:3]),
 y= as.factor(df_train$churn),
#    x =  as.matrix(df_train[,1:2]),
#  y = factor(df_train$churn),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree",
  metric = "ROC",
  verbose = 1,
  num_class = 2
)

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

########################################################

}