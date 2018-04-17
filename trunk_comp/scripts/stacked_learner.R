# Stacked learner to combine the best base learners
# Usage: First run with the "BASE_LEARNER_PARAMETERS_KNOWN" set to FALSE, then get the parameters of the base learners
# Manually input the parameters and then set the boolean to TRUE
# Assumption is thresholds set for maximum F1

rm(list = ls())

set.seed(123)
# Indicate if the parameters of the base learners have been printed out or is it still the first run
BASE_LEARNER_PARAMETERS_KNOWN <- TRUE

WITH_TOP_PERFORMER <- FALSE
WITH_IMP_FEATURES <- FALSE

# Function to move the target to the end of the dataset
target_to_the_end <- function(data_set,targ_Name)
{
  tmp <-data.frame(data_set[,targ_Name])
  colnames(tmp)<-targ_Name
  data_set[,targ_Name]<-NULL
  dataset_ret <- cbind(data_set, tmp)
  return(dataset_ret)
  
}

get_mod_id<-function(st)
{
   model1<-st
   model1<-substr(model1,4,nchar(model1))
   pos1 = regexpr('_', model1)
   model1_id<-as.numeric(substr(model1,pos1+1,nchar(model1)))
   model1_type<-substr(model1,1,pos1-1)
  return(model1_id)
}

get_mod_type<-function(st)
{
  model1<-st
  model1<-substr(model1,4,nchar(model1))
  pos1 = regexpr('_', model1)
  model1_id<-as.numeric(substr(model1,pos1+1,nchar(model1)))
  model1_type<-substr(model1,1,pos1-1)
  return(model1_type)
}



# Number of features to be included from the original dataset in second level
N_feat_orig<-12

  
  targetName<-"churn"


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



if(!WITH_IMP_FEATURES)
{
  features_to_include_from_orig_dataset <-NULL
}else{
  
  features_to_include_from_orig_dataset <- feats[1:N_feat_orig]
  
}

library(readr)
library(caret)
library(ModelMetrics)
#_preds <-read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/scripts/preds/_preds.csv")

# Load the test set
testset <- read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/test/normal/data_OFF_OFF_OFF.csv")

# Load the original data set
trainset_orig <- read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/data/normal/data_OFF_OFF_OFF.csv")
trainset_orig$churn <- factor(trainset_orig$churn)

# Read the base learner predictions on the train set
df_top_max_acc<-read.csv(file = "scripts/preds/filtered_dataset_with_top_preds_max_ACCURACY.csv")
df_top_max_f1<-read.csv(file = "scripts/preds/filtered_dataset_with_top_preds_max_F1.csv")
df_notop_max_acc<-read.csv(file = "scripts/preds/filtered_dataset_without_top_preds_max_ACCURACY.csv")
df_notop_max_f1<-read.csv(file = "scripts/preds/filtered_dataset_without_top_preds_max_F1.csv")

# Load top performer or not
if(WITH_TOP_PERFORMER)
{
  df<-df_top_max_f1
} else{
  df<-df_notop_max_f1
} 
  
base_learners<-setdiff(names(df),targetName)
if(WITH_IMP_FEATURES)
{
  df<-cbind(df , trainset_orig[,features_to_include_from_orig_dataset])
}
df<-target_to_the_end(df,"churn")




#------------------- Train the second layer
library(h2o)
h2o.init(nthreads = -1)

train_h2o <- as.h2o(df)

y <- targetName
x <- setdiff(names(train_h2o), y)
train_h2o[,y] <- as.factor(train_h2o[,y])

my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train_h2o,
                  distribution = "bernoulli",
                  ntrees = 100,
                  max_depth = 5,
                  col_sample_rate = 1,
                  learn_rate = 0.1,
                  seed = 123)
my_gbm_max_f1_thresh <- my_gbm@model$training_metrics@metrics$max_criteria_and_metric_scores$threshold[1]

my_glm <- h2o.glm(x = x,
                  y = y,
                  training_frame = train_h2o,
                  family='binomial',
                  seed = 123)





#-------------- Train the base learners on the whole train data

if(!BASE_LEARNER_PARAMETERS_KNOWN)
  {

print("---------------------------")
df_thresholds <- read.csv(file = "scripts/preds/ACCURACY_F1_RESULTS.csv")

# Extract the base learner parameters
i<-1
while(i<length(base_learners)+1)
{
  model_type<-get_mod_type(base_learners[i])
  model_id<-get_mod_id(base_learners[i])
  
  fileName<-paste0("scripts/preds/",model_type,"_params.csv")
  model_params<-read.csv(file = fileName)
  print(base_learners[i])
  print(model_params[model_id+1,])
  
  print(paste0("Threshold: ",df_thresholds[ df_thresholds[,"X"]==base_learners[i]     ,"f1_threshold"]))
  print("---------------------------")
  
  i<-i+1
}


}else{
  #-------- TRAINING BASE LEARNERS ON THE WHOLE TRAINING SET + TEST
  library(xgboost)
  # Fix data first
  test_h2o<-as.h2o(testset)
  train_orig_h2o<-as.h2o(trainset_orig)
  N_target<-ncol(trainset_orig)
  y <- targetName
  x <- setdiff(names(train_orig_h2o), y)
  trainset_orig$churn<-as.integer(as.factor(trainset_orig$churn))-1
  # Reordering the columns to fit the exact order of training set
  xgb_newdata <- testset[names(trainset_orig)]
  
  
  # If we include best base learner
  if(WITH_TOP_PERFORMER)
  {
    b0 <-  xgboost(data = as.matrix(trainset_orig[-N_target]),
                   label =  trainset_orig$churn, nrounds = 100,
                   eval_metric = 'auc',
                   max.depth = 5,
                   eta = 0.1,
                   colsample_bytree=0.8,
                   nthread = 8,  objective = "binary:logistic", verbose = FALSE)
    b0_pred = predict(b0, newdata = as.matrix(xgb_newdata[-N_target]))
    b0_pred<-as.data.frame(as.numeric(b0_pred>0.15848655026892))
    
  }
    # XGB_17
   
  
    b1 <-  xgboost(data = as.matrix(trainset_orig[-N_target]),
               label =  trainset_orig$churn, nrounds = 150,
               eval_metric = 'auc',
               max.depth = 10,
               eta = 0.3,
               colsample_bytree = 0.8,
               nthread = 8,  objective = "binary:logistic", verbose = FALSE)
    b1_pred = predict(b1, newdata = as.matrix(xgb_newdata[-N_target]))
    b1_pred<-as.data.frame(as.numeric(b1_pred>0.140713699162006))

# GBM 
#b2 <- h2o.gbm(x = x,
#              y = y,
#              training_frame = train_h2o,
#              distribution = "bernoulli",
#              ntrees = 100,
#              max_depth = 5,
#              col_sample_rate = 0.8,
#              learn_rate = 0.1,
#              seed = 123)


# RF_0  
b2 <-  h2o.randomForest(x = x,
                        y = y,
                        training_frame = train_orig_h2o,
                        max_depth = 3,
                        ntrees=50,
                        col_sample_rate_per_tree = 0.8,
                        seed = 123)

b2_pred = h2o.predict(b2, newdata = test_h2o)[,3]
b2_pred<-b2_pred>0.111617708068628

# GLM_1 
b3 <- h2o.glm(
  training_frame=train_orig_h2o, 
  x=x, 
  y=y, 
  solver= "L_BFGS",
  alpha = 0,
  lambda = 0,
  family='binomial',
  seed=123
) 
b3_pred = h2o.predict(b3, newdata = test_h2o)[,3]
b3_pred<-b3_pred>0.13008742023537

Nfeats<-ncol(train_orig_h2o)-1
layers<-3
midLayers<-rev(round((Nfeats)*seq(1:layers)/(layers+1)))

# NN_1  
b4 <-   h2o.deeplearning(
  training_frame=train_orig_h2o, 
  x=x, 
  y=y, 
  hidden=midLayers,   
  epochs=50,             
  l1=0,
  l2=0,
 seed=123
)
b4_pred = h2o.predict(b4, newdata = test_h2o)[,3]
b4_pred<-b4_pred>0.138540887614756


layers<-3
midLayers<-rev(round((Nfeats)*seq(1:layers)/(layers+1)))

#NN_2
b5 <-   h2o.deeplearning(
  training_frame=train_orig_h2o, 
  x=x, 
  y=y, 
  hidden=midLayers,   
  epochs=100,             
  l1=0,
  l2=0,
  seed=123
) 

b5_pred = h2o.predict(b5, newdata = test_h2o)[,3]
b5_pred<-b4_pred>0.123947399630202



layers<-3
midLayers<-rev(round((Nfeats)*seq(1:layers)/(layers+1)))

#NN_3
b6 <-   h2o.deeplearning(
  training_frame=train_orig_h2o, 
  x=x, 
  y=y, 
  hidden=midLayers,   
  epochs=30,             
  l1=1e-4,
  l2=1e-4,
  seed=123
) 
  
  
b6_pred = h2o.predict(b6, newdata = test_h2o)[,3]
b6_pred<-b4_pred>0.140104953352592




# Stacking all the predictions
if(WITH_TOP_PERFORMER)
{
  if(WITH_IMP_FEATURES)
  {
  df_test_layer_2<-cbind(testset[,features_to_include_from_orig_dataset],as.vector(b0_pred),as.vector(b1_pred),as.vector(b2_pred),as.vector(b3_pred),as.vector(b4_pred),as.vector(b5_pred),as.vector(b6_pred))
  }else{
    df_test_layer_2<-cbind(as.vector(b0_pred),as.vector(b1_pred),as.vector(b2_pred),as.vector(b3_pred),as.vector(b4_pred),as.vector(b5_pred),as.vector(b6_pred))
    
  }
}else{
  if(WITH_IMP_FEATURES)
  {
  df_test_layer_2<-cbind(testset[,features_to_include_from_orig_dataset],as.vector(b1_pred),as.vector(b2_pred),as.vector(b3_pred),as.vector(b4_pred),as.vector(b5_pred),as.vector(b6_pred))
  }else{
    df_test_layer_2<-cbind(as.vector(b1_pred),as.vector(b2_pred),as.vector(b3_pred),as.vector(b4_pred),as.vector(b5_pred),as.vector(b6_pred))
    
  }
  
  }
if(WITH_IMP_FEATURES)
{
names(df_test_layer_2)<-c(features_to_include_from_orig_dataset,base_learners)
}
else{
  names(df_test_layer_2)<-c(base_learners)
}
# Reordering columns
df_test_layer_2<-df_test_layer_2[names(df)[1:ncol(df)-1]]
# Converting actual for ModelMetrics
actual<-as.double(testset$churn)
df_test_layer_2_h2o<-as.h2o(df_test_layer_2)
predictions_GBM<-h2o.predict(my_gbm, newdata = df_test_layer_2_h2o)
predictions_GBM<-predictions_GBM[,3]>my_gbm_max_f1_thresh

predictions_GLM<-h2o.predict(my_glm, newdata = df_test_layer_2_h2o)[,1]

f1_GBM<-ModelMetrics::f1Score(actual,as.double(as.vector(predictions_GBM)))
f1_GLM<-ModelMetrics::f1Score(actual,as.double(as.vector(predictions_GLM[["predict"]])))



df_pred_res<-cbind(df_test_layer_2,actual)
df_pred_res<-cbind(df_pred_res,GBM=as.vector(predictions_GBM))
df_pred_res<-cbind(df_pred_res,GLM=as.vector(predictions_GLM[["predict"]]))

print("---------------------------------")
print(paste0("f1_GBM  f1 : ",f1_GBM))
print(paste0("f1_GLM  f1 : ",f1_GLM))
print("---------------------------------")
if(WITH_TOP_PERFORMER)
{
print(paste0("XGB_8  f1 : ",ModelMetrics::f1Score(actual,as.double(df_test_layer_2$CV_XGB_8))))
}

print(paste0("XGB_17  f1 : ",ModelMetrics::f1Score(actual,as.double(df_test_layer_2$CV_XGB_17))))
print(paste0("GLM_1  f1 : ",ModelMetrics::f1Score(actual,as.double(df_test_layer_2$CV_GLM_1))))
print(paste0("NN_1  f1 : ",ModelMetrics::f1Score(actual,as.double(df_test_layer_2$CV_NN_1))))
print(paste0("RF_0  f1 : ",ModelMetrics::f1Score(actual,as.double(df_test_layer_2$CV_RF_0))))
print(paste0("NN_2  f1 : ",ModelMetrics::f1Score(actual,as.double(df_test_layer_2$CV_NN_2))))
print(paste0("NN_14  f1 : ",ModelMetrics::f1Score(actual,as.double(df_test_layer_2$CV_NN_3))))


}
