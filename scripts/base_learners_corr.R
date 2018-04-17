

# Script to compute the correlations of the base learners


rm(list = ls())


# Number of base learners without including top learner
N_baselearners<-6

INCLUDE_KNN <- FALSE

insert_model_name<-function(dataset,id)
{
  i<-1
  dataset$model_id<-0
  while(i<(nrow(dataset)+1))
  {
    dataset[i,"model_id"]<-paste0(id,"_",i-1)
    
    i<-i+1
  }
  return(dataset)
}

# Get correlation matrix and plot it
Get_Fast_Correlations <- function(data_set, features_to_ignore=c(), size_cap=50000) {
  require(dplyr)
  require(reshape2)
  data_set <- data_set[,setdiff(names(data_set), features_to_ignore)]
  if (size_cap < nrow(data_set)) {
    data_set = data_set[sample(nrow(data_set), size_cap),]
  } else {
    data_set = data_set[sample(nrow(data_set), nrow(data_set)),]
  }
  d_cor <- as.matrix(cor(data_set))
  d_cor_melt <- arrange(melt(d_cor), -(value))
  pair_wise_correlation_matrix <- filter(d_cor_melt, Var1 != Var2)
  pair_wise_correlation_matrix <- filter(pair_wise_correlation_matrix, is.na(value)==FALSE)
  dim(pair_wise_correlation_matrix)
  pair_wise_correlation_matrix <- pair_wise_correlation_matrix[seq(1, nrow(pair_wise_correlation_matrix), by=2),]
  dim(pair_wise_correlation_matrix)
  png("figs/Correlations_plot_baselearners.png")
  plot(pair_wise_correlation_matrix$value)
  dev.off()
  return(pair_wise_correlation_matrix)
}




# Get the top pair correlations in order to exclude features which are correlated to each other
Get_Top_Relationships <-function(data_set,correlation_abs_threshold = 0.8,pvalue_threshold = 0.01) 
{
  require(psych)
  require(dplyr)
  require(corrplot)
  feature_names <- names(data_set)
  names(data_set) <- seq(1:ncol(data_set))
  cor_data_df <- corr.test(data_set)
  rownames(cor_data_df$r) <- feature_names
  colnames(cor_data_df$r) <- feature_names
  png("figs/Correlations_matrix_baselearners.png")
  corrplot.mixed(cor_data_df$r, lower="circle", upper="color",
                 tl.pos="lt", diag="n", order="hclust", hclust.method="complete")
  dev.off()
  relationships_set <- cor_data_df$ci[, c('r', 'p')]
  relationships_set$feature_1 <-feature_names[as.numeric(sapply(strsplit(rownames(relationships_set), "-"), `[`, 1))]
  relationships_set$feature_2 <-feature_names[as.numeric(sapply(strsplit(rownames(relationships_set), "-"), `[`, 2))]
  relationships_set <-select(relationships_set, feature_1, feature_2, r, p) %>% dplyr::rename(correlaton =r, pvalue = p)
   return(filter(relationships_set,abs(correlaton) < correlation_abs_threshold  ) %>% arrange(abs(correlaton)))
  
  
}

# Threshold for removing the high correlation base learners
max_correlation_1 <- 0.75

# Maximum correlation to consider for final stage of model selection
maximum_correlation_2 <- 0.4

library(readr)
library(caret)
library(ModelMetrics)


#Loading the predictions of all algorithms
XGB_preds <-read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/scripts/preds/XGB_preds.csv")
GBM_preds <-read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/scripts/preds/GBM_preds.csv")
GLM_preds <-read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/scripts/preds/GLM_preds.csv")
NN_preds <-read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/scripts/preds/NN_preds.csv")
if(INCLUDE_KNN)
{
  KNN_preds <-read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/scripts/preds/KNN_preds.csv")
}
RF_preds <-read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/scripts/preds/RF_preds.csv")

# Combining all predictions in one huge matrix
comb_res<-as.data.frame(XGB_preds$churn)
names(comb_res)<-"churn"
comb_res<-cbind(comb_res, XGB_preds[,2:(ncol(XGB_preds)-1)])
comb_res<-cbind(comb_res, GBM_preds[,2:(ncol(GBM_preds)-1)])
comb_res<-cbind(comb_res, GLM_preds[,2:(ncol(GLM_preds)-1)])
comb_res<-cbind(comb_res, NN_preds[,2:(ncol(NN_preds)-1)])
if(INCLUDE_KNN)
{
  comb_res<-cbind(comb_res, KNN_preds[,2:(ncol(KNN_preds)-1)])
}

comb_res<-cbind(comb_res, RF_preds[,2:(ncol(RF_preds)-1)])

# Throwing the target to the last column
tmp<-comb_res$churn
comb_res$churn<-NULL
comb_res_orig<-comb_res
comb_res_orig$churn<-tmp

# Remove high correlations
preprocessParams <- preProcess(comb_res, method=c("corr"), cutoff = max_correlation_1)
CORR_comp <- predict(preprocessParams, comb_res)
comb_res<-CORR_comp
limited_list<-names(CORR_comp)

# Removing the "CV_" part in the names of the models
i<-1
while(i<length(limited_list)+1)
{
  limited_list[i]<-substr(limited_list[i],4,nchar(limited_list[i]) )
  
  i<-i+1
}

XGB_perf <-insert_model_name( read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/scripts/preds/XGB_params.csv"),"XGB")
GBM_perf <-insert_model_name( read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/scripts/preds/GBM_params.csv"),"GBM")
GLM_perf <-insert_model_name( read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/scripts/preds/GLM_params.csv"),"GLM")
NN_perf <-insert_model_name( read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/scripts/preds/NN_params.csv"),"NN")
if(INCLUDE_KNN)
{
KNN_perf <-insert_model_name(read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/scripts/preds/KNN_params.csv"),"KNN")
}
RF_perf <-insert_model_name( read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/scripts/preds/RF_params.csv"),"RF")

XGB_perf[, setdiff(names(XGB_perf), c("AUC","model_id"))  ]<-NULL
GBM_perf[, setdiff(names(GBM_perf), c("AUC","model_id"))  ]<-NULL
GLM_perf[, setdiff(names(GLM_perf), c("AUC","model_id"))  ]<-NULL
NN_perf[, setdiff(names(NN_perf), c("AUC","model_id"))  ]<-NULL
if(INCLUDE_KNN)
{
KNN_perf[, setdiff(names(KNN_perf), c("AUC","model_id"))  ]<-NULL
}
RF_perf[, setdiff(names(RF_perf), c("AUC","model_id"))  ]<-NULL

print("A")

comb_perf <- data.frame(model_id=XGB_perf$model_id,AUC=XGB_perf$AUC, stringsAsFactors=FALSE)
comb_perf <-rbind(comb_perf, GBM_perf)
comb_perf <-rbind(comb_perf, GLM_perf)
comb_perf <-rbind(comb_perf, NN_perf)
if(INCLUDE_KNN)
{
  comb_perf <-rbind(comb_perf, KNN_perf)
}
  comb_perf <-rbind(comb_perf, RF_perf)


comb_perf_short_listed<-comb_perf[comb_perf[,"model_id"] %in% limited_list,]

# Gives a list of the base learners, ranked according to their correlations
least_correlated_learners<-Get_Top_Relationships(comb_res,correlation_abs_threshold=maximum_correlation_2)

# All the correlations between all the considered base learners
baselearner_correlations<-Get_Fast_Correlations(comb_res)

# Initialize the score of each pair of base learners
least_correlated_learners$pvalue<-NULL
least_correlated_learners$auc1<-0
least_correlated_learners$auc2<-0
least_correlated_learners$score<-0


print("B")

i<-1
Npairs<-nrow(least_correlated_learners)
while(i<Npairs+1)
{
  model1<-as.character(least_correlated_learners[i,"feature_1"])
  model2<-as.character(least_correlated_learners[i,"feature_2"])
  model1<-substr(model1,4,nchar(model1))
  model2<-substr(model2,4,nchar(model2))
  
  least_correlated_learners[i,"auc1"]<- comb_perf[ comb_perf[,"model_id"]==model1  , "AUC" ]
  least_correlated_learners[i,"auc2"]<- comb_perf[ comb_perf[,"model_id"]==model2  , "AUC" ]
  
  least_correlated_learners[i,"score"] <- ((least_correlated_learners[i,"auc1"]+least_correlated_learners[i,"auc2"]-1)/least_correlated_learners[i,"correlaton"])
  
  if(FALSE)
    {
  model1<-substr(model1,4,nchar(model1))
  model2<-substr(model2,4,nchar(model2))
  pos1 = regexpr('_', model1)
  pos2 = regexpr('_', model2)
  model1_id<-as.numeric(substr(model1,pos1+1,nchar(model1)))
  model2_id<-as.numeric(substr(model2,pos2+1,nchar(model2)))
  model1_type<-substr(model1,1,pos1-1)
  model2_type<-substr(model2,1,pos2-1)
  }
  
  
  i<-i+1
}




print("C")




df_result<-least_correlated_learners[with(least_correlated_learners, order(-score)), ]

i<-2
flag<-TRUE

models_base = c(df_result[1,"feature_1"],df_result[1,"feature_2"])
N_learners<-2
while(i<nrow(df_result)+1  & flag)
{
  md1<-df_result[i,"feature_1"]
  md2<-df_result[i,"feature_2"]
  
  if(!(md1 %in% models_base))
  {
    models_base = c(models_base,md1)
    N_learners<-N_learners+1
    
  }
  if(!(md2 %in% models_base))
  {
    models_base = c(models_base,md2)
    N_learners<-N_learners+1
  }
  
  if(N_learners>N_baselearners || N_learners==N_baselearners)
  {
    flag = FALSE
    
  }
  
  
  i<-i+1
}

print("D")

# Appending best learner if not already there
comb_perf_sorted<-comb_perf[with(comb_perf, order(-AUC)), ]

print("Chosen models without top model")
print(models_base)

models_base_with_top<-models_base
best_learner<-comb_perf_sorted[1,"model_id"]

if(!(best_learner %in% models_base_with_top))
{
  models_base_with_top = c(models_base_with_top,paste0("CV_",best_learner))

  
}

print("Chosen models with top model")

print(models_base_with_top)

# Saving the pobability predictions of each base learner

# With the top model included
dataset_layer1<-comb_res_orig[,c(models_base_with_top,"churn")]
write.csv(dataset_layer1, file = "scripts/preds/filtered_dataset_with_top_probs.csv",row.names = FALSE)

# Without top model
dataset_layer2<-comb_res_orig[,c(models_base,"churn")]
write.csv(dataset_layer2, file = "scripts/preds/filtered_dataset_without_top_probs.csv",row.names = FALSE)


# Computing and saving the exact predictions
library(h2o)

h2o.init(nthreads=-1)
df_1<-as.h2o(dataset_layer1)
df_2<-as.h2o(dataset_layer2)

df_1$churn<-as.factor(df_1$churn)
df_2$churn<-as.factor(df_2$churn)

print("E")


# predictions with top learner included
df_max_accuracy_with_top<-df_1
df_max_f1_with_top<-df_1

# predictions without top learner
df_max_accuracy_without_top<-df_2
df_max_f1_without_top<-df_2

# Matrix storing the f1 and accuracy of each base learner
acc_f1_results<-data.frame(f1=numeric(length(names(dataset_layer1))-1),f1_threshold=numeric(length(names(dataset_layer1))-1),accuracy=numeric(length(names(dataset_layer1))-1),accuracy_threshold=numeric(length(names(dataset_layer1))-1), row.names = setdiff(names(dataset_layer1),"churn") )

print("F")

# Computing predictions of max accuracy and f1 with top model 
i<-1
while(i<ncol(df_1))
{
  
metrics<-h2o.make_metrics(df_1[,i],df_1$churn)
max_accuracy_threshold<-h2o.find_threshold_by_max_metric(metrics, "accuracy")
max_f1_threshold<-h2o.find_threshold_by_max_metric(metrics, "f1")

df_max_accuracy_with_top[,i]<-df_1[,i]> max_accuracy_threshold
df_max_f1_with_top[,i]<-df_1[,i]> max_f1_threshold

acc_f1_results[names(df_1[,i]),"accuracy"]<-h2o.accuracy(metrics,max_accuracy_threshold)
acc_f1_results[names(df_1[,i]),"f1"]<-h2o.F1(metrics,max_f1_threshold)

acc_f1_results[names(df_1[,i]),"accuracy_threshold"]<-max_accuracy_threshold
acc_f1_results[names(df_1[,i]),"f1_threshold"]<-max_f1_threshold


i<-i+1

}
print("G")


# Computing predictions of max accuracy and f1 without top model 
i<-1
while(i<ncol(df_2))
{
  
  metrics<-h2o.make_metrics(df_2[,i],df_2$churn)
  max_accuracy_threshold<-h2o.find_threshold_by_max_metric(metrics, "accuracy")
  max_f1_threshold<-h2o.find_threshold_by_max_metric(metrics, "f1")
  
  df_max_accuracy_without_top[,i]<-df_2[,i]> max_accuracy_threshold
  df_max_f1_without_top[,i]<-df_2[,i]> max_f1_threshold
  
  acc_f1_results[names(df_1[,i]),"accuracy"]<-h2o.accuracy(metrics,max_accuracy_threshold)
  acc_f1_results[names(df_1[,i]),"f1"]<-h2o.F1(metrics,max_f1_threshold)
  
  acc_f1_results[names(df_1[,i]),"accuracy_threshold"]<-max_accuracy_threshold
  acc_f1_results[names(df_1[,i]),"f1_threshold"]<-max_f1_threshold
  
  
  i<-i+1
  
}
print("H")


# Just to take a look at the predictions
t<-as.data.frame(df_max_f1_with_top)


# Saving the max accuracy and F1 
write.csv(acc_f1_results, file = "scripts/preds/ACCURACY_F1_RESULTS.csv",row.names = TRUE)

# Saving data frames with predictions including top base learner
write.csv(as.data.frame(df_max_accuracy_with_top), file = "scripts/preds/filtered_dataset_with_top_preds_max_ACCURACY.csv",row.names = FALSE)
write.csv(as.data.frame(df_max_f1_with_top), file = "scripts/preds/filtered_dataset_with_top_preds_max_F1.csv",row.names = FALSE)

# Saving data frames with predictions without top base learner
write.csv(as.data.frame(df_max_accuracy_without_top), file = "scripts/preds/filtered_dataset_without_top_preds_max_ACCURACY.csv",row.names = FALSE)
write.csv(as.data.frame(df_max_f1_without_top), file = "scripts/preds/filtered_dataset_without_top_preds_max_F1.csv",row.names = FALSE)






