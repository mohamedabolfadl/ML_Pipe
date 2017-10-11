#### Pipeline to automate data cleaning, feature compression, feature importance, and metrics display

# Remove everything from Memory
rm(list = ls())
#------------ FUNCTIONS
# Formatting the dates to extract:
# - Extract day, month, and short and long year
# - Day count in year
# - Day of the week
# - Weekend
# - Quarter
Feature_Engineer_Dates <- function(data_set, remove_original_date=TRUE) {
  require(lubridate)
  data_set <- data.frame(data_set)
  date_features <- names(data_set[sapply(data_set, is.Date)])
  for (feature_name in date_features) {
    data_set[,paste0(feature_name,'_DateInt')] <- as.numeric(data_set[,feature_name])
    data_set[,paste0(feature_name,'_Month')] <- as.integer(format(data_set[,feature_name], "%m"))
    #data_set[,paste0(feature_name,'_ShortYear')] <- as.integer(format(data_set[,feature_name], "%y"))
    data_set[,paste0(feature_name,'_LongYear')] <- as.integer(format(data_set[,feature_name], "%Y"))
    data_set[,paste0(feature_name,'_Day')] <- as.integer(format(data_set[,feature_name], "%d"))
    # week day number requires first pulling the weekday label, creating the 7 week day levels, and casting to integer
    data_set[,paste0(feature_name,'_WeekDayNumber')] <- as.factor(weekdays(data_set[,feature_name]))
    levels(data_set[,paste0(feature_name,'_WeekDayNumber')]) <- list(Monday=1, Tuesday=2, Wednesday=3, Thursday=4, Friday=5, Saturday=6, Sunday=7)
    data_set[,paste0(feature_name,'_WeekDayNumber')] <- as.integer(data_set[,paste0(feature_name,'_WeekDayNumber')])
    data_set[,paste0(feature_name,'_IsWeekend')] <- as.numeric(grepl("Saturday|Sunday", weekdays(data_set[,feature_name])))
    data_set[,paste0(feature_name,'_YearDayCount')] <- yday(data_set[,feature_name])
    data_set[,paste0(feature_name,'_Quarter')] <- lubridate::quarter(data_set[,feature_name], with_year = FALSE)
    #data_set[,paste0(feature_name,'_Quarter')] <- lubridate::quarter(data_set[,feature_name], with_year = TRUE)
    if (remove_original_date)
      data_set[, feature_name] <- NULL
  }
  return(data_set)
}




# Creating dummy variables automatically
Binarize_Features <- function(data_set, features_to_ignore=c(), leave_out_one_level=FALSE) {
  text_features <- c(names(data_set[sapply(data_set, is.character)]), names(data_set[sapply(data_set, is.factor)]))
  print(text_features)
  for (feature_name in setdiff(text_features, features_to_ignore)) {
    feature_vector <- as.character(data_set[[feature_name]])
    # check that data has more than one level
    if (length(unique(feature_vector)) == 1)
      #print(feature_name)
      next
    # We set any non-data to text
    feature_vector[is.na(feature_vector)] <- 'NA'
    feature_vector[is.infinite(feature_vector)] <- 'INF'
    feature_vector[is.nan(feature_vector)] <- 'NAN'
    # loop through each level of a feature and create a new column
    first_level=TRUE
    for (newcol in unique(feature_vector)) {
      if (first_level && leave_out_one_level) {
        # avoid dummy trap and skip first level
        first_level=FALSE
      } else {
        data_set[,paste0(feature_name,"_",newcol)] <- ifelse(feature_vector==newcol,1,0)
      }
    }
    # remove original feature
    data_set <- data_set[,setdiff(names(data_set),feature_name)]
  }
  return (data_set)
}


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



# Get correlation matrix and plot it
Get_Fast_Correlations <- function(data_set, features_to_ignore=c(), size_cap=5000) {
  require(dplyr)
  require(reshape2)
  data_set <- data_set[,setdiff(names(data_set), features_to_ignore)]
  if (size_cap > nrow(data_set)) {
    data_set = data_set[sample(nrow(data_set), size_cap),]
  } else {
    data_set = data_set[sample(nrow(data_set), nrow(data_set)),]
  }
  d_cor <- as.matrix(cor(data_set))
  d_cor_melt <- arrange(melt(d_cor), -(value))
  # clean up
  pair_wise_correlation_matrix <- filter(d_cor_melt, Var1 != Var2)
  pair_wise_correlation_matrix <- filter(pair_wise_correlation_matrix, is.na(value)==FALSE)
  # remove pair dups
  dim(pair_wise_correlation_matrix)
  pair_wise_correlation_matrix <- pair_wise_correlation_matrix[seq(1, nrow(pair_wise_correlation_matrix), by=2),
                                                               ]
  dim(pair_wise_correlation_matrix)
  png("figs/Correlations_plot.png")
  
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
  # strip var names to index for pair-wise identification
  names(data_set) <- seq(1:ncol(data_set))
  # calculate correlation and significance numbers
  cor_data_df <- corr.test(data_set)
  
  # apply var names to correlation matrix over index
  rownames(cor_data_df$r) <- feature_names
  colnames(cor_data_df$r) <- feature_names
  png("figs/Correlations_matrix.png")
  corrplot.mixed(cor_data_df$r, lower="circle", upper="color",
                 tl.pos="lt", diag="n", order="hclust", hclust.method="complete")
  dev.off()
  # top cor and sig
  relationships_set <- cor_data_df$ci[, c('r', 'p')]
  # apply var names to data over index pairs
  relationships_set$feature_1 <-feature_names[as.numeric(sapply(strsplit(rownames(relationships_set), "-"), `[`, 1))]
  relationships_set$feature_2 <-feature_names[as.numeric(sapply(strsplit(rownames(relationships_set), "-"), `[`, 2))]
  relationships_set <-select(relationships_set, feature_1, feature_2, r, p) %>% rename(correlaton =r, pvalue = p)
  # return only the most insteresting relationships
  # return only top correlations or p values at some threshold etc
  #return(filter(relationships_set,abs(correlaton) > correlation_abs_threshold | pvalue < pvalue_threshold ) %>% arrange(pvalue)) 
  return(filter(relationships_set,abs(correlaton) > correlation_abs_threshold  ) %>% arrange(desc(abs(correlaton))))
  
}

saveHistogram <- function(dataset,featureName)
{
  if( class(dataset[[featureName]])=="character")
  {
    png(filename = paste("figs/hist_", featureName, ".png"))
    barplot(prop.table(table(dataset[[featureName]])),main=paste("Histogram for ", featureName))
    dev.off()
  }
  if( class(dataset[[featureName]])=="numeric")
  {
    
    h = hist(as.matrix(dataset[, featureName]))
    h$density = h$counts / sum(h$counts) * 100
    png(filename = paste("figs/hist_", featureName, ".png"))
    plot(
      h,
      freq = FALSE,
      ylim = c(0, 100),
      main = paste("Histogram for ", featureName),
      xlab = "",
      ylab = "[%] of Total"
    )
    dev.off()
  }
  
  
}

# Libraries
library(dplyr)
library(lubridate)
#-------- PARAMETER TUNING -----------
# Input variables
fileName = "data.csv"
targetName = "churn"
targetClasses = 2 # Number of classes in target. E.g. 2 for binary
MAX_NUMBER_OF_ROWS_TO_READ = -1 # -1 for all
CATEGORIAL_NA_STRATEGY = "KNN" # Options are "KNN" or "Other" for NA as separate category
N_most_popular = 10  # Capping the categories to the N most popular categories
balanceThreshold = 0.85 # The threshold of negative to positive data above which we conside the data unbalanced and resort to balancing techniqques
BALANCE_TECHNIQUE = "SMOTE" # Strategy to balance the data to be roughly equal. Options are "SMOTE", "KNN", "RANDOM"
FEATURE_COMPRESSION_TECHNIQUE = "PCA" # Strategy to reduce the feature space to reduce overfitting. Options are "PCA" and "CORR"
PCA_info_thresh = 0.95  # Maximum components to keep to preserve the information. make -1 to prefer choosing number of components
PCA_max_components = 10 # Maximum number of features to be selected outside the PCA. This is disabled if PCA_info_threshis >0
CORR_threshold = 0.7 # Threshold of cross correlation above which features are dropped in case "CORR" is used
Cuttoff_Missing_data = 80 # Percentage above which we exclude the feature being mostly missing
Cuttoff_Imputatio_data = 80  # Percentage under which the imputation uses simple techniques such as mean or mode
excluded_category_feature = "id" # The feature which should be excluded from categorial analysis, usually this is the id of the user which has no predictive power
PERCENT_UNIQUE_FEATS = 1  # Features which are more than this % are chosen, the rest are dropped as they are not unique

# Model parameters
L1 = 0.0001
KNN_k = 5
RF_nTrees = 50
NN_layers = c(200, 200)
NN_activation = "TahnWithDropout"
NN_dropOut = 0.1
GB_nTrees = 50





#--------- READING DATA ------------
library(readr)
if (MAX_NUMBER_OF_ROWS_TO_READ < 1)
{
  dataset <- read_csv(fileName)
} else {
  dataset <- read_csv(fileName, n_max = MAX_NUMBER_OF_ROWS_TO_READ)
}
dataset[,excluded_category_feature]<-NULL

str(dataset)
dataset_orig <- dataset
featureNames <- names(dataset)
# Creating analytics dataframe
dataset_an <-data.frame(matrix(, nrow = length(names(dataset)), ncol = 0))
rownames(dataset_an) <- names(dataset)
dims = dim(dataset)


#---------  FEATURE TYPES ANALYTICS ----
# Appending the results of data types
dataset_an$types = lapply(dataset, class)


#--------- MISSING DATA ANALYTICS ----
na_count = numeric(dims[2])
i <- 1
for (col in names(dataset))
{
  na_count[i] = 100.0 * sum(is.na(dataset[, col])) / dims[1]
  i <- i + 1
}
# Appending the results of missing values
dataset_an <- cbind(dataset_an, NA_Perc = na_count)




#-------- IMPUTING MISSING DATA  -----

# Get the NA data and initialize imputed data
vars <- rownames(dataset_an)
dataset_imputed <- dataset

# Subdivide features into 3 categories

# 1. Above threshold of imputing missing
remove_Imputation_features <- vars[which(dataset_an$NA_Perc > Cuttoff_Missing_data, arr.ind =TRUE)]
dataset_imputed[,remove_Imputation_features] <- NULL

# 2. below threshold of going through kNN or RF imputation
simple_Imputation_features <- vars[which(   (dataset_an$NA_Perc < Cuttoff_Imputatio_data) & (dataset_an$NA_Perc > 0), arr.ind =TRUE)]
numericList <- simple_Imputation_features[sapply(dataset[,simple_Imputation_features],is.numeric)]
nonNumericList <- setdiff(simple_Imputation_features,numericList)
# 2.1 Substituting numerics with their mean
for (numFeat in numericList)
{
  dataset_imputed[[numFeat]] <- ifelse(is.na(dataset[[numFeat]]), ave(dataset[[numFeat]], FUN = function(x) mean(x, na.rm = TRUE)), dataset[[numFeat]])
}

# 2.2 Substituting categorial with their mode
for (nonnumFeat in nonNumericList)
{
featCol <- dataset[,nonnumFeat]
featCol <- featCol[!is.na(featCol),]
if( is.Date(dataset[[nonnumFeat]]) )
{
  md<-as.Date(Mode(as.matrix(featCol)),format = "%Y-%m-%d")
}else{
  
  md<-Mode(as.matrix(featCol))
}

dataset_imputed[is.na(dataset_imputed[,nonnumFeat]),nonnumFeat]<-md
}
str(dataset_imputed)

# 3 advanced imputations [For future improvements]
#advanced_Imputation_features <- vars[which(   (dataset_an$NA_Perc > Cuttoff_Imputatio_data) & (dataset_an$NA_Perc < Cuttoff_Missing_data), arr.ind =TRUE)]
#numericList <- advanced_Imputation_features[sapply(dataset[,advanced_Imputation_features],is.numeric)]
#nonNumericList <- setdiff(advanced_Imputation_features,numericList)
# 3.1 Processing numerics
#for (numFeat in numericList)
#{
#  dataset_imputed[[numFeat]] <- ifelse(is.na(dataset[[numFeat]]), ave(dataset[[numFeat]], FUN = function(x) mean(x, na.rm = TRUE)), dataset[[numFeat]])
#}



#---------- DISTRIBUTIONS -------------

dataset <- dataset_imputed
dims <- dim(dataset_orig)

rm(dataset_imputed)

# Getting Number of categories of character features
category_count <- numeric(dims[2])-1
i<-1
for (name in colnames(dataset))
{
  if (lapply(dataset[,name],class)=="character")
  {
    category_count[i]=length(unique(as.matrix(dataset[,name])))
    
    if(category_count[i]>N_most_popular && name!=excluded_category_feature)
    {
      print(paste("The feature ",name,"has",category_count[i]," classes hence will be capped"))
      temp_vect<-dataset[[name]]
      temp_vect <- data.frame(table(temp_vect)) %>% arrange(desc(Freq)) %>% head(N_most_popular-1)
      dataset[,name] <- ifelse(dataset[[name]] %in% temp_vect$temp_vect,dataset[[name]], 'Other')
    }
    
    
  }  else{
    category_count[i]=0
    
  }
  i<-i+1
}
dataset_an <- cbind(dataset_an, Number_categories = category_count)


#----------------- SAVING THE HISTOGRAMS

for (name in names(dataset))
{
  saveHistogram(dataset,name)
  
}


#------------------- CORRELATIONS -------------------------------

# Getting numeric features only
feats <- colnames(dataset)
numericList <- feats[sapply(dataset[,feats],is.numeric)]

lis1<-Get_Fast_Correlations(dataset[,numericList])
top_correlated_numeric_features<-Get_Top_Relationships(dataset[,numericList],correlation_abs_threshold=CORR_threshold)

#------------------ FACTORING NON-NUMERIC DATA
dataset_binarized<-Binarize_Features(dataset,leave_out_one_level=TRUE)
dataset_binarized_dates<-Feature_Engineer_Dates(dataset_binarized)
rm(dataset_binarized)

# Binding target at the end of the dataframe
tmp <-data.frame(dataset_binarized_dates[,targetName])
colnames(tmp)<-targetName
dataset_binarized_dates[,targetName]<-NULL
dataset <- cbind(dataset_binarized_dates, tmp)
rm(dataset_binarized_dates)


str(dataset)
#----------- BALANCING THE DATASET
# Libraries: 
# ROSE
# unbalanced


# Distribution of the target
# First check if target name is correct
featureNames = names(dataset)
if (!(targetName %in% featureNames))
{
  print(paste(
    "Variable ",
    targetName,
    " not present in data set. Could it be ",
    tail(featureNames, n = 1)
  ))
  
  stop("-------------- Exiting ------------------------------")
}



# Deciding if the data is imbalanced or not
target_hist <-hist(as.matrix(dataset[, targetName]), nclass = targetClasses)
targ_weights <- target_hist[2]
if (targetClasses == 2)
{
  majClassRatio = (
    max(targ_weights$counts[1], targ_weights$counts[2]) / (targ_weights$counts[1] + targ_weights$counts[2])
  )
  if (majClassRatio > balanceThreshold)
  {
    print(paste("Target is imbalanced. Ratio of majority class is", 100 * majClassRatio,"%"))
    IMBALANCED_TARGET = TRUE
    
  }
  else
  {
    print(paste(
      "Target balance is OK. Ratio of majority class is",
      100 * majClassRatio,
      "%"
    ))
    IMBALANCED_TARGET = FALSE
  }
  
}

featureNames <- names(dataset)
  
if(IMBALANCED_TARGET)
{
  library(unbalanced)
  input <- dataset[,setdiff(featureNames,targetName)]
  output <- factor(dataset[,targetName])
  #output <- factor(dataset$churn)
  
  
  data<-ubBalance(X= input, Y=output, type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
  balancedData<-cbind(data$X,data$Y)
  colnames(balancedData)[length(colnames(balancedData))]<-targetName
  dataset<-balancedData
  
}

rm(balancedData,dataset_binarized,dataset_binarized_dates,dataset_imputed,featCol,input,lis1,temp_vect,tmp,data)

#------------ Feature reduction

# Remove near 0 variance columns
nzv <- nearZeroVar(dataset,saveMetrics=TRUE)
# Filtering according to % unique. If unique is more than 1%
near_constant_feats<-rownames(nzv[nzv$zeroVar,])
dataset[,near_constant_feats]<-NULL

# PCA
if(FEATURE_COMPRESSION_TECHNIQUE=="PCA")
{
  
  
  
  library(caret)
  preprocessParams <- preProcess(dataset[,1:length(names(dataset))-1], method=c("center", "scale", "pca"), thresh = PCA_info_thresh)
  #preprocessParams <- preProcess(test_dataset_num, method=c("center", "scale", "pca"), pcaComp =3)
  # summarize transform parameters
  print(preprocessParams)
  # transform the dataset using the parameters
  feat_comp <- predict(preprocessParams, dataset[,1:length(names(dataset))-1])
  # summarize the transformed dataset
  summary(feat_comp)
  dataset_comp<-cbind(feat_comp,dataset[,targetName])
  colnames(dataset_comp)[length(colnames(dataset_comp))]<-targetName
}else{
  # Correlations
  
  
}


