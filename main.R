#### Pipeline to automate data cleaning, feature compression, feature importance, and metrics display

# Libraries
library(dplyr)

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
FEATURE_COMPRESSION_TECHNIQUE = "PCA" # Strategy to reduce the feature space to reduce overfitting. Options are "PCA" and "HCORR"
PCA_info_thresh = 0.95  # Maximum components to keep to preserve the information. make -1 to prefer choosing number of components
PCA_max_components = 10 # Maximum number of features to be selected outside the PCA. This is disabled if PCA_info_threshis >0
Cuttoff_Missing_data = 80 # Percentage above which we exclude the feature being mostly missing
excluded_category_feature = "id" # The feature which should be excluded from categorial analysis, usually this is the id of the user which has no predictive power

# Model parameters
L1 = 0.0001
KNN_k = 5
RF_nTrees = 50
NN_layers = c(200, 200)
NN_activation = "TahnWithDropout"
NN_dropOut = 0.1
GB_nTrees = 50

saveHistogram <- function(dataset,featureName)
{
  if( class(dataset[[featureName]])=="character")
  {
    png(filename = paste("hist_", featureName, ".png"))
    barplot(prop.table(table(dataset[[featureName]])),main=paste("Histogram for ", featureName))
    dev.off()
  }
  if( class(dataset[[featureName]])=="numeric")
    {
    
    h = hist(as.matrix(dataset[, featureName]))
    h$density = h$counts / sum(h$counts) * 100
    png(filename = paste("hist_", featureName, ".png"))
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



#--------- READING DATA ------------
library(readr)
if (MAX_NUMBER_OF_ROWS_TO_READ < 1)
{
  dataset <- read_csv(fileName)
} else {
  dataset <- read_csv(fileName, n_max = MAX_NUMBER_OF_ROWS_TO_READ)
}
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
install.packages("DMwR")
install.packages("VIM")
install.packages("bnstruct")

library(VIM)


#---------- DISTRIBUTIONS -------------

# Getting Number of categories of character features
category_count = numeric(dims[2])
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

# Removing features with missing values above threshold
vars <- rownames(dataset_an)
dataset[, vars[which(dataset_an$NA_Perc > Cuttoff_Missing_data, arr.ind =TRUE)]] <- NULL


# Distribution of the target
# First check if target name is correct
if (!(targetName %in% featureNames))
{
  print(paste(
    "Variable ",
    targetName,
    " not present in data set. Could it be ",
    tail(featureNames, n = 1)
  ))
  
  stop("-------------- Exitting ------------------------------")
}

# Deciding if the data is imbalanced or not
target_hist <-
  hist(as.matrix(dataset[, targetName]), nclass = targetClasses)
targ_weights <- target_hist[2]
if (targetClasses == 2)
{
  majClassRatio = (
    max(targ_weights$counts[1], targ_weights$counts[2]) / (targ_weights$counts[1] +
                                                             targ_weights$counts[2])
  )
  if (majClassRatio > balanceThreshold)
  {
    print(paste(
      "Target is imbalanced. Ratio of majority class is",
      100 * majClassRatio,
      "%"
    ))
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



for (name in names(dataset))
{
  saveHistogram(dataset,name)
  
}





