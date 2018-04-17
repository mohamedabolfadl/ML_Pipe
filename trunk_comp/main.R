#### Pipeline to automate data cleaning, feature compression, feature importance, and metrics display

# Remove everything from Memory
rm(list = ls())

# Shifting the column
shift_vec <- function(x, n){
 return( c( rep(NA, n), x[1:(length(x)-n)]))
}


# Outlier detection
Identify_Outliers <- function(data_set, features_to_ignore=c(),
                              outlier_sd_threshold = 3,
                              remove_outlier_observations = FALSE
                              )
  {
  # get standard deviation for each feature
  require(dplyr)
  outliers <- c()
  total_outliers<-0
 
  for (feature_name in setdiff(names(data_set),features_to_ignore)) {
    feature_mean <- mean(data_set[,feature_name], na.rm = TRUE)
    feature_sd <- sd(data_set[,feature_name], na.rm = TRUE)
    outlier_count <- sum( data_set[,feature_name] > (feature_mean + (feature_sd * outlier_sd_threshold)) |  data_set[,feature_name] < (feature_mean - (feature_sd * outlier_sd_threshold)))
    #print(paste0(feature_name,":",outlier_count))
    total_outliers<-total_outliers+outlier_count
    #print(feature_name)
    if (outlier_count > 0) 
      {
      outliers <- rbind(outliers, c(feature_name, outlier_count))
      
      if(remove_outlier_observations)
      {
        inds <-  data_set[,feature_name] > (feature_mean + (feature_sd * outlier_sd_threshold)) |  data_set[,feature_name] < (feature_mean - (feature_sd * outlier_sd_threshold))
        
        v = seq(1:length(inds))
        locs<-v[inds]
        #origRows <- nrow(data_set)
        data_set<-data_set[-locs,]
        #currentRows <- nrow(data_set)
        #print(sum(inds))
        #print(paste0(feature_name," reduced ",(origRows-currentRows)))
        
        }
      
      
      }
  }
  outliers <- rbind(outliers, c("Total", total_outliers))
  outliers <- data.frame(outliers) %>% dplyr::rename(feature_name=X1, outlier_count=X2) %>%
    mutate(outlier_count=as.numeric(as.character(outlier_count))) %>% arrange(desc(outlier_count))
  
  
  if (remove_outlier_observations) {
    return(data_set)
  } else {
    return(outliers)
  }
}

# Column operations
mult_columns <- function(col1,col2)
{
    return(as.numeric(col1)*as.numeric(col2))
}
sum_columns <- function(col1,col2)
{
  return(as.numeric(col1)+as.numeric(col2))
}
sub_columns <- function(col1,col2)
{
  return(as.numeric(col1)-as.numeric(col2))
}
log_columns <- function(col,minVal)
{
    return(log(1+as.numeric(col)-as.numeric(minVal)))
}


# Save a plot
savePlot <- function(myPlot,figName) {
  #pdf(fileName)
  png(figName)
  print(myPlot)
  dev.off()
}

continuous_discrete_correlation<-function(data_set, cont_var, disc_var)
{
  
  out<-aov(as.formula(paste(cont_var," ~ ", disc_var)),data=data_set)
  fval<-as.numeric(summary(out)[[1]][["Pr(>F)"]])
  
  require(ggplot2)
  if(FALSE)
  {
  png(paste("figs/ANOVA_",cont_var,"_",disc_var,"_plot.png"))
  boxplot(as.formula(paste(cont_var," ~ ", disc_var)),data=data_set, main=fval,xlab=disc_var, ylab=cont_var,outline=FALSE)
  means_obj <- aggregate(as.formula(paste(cont_var," ~ ", disc_var)), data_set, mean)
  means<-means_obj[,cont_var]
  points(1:2, means, pch = 23, cex = 0.75,bg = "red")
  text(1:2 - 0.4, means, labels = formatC(means, format = "f",digits = 1),pos = 2, cex = 0.9, col = "red")
  dev.off()
 }else{ 
  figName<-paste("figs/ANOVA_",cont_var,"_",disc_var,"_plot.png")
  #myPlot <- ggplot(data_set, aes_string(factor(disc_var), cont_var))+ geom_violin()+ labs(title = fval)
  #data_set[,disc_var]<-factor(as.character(data_set[,disc_var]))
  myPlot <- ggplot(data_set, aes_string(factor(data_set[[disc_var]]), cont_var))+ geom_violin()+ labs(x=disc_var,title = fval)+ geom_violin(trim = FALSE)+ stat_summary(fun.y=mean, geom="point", shape=23, size=2, color="red")
  
  savePlot(myPlot,figName)
 }
  return(fval)
}


discrete_discrete_correlation<-function(data_set, disc_x, disc_y)
{
  #fisher.test(table(data_set[,disc_x],data_set[,disc_y]))
  #disc_x<-"origin_up"
  #disc_y<-"churn"
  require(ggplot2)
  #out<-chisq.test(data_set[,disc_y], data_set[,disc_x], correct=FALSE, simulate.p.value = TRUE)
  out<-chisq.test(data_set[,disc_y], data_set[,disc_x], correct=FALSE)
  
  pval<-as.numeric(out[3])
  figName<-paste("figs/CHI_",disc_x,"_",disc_y,"_plot.png")
  
  dat <- data.frame(table(data_set[,disc_x],data_set[,disc_y]))
  #dat <- data.frame(prop.table(table(data_set[,disc_x],data_set[,disc_y])))
  
  names(dat) <- c(disc_x,disc_y,"Count")
  
  count_str <- "Count"
  myPlot <-ggplot(data=dat, aes_string(x=disc_x, y=count_str, fill=disc_y)) + geom_bar(stat="identity") + labs(title = pval)
  #dev.off()
  savePlot(myPlot,figName)
  return(pval)
}


# Function to move the target to the end of the dataset
target_to_the_end <- function(data_set,targ_Name)
{
  tmp <-data.frame(data_set[,targ_Name])
  colnames(tmp)<-targ_Name
  data_set[,targ_Name]<-NULL
  dataset_ret <- cbind(data_set, tmp)
  return(dataset_ret)
  
}


Feature_Engineer_Hour<-function(POSIxct_str)
{
  
  
  
  
}



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
    #data_set[,paste0(feature_name,'_DateInt')] <- as.numeric(data_set[,feature_name])
    #data_set[,paste0(feature_name,'_Month')] <- as.integer(format(data_set[,feature_name], "%m"))
    #data_set[,paste0(feature_name,'_ShortYear')] <- as.integer(format(data_set[,feature_name], "%y"))
    #data_set[,paste0(feature_name,'_LongYear')] <- as.integer(format(data_set[,feature_name], "%Y"))
    #data_set[,paste0(feature_name,'_Day')] <- as.integer(format(data_set[,feature_name], "%d"))
    # week day number requires first pulling the weekday label, creating the 7 week day levels, and casting to integer
    data_set[,paste0(feature_name,'_WeekDayNumber')] <- as.factor(weekdays(data_set[,feature_name]))
    levels(data_set[,paste0(feature_name,'_WeekDayNumber')]) <- list(Monday=1, Tuesday=2, Wednesday=3, Thursday=4, Friday=5, Saturday=6, Sunday=7)
    data_set[,paste0(feature_name,'_WeekDayNumber')] <- as.integer(data_set[,paste0(feature_name,'_WeekDayNumber')])
    #data_set[,paste0(feature_name,'_IsWeekend')] <- as.numeric(grepl("Saturday|Sunday", weekdays(data_set[,feature_name])))
    #data_set[,paste0(feature_name,'_YearDayCount')] <- yday(data_set[,feature_name])
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
  relationships_set <-select(relationships_set, feature_1, feature_2, r, p) %>% dplyr::rename(correlaton =r, pvalue = p)
  # return only the most insteresting relationships
  # return only top correlations or p values at some threshold etc
  #return(filter(relationships_set,abs(correlaton) > correlation_abs_threshold | pvalue < pvalue_threshold ) %>% arrange(pvalue)) 
  return(filter(relationships_set,abs(correlaton) > correlation_abs_threshold  ) %>% arrange(desc(abs(correlaton))))
  
}

saveHistogram <- function(dataset,featureName)
{
  require(ggplot2)
  if( class(dataset[[featureName]])=="character" || class(dataset[[featureName]])=="integer")
  {
    png(filename = paste("figs/hist_", featureName, ".png"))
    #myPlot <-ggplot(data=dat, aes_string(x=disc_x, y=count_str, fill=disc_y)) + geom_bar(stat="identity") + labs(title = pval)
    barplot(prop.table(table(dataset[[featureName]])),main=paste("Histogram for ", featureName))
    dev.off()
  }
  if( class(dataset[[featureName]])=="numeric")
  {
    figName <- paste("figs/hist_", featureName, ".png")
    #h = hist(as.matrix(dataset[, featureName]))
    #h$density = h$counts / sum(h$counts) * 100
    #myPlot <-ggplot(data=dataset, aes_string(x=featureName,y="Count")) + geom_bar(stat="identity") + labs(title = paste("Histogram of",featureName))
    myPlot <-ggplot(data=dataset, aes(dataset[[featureName]])) + geom_histogram()+ labs(x = featureName)
    
    savePlot(myPlot,figName)
    #png(filename = paste("figs/hist_", featureName, ".png"))
    #plot(h,freq = FALSE,ylim = c(0, 100),main = paste("Histogram for ", featureName),xlab = "",ylab = "[%] of Total")
    #dev.off()
  }
  
  
}

# Libraries
library(dplyr)
library(lubridate)
library(caTools)
library(plyr)
#-------- PARAMETER INPUT -----------
# READING FILE
fileName = "data.csv" # CHOOSE "data.csv" or "data_test.csv" !!!!
#fileName = "data_num.csv" # CHOOSE "data.csv" or "data_test.csv" !!!!
MAX_NUMBER_OF_ROWS_TO_READ = -1 # -1 for all
targetName = "btarget"
#targetName = "target"
#excluded_category_feature = c("id","era","data_type") # The feature which should be excluded from categorial analysis, usually this is the id of the user which has no predictive power
excluded_category_feature = c() # The feature which should be excluded from categorial analysis, usually this is the id of the user which has no predictive power
#excluded_category_feature = c("customerID") # The feature which should be excluded from categorial analysis, usually this is the id of the user which has no predictive power
FEAT_ENGINEER_FLAG = FALSE
targetClasses = 2 # Number of classes in target. E.g. 2 for binary
# CATEGORIAL FEATS
N_most_popular = 8  # Capping the categories to the N most popular categories
CATEGORY_CLASSES_LIMIT_THRESHOLD = 80 # Maximum number of categories within a categorial variable. Aboce this threshold, the variable will be deleted
# BALANCING
BALANCE_DATASET = TRUE # Should the data be balanced or not
balanceThreshold = 0.7 # The threshold of negative to positive data above which we conside the data unbalanced and resort to balancing techniqques
# COMPRESSION THRESHOLDS
PCA_info_thresh = 0.95  # Maximum components to keep to preserve the information. make -1 to prefer choosing number of components
CORR_threshold = 0.8 # Threshold of cross correlation above which features are dropped in case REMOVE_SIMILAR_NUMERIC_FEATURES is TRUE
# NA PARAMS
Cuttoff_Missing_data = 70 # Percentage above which we exclude the feature being mostly missing
NUMERIC_MODE_LIMIT = 20 # limit above which the numeric values are not imputed with mean but rather with the mode 
# Outlier
OUTLIER_FLAG = FALSE
OUTLIER_PREC = 0.01
# High cardinality
RIDGE_AND_FREQ_OF_HIGH_CARDINALITY_FEATURES = FALSE
# FIGURES
FIGURES_FLAG = FALSE # Flag to save figures. Switch off after obtaining good figures to save time


#-------- set data saving directory
if(fileName=="data.csv")
{
  saveDir<-"data"
}else{
  
  saveDir<-"test"
  
}

#-------- Initializing our logs file
logs <- data.frame(log="---- Started ----",stringsAsFactors=FALSE)
ilog <-2

#--------- READING DATA ------------
library(readr)
if (MAX_NUMBER_OF_ROWS_TO_READ < 1)
{
  dataset <- read_csv(fileName)
  logs[ilog,1] <- "Read the whole dataset"
  ilog<-ilog+1
  
} else {
  dataset <- read_csv(fileName, n_max = MAX_NUMBER_OF_ROWS_TO_READ)
  logs[ilog,1] <- paste("Read part of the file....",MAX_NUMBER_OF_ROWS_TO_READ,"lines")
  ilog<-ilog+1
  
}

#library(lubridate)


# Some feature engineering
dataset$Time_date<-as.Date(dataset$Time,tz="UTC")
dataset$Time_hour<-as.numeric(strftime(dataset$Time, format="%H"))
dataset$Time<-NULL
#dataset$btarget_1<-shift_vec(as.vector(dataset$btarget),1)
#dataset$btarget_2<-shift_vec(as.vector(dataset$btarget),2)
#dataset$btarget_3<-shift_vec(as.vector(dataset$btarget),3)
dataset$WilliamsAD_1<-shift_vec(as.vector(dataset$williamsAD),1)
dataset$WilliamsAD_2<-shift_vec(as.vector(dataset$williamsAD),2)
dataset$WilliamsAD_3<-shift_vec(as.vector(dataset$williamsAD),3)
dataset$ROC_1<-shift_vec(as.vector(dataset$ROC),1)
dataset$ROC_2<-shift_vec(as.vector(dataset$ROC),2)
dataset$ROC_3<-shift_vec(as.vector(dataset$ROC),3)
dataset$adx_1<-shift_vec(as.vector(dataset$adx),1)
dataset$adx_2<-shift_vec(as.vector(dataset$adx),2)
dataset$adx_3<-shift_vec(as.vector(dataset$adx),3)
dataset$VHF_1<-shift_vec(as.vector(dataset$VHF),1)
dataset$VHF_2<-shift_vec(as.vector(dataset$VHF),2)
dataset$VHF_3<-shift_vec(as.vector(dataset$VHF),3)

#dataset$_1<-shift_vec(as.vector(dataset$),1)
#dataset$_2<-shift_vec(as.vector(dataset$),2)
#dataset$_3<-shift_vec(as.vector(dataset$),3)



dataset<-target_to_the_end(dataset,targetName)




if(length(excluded_category_feature)>0 )
{
  if( excluded_category_feature %in% names(dataset))
  {
  dataset[,excluded_category_feature]<-NULL
  logs[ilog,1] <- paste("Excluded ",excluded_category_feature," from dataset", collapse=', ')
 ilog<-ilog+1
  }
} else
{
  logs[ilog,1] <- paste("Warning:",excluded_category_feature," not present in the data set to be excluded")
  ilog<-ilog+1
  
}

dataset$X1<-NULL




#str(dataset)
dataset_orig <- dataset
featureNames <- names(dataset)
logs[ilog,1] <- paste("We have ",length(featureNames)-1," features")
ilog<-ilog+1

# Creating analytics dataframe
logs[ilog,1] <- "Creating analytics dataframe"
ilog<-ilog+1
dataset_an <-data.frame(matrix(, nrow = length(names(dataset)), ncol = 0))
rownames(dataset_an) <- names(dataset)
dims = dim(dataset)


#---------  FEATURE TYPES ANALYTICS ----
# Appending the results of data types
logs[ilog,1] <- "Saving types of the features to analytics data frame"
ilog<-ilog+1

dataset_an$types = lapply(dataset, class)


#--------- MISSING DATA ANALYTICS ----
logs[ilog,1] <- "------------ MISSING DATA -------------------"
ilog<-ilog+1

na_count = numeric(dims[2])
i <- 1
for (col in names(dataset))
{
  na_count[i] = 100.0 * sum(is.na(dataset[, col])) / dims[1]
  i <- i + 1
}
# Appending the results of missing values
dataset_an <- cbind(dataset_an, NA_Perc = na_count)

if(sum(na_count)>0)
{
logs[ilog,1] <- "NA values found"
ilog<-ilog+1
}else{
  
  logs[ilog,1] <- "No NA values found"
  ilog<-ilog+1
}



#-------- IMPUTING MISSING DATA  -----

# Get the NA data and initialize imputed data
vars <- rownames(dataset_an)
dataset_imputed <- dataset

# Subdivide features into 3 categories

# 1. Above threshold of imputing missing
remove_Imputation_features <- vars[which(dataset_an$NA_Perc > Cuttoff_Missing_data, arr.ind =TRUE)]
dataset_imputed[,remove_Imputation_features] <- NULL

if(length(remove_Imputation_features)>0)
{
  logs[ilog,1] <- paste("Feature(s)",remove_Imputation_features," are/is totally empty or above threshold of missing data, hence discarded", collapse=', ')
  ilog<-ilog+1
}

# 2. below threshold of going through kNN or RF imputation
simple_Imputation_features <- vars[which(   (dataset_an$NA_Perc < Cuttoff_Missing_data) & (dataset_an$NA_Perc > 0), arr.ind =TRUE)]
if(length(simple_Imputation_features)>0)
{
  logs[ilog,1] <- paste("Feature(s)",simple_Imputation_features," have some missing values", collapse=', ')
  ilog<-ilog+1
  
numericList <- simple_Imputation_features[sapply(dataset[,simple_Imputation_features],is.numeric)]
nonNumericList <- setdiff(simple_Imputation_features,numericList)
logs[ilog,1] <- paste("Numeric missing:",numericList, collapse=', ')
ilog<-ilog+1
logs[ilog,1] <- paste("Nonnumeric missing:",nonNumericList, collapse=', ')
ilog<-ilog+1

write_csv(dataset,"train_reference.csv")

# 2.1 Substituting numerics with their mean
for (numFeat in numericList)
{
  featCol <- dataset[,numFeat]
  featCol <- featCol[!is.na(featCol)]
  md<-Mode(as.matrix(featCol))
  if( 100*(nrow(dataset[dataset[,numFeat]==md,])/length(featCol))> NUMERIC_MODE_LIMIT  )
  {
    dataset_imputed[is.na(dataset_imputed[,numFeat]),numFeat]<-md
  }else
  {
    dataset_imputed[[numFeat]] <- ifelse(is.na(dataset[[numFeat]]), ave(dataset[[numFeat]], FUN = function(x) mean(x, na.rm = TRUE)), dataset[[numFeat]])

  }
  
}

# 2.2 Substituting categorial with their mode
for (nonnumFeat in nonNumericList)
{
  featCol <- dataset[,nonnumFeat]
  featCol <- featCol[!is.na(featCol)]
  if( is.Date(dataset[[nonnumFeat]]) )
  {
    md<-as.Date(Mode(as.matrix(featCol)),format = "%Y-%m-%d")
  }else{
    
    md<-Mode(as.matrix(featCol))
  }
  
  dataset_imputed[is.na(dataset_imputed[,nonnumFeat]),nonnumFeat]<-md
}
#str(dataset_imputed)

}


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
logs[ilog,1] <- "--------- DISTRIBUTIONS OF FEATURES  --------------"
ilog<-ilog+1
dataset <- dataset_imputed
dims <- dim(dataset_orig)

rm(dataset_imputed)
logs[ilog,1] <- "Getting Number of categories of character features and removing/clipping many categories"
ilog<-ilog+1

# Getting Number of categories of character features
category_count <- numeric(dims[2])-1
i<-1
library(h2o)
h2o.init(nthreads = -1)

t<-1
for (name in colnames(dataset))
{
  if (lapply(dataset[,name],class)=="character")
  {
    #category_count[i]<-length(unique(as.matrix(dataset[,name])))
    dataset_an[name,'Number_categories'] <-length(unique(as.matrix(dataset[,name])))
    
    if(length(unique(as.matrix(dataset[,name])))>N_most_popular && name!=excluded_category_feature)
    {
      if(length(unique(as.matrix(dataset[,name])))>CATEGORY_CLASSES_LIMIT_THRESHOLD)
      {
       
        if(RIDGE_AND_FREQ_OF_HIGH_CARDINALITY_FEATURES)
          {
         #print(paste("The feature ",name,"has",length(unique(as.matrix(dataset[,name])))," classes which is larger than the limti of ",CATEGORY_CLASSES_LIMIT_THRESHOLD,", hence it will be removed"))
        
        # Replace category with its frequency occurance
        t<-as.data.frame(table(dataset[,name]))
        names(t)[1] = name
        write_csv(t,paste0('data/',name,'_legend.csv'))
        
        #b<-transform(dataset, freq.loc = ave(seq(nrow(dataset)), activity_new, FUN=length))
        #b<-transform(dataset, freq.loc = ave(seq(nrow(dataset)), name, FUN=length))
        dataset[,paste0(name,"_freq")] <- ave( as.numeric(dataset[[1]]), dataset[[name]] ,FUN=length)
        
        
        # Two fold glm modeling
        set.seed(123)
        ids1 = sample(1:nrow(dataset), round(nrow(dataset)/2))
        ids2 = setdiff(1:nrow(dataset),ids1)
        
        trainRidge1 <- as.h2o(dataset[ids1,c(name,targetName)])
        trainRidge1[,targetName]<-as.factor( trainRidge1[,targetName] )
        trainRidge1[,name]<-as.factor( trainRidge1[,name] )

        trainRidge2 <- as.h2o(dataset[ids2,c(name,targetName)])
        trainRidge2[,targetName]<-as.factor( trainRidge2[,targetName] )
        trainRidge2[,name]<-as.factor( trainRidge2[,name] )
        
        
        # Model 1 
        glm_clf1<-h2o.glm(x =name ,y= targetName, training_frame =trainRidge1 , family ="binomial", model_id = paste0(name,"_ridge_model"))
        # Model 2
        glm_clf2<-h2o.glm(x =name ,y= targetName, training_frame =trainRidge2 , family ="binomial", model_id = paste0(name,"_ridge_model"))

        predRidge1 <- h2o.predict(glm_clf2,newdata = trainRidge1[,name])
        predRidge2 <- h2o.predict(glm_clf1,newdata = trainRidge2[,name])
        
        dataset[ids1,paste0(name,"_ridge")] <- as.data.frame(predRidge1$p1)
        dataset[ids2,paste0(name,"_ridge")] <- as.data.frame(predRidge2$p1)
        
        perf1 <- h2o.performance(glm_clf1, newdata = trainRidge2)
        perf2 <- h2o.performance(glm_clf2, newdata = trainRidge1)
        
        if(h2o.auc(perf1)>h2o.auc(perf2))
        {
          h2o.saveModel(glm_clf1, path=paste0("models/",name,"_ridge_model"),force = TRUE)
        }
        else
        {
          h2o.saveModel(glm_clf2, path=paste0("models/",name,"_ridge_model"),force = TRUE)
        }
        
        }
        
        
        dataset[,name] <- NULL
        
      }
        else{
          
          print(paste("The feature ",name,"has",length(unique(as.matrix(dataset[,name])))," classes hence will be capped"))
          temp_vect<-dataset[[name]]
          temp_vect <- data.frame(table(temp_vect)) %>% arrange(desc(Freq)) %>% head(N_most_popular-1)
          dataset[,name] <- ifelse(dataset[[name]] %in% temp_vect$temp_vect,dataset[[name]], 'Other')
        }
      
     
    }
    
    
  }  else{
    #category_count[i]<-0
    dataset_an[name,'Number_categories'] <- 0
    
  }
  i<-i+1
}


dataset<-target_to_the_end(dataset,targetName)


#----------------- SAVING THE HISTOGRAMS
if(FIGURES_FLAG)
{
  logs[ilog,1] <- "Saving the histograms of features"
  ilog<-ilog+1
  
  for (name in names(dataset))
  {
    saveHistogram(dataset,name)
    
  }
}

#------------------- CORRELATIONS -------------------------------
logs[ilog,1] <- "------ CORRELATIONS --------------"
ilog<-ilog+1

# Getting numeric features only
feats <- colnames(dataset)
numericList <- feats[sapply(dataset[,feats],is.numeric)]
dateList <- feats[sapply(dataset[,feats],is.Date)]
lis1<-Get_Fast_Correlations(dataset[,numericList])
top_correlated_numeric_features<-Get_Top_Relationships(dataset[,numericList],correlation_abs_threshold=CORR_threshold)

if(FIGURES_FLAG)
{
  logs[ilog,1] <- "saving correlations between numerical features and output"
  ilog<-ilog+1
  
# Getting the correlations between output (if categorial) and numerical features
for (name in setdiff(numericList,targetName))
{
  dataset_an[name,'F-val']<-continuous_discrete_correlation(dataset, name, targetName)[1]
  dataset_an[name,'Mean']<-mean(dataset[[name]], na.rm=TRUE)
  dataset_an[name,'Standard_Deviation']<-sd(dataset[[name]], na.rm=TRUE)
  
}
}
#------------------ FACTORING NON-NUMERIC DATA
# Reorganizing data to Numerical-Categorial-Target
catList <-setdiff(feats,c(numericList,dateList))
dataset_organized<-cbind(dataset[,numericList],dataset[,dateList],dataset[,catList])
names(dataset_organized)<-c(numericList,dateList,catList)
dataset_organized<-target_to_the_end(dataset_organized,targetName)
# Removing X1 column which shows up somewhere
dataset_organized$X1<-NULL

# Converting categories to factors
logs[ilog,1] <- "Converting categories to factors"
ilog<-ilog+1

for (cat in catList)
{
  dataset_organized[,cat]<-as.factor(dataset_organized[,cat])
  
}

if(FIGURES_FLAG)
{
  logs[ilog,1] <- "Saving correlations of categorial features to target"
  ilog<-ilog+1
  
# Saving Chi2 values for categorial data
factorList <- names(dataset_organized)[sapply(dataset_organized,is.factor)]
for (name in setdiff(factorList,targetName))
{
  dataset_an[name,'Chi-val']<-discrete_discrete_correlation(dataset_organized, name, targetName)
}
}
dataset<-dataset_organized

rm(dataset_organized)
#str(dataset)
logs[ilog,1] <- "Saving dataset with no NA and with categorial features non-binarized"
ilog<-ilog+1

write.csv(dataset, file = paste0(saveDir,"/data_full.csv"),row.names = FALSE)





#-------------------- OUTLIERS -----------------------
# TODO: Think about including date in outlier detection or not

if(OUTLIER_FLAG)
{
outlierThresh <- round(OUTLIER_PREC*nrow(dataset))+1
i<-3
foundFLAG = FALSE
while(!foundFLAG & i<50 )
{
  ignoreList = setdiff(names(dataset),numericList)
#res<-Identify_Outliers(dataset,outlier_sd_threshold=i,features_to_ignore = c("channel_sales","origin_up","has_gas","churn"))
res<-Identify_Outliers(dataset,outlier_sd_threshold=i,features_to_ignore = c(ignoreList,targetName))

if(res[1,2]< outlierThresh)
{
  foundFLAG = TRUE
  
  
}
  i<-i+1
}

dataset<-Identify_Outliers(dataset,outlier_sd_threshold=i-1,remove_outlier_observations = TRUE,features_to_ignore =  c(ignoreList,targetName))

}



#---------------- FEATURE ENGINEERING ----------------
logs[ilog,1] <- "---------------- FEATURE ENGINEERING ----------------"
ilog<-ilog+1

# Collecting lists of numeric, date and category
feats <- colnames(dataset)
numericList <- setdiff(feats[sapply(dataset[,feats],is.numeric)],targetName)

logs[ilog,1] <- " Encoding dummy variables "
ilog<-ilog+1

# Encoding dummy variables
dataset_binarized<-Binarize_Features(dataset,leave_out_one_level=FALSE,features_to_ignore = targetName)
catList<-setdiff(names(dataset_binarized),names(dataset))
logs[ilog,1] <- " Extracting date information "
ilog<-ilog+1

# Extracting date information
dataset_binarized_dates<-Feature_Engineer_Dates(dataset_binarized)
dateList<-setdiff(names(dataset_binarized_dates),names(dataset_binarized))
rm(dataset_binarized)

# Binding target at the end of the dataframe
dataset_full<-target_to_the_end(dataset_binarized_dates,targetName)
rm(dataset_binarized_dates)


# Inserting data into the analytics table
feats<-names(dataset_full)
for (name in feats)
{
  if(grepl("_DateInt",name))
  {
    dataset_an[gsub("_DateInt",'',name),'F-val']<-continuous_discrete_correlation(dataset_full, name, targetName)[1] 
    
  }
}

if(FEAT_ENGINEER_FLAG)
{# Custom Feature engineering using polynomials
# Log
dataset_full$log_pow_max<-mapply(log_columns,dataset_full$pow_max,min(dataset_full$pow_max))
dataset_full$log_imp_cons<-mapply(log_columns,dataset_full$imp_cons,min(dataset_full$imp_cons))
#Differences
dataset_full$contract_duration <- mapply(sub_columns,dataset_full$date_activ_DateInt,dataset_full$date_end_DateInt )
dataset_full$contract_allowance <- mapply(sub_columns,dataset_full$date_end_DateInt,dataset_full$date_renewal_DateInt )

# Ratios
dataset_full$total_net_margin_per_product<-mapply(mult_columns,dataset_full$net_margin,1/dataset_full$nb_prod_act)
dataset_full$gross_margin_per_product<-mapply(mult_columns,dataset_full$margin_gross_pow_ele,1/dataset_full$nb_prod_act)
dataset_full$net_margin_per_product<-mapply(mult_columns,dataset_full$margin_net_pow_ele,1/dataset_full$nb_prod_act)
dataset_full$last_month_per_year <- 0
dataset_full[dataset_full$cons_12m!=0,"last_month_per_year"] <- mapply(mult_columns,dataset_full[dataset_full$cons_12m!=0,"cons_last_month"],1/dataset_full[dataset_full$cons_12m!=0,"cons_12m"])

#Polynomial
t<-as.data.frame(poly(dataset_full$date_end_DateInt,dataset_full$date_activ_DateInt,dataset_full$margin_net_pow_ele, degree=2, raw=T))
t$`1.0.0`<-NULL
t$`0.0.1`<-NULL
t$`0.1.0`<-NULL
dataset_full<-cbind(dataset_full,t)

numericList<-c(numericList,"log_pow_max","log_imp_cons","total_net_margin_per_product","gross_margin_per_product","net_margin_per_product","last_month_per_year","contract_duration","contract_allowance")

# Binding target at the end of the dataframe
dataset_full<-target_to_the_end(dataset_full,targetName)

}
# Save full data
logs[ilog,1] <- "Saving dataset with feature engineering"
ilog<-ilog+1

write.csv(dataset_full, file = paste0(saveDir,"/data_feature_eng.csv"),row.names = FALSE)
#write_csv(dataset_an, path = "res/dataset_an.csv")


top_correlated_numeric_features<-Get_Top_Relationships(dataset_full[,c(numericList,dateList)],correlation_abs_threshold=CORR_threshold)


#------------ FEATURE REDUCTION --------------------
logs[ilog,1] <- "------------ FEATURE REDUCTION --------------------"
ilog<-ilog+1

library(caret)
dataset_no_target<-dataset_full[,1:ncol(dataset_full)-1]
# PCA
preprocessParams <- preProcess(dataset_no_target, method=c("center", "scale", "pca"), thresh = PCA_info_thresh)
PCA_comp <- predict(preprocessParams, dataset_no_target)
PCA_comp<-cbind(PCA_comp,dataset_full[,targetName])
names(PCA_comp)[length(names(PCA_comp))]<-targetName

write.csv(PCA_comp,file = "data/normal/data_PCA_PCA_PCA.csv",row.names = FALSE)

#CORR
preprocessParams <- preProcess(dataset_no_target, method=c("corr"), cutoff = CORR_threshold)
CORR_comp <- predict(preprocessParams, dataset_no_target)
CORR_comp<-cbind(CORR_comp,dataset_full[,targetName])
names(CORR_comp)[length(names(CORR_comp))]<-targetName
write.csv(CORR_comp,file = "data/normal/data_CORR_CORR_CORR.csv",row.names = FALSE)



dataset_comp<-dataset_full
logs[ilog,1] <- "Compressed data set after feature selection is obtained"
ilog<-ilog+1

# Write compressed feature
write.csv(dataset_comp, file = "data/normal/data_OFF_OFF_OFF.csv",row.names = FALSE)


#----------- BALANCING THE DATASET

FEAT_REDUC_NUMERIC<-"OFF"
FEAT_REDUC_DATES<-"OFF"
FEAT_REDUC_CAT<-"OFF"

if(BALANCE_DATASET)
{
  logs[ilog,1] <- "------------ BALANCING THE DATASET --------------------"
  ilog<-ilog+1
  
  # Distribution of the target
  # First check if target name is correct
  featureNames = names(dataset_comp)
  if (!(targetName %in% featureNames))
  {
    logs[ilog,1] <- "ERROR: WRONG TARGET NAME"
    ilog<-ilog+1
    
    print(paste(
      "Variable ",
      targetName,
      " not present in data set. Could it be ",
      tail(featureNames, n = 1)
    ))
    
    stop("-------------- Exiting ------------------------------")
  }
  
  
  
  # Deciding if the data is imbalanced or not
  
  # Changing factor to integer to be able to calculate the histogram
  if(class(dataset_comp[, targetName])=="factor")
  {
  dataset_comp[,targetName]<-as.numeric(dataset_comp[,targetName])
  dataset_comp[,targetName]<-dataset_comp[,targetName]-1
  }
  target_hist <-hist(as.matrix(dataset_comp[, targetName]), nclass = targetClasses)
  
  
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
      logs[ilog,1] <- "Target is imbalanced"
      ilog<-ilog+1
      
      
    }
    else
    {
      print(paste(
        "Target balance is OK. Ratio of majority class is",
        100 * majClassRatio,
        "%"
      ))
      IMBALANCED_TARGET = FALSE
      logs[ilog,1] <- "Target is balanced"
      ilog<-ilog+1
    }
    
  }
  
  featureNames <- names(dataset_comp)
  
  if(IMBALANCED_TARGET)
  {
    library(unbalanced)
    
    input <- dataset_comp[,setdiff(featureNames,targetName)]
    output <- factor(dataset_comp[,targetName])
    #output <- factor(dataset$churn)
    
    
    data<-ubBalance(X= input, Y=output, type="ubSMOTE", percOver=200, percUnder=200, verbose=TRUE)
    balancedData<-cbind(data$X,data$Y)
    colnames(balancedData)[length(colnames(balancedData))]<-targetName
    dataset_comp<-balancedData

    write.csv(dataset_comp, file = paste0(saveDir,"/smote/data_",FEAT_REDUC_NUMERIC,"_",FEAT_REDUC_DATES,"_",FEAT_REDUC_CAT,"_SMOTE.csv"),row.names = FALSE)
    
    data<-ubBalance(X= input, Y=output, type="ubUnder", verbose=TRUE)
    balancedData<-cbind(data$X,data$Y)
    colnames(balancedData)[length(colnames(balancedData))]<-targetName
    dataset_comp<-balancedData

    write.csv(dataset_comp, file = paste0(saveDir,"/under/data_",FEAT_REDUC_NUMERIC,"_",FEAT_REDUC_DATES,"_",FEAT_REDUC_CAT,"_UNDER.csv"),row.names = FALSE)
    
    
  }
  
  rm(balancedData,dataset_binarized,dataset_binarized_dates,dataset_imputed,featCol,input,lis1,temp_vect,tmp,data)
}





write.table(logs, "logs.txt", sep="\t")


