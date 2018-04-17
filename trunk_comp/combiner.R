# Check if the NA filling is done using lablels or not
rm(list = ls())


library(readr)
library(VIM)

targetName<-"churn"
train_ratio<-0.8
drop_feats<-c("forecast_cons","id","campaign_disc_ele","forecast_base_bill_ele","forecast_base_bill_year","forecast_bill_12m","date_first_activ")
imputefeats = c("channel_sales")
#,"activity_new"

##------------- READING ALL RELEVANT CSV DATA
dataset_feats <- read_csv("ml_case_training_data.csv")
dataset_churn <- read_csv("ml_case_training_output.csv")
dataset_prices <- read_csv("ml_case_training_hist_data.csv")

##------------- COMBINING THE DATA IN A MEANINGFUL WAY
dataset_prices_aver<-aggregate(dataset_prices[,setdiff(names(dataset_prices),c("id","price_date"))], list(dataset_prices$id), mean)
names(dataset_prices_aver)[1]<-"id"
dataset = merge(x=dataset_feats, y = dataset_prices_aver,by="id")
dataset = merge(x=dataset,y=dataset_churn,by="id")

##------------- FEATURES TO DROP

dataset[,drop_feats]<-NULL

##------------ SPLITTING THE DATA TO GET THE TEST SET
smp_size <- floor(train_ratio * nrow(dataset))

## Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

## Split them
test <- dataset[-train_ind, ]

# Write the test set
write.csv(test, file = "data_test.csv",row.names = FALSE)
dataset <- dataset[train_ind, ]




##------------- KNN IMPUTATION OF CATEGORIES WITH HIGH NA

library(h2o)
h2o.init(nthreads = -1)

for(feat in imputefeats)
{
train_df <- as.h2o(dataset[!is.na(dataset[,feat]),])
test_df <- as.h2o(dataset[is.na(dataset[,feat]),])

y <- feat
x <- setdiff(names(train_df), c(y,targetName))

train_df[,y] <- as.factor(train_df[[y]])
test_df[,y] <- NULL
print(paste0("Imputing ",feat))
my_gbm1 <- h2o.gbm(x = x,
                   y = y,
                   training_frame = train_df[,setdiff(names(train_df),targetName)],
                   nfolds = 5,
                   distribution = "AUTO",
                   seed = 123)

print(my_gbm1@model$cross_validation_metrics_summary)

pred <- h2o.predict(my_gbm1, newdata = test_df[,setdiff(names(test_df),targetName)])
dataset[is.na(dataset[,feat]),feat]<-as.data.frame(as.character(pred$predict))

}

write.csv(dataset, file = "data.csv",row.names = FALSE)
