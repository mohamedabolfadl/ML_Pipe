

library(readr)

comp<-"OFF"
train_FileName <- paste0("C:/Users/m00760171/Desktop/Templates/trunk/data/normal/data_",comp,"_",comp,"_",comp,".csv")
test_FileName <- paste0("C:/Users/m00760171/Desktop/Templates/trunk/test/normal/data_",comp,"_",comp,"_",comp,".csv")
dataset_train <- read_csv(train_FileName)
dataset_test <- read_csv(test_FileName)


if(length(setdiff(names(dataset_train),names(dataset_test)))>0)
{
  print(setdiff(names(dataset_train),names(dataset_test)))
  stop("Train and test have different features")
  
}
library(h2o)
#h2o.shutdown(prompt=FALSE)
h2o.init()

# Import a sample binary outcome train/test set into H2O
train <- h2o.importFile(train_FileName)
test <- h2o.importFile(test_FileName)

# Identify predictors and response
y <- "churn"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  leaderboard_frame = test,
                  max_runtime_secs = 6000)

# View the AutoML Leaderboard
lb <- aml@leaderboard
lb


# The leader model is stored here
aml@leader


# If you need to generate predictions on a test set, you can make
# predictions directly on the `"H2OAutoML"` object, or on the leader
# model object directly

#pred <- h2o.predict(aml, test)  #Not functional yet: https://0xdata.atlassian.net/browse/PUBDEV-4428

# or:
#pred <- h2o.predict(aml@leader, test)
h2o.performance(aml@leader,newdata = test)
model_path <- h2o.saveModel(object=aml@leader, path="C:/Users/m00760171/Desktop/Templates/trunk/models", force=TRUE)




#h2o.shutdown()


