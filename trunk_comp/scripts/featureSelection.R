## GOOD READS
# Reference of caret on all methods:
# https://topepo.github.io/caret/feature-selection-overview.html
# http://r-statistics.co/Variable-Selection-and-Importance-With-R.html
# https://www.analyticsvidhya.com/blog/2016/12/introduction-to-feature-selection-methods-with-an-example-or-how-to-select-the-right-variables/
# Libraries: FSelector , varSelRF



##-------- CARET
# source : http://topepo.github.io/caret/recursive-feature-elimination.html#rfe
# source : https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/


#Read the data
library(readr)
##------------- READING ALL RELEVANT CSV DATA
dataset <- read_csv("data/normal/data_OFF_OFF_OFF.csv")
x<-dataset[,1:ncol(dataset)-1]
dataset$churn<-as.factor(dataset$churn)
y<-dataset$churn



# CORRELATIONS
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# calculate correlation matrix
correlationMatrix <- cor(PimaIndiansDiabetes[,1:8])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.3)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# Learning Vector Quantization
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


# RFE
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
# Options: lmFuncs rfFuncs nbFuncs treebagFuncs caretFuncs
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))



##------- Random forest importance
# Source: https://www.r-bloggers.com/variable-importance-plot-and-variable-selection/
data(PimaIndiansDiabetes)
library(randomForest)
# instead of diabetes just put the name of the target ex: churn
fit=randomForest(factor(diabetes)~.,importance=TRUE,  data=PimaIndiansDiabetes)
# plot rank of features
varImpPlot(fit)
# Plot how many trees are needed
plot(fit)


#--------- FSelector
# Source : http://miningthedetails.com/blog/r/fselector/
library(FSelector)
data(PimaIndiansDiabetes)
PimaIndiansDiabetes$diabetes <- as.factor(PimaIndiansDiabetes$diabetes)
att.scores <- random.forest.importance(diabetes ~ ., PimaIndiansDiabetes)
#Top k features
cutoff.k(att.scores, k = 4)
# top % of feature set
cutoff.k.percent(att.scores, 0.4)
# Continuous and discrete
subset <- cfs(diabetes~., PimaIndiansDiabetes)


#------ varSelRF
## varSelRF package
library(varSelRF)
x<-PimaIndiansDiabetes[,1:ncol(PimaIndiansDiabetes)-1]
PimaIndiansDiabetes$diabetes<-as.factor(PimaIndiansDiabetes$diabetes)
y<-PimaIndiansDiabetes$diabetes
rf.vs1 = varSelRF(x, y, ntree = 200, ntreeIterat = 100,vars.drop.frac = 0.2)
rf.vs1
plot(rf.vs1)

#------ Genetic Algorithm [Very Slow]
library(caret)
data(PimaIndiansDiabetes)
x<-PimaIndiansDiabetes[,1:ncol(PimaIndiansDiabetes)-1]
PimaIndiansDiabetes$diabetes<-as.factor(PimaIndiansDiabetes$diabetes)
y<-PimaIndiansDiabetes$diabetes

ctrl <- gafsControl(functions = caretGA)
obj <- gafs(x = x, 
            y = y,
            iters = 100,
            gafsControl = ctrl,
            ## Now pass options to `train`
            method = "lm")








