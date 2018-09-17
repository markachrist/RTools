#
# Builds random forest model and assesses feature importance using the given data.
#
# Mark Christopher
# 2015
#

library(randomForest)

# Read cl args
options(echo = FALSE)
args = commandArgs(trailingOnly = TRUE)

dataPath = args[1]
classIdx = strtoi(args[2])

ntree = 100
if(length(args) >= 3){
  ntree = strtoi(args[3])
}

# Get data and split into predictors/response
data = read.csv(dataPath)

predictors = data[, -classIdx]
response = data[, classIdx]

rf = randomForest(predictors, as.factor(response))

imp = as.data.frame(importance(rf))

print(imp)
