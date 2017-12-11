source(file="BialystokCrimeDataClass.R")
source(file="BostonCrimeDataClass.R")

source(file="ModelCheckerClass.R")
source(file="RatingClass.R")
source(file="TimeLoggingClass.R")
source(file="CheckerWithResultSaving.R")

source(file="LogisticRegressionModelClass.R")
source(file="NaiveBayesModelClass.R")
source(file="kNNModelClass.R")
source(file="RandomForestModelClass.R")
source(file="SVMModelClass.R")

checkClassifier <- function(data, Classifier) {
  timeLoggingClass <- TimeLoggingClass()
  timeLoggingClass$start()
  modelChekcerClass <- CheckerWithResultSaving(data, Classifier)
  results <- modelChekcerClass$crossValidation()
  
  ratingClass <- RatingClass(data, modelChekcerClass$resultsPath)
  auc <- ratingClass$getAvreageAUC(results)
  timeLoggingClass$stop()
  
  #saveAUCinFile(auc, modelChekcerClass$resultsPath)
  saveTimeInGlobal(modelChekcerClass$resultsPath)
  auc
}

saveAUCinFile <- function(auc, resultsPath) {
  filePath <- paste(resultsPath, 'AUC.csv', sep = '/')
  write.csv(auc, file = filePath)
}

# TODO można zastanowić się nad wydzieleniem do osobnej klasy
saveTimeInGlobal = function(resultsPath) {
  trainTime <- read.csv(file = paste(resultsPath, 'timeTrain.csv', sep = '/'))
  testTime <- read.csv(file = paste(resultsPath, 'timeTest.csv', sep = '/'))
  
  res <- data.frame(data.frame(matrix(NA, nrow = 10, ncol = 4)))
  colnames(res) <- c('train', 'test', 'trainf', 'testf')
  res$train <- trainTime$diff
  res$test <- testTime$diff
  res$trainf <- gsub("[A-z]+|\\s+", "", res$train)
  res$trainf <- gsub("\\.", ",", res$trainf)
  res$testf <- gsub("[A-z]+|\\s+", "", res$test)
  res$testf <- gsub("\\.", ",", res$testf)
  
  resultTime <<- data.frame(res$train, res$test)
  resultTimeFormatted <<- data.frame(res$trainf, res$testf)
}










