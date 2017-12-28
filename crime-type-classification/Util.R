source(file="./data/BialystokCrimeDataClass.R")
source(file="./data/BostonCrimeDataClass.R")
source(file="./data/BournemouthCrimeDataClass.R")

source(file="./strategies/ModelCheckerClass.R")
source(file="./strategies/RatingClass.R")
source(file="./strategies/CheckerWithResultSaving.R")
source(file="./time/TimeLoggingClass.R")

source(file="./model/LogisticRegressionModelClass.R")
source(file="./model/NaiveBayesModelClass.R")
source(file="./model/kNNModelClass.R")
source(file="./model/RandomForestModelClass.R")
source(file="./model/SVMModelClass.R")

checkClassifier <- function(data, Classifier, crossValidationSets = 1:10, withRating = T) {
  timeLoggingClass <- TimeLoggingClass()
  timeLoggingClass$start()
  modelChekcerClass <- CheckerWithResultSaving(data, Classifier)
  modelChekcerClass$crossValidation(crossValidationSets)
  
  if(withRating) {
    results <- modelChekcerClass$getResults()
    ratingClass <- RatingClass(data, modelChekcerClass$resultsPath)
    auc <- ratingClass$getAvreageAUC(results)
  }
  timeLoggingClass$stop()
}










