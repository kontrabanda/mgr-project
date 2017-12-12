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










