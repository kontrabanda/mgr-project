source(file="BialystokCrimeDataClass.R")
source(file="LogisticRegressionModelClass.R")
source(file="ModelCheckerClass.R")
source(file="RatingClass.R")

bialystokCrimeDataClass <- BialystokCrimeDataClass()
bialystokCrimeDataClass$init()
logisticRegressionModelClass <- LogisticRegressionModelClass()
modelChekcerClass <- ModelCheckerClass()
modelChekcerClass$init(bialystokCrimeDataClass, logisticRegressionModelClass)
#results <- modelChekcerClass$normalChecking()
results <- modelChekcerClass$crossValidation()


ratingClass <- RatingClass()
ratingClass$init(bialystokCrimeDataClass)
auc <- ratingClass$getAvreageAUC(results)
