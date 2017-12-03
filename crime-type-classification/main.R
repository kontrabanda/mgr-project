source(file="BialystokCrimeDataClass.R")
source(file="ModelCheckerClass.R")
source(file="RatingClass.R")
source(file="TimeLoggingClass.R")
source(file="CheckerWithResultSaving.R")

timeLoggingClass <- TimeLoggingClass()
bialystokCrimeDataClass <- BialystokCrimeDataClass()

#################### LOGISTIC REGRESSION  ###########################
timeLoggingClass$start()
source(file="LogisticRegressionModelClass.R")
modelChekcerClass <- ModelCheckerClass(bialystokCrimeDataClass, LogisticRegressionModelClass)
results1 <- modelChekcerClass$crossValidation()

ratingClass <- RatingClass(bialystokCrimeDataClass)
auc1 <- ratingClass$getAvreageAUC(results1)
timeLoggingClass$stop()
###############################################

################### BAYES  ############################
timeLoggingClass$start()
source(file="NaiveBayesModelClass.R")
modelChekcerClass <- ModelCheckerClass(bialystokCrimeDataClass, NaiveBayesModelClass)
results2 <- modelChekcerClass$crossValidation()

ratingClass <- RatingClass(bialystokCrimeDataClass)
auc2 <- ratingClass$getAvreageAUC(results2)
timeLoggingClass$stop()
###############################################

################### BAYES  ############################
timeLoggingClass$start()
source(file="NaiveBayesModelClass.R")
checker <- CheckerWithResultSaving(bialystokCrimeDataClass, NaiveBayesModelClass)
results2 <- checker$crossValidation()
#results2 <- checker$getResults()

ratingClass <- RatingClass(bialystokCrimeDataClass)
auc2 <- ratingClass$getAvreageAUC(results2)
timeLoggingClass$stop()
###############################################

##################### SVM ###########################
source(file="SVMModelClass.R")
timeLoggingClass$start()
checker <- CheckerWithResultSaving(bialystokCrimeDataClass, SVMModelClass)
results3 <- checker$crossValidation(sets = 1:1)

ratingClass <- RatingClass(bialystokCrimeDataClass)
auc3 <- ratingClass$getAvreageAUC(results3)
timeLoggingClass$stop()
######################################################


data <- bialystokCrimeDataClass$getData()
trainData <- data[1:1000, ]
testData <- data[1:1000, 1:3]
source(file="SVMModelClass.R")
svmModel <- SVMModelClass()
svmModel$trainModel(trainData)
res1 <- svmModel$predictLabels(testData)






