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
results3 <- checker$crossValidation(sets = 6:10)
results3 <- checker$getResults()

ratingClass <- RatingClass(bialystokCrimeDataClass)
auc3 <- ratingClass$getAvreageAUC(results3)
timeLoggingClass$stop()
######################################################

##################### kNN ###########################
source(file="kNNModelClass.R")
timeLoggingClass$start()
checker <- CheckerWithResultSaving(bialystokCrimeDataClass, kNNModelClass)
results4 <- checker$crossValidation()
results4 <- checker$getResults()

ratingClass <- RatingClass(bialystokCrimeDataClass)
auc4 <- ratingClass$getAvreageAUC(results4)
timeLoggingClass$stop()
######################################################

##################### random forest ###########################
source(file="RandomForestModelClass.R")
timeLoggingClass$start()
checker <- CheckerWithResultSaving(bialystokCrimeDataClass, RandomForestModelClass)
results5 <- checker$crossValidation()

ratingClass <- RatingClass(bialystokCrimeDataClass)
auc5 <- ratingClass$getAvreageAUC(results5)
timeLoggingClass$stop()
######################################################
