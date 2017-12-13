source(file="Util.R")

#### Białystok
bialystokCrimeDataClass <- BialystokCrimeDataClass()

#LOGISTIC REGRESSION
checkClassifier(bialystokCrimeDataClass, LogisticRegressionModelClass)

#BAYES
checkClassifier(bialystokCrimeDataClass, NaiveBayesModelClass)

#kNN
checkClassifier(bialystokCrimeDataClass, kNNModelClass)

#RANDOM FOREST
checkClassifier(bialystokCrimeDataClass, RandomForestModelClass)

#SVM
checkClassifier(bialystokCrimeDataClass, SVMModelClass)


#### Boston
bostonCrimeDataClass <- BostonCrimeDataClass()

#LOGISTIC REGRESSION
checkClassifier(bostonCrimeDataClass, LogisticRegressionModelClass)

#BAYES
checkClassifier(bostonCrimeDataClass, NaiveBayesModelClass)

#kNN
checkClassifier(bostonCrimeDataClass, kNNModelClass)

#RANDOM FOREST
checkClassifier(bostonCrimeDataClass, RandomForestModelClass)

#SVM
checkClassifier(bostonCrimeDataClass, SVMModelClass)

#### Bournemouth
bournemouthCrimeDataClass <- BournemouthCrimeDataClass()
data <- bournemouthCrimeDataClass$getData()

