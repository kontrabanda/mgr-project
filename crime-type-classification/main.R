source(file="Util.R")

#### Białystok
bialystokCrimeDataClass <- BialystokCrimeDataClass()

#LOGISTIC REGRESSION
aucLogisticRegressionBialystok <- checkClassifier(bialystokCrimeDataClass, LogisticRegressionModelClass)

#BAYES
aucBayesBialystok <- checkClassifier(bialystokCrimeDataClass, NaiveBayesModelClass)

#kNN
aucKNNBialystok <- checkClassifier(bialystokCrimeDataClass, kNNModelClass)

#RANDOM FOREST
aucRandomForestBialystok <- checkClassifier(bialystokCrimeDataClass, RandomForestModelClass)

#SVM
aucSVMBialystok <- checkClassifier(bialystokCrimeDataClass, SVMModelClass)


#### Boston
bostonCrimeDataClass <- BostonCrimeDataClass()

#LOGISTIC REGRESSION
aucLogisticRegressionBoston <- checkClassifier(bostonCrimeDataClass, LogisticRegressionModelClass)

#BAYES
aucBayesBoston <- checkClassifier(bostonCrimeDataClass, NaiveBayesModelClass)

#kNN
aucKNNBoston <- checkClassifier(bostonCrimeDataClass, kNNModelClass)

#RANDOM FOREST
aucRandomForestBoston <- checkClassifier(bostonCrimeDataClass, RandomForestModelClass)

#SVM
aucSVMBoston <- checkClassifier(bostonCrimeDataClass, SVMModelClass)
