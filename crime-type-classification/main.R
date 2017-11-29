source(file="BialystokCrimeDataClass.R")
source(file="ModelCheckerClass.R")
source(file="RatingClass.R")
source(file="TimeLoggingClass.R")

withTraining <- T
timeLoggingClass <- TimeLoggingClass()
bialystokCrimeDataClass <- BialystokCrimeDataClass()

#################### LOGISTIC REGRESSION  ###########################
timeLoggingClass$start()
source(file="LogisticRegressionModelClass.R")
modelChekcerClass <- ModelCheckerClass(bialystokCrimeDataClass, LogisticRegressionModelClass, 'logicRegression', withTraining = withTraining)
results1 <- modelChekcerClass$crossValidation()

ratingClass <- RatingClass(bialystokCrimeDataClass)
auc1 <- ratingClass$getAvreageAUC(results1)
timeLoggingClass$stop()
###############################################

################### BAYES  ############################
timeLoggingClass$start()
source(file="NaiveBayesModelClass.R")
modelChekcerClass <- ModelCheckerClass(bialystokCrimeDataClass, NaiveBayesModelClass, 'naiveBayes', withTraining = withTraining)
results2 <- modelChekcerClass$crossValidation()

ratingClass <- RatingClass(bialystokCrimeDataClass)
auc2 <- ratingClass$getAvreageAUC(results2)
timeLoggingClass$stop()
###############################################
library(e1071)
data <- bialystokCrimeDataClass$getData()
train <- data[1:3500, ]
test <- data[3500:8000, c("lng", "lat", "date")]

model1 <- svm(label~., train, probability = TRUE)
pred <- predict(model1, test, probability = TRUE)
attr(pred, "prob")[1:10,]

rpusvm(x, y, type="eps-regression", scale=FALSE)

train <- data
test <- data[10000:20000,]

# strasznie wolny!!!!!!!!!! nie doszedłem do końca
start.time <- Sys.time()
model1 <- svm(label~., train, probability = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# zdecydowanie lepiej jeżeli chodzi o uczenie (ale trzeba sprawdzić jego jakość)
library(parallelSVM)
source(file="SVMModelClass.R")
start.time <- Sys.time()
model2 <- parallelSVM(label~., train, probability = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



pred <- predict(model2, test, probability = TRUE)
attr(pred, "prob")
##################### SVM ###########################
source(file="SVMModelClass.R")
timeLoggingClass$start()
modelChekcerClass <- ModelCheckerClass(bialystokCrimeDataClass, SVMModelClass, 'svmModelClass', withTraining = withTraining)
results3 <- modelChekcerClass$crossValidation()

ratingClass <- RatingClass(bialystokCrimeDataClass)
auc3 <- ratingClass$getAvreageAUC(results3)
timeLoggingClass$stop()
######################################################

data <- bialystokCrimeDataClass$getData()
svmModel <- SVMModelClass()

svmModel$trainModel(data)
timeLoggingClass$start()
svmModel$predictLabels(data[, 1:3])
timeLoggingClass$stop()

model <- parallelSVM(label~., data, probability = TRUE)
timeLoggingClass$stop()

