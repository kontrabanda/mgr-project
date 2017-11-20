library(caret)

source(file="BialystokCrimeDataClass.R")
source(file="LogisticRegressionModelClass.R")
source(file="ModelCheckerClass.R")

bialystokCrimeDataClass <- BialystokCrimeDataClass()
logisticRegressionModelClass <- LogisticRegressionModelClass()
modelChekcerClass <- ModelCheckerClass()
modelChekcerClass$init(bialystokCrimeDataClass, logisticRegressionModelClass)
#results <- modelChekcerClass$normalChecking()
results <- modelChekcerClass$crossValidation()

bialystokCrimeDataClass$init()

data <- bialystokCrimeDataClass$getData()

smp_size <- floor(0.8 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

logisticRegressionModelClass <- LogisticRegressionModelClass()
logisticRegressionModelClass$trainModel(train)
results <- logisticRegressionModelClass$predictLabels(test[, c("lat", "lng", "date")])

k = 10
set.seed(123)
folds <- createFolds(factor(data$label), k = k, list = FALSE)

categories <- unique(data$label)
results <- data.frame(matrix(NA, nrow = 0, ncol = length(categories)))
colnames(results) <- categories

for(i in 1:k) {
  train <- data[folds != i, ]
  test <- data[folds == i, ]
  
  logisticRegressionModelClass <- LogisticRegressionModelClass()
  logisticRegressionModelClass$trainModel(train)
  testWithoutLabels <- test[, -which(names(test) == "label")]
  results <- rbind(results ,logisticRegressionModelClass$predictLabels(testWithoutLabels))
}

testWithoutLabels <- test[, -which(names(test) == "label")]

pr <- prediction(results, test$label)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

categoryNames <- unique(data$label)

model <- data.frame(matrix(NA, nrow = length(categoryNames), ncol = 1))
colnames(model) <- c("file")
rownames(model) <- categoryNames

temp <- c(1:10)

# nie sprawdza się trzeba jednak stworzyć macierz wyników, a nie macierz modeli -> każdy model to mega dużo pamięci
for (categoryName in categoryNames) {
  filename <- paste("./model_", categoryName, ".R", sep="")
  model[categoryName, "file"] <- filename
}

model["CHU", "file"]


unlist(models)
modelsDF <- data.frame(categoryNames, unlist(models))
