library(caret)
source(file="ClassificationModelClass.R")

ModelCheckerClass <- setRefClass(
  Class="ModelCheckerClass",
  fields=list(
    data="data.frame",
    classificationModel="ClassificationModelClass"
  ),
  methods = list(
    init = function(crimeDataClass, classificationModel) {
      data <<- crimeDataClass$getData()
      classificationModel <<- classificationModel
    },
    normalChecking = function() {
      smp_size <- floor(0.8 * nrow(data))
      train_ind <- sample(seq_len(nrow(data)), size = smp_size)
      train <- data[train_ind, ]
      test <- data[-train_ind, ]
      
      classificationModel$trainModel(train)
      results <- classificationModel$predictLabels(getDataWithoutLabels(test))
    },
    crossValidation = function(k = 10) {
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
      
      results
    },
    getDataWithoutLabels = function(data) {
      data[, -which(names(data) == "label")]
    }
  )
)