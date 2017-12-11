library(ROCR)
source(file="DataClass.R")

RatingClass <- setRefClass(
  Class="RatingClass",
  fields=list(
    dataClass="DataClass",
    filepath="character"
  ),
  methods = list(
    initialize = function(dataClassInput, path = './temp') {
      dataClass <<- dataClassInput
      filepath <<- getFilePath(path)
    },
    getFilePath = function(path) {
      paste(path, 'auc.csv', sep = '/')
    },
    getAvreageAUC = function(results) {
      categoriesNames <- dataClass$getCategoriesNames()
      aucs <- data.frame(data.frame(matrix(NA, nrow = length(categoriesNames), ncol = 2)))
      colnames(aucs) <- c('category', 'value')
      
      for(i in 1:length(categoriesNames)) {
        category <- categoriesNames[i]
        data <- dataClass$getDataLabeledByCategory(category)
        pr <- prediction(results[, category], data$label)
        auc <- performance(pr, measure = "auc")
        aucs[i, 'category'] <- as.character(category)
        aucs[i, 'value'] <- auc@y.values[[1]]
      }
      
      meanValue <- mean(aucs$value)
      aucs[nrow(aucs), ] <- c('mean', meanValue)
      write.csv(aucs, file = filepath)
      
      meanValue
    }
  )
)