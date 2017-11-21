library(ROCR)
source(file="DataClass.R")

RatingClass <- setRefClass(
  Class="RatingClass",
  fields=list(
    dataClass="DataClass"
  ),
  methods = list(
    init = function(dataClassInput) {
      dataClass <<- dataClassInput
    },
    getAvreageAUC = function(results) {
      categoriesNames <- dataClass$getCategoriesNames()
      aucs <- c()
      
      for(i in 1:length(categoriesNames)) {
        category <- categoriesNames[i]
        data <- dataClass$getDataLabeledByCategory(category)
        pr <- prediction(results[, category], data$label)
        auc <- performance(pr, measure = "auc")
        aucs[i] <- auc@y.values[[1]]
      }
      
      mean(aucs)
    }
  )
)