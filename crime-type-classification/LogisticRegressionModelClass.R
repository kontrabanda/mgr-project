source(file="ClassificationModelClass.R")

LogisticRegressionModelClass <- setRefClass(
  Class="LogisticRegressionModelClass",
  fields=list(
    models="data.frame"
  ),
  methods = list(
    trainModel = function(data) {
      labels <- unique(data$label)
      models <<- createModelsDF(labels)
      for(label in labels) {
        createSingleCategoryModel(label, data)
      }
    },
    createSingleCategoryModel = function(label, data) {
      train <- data
      train$label <- ifelse(train$label==label, 1, 0)
      model <- glm(label~., family=binomial(link='logit'), data=train)
      save(model, file=models[label, "filename"])
      rm(model)
    },
    createModelsDF = function(labels) {
      modelsL <- data.frame(matrix(NA, nrow = length(labels), ncol = 1))
      colnames(modelsL) <- c("filename")
      rownames(modelsL) <- labels
      for (label in labels) {
        modelsL[label, "filename"] <- paste("./temp/LogisticRegressionModelClass_model_", label, ".RData", sep="")
      }
      modelsL
    },
    predictLabels = function(test) {
      labels <- data.frame(matrix(NA, nrow = nrow(test), ncol = nrow(models)))
      colnames(labels) <- rownames(models)
      nrowNames <- rownames(models)
      
      for(i in 1:nrow(models)) {
        category <- rownames(models)[i]
        load(models[category, "filename"])
        labels[, c(category)] <- predict(model, newdata = test[, c("lat", "lng", "date")], type = 'response')
        rm(model)
      }
      
      labels
    }
  ),
  contains=c("ClassificationModelClass")
)