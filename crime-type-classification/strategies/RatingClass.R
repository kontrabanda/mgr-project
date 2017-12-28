library(ROCR)
source(file="./data/DataClass.R")

RatingClass <- setRefClass(
  Class="RatingClass",
  fields=list(
    dataClass="DataClass",
    aucFilePath="character",
    rocPlotsPath="character"
  ),
  methods = list(
    initialize = function(dataClassInput, path = './temp') {
      dataClass <<- dataClassInput
      aucFilePath <<- getAucFilePath(path)
      rocPlotsPath <<- getRocPlotsPath(path)
    },
    getAucFilePath = function(path) {
      paste(path, 'auc.csv', sep = '/')
    },
    getRocPlotsPath = function(path) {
      dirPath <- paste(path, 'roc-plots', sep = '/')
      dir.create(dirPath)
      dirPath
    },
    getAvreageAUC = function(results) {
      categoriesNames <- dataClass$getCategoriesNames()
      aucs <- data.frame(data.frame(matrix(NA, nrow = length(categoriesNames), ncol = 2)))
      colnames(aucs) <- c('category', 'value')
      
      for(i in 1:length(categoriesNames)) {
        category <- categoriesNames[i]
        data <- dataClass$getDataLabeledByCategory(category)
        pr <- prediction(results[, category], data$label)
        saveRocPlot(pr, category)
        roc <- performance(pr, measure = "auc")
        aucs[i, 'category'] <- as.character(category)
        aucs[i, 'value'] <- roc@y.values[[1]]
      }
      
      meanValue <- mean(aucs$value)
      aucs[nrow(aucs), ] <- c('mean', meanValue)
      write.csv(aucs, file = aucFilePath)
      
      meanValue
    },
    saveRocPlot = function(pred, category) {
      rocPerf <- performance(pred, measure = "tpr", x.measure = "fpr")
      fileName <- paste(as.character(category), 'png', sep = '.')
      filePath <- paste(rocPlotsPath, fileName, sep = '/')
      png(filePath, 2000, 2000, res=300)
      plot(rocPerf)
      abline(a = 0, b = 1, col = 'red')
      dev.off()
    }
  )
)