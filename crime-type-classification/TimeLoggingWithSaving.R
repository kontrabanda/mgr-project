library(beepr)

TimeLoggingWithSaving <- setRefClass(
  Class="TimeLoggingWithSaving",
  fields=list(
    name="character",
    path="character",
    results="data.frame"
  ),
  methods = list(
    initialize = function(name, resultPath, iterationCount = 10) {
      results <<- data.frame(matrix(NA, nrow = 0, ncol = 3))
      colnames(results) <<- c('start', 'end', 'diff')
      name <<- name
      path <<- getPath(resultPath)
    },
    start = function(i) {
      startTime <- Sys.time()
      cat(sprintf('%s %s iteration. Start time: %s \n', name, i, format(startTime, usetz = TRUE)))
      results[i, 'start'] <<- format(startTime, "%H:%M:%S")
    },
    stop = function(i) {
      endTime <- Sys.time()
      startTime <- as.POSIXct(results[i, 'start'], format = "%H:%M:%S")
      results[i, 'end'] <<- format(endTime, "%H:%M:%S")
      timeTaken <- endTime - startTime
      results[i, 'diff'] <<- format(timeTaken, usetz = TRUE)
      print(format(timeTaken, usetz = TRUE))
      save()
      beep()
    },
    save = function() {
      write.csv(getFormatedResults(), file = path)
    },
    getPath = function(resultPath) {
      fileName <- paste('time', name, sep = "")
      fileName <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), fileName, sep = "--")
      filepath <- paste(resultPath, fileName, sep = '/')
      filepath <- paste(filepath, 'csv', sep = '.')
      filepath
    },
    getFormatedResults = function() {
      res <- results
      res$start <- format(results$start, usetz = TRUE)
      res$end <- format(results$end, usetz = TRUE)
      res
    }
  )
)