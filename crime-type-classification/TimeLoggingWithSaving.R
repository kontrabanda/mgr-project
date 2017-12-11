library(beepr)

TimeLoggingWithSaving <- setRefClass(
  Class="TimeLoggingWithSaving",
  fields=list(
    name="character",
    results="data.frame"
  ),
  methods = list(
    initialize = function(name, iterationCount = 10) {
      results <<- data.frame(matrix(NA, nrow = 0, ncol = 3))
      colnames(results) <<- c('start', 'end', 'diff')
      name <<- name
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
      beep()
    },
    save = function(path) {
      write.csv(getFormatedResults(), file = getPath(path))
    },
    getPath = function(path) {
      filepath <- paste(path, "time", sep = '/')
      filepath <- paste(filepath, name, sep = '')
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