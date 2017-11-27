library(beepr)

TimeLoggingClass <- setRefClass(
  Class="TimeLoggingClass",
  fields=list(
    startTime = "POSIXct"
  ),
  methods = list(
    start = function() {
      startTime <<- Sys.time()
    },
    stop = function() {
      endTime <- Sys.time()
      timeTaken <- endTime - startTime
      print(timeTaken)
      timeTaken
      beep()
    }
  )
)