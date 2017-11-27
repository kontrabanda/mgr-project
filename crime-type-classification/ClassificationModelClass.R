ClassificationModelClass <- setRefClass(
  Class="ClassificationModelClass",
  methods = list(
    trainModel = function(data, name) {
      print("ClassificationModelClass: trainModel")
    },
    predictLabels = function(test, name) {
      print("ClassificationModelClass: predictLabels")
    }
  )
)