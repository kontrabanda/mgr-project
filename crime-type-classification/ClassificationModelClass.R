ClassificationModelClass <- setRefClass(
  Class="ClassificationModelClass",
  methods = list(
    init = function() {
      print("ClassificationModelClass: init")
    },
    trainModel = function(data) {
      print("ClassificationModelClass: trainModel")
    },
    predictLabels = function(test) {
      print("ClassificationModelClass: predictLabels")
    }
  )
)