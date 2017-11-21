DataClass <- setRefClass(
  Class="DataClass",
  fields=list(
    rawData="data.frame"
  ),
  methods = list(
    init = function() {
      print("DataClass: init") 
    },
    getData = function() {
      print("DataClass: getData, data with Label as Category")
    },
    getDataLabeledByCategory = function(categoryName) {
      print("DataClass: getDataLabeledByCategory, data with Label as 0 or 1 for specyfic category")
    },
    getCategoriesNames = function() {
      print("DataClass: getCategoriesNames")
    }
  )
)