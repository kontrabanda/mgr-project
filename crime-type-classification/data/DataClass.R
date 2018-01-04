DataClass <- setRefClass(
  Class="DataClass",
  fields=list(
    rawData="data.frame",
    name='character'
  ),
  methods = list(
    init = function() {
      print("DataClass: init") 
    },
    getData = function() {
      print("DataClass: getData, data with Label as Category")
    },
    getDataWithFullDate = function() {
      print("DataClass: getDataWithFullDate, data with full date")
    },
    getDataLabeledByCategory = function(categoryName) {
      print("DataClass: getDataLabeledByCategory, data with Label as 0 or 1 for specyfic category")
    },
    getCategoriesNames = function() {
      print("DataClass: getCategoriesNames")
    }
  )
)