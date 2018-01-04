source(file="./data/BialystokCrimeDataClass.R")
source(file="./data/BostonCrimeDataClass.R")
source(file="./data/BournemouthCrimeDataClass.R")

### Białystok
bialystokCrimeDataClass <- BialystokCrimeDataClass()
data <- bialystokCrimeDataClass$getDataWithFullDate()

dataWithOnlyMonthWithoutLabel <- data[, c("lat", "lng", "month")]
dataWithFullDateWithoutLabel <- data[, c("lat", "lng", "day", "month", "year")]
dataWithOnlyMonth <- data[, c("lat", "lng", "month", "label")]
dataWithFullDate <- data[, c("lat", "lng", "day", "month", "year", "label")]

rowsCount <- nrow(data)
onlyMonthCount <- nrow(unique(dataWithOnlyMonthWithoutLabel))
fullCount <- nrow(unique(dataWithFullDateWithoutLabel))
onlyMonthLabelCount <- nrow(unique(dataWithOnlyMonth))
fullLabelCount <- nrow(unique(dataWithFullDate))
multiclassCountMounth <- rowsCount - onlyMonthLabelCount
multiclassFull <- rowsCount - fullLabelCount

cat("\nBiałystok:",
    "\nOgółem -", rowsCount, 
    "\nUnikalne z datą w postaci miesiąca bez labelki - ", onlyMonthCount,
    "\nUnikalne z pełną datą bez labelki - ", fullCount,
    "\nUnikalne z datą w postaci miesiąca z labelką - ", onlyMonthLabelCount,
    "\nUnikalne z pełną datą z labelką - ", fullLabelCount,
    "\nDane multiklasowe data w postaci miesiąca - ", multiclassCountMounth,
    "\nDane multiklasowe pełna data - ", multiclassFull,
    "\nProcentowo:",
    "\nOgółem -", 100, "%",
    "\nUnikalne z datą w postaci miesiąca bez labelki - ", onlyMonthCount/rowsCount * 100, "%",
    "\nUnikalne z pełną datą bez labelki - ", fullCount/rowsCount * 100, "%",
    "\nUnikalne z datą w postaci miesiąca z labelką - ", onlyMonthLabelCount/rowsCount * 100, "%",
    "\nUnikalne z pełną datą z labelką - ", fullLabelCount/rowsCount * 100, "%",
    "\nDane multiklasowe data w postaci miesiąca - ", multiclassCountMounth/rowsCount * 100, "%",
    "\nDane multiklasowe pełna data - ", multiclassFull/rowsCount * 100, "%")

### Bournemouth
bournemouthCrimeDataClass <- BournemouthCrimeDataClass()
data <- bournemouthCrimeDataClass$getDataWithFullDate()

dataWithOnlyMonthWithoutLabel <- data[, c("lat", "lng", "month")]
dataWithFullDateWithoutLabel <- data[, c("lat", "lng", "month")]
dataWithOnlyMonth <- data[, c("lat", "lng", "month", "label")]
dataWithFullDate <- data[, c("lat", "lng", "month", "label")]

rowsCount <- nrow(data)
onlyMonthCount <- nrow(unique(dataWithOnlyMonthWithoutLabel))
fullCount <- nrow(unique(dataWithFullDateWithoutLabel))
onlyMonthLabelCount <- nrow(unique(dataWithOnlyMonth))
fullLabelCount <- nrow(unique(dataWithFullDate))
multiclassCountMounth <- rowsCount - onlyMonthLabelCount
multiclassFull <- rowsCount - fullLabelCount

cat("\nBournemouth:",
    "\nOgółem -", rowsCount, 
    "\nUnikalne z datą w postaci miesiąca bez labelki - ", onlyMonthCount,
    "\nUnikalne z pełną datą bez labelki - ", fullCount,
    "\nUnikalne z datą w postaci miesiąca z labelką - ", onlyMonthLabelCount,
    "\nUnikalne z pełną datą z labelką - ", fullLabelCount,
    "\nDane multiklasowe data w postaci miesiąca - ", multiclassCountMounth,
    "\nDane multiklasowe pełna data - ", multiclassFull,
    "\nProcentowo:",
    "\nOgółem -", 100, "%",
    "\nUnikalne z datą w postaci miesiąca bez labelki - ", onlyMonthCount/rowsCount * 100, "%",
    "\nUnikalne z pełną datą bez labelki - ", fullCount/rowsCount * 100, "%",
    "\nUnikalne z datą w postaci miesiąca z labelką - ", onlyMonthLabelCount/rowsCount * 100, "%",
    "\nUnikalne z pełną datą z labelką - ", fullLabelCount/rowsCount * 100, "%",
    "\nDane multiklasowe data w postaci miesiąca - ", multiclassCountMounth/rowsCount * 100, "%",
    "\nDane multiklasowe pełna data - ", multiclassFull/rowsCount * 100, "%")

### Boston
bostonCrimeDataClass <- BostonCrimeDataClass()
data <- bostonCrimeDataClass$getDataWithFullDate()

dataWithOnlyMonthWithoutLabel <- data[, c("lat", "lng", "month")]
dataWithFullDateWithoutLabel <- data[, c("lat", "lng", "hour", "day", "month", "year")]
dataWithOnlyMonth <- data[, c("lat", "lng", "month", "label")]
dataWithFullDate <- data[, c("lat", "lng", "hour", "day", "month", "year", "label")]

rowsCount <- nrow(data)
onlyMonthCount <- nrow(unique(dataWithOnlyMonthWithoutLabel))
fullCount <- nrow(unique(dataWithFullDateWithoutLabel))
onlyMonthLabelCount <- nrow(unique(dataWithOnlyMonth))
fullLabelCount <- nrow(unique(dataWithFullDate))
multiclassCountMounth <- rowsCount - onlyMonthLabelCount
multiclassFull <- rowsCount - fullLabelCount

cat("\nBoston:",
    "\nOgółem -", rowsCount, 
    "\nUnikalne z datą w postaci miesiąca bez labelki - ", onlyMonthCount,
    "\nUnikalne z pełną datą bez labelki - ", fullCount,
    "\nUnikalne z datą w postaci miesiąca z labelką - ", onlyMonthLabelCount,
    "\nUnikalne z pełną datą z labelką - ", fullLabelCount,
    "\nDane multiklasowe data w postaci miesiąca - ", multiclassCountMounth,
    "\nDane multiklasowe pełna data - ", multiclassFull,
    "\nProcentowo:",
    "\nOgółem -", 100, "%",
    "\nUnikalne z datą w postaci miesiąca bez labelki - ", onlyMonthCount/rowsCount * 100, "%",
    "\nUnikalne z pełną datą bez labelki - ", fullCount/rowsCount * 100, "%",
    "\nUnikalne z datą w postaci miesiąca z labelką - ", onlyMonthLabelCount/rowsCount * 100, "%",
    "\nUnikalne z pełną datą z labelką - ", fullLabelCount/rowsCount * 100, "%",
    "\nDane multiklasowe data w postaci miesiąca - ", multiclassCountMounth/rowsCount * 100, "%",
    "\nDane multiklasowe pełna data - ", multiclassFull/rowsCount * 100, "%")
