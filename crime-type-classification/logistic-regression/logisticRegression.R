library(raster)
library(sp)
library(latticeExtra)
library(RColorBrewer)
library(ggplot2)
library(maptools)
library(spatstat)
library(ROCR)
library(pscl)

crimeBialystokDF <- read.csv("../../../data/Polska/zdarzenia_rsow_bialystok.csv", sep = ",")
crimeBialystokDF$DATA <- as.Date(crimeBialystokDF$DATA, "%y/%m/%d")

data <- crimeBialystokDF[, c("KAT", "DATA", "LAT", "LNG")]
data$LABEL = ifelse(crimeBialystokDF$KAT=='CHU', 1, 0)

set.seed(123)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(data))

## set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

createLogicRegressionModelForCrimeCategory <- function(categoryName, train, test) {
  testLabels <- ifelse(test$KAT==categoryName, 1, 0)
  train$LABEL <- ifelse(train$KAT==categoryName, 1, 0)
  train <- train[, c("DATA", "LAT", "LNG", "LABEL")]
  test <- test[, c("DATA", "LAT", "LNG")]
  
  model <- glm(LABEL~., family=binomial(link='logit'), data=train)
  anova(model, test="Chisq")
  
  fitted.results <- predict(model, newdata=test, type='response')
  fitted.results <- ifelse(fitted.results > 0.5,1,0)
  
  misClasificError <- mean(fitted.results != testLabels)
  print(paste('Accuracy', 1-misClasificError))
  
  # ROC
  p <- predict(model, newdata=test, type="response")
  pr <- prediction(p, testLabels)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  auc
  
  filename <- paste('./results/LR_ROC', categoryName, sep='_')
  filename <- paste(filename, 'png', sep='.')
  png(filename = filename, width=2500, height=3000, res=300)
  plot(prf)
  plot.title <- paste('AUC', auc, sep='=')
  title(main = plot.title)
  dev.off()
}

categoryNames <- unique(data$KAT)
sapply(categoryNames, createLogicRegressionModelForCrimeCategory, train=train, test=test)
#createLogicRegressionModelForCrimeCategory("CHU", train, test)
