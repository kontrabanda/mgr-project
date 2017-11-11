library(raster)
library(sp)
library(latticeExtra)
library(RColorBrewer)
library(ggplot2)
library(maptools)
library(spatstat)
library(ROCR)
library(pscl)

bialystok <- shapefile("../../data/bialystok/bialystok.shp")
bialystok <- spTransform(bialystok, CRS("+init=epsg:4326"))
onlyBialystok <- aggregate(bialystok)

crime <- read.csv("../../data/Polska/zdarzenia_rsow.csv", sep = "|")
crime <- crime[!is.na(crime$LAT)&!is.na(crime$LNG), ]

# zmiana z , na . w danych (inaczej traktowane są jako string)
crime$LAT <- as.numeric(gsub(",", ".", gsub("\\.", "", crime[["LAT"]])))
crime$LNG <- as.numeric(gsub(",", ".", gsub("\\.", "", crime[["LNG"]])))

coordinates(crime) =~ LNG+LAT
projection(crime) = projection(bialystok)

## wybranie przestępstw tylko z Białegostoku
# crimeBialystok <- intersect(crime, onlyBialystok)
# crimeBialystokDF <- data.frame(crimeBialystok)
# write.csv(crimeBialystokDF, file = "../../data/Polska/zdarzenia_rsow_bialystok.csv", sep = "|")

crimeBialystokDF <- read.csv("../../data/Polska/zdarzenia_rsow_bialystok.csv", sep = ",")
crimeBialystokDF$DATA <- as.Date(crimeBialystokDF$DATA, "%y/%m/%d")

data <- crimeBialystokDF
data$LABEL = ifelse(crimeBialystokDF$KAT=='CHU', 1, 0)
data <- subset(data, select=c(3, 6, 7, 9))

set.seed(123)
# data_ind <- sample(seq_len(nrow(data)), size = 50000)
# data <- data[data_ind, ]

## 75% of the sample size
smp_size <- floor(0.75 * nrow(data))

## set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]
testLabels <- test$LABEL
test <- test[, 1:3]

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
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


