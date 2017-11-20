library(cvAUC)

data(ROCR.simple)
auc <- AUC(ROCR.simple$predictions, ROCR.simple$labels)
auc


iid_example <- function(data, V=10){
  .cvFolds <- function(Y, V){ #Create CV folds (stratify by outcome)
    Y0 <- split(sample(which(Y==0)), rep(1:V, length=length(which(Y==0))))
    Y1 <- split(sample(which(Y==1)), rep(1:V, length=length(which(Y==1))))
    folds <- vector("list", length=V)
    for (v in seq(V)) {folds[[v]] <- c(Y0[[v]], Y1[[v]])}
    return(folds)
  }
  .doFit <- function(v, folds, data){ #Train/test glm for each fold
    fit <- glm(Y~., data=data[-folds[[v]],], family=binomial)
    pred <- predict(fit, newdata=data[folds[[v]],], type="response")
    return(pred)
  }
  folds <- .cvFolds(Y=data$Y, V=V) #Create folds
  predictions <- unlist(sapply(seq(V), .doFit, folds=folds, data=data)) #CV train/predict
  predictions[unlist(folds)] <- predictions #Re-order pred values
  # Get CV AUC and confidence interval
  out <- ci.cvAUC(predictions=predictions, labels=data$Y, folds=folds, confidence=0.95)
  return(out)
}

data(admissions)

set.seed(1)
out <- iid_example(data=admissions, V=10)

V <- 10
.cvFolds <- function(Y, V){ #Create CV folds (stratify by outcome)
  Y0 <- split(sample(which(Y==0)), rep(1:V, length=length(which(Y==0))))
  Y1 <- split(sample(which(Y==1)), rep(1:V, length=length(which(Y==1))))
  folds <- vector("list", length=V)
  for (v in seq(V)) {folds[[v]] <- c(Y0[[v]], Y1[[v]])}
  return(folds)
}
.doFit <- function(v, folds, data){ #Train/test glm for each fold
  fit <- glm(Y~., data=data[-folds[[v]],], family=binomial)
  pred <- predict(fit, newdata=data[folds[[v]],], type="response")
  return(pred)
}
folds <- .cvFolds(Y=admissions$Y, V=V) #Create folds
predictions <- unlist(sapply(seq(V), .doFit, folds=folds, data=admissions)) #CV train/predict
predictions[unlist(folds)] <- predictions #Re-order pred values
# Get CV AUC and confidence interval
out <- ci.cvAUC(predictions=predictions, labels=data$Y, folds=folds, confidence=0.95)