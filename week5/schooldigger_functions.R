

week5sampleFromSelf <- function(csv.file, testVariable){
  setwd("/Users/worshamn/Dropbox/Documents/Regis/MSDS600/week5/")
  DF <- read.csv (csv.file, header = TRUE)
  library(e1071)
  train.sample.DF <- sample(nrow(DF), ceiling(nrow(DF) * 0.7))
  test.sample.DF <- (1:nrow(DF)) [- train.sample.DF]
  x <- DF[train.sample.DF, !colnames(DF) %in% testVariable]
  y <- DF[train.sample.DF, colnames(DF) %in% testVariable]
  model <- svm(x, y)
  test.DF <- DF[test.sample.DF, !colnames(DF) %in% testVariable]
  test.DF.testVariable <- DF[test.sample.DF, colnames(DF) %in% testVariable]
  pred <- predict(model, test.DF)
  conf.mat <- table("Predictions" = pred, Actual = test.DF.testVariable)
  print(conf.mat)
  (accuracy <- sum(diag(conf.mat)) / length(test.sample.DF) * 100)
}

week5sampleFromModel <- function(model.csv.file, test.csv.file, testVariable){
  setwd("/Users/worshamn/Dropbox/Documents/Regis/MSDS600/week5/")
  model.DF <- read.csv (model.csv.file, header = TRUE)
  test.DF <- read.csv (test.csv.file, header = TRUE)
  library(e1071)
  #train.sample.DF <- sample(nrow(DF), ceiling(nrow(DF) * 0.7))
  #test.sample.DF <- (1:nrow(DF)) [- train.sample.DF]
  x <- model.DF[, !colnames(model.DF) %in% testVariable]
  y <- model.DF[, colnames(model.DF) %in% testVariable]
  model <- svm(x, y)
  test.DF.minusVariable <- test.DF[, !colnames(test.DF) %in% testVariable]
  test.DF.testVariable <- test.DF[, colnames(test.DF) %in% testVariable]
  pred <- predict(model, test.DF.minusVariable)
  conf.mat <- table("Predictions" = pred, Actual = test.DF.testVariable)
  print(conf.mat)
  (accuracy <- sum(diag(conf.mat)) / length(test.DF.testVariable) * 100)
}