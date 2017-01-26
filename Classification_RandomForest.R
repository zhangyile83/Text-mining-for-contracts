rm(list = ls())

Contract <- read.csv(file = "G:\\Codes\\Text_Mining\\R script_Yile\\TDMatrix77_classified_trimed_2.csv", header = T)


library(randomForest)

s <- sample(27, 17)

colN <- 100
  
Cont_train <- Contract[s, 3:(3+colN)]
Cont_test <- Contract[-s, 3:(3+colN)]

rfm <- randomForest(TYPE ~ ., data = Cont_train)

p <- predict(rfm, Cont_test)

table(Cont_test[,1], p)
Success_Rate = mean(Cont_test[,1] == p)
importance(rfm)
getTree(rfm, 500, labelVar = T)

Success_Rate