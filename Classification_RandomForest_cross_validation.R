rm(list = ls())

Contract <- read.csv(file = "G:\\Codes\\Text_Mining\\R script_Yile\\TDMatrix77_classified_trimed_2.csv", header = T)


library(randomForest)



Rate1 = 0

for (i in c(5, 10, 20, 50, 100, 500)){
  
  Rate = 0
  
  for (j in 1:100){

    s <- sample(27, 25)
    
colN <- i
  
print(colN)

Cont_train <- Contract[s, 3:(3+colN)]
Cont_test <- Contract[-s, 3:(3+colN)]

rfm <- randomForest(TYPE ~ ., data = Cont_train)

p <- predict(rfm, Cont_test)

table(Cont_test[,1], p)
Success_Rate = mean(Cont_test[,1] == p)

print(Success_Rate)
importance(rfm)
getTree(rfm, 500, labelVar = T)

Rate = c(Rate, Success_Rate)
  }
  
  Rate1 = c(Rate1, mean(Rate))

}

Rate1