library(e1071)
rm(list = ls())

Contract <- read.csv(file = "G:\\Codes\\Text_Mining\\R script_Yile\\TDMatrix77_classified_trimed_2.csv", header = T)

# 2 - 27   3 - 40    7 - 77

s <- sample(27, 17)

colN <- 500

Cont_train <- Contract[s, 3:(3+colN)]
Cont_test <- Contract[-s, 3:(3+colN)]


svmfit <- svm(TYPE~. , data = Cont_train, kernel = "linear", cost = .001, scale = F)
print(svmfit)
plot(svmfit, Cont_train[ , col])

tuned <- tune(svm, TYPE~., data = Cont_train, kernel = "linear", ranges = list(cost = c(0.001, 0.01, .1, 1, 10, 100)))
summary(tuned)

p <- predict(svmfit, Cont_test, type = "class")
plot(p)
table(Cont_test[,1], p)
Success_Rate = mean(Cont_test[,1] == p)

Success_Rate
