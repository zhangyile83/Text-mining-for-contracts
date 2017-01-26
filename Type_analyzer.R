setwd("G:\\Codes\\Text_Mining\\R script_v3")
rm(list = ls())
A <- read.csv(file = "DB3.csv")
A1 <- A
B <- rep("", 24)
for(i in 5:24){
print(i)
A <- A1[as.matrix(A1[,i]) %in% c("model", "models"),]
print(A)
B <- rbind(B, as.matrix(A))
}
View(B)

c("mailout", "inkdit")
c("game", "games", "play", "playing")
c("company", "employment")
c("model")
c("festival")
c("party", "parties")
c("renter", "apartment")
c("child", "children")
C("yoga")
c("Univerisy")
c("footage", "video")

# B <- as.matrix(B)
# B < B[2:nrow(B),]
# A <- A[as.matrix(A[,10]) %in% c("yoga"),]
# print(A)
# 
# num_index <- 1:100
# num_index <- 2*num_index
# for(i in num_index){
#   # print(A[i,5:24])
#   # class(A[2,5:24])
#   # a <- as.matrix(A[2,5:24])
#   # a <- as.character(a)
#   if(grepl(as.matrix(A[i, 5:24]), "yoga")){
#     temp <- A[i,]
#     print(temp)
#   }
#   # View(temp)
#   #   # 
#   #   # temp1 <- rbind(temp1,temp)
#   #   print(temp)
# }
# 
# c("mailout", "inkdit")
# c("game", "games", "play", "playing")
# c("company", "employment")
# 
# 
# as.matrix(A[6, 5:24])
# 
# k <- as.matrix(A[, 5]) %in% c("inkdit")
