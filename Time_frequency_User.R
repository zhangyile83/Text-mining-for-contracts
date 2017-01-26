rm(list = ls())
setwd("G:\\Codes\\Text_Mining\\R script_v3") 
library(RPostgreSQL)
source("Time_Interval_Generator.R")



TimeGen <- function(Year = 2011, Month = 1, Day = 1){
  return(paste("'", as.character(Year), "-", as.character(Month), "-", as.character(Day), "'", sep=""))
}

source("User_Frequency.R")
FrequentList = FrequentUser(100)
n = 1

Interval_FreqCount <- function(Time1, Time2){
  QueryC = paste("SELECT creator_id, created_at FROM contracts WHERE created_at BETWEEN ", Time1, " AND ", Time2, "AND creator_id =" , as.character(FrequentList[nrow(FrequentList)-n+1, 1]) , 
                 " ORDER BY created_at")
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
  result_temp <- dbGetQuery(conn, QueryC)
  dbDisconnect(conn)
  return(result_temp)
}

Year1 = 2011
Month1 = 10
Year2 = 2016
Month2 = 10
A = Time_Generator(c(Year1, Month1), c(Year2, Month2))

d = 0
for(i in 1:(nrow(A)-1)){
  a = Interval_FreqCount(TimeGen(A[i, 1], A[i, 2]), TimeGen(A[i+1, 1], A[i+1, 2]))
  temp = nrow(a)
  print(temp)
  d = c(d, temp)
}

x = 1:(length(d)-1)
y = d[2:length(d)]
plot(x, y, xaxt = "n", xlab='Month')
axis(1,at = 1:length(d), labels = A[,2]) 
lines(x,predict(loess(y~x)))

View(FrequentList)
# [nrow(FrequentList)-n+1, 2]