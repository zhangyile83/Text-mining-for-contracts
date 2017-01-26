rm(list = ls())
library(RPostgreSQL)



TimeGen <- function(Year = 2011, Month = 1, Day = 1){
  return(paste("'", as.character(Year), "-", as.character(Month), "-", as.character(Day), "'", sep=""))
}

Interval_FreqCount <- function(Time1, Time2){
  QueryC = paste("SELECT count(TEM1.id) FROM
                 (SELECT id, created_at, last_login_at
                 FROM users
                 WHERE last_login_at BETWEEN '2010-10-1' AND '2018-10-1') AS TEM1
                 WHERE TEM1.created_at BETWEEN  ", Time1, " AND ", Time2)
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
  result_temp <- dbGetQuery(conn, QueryC)
  dbDisconnect(conn)
  return(result_temp)
}


source("Time_Interval_Generator.R")
Year1 = 2011
Month1 = 10
Year2 = 2016
Month2 = 10
A = Time_Generator(c(Year1, Month1), c(Year2, Month2))

d = 0
for(i in 1:(nrow(A)-1)){
  temp = Interval_FreqCount(TimeGen(A[i, 1], A[i, 2]), TimeGen(A[i+1, 1], A[i+1, 2]))
  print(temp)
  d = c(d, temp)
}
d <- as.matrix(as.data.frame(d))

x = 1:(length(d)-1)
y = d[2:length(d)]
plot(x, y, xaxt = "n", xlab='Month')
axis(1,at = 1:length(d), labels = A[,2])
# lines(lowess(1:(length(d)-1),d[2:length(d)])); 
lines(x,predict(loess(y~x)))
MonthlyNewCreator <- t(y)
MonthlyNewCreator <- as.numeric(MonthlyNewCreator)

TotalCreator = 0
for(i in 1:length(y)){
  TotalCreator[i] = sum(MonthlyNewCreator[1:i])
}

############################################################################


TimeGen <- function(Year = 2011, Month = 1, Day = 1){
  return(paste("'", as.character(Year), "-", as.character(Month), "-", as.character(Day), "'", sep=""))
}

Interval_FreqCount <- function(Time1, Time2){
  QueryC = paste("SELECT COUNT(*) FROM (SELECT creator_id, created_at FROM contracts WHERE created_at BETWEEN ", Time1, " AND ", Time2,
                 " ORDER BY created_at) AS TEM WHERE creator_id <> 6 AND creator_id <> 4")
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
  result_temp <- dbGetQuery(conn, QueryC)
  dbDisconnect(conn)
  return(result_temp)
}


source("Time_Interval_Generator.R")
Year1 = 2011
Month1 = 10
Year2 = 2016
Month2 = 10
A = Time_Generator(c(Year1, Month1), c(Year2, Month2))

d = 0
for(i in 1:(nrow(A)-1)){
  a = Interval_FreqCount(TimeGen(A[i, 1], A[i, 2]), TimeGen(A[i+1, 1], A[i+1, 2]))
  print(a)
  d = c(d, a)
}

d <- as.matrix(as.data.frame(d))
x = 1:(length(d)-1)
y = d[2:length(d)]
plot(x, y, xaxt = "n", xlab='Month', ylab='Monthly Total Contract Created')
axis(1,at = 1:length(d), labels = A[,2])
# lines(lowess(1:(length(d)-1),d[2:length(d)])); 
lines(x,predict(loess(y~x)))

MonthlyContract <- y

y1 = 0
for(i in 1:length(y)){
  y1[i] = sum(y[1:i])
}

barplot(y1, x, xaxt = "n", xlab='Month', ylab='Total Contract Accumulated')
axis(1,at = 1:length(d), labels = A[,2])
# lines(lowess(1:(length(d)-1),d[2:length(d)])); 
lines(x,predict(loess(y~x)))

TotalContract <- y1

AverageUsage <- TotalContract/TotalCreator
AverageUsage[AverageUsage == NaN] = 1

barplot(AverageUsage[12:length(AverageUsage)], xaxt = "n", xlab='Month', ylab='Average Usage per Account')
