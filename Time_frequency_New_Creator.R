rm(list = ls())
library(RPostgreSQL)
DBNAME = "Inkdit"
USER = "postgres" 
PASSWORD = "314159"



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
  conn <- dbConnect(drv, dbname = DBNAME, user = USER, password = PASSWORD)
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

write.csv(y, file = "G:\\Codes\\Text_Mining\\R script_v3\\NewCreater.csv")
