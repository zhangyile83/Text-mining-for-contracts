rm(list = ls())
library(RPostgreSQL)
source("stopkey.R")


TimeGen <- function(Year = 2011, Month = 1, Day = 1){
  return(paste("'", as.character(Year), "-", as.character(Month), "-", as.character(Day), "'", sep=""))
}

Interval_FreqCount <- function(Time1, Time2){
  QueryC = paste("SELECT * FROM
                 (SELECT signings.signer_id, MIN(signings.created_at) MinSignTime
                 FROM signings Group BY signings.signer_id ORDER BY MinSignTime) AS TEM1
                 INNER JOIN
                 (SELECT contracts.creator_id, MIN(contracts.created_at) MinCreatTime
                 FROM contracts Group BY contracts.creator_id ORDER BY MinCreatTime) AS TEM2
                 ON TEM1.signer_id = TEM2.creator_id
                 WHERE MinCreatTime BETWEEN", Time1, " AND ", Time2, "AND MinCreatTime > MinSignTime")
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

MonthTotal <- 0
MonthAverage <- 0
No.User = 0
d = 0
for(i in 1:(nrow(A)-1)){
  temp = Interval_FreqCount(TimeGen(A[i, 1], A[i, 2]), TimeGen(A[i+1, 1], A[i+1, 2]))
  interval_total <- 0
  interval_average <- 0
  for(j in 1:(nrow(temp))){
    UserID <- temp[j,1]
    if(length(UserID)==0){
      interval_average <- 0
      interval_total <- 0
      next
    }else{
      QueryC = paste("SELECT COUNT(*) FROM contracts WHERE creator_id = ", UserID)
      drv <- dbDriver("PostgreSQL")
      conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
      user_total_usage <- dbGetQuery(conn, QueryC)
      interval_total <- interval_total + user_total_usage
      print(interval_total)
      interval_average <- interval_average + user_total_usage/(nrow(A)-i)
      print(interval_average)
      dbDisconnect(conn)    
    }
  }
  MonthTotal <- c(MonthTotal, interval_total)
  MonthAverage <- c(MonthAverage, interval_average)
  
  # print(nrow(temp))
  d = c(d, nrow(temp))
}
MonthTotal <- as.matrix(as.data.frame(MonthTotal))
MonthTotal <- MonthTotal[2:ncol(MonthTotal)]

MonthAverage <- as.matrix(as.data.frame(MonthAverage))
MonthAverage <- MonthAverage[2:ncol(MonthAverage)]

d <- as.matrix(as.data.frame(d))

x = 1:(length(d)-1)
y = d[2:length(d)]
plot(x, y, xaxt = "n", xlab='Month')
axis(1,at = 1:length(d), labels = A[,2])
# lines(lowess(1:(length(d)-1),d[2:length(d)])); 
lines(x,predict(loess(y~x)))


Monthlog <- A[,2]
Monthlog <- Monthlog[2:length(Monthlog)]

MonthTotal1= 0
for(i in 1:length(MonthTotal)){
  MonthTotal1[i] = sum(MonthTotal[1:i])
}

y1 = 0
for(i in 1:length(y)){
  y1[i] = sum(y[1:i])
}

V1 <- rbind(x, Monthlog, y, y1)
V2 <- rbind(x, Monthlog, MonthTotal, MonthTotal1)

V3 <- rbind(x, Monthlog, MonthAverage)
write.csv(V1, file = "G:\\Codes\\Text_Mining\\R script_v3\\NumOfSwitcher.csv")

write.csv(V2, file = "G:\\Codes\\Text_Mining\\R script_v3\\ContractCreatedBySwitcher.csv")

write.csv(V3, file = "G:\\Codes\\Text_Mining\\R script_v3\\MonthlyAverCreatedBySwitcher.csv")

sum(d)/1134

plot(x, MonthTotal, xaxt = "n", xlab='Month')
axis(1,at = 1:length(d), labels = A[,2])


sum(MonthTotal)/65500

plot(x, MonthAverage, xaxt = "n", xlab='Month')
axis(1,at = 1:length(d), labels = A[,2])

####################################################################################################


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
plot(x, y, xaxt = "n", xlab='Month')
axis(1,at = 1:length(d), labels = A[,2])
# lines(lowess(1:(length(d)-1),d[2:length(d)])); 
lines(x,predict(loess(y~x)))

y1 = 0
for(i in 1:length(y)){
  y1[i] = sum(y[1:i])
}

Total1 <- rbind(y, y1)

plot(MonthTotal/y)

