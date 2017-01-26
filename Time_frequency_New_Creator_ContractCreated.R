rm(list = ls())
library(RPostgreSQL)



TimeGen <- function(Year = 2011, Month = 1, Day = 1){
  return(paste("'", as.character(Year), "-", as.character(Month), "-", as.character(Day), "'", sep=""))
}

Interval_FreqCount <- function(Time1, Time2){
  QueryC = paste("SELECT creator_id FROM
                 (SELECT contracts.creator_id, MIN(contracts.created_at) MinCreatTime
                 FROM contracts Group BY contracts.creator_id ORDER BY MinCreatTime) TEM
                 WHERE TEM.MinCreatTime BETWEEN ", Time1, " AND ", Time2)
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
  result_temp <- dbGetQuery(conn, QueryC)
  dbDisconnect(conn)
  return(result_temp)
}


source("Time_Interval_Generator.R")
Year1 = 2011
Month1 = 6
Year2 = 2016
Month2 = 11
A = Time_Generator(c(Year1, Month1), c(Year2, Month2))

MonthTotal <- 0
MonthAverage <- 0

No.User = 0
for(i in 1:(nrow(A)-1)){
  temp = Interval_FreqCount(TimeGen(A[i, 1], A[i, 2]), TimeGen(A[i+1, 1], A[i+1, 2]))
  interval_total <- 0
  interval_average <- 0
  for(j in 1:(nrow(temp))){
    UserID <- temp[j,1]
    QueryC = paste("SELECT COUNT(*) FROM contracts WHERE creator_id = ", UserID)
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
    user_total_usage <- dbGetQuery(conn, QueryC)
    interval_total <- interval_total + user_total_usage
    interval_average <- interval_average + user_total_usage/(nrow(A)-i)
    dbDisconnect(conn)    
  }
  MonthTotal <- c(MonthTotal, interval_total)
  MonthAverage <- c(MonthAverage, interval_average)
  print(MonthTotal)
  # d = c(d, temp)
}
MonthTotal <- as.matrix(as.data.frame(MonthTotal))
MonthTotal <- as.vector(MonthTotal)
MonthTotal <- MonthTotal[2:length(MonthTotal)]

MonthAverage <- as.matrix(as.data.frame(MonthAverage))
MonthAverage <- as.vector(MonthAverage)
MonthAverage <- MonthAverage[2:length(MonthAverage)]


## dummy version of your data
mat <- matrix(c(1:17, rep(0, 3)), ncol = 2)

## create object suitable for plotting with boxplot
## I.e. convert to melted or long format
df <- data.frame(values = mat[1:17],
                 vars = rep(c("Col1","Col2"), times = c(10,7)))

## draw the boxplot
boxplot(values ~ vars, data = df)