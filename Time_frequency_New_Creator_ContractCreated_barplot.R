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

interval_total <- 0
interval_average <- 0
Year <- 0

No.User = 0
for(i in 1:(nrow(A)-1)){
  temp = Interval_FreqCount(TimeGen(A[i, 1], A[i, 2]), TimeGen(A[i+1, 1], A[i+1, 2]))

  for(j in 1:(nrow(temp))){
    UserID <- temp[j,1]
    QueryC = paste("SELECT COUNT(*) FROM contracts WHERE creator_id = ", UserID)
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
    user_total_usage <- dbGetQuery(conn, QueryC)
    interval_total <- c(interval_total, user_total_usage)
    interval_average <- c(interval_average, user_total_usage/(nrow(A)-i))
    Year <- c(Year, A[i, 1])
    dbDisconnect(conn)    
  }
  Time <- TimeGen(A[i, 1], A[i, 2])
  print(Time)
}

interval_total <- as.matrix(as.data.frame(interval_total))
interval_total <- as.vector(interval_total)
interval_total <- interval_total[2:length(interval_total)]

interval_average <- as.matrix(as.data.frame(interval_average))
interval_average <- as.vector(interval_average)
interval_average <- interval_average[2:length(interval_average)]

Year <- Year[2:length(Year)]
# 
# 
# ## dummy version of your data
# mat <- matrix(c(1:17, rep(0, 3)), ncol = 2)
# 
## create object suitable for plotting with boxplot
## I.e. convert to melted or long format
df <- data.frame(values = interval_average,
                 vars = rep(c("2011","2012","2013","2014","2015","2016"),
                 times = c(length(Year[Year == 2011]),length(Year[Year == 2012]),
                           length(Year[Year == 2013]),length(Year[Year == 2014]),
                           length(Year[Year == 2015]),length(Year[Year == 2016]))))

## draw the boxplot
boxplot(values ~ vars, data = df)

