rm(list = ls())
library(RPostgreSQL)



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

MonthlyContract <- y

y1 = 0
for(i in 1:length(y)){
  y1[i] = sum(y[1:i])
}

TotalContract <- y1

Total1 <- rbind(y, y1)

write.csv(Total1, file = "G:\\Codes\\Text_Mining\\R script_v3\\TotalContracts.csv")

plot(TotalContract, xlab = 'Number of Month', ylab = 'Total Contracts Created')

# 
# 
# i = 3
# A[i,1]
# a = Interval_FreqCount(TimeGen(A[i, 1], A[i, 2]), TimeGen(A[i+1, 1], A[i+1, 2]))
# a