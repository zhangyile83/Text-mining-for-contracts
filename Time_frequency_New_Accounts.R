rm(list = ls())
library(RPostgreSQL)
DBNAME = "Inkdit"
USER = "postgres" 
PASSWORD = "314159"


TimeGen <- function(Year = 2011, Month = 1, Day = 1){
  return(paste("'", as.character(Year), "-", as.character(Month), "-", as.character(Day), "'", sep=""))
}

Interval_FreqCount <- function(Time1, Time2){
  QueryC = paste("SELECT COUNT(*) FROM
(
  SELECT created_by_organization_id, MIN(signed_at_date) MinSignTime FROM 
  (SELECT    contracts.id         AS contract_id, 
  signings.user_id     AS signing_user_id, 
  signings.created_at  AS signed_at_date,
  COALESCE(template_creating_user_id, creating_user_id) AS creator_user_id,
  created_by_organization_id AS created_by_organization_id
  FROM      contracts LEFT JOIN signings 
  ON signings.contract_id = contracts.id) AS TEM
  GROUP BY created_by_organization_id ORDER BY MinSignTime
  ) AS TEM1
  WHERE TEM1.MinSignTime BETWEEN", Time1, " AND ", Time2)
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

MonthTotal <- 0
MonthAverage <- 0
No.User = 0
d = 0
for(i in 1:(nrow(A)-1)){
  NewAccount = Interval_FreqCount(TimeGen(A[i, 1], A[i, 2]), TimeGen(A[i+1, 1], A[i+1, 2]))
  MonthTotal <- c(MonthTotal, NewAccount)
}
MonthTotal <- as.matrix(as.data.frame(MonthTotal))
MonthTotal <- MonthTotal[2:ncol(MonthTotal)]
MonthTotal <- as.numeric(MonthTotal)
MonthTotal <- t(MonthTotal)
MonthTotal <- as.numeric(MonthTotal)
MonthTotal1= 0
for(i in 1:length(MonthTotal)){
  MonthTotal1[i] = sum(MonthTotal[1:i])
}

MonthTotal2 <- rbind(MonthTotal, MonthTotal1)
write.csv(MonthTotal2, file = "G:\\Codes\\Text_Mining\\R script_v3\\NumOfNewAccounts.csv")
# 
# 
# 
# MonthAverage <- as.matrix(as.data.frame(MonthAverage))
# MonthAverage <- MonthAverage[2:ncol(MonthAverage)]
# 
# d <- as.matrix(as.data.frame(d))
# 
# x = 1:(length(d)-1)
# y = d[2:length(d)]
# plot(x, y, xaxt = "n", xlab='Month')
# axis(1,at = 1:length(d), labels = A[,2])
# # lines(lowess(1:(length(d)-1),d[2:length(d)])); 
# lines(x,predict(loess(y~x)))
# 
# 
# Monthlog <- A[,2]
# Monthlog <- Monthlog[2:length(Monthlog)]
# 
# MonthTotal1= 0
# for(i in 1:length(MonthTotal)){
#   MonthTotal1[i] = sum(MonthTotal[1:i])
# }
# 
# y1 = 0
# for(i in 1:length(y)){
#   y1[i] = sum(y[1:i])
# }
# 
# V1 <- rbind(x, Monthlog, y, y1)
# V2 <- rbind(x, Monthlog, MonthTotal, MonthTotal1)
# 
# V3 <- rbind(x, Monthlog, MonthAverage)
# write.csv(V1, file = "G:\\Codes\\Text_Mining\\R script_v3\\NumOfSwitcher.csv")
# 
# write.csv(V2, file = "G:\\Codes\\Text_Mining\\R script_v3\\ContractCreatedBySwitcher.csv")
# 
# write.csv(V3, file = "G:\\Codes\\Text_Mining\\R script_v3\\MonthlyAverCreatedBySwitcher.csv")
# 
# sum(d)/1134
# 
# plot(x, MonthTotal, xaxt = "n", xlab='Month')
# axis(1,at = 1:length(d), labels = A[,2])
# 
# 
# sum(MonthTotal)/65500
# 
# plot(x, MonthAverage, xaxt = "n", xlab='Month')
# axis(1,at = 1:length(d), labels = A[,2])
# 
# 
# 
