rm(list = ls())
library(RPostgreSQL)

DBNAME = "Inkdit_2"
USER = "postgres" 
PASSWORD = "314159"


##################### New Organization creator ID #######################

TimeGen <- function(Year = 2011, Month = 1, Day = 1){
  return(paste("'", as.character(Year), "-", as.character(Month), "-", as.character(Day), "'", sep=""))
}

Interval_FreqCount <- function(Time1, Time2){
  QueryC = paste("SELECT created_by_organization_id FROM
                (SELECT contracts.created_by_organization_id, MIN(contracts.created_at) MinCreatTime
                 FROM contracts Group BY contracts.created_by_organization_id ORDER BY MinCreatTime) TEM
                 WHERE TEM.MinCreatTime BETWEEN ", Time1, " AND ", Time2)
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
  d = c(d, temp)
}

k <- unlist(d)
k <- as.vector(k)
OrganizationID <- k





#####################  Created_by_user ID #######################

TimeGen <- function(Year = 2011, Month = 1, Day = 1){
  return(paste("'", as.character(Year), "-", as.character(Month), "-", as.character(Day), "'", sep=""))
}

Interval_FreqCount <- function(Time1, Time2){
  QueryC = paste("SELECT created_by_user_id FROM
                 (SELECT contracts.created_by_user_id, MIN(contracts.created_at) MinCreatTime
                 FROM contracts Group BY contracts.created_by_user_id ORDER BY MinCreatTime) TEM
                 WHERE TEM.MinCreatTime BETWEEN ", Time1, " AND ", Time2)
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
  d = c(d, temp)
}

k <- unlist(d)
k <- as.vector(k)
Created_by_User_ID <- k



##################### New Creating_user_ID #######################


TimeGen <- function(Year = 2011, Month = 1, Day = 1){
  return(paste("'", as.character(Year), "-", as.character(Month), "-", as.character(Day), "'", sep=""))
}

Interval_FreqCount <- function(Time1, Time2){
  QueryC = paste("SELECT creating_user_id FROM
                 (SELECT contracts.creating_user_id, MIN(contracts.created_at) MinCreatTime
                 FROM contracts Group BY contracts.creating_user_id ORDER BY MinCreatTime) TEM
                 WHERE TEM.MinCreatTime BETWEEN ", Time1, " AND ", Time2)
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
  d = c(d, temp)
}

k <- unlist(d)
k <- as.vector(k)
Creating_User_ID <- k


##################### template_ID #######################

TimeGen <- function(Year = 2011, Month = 1, Day = 1){
  return(paste("'", as.character(Year), "-", as.character(Month), "-", as.character(Day), "'", sep=""))
}

Interval_FreqCount <- function(Time1, Time2){
  QueryC = paste("SELECT template_id FROM
                 (SELECT contracts.template_id, MIN(contracts.created_at) MinCreatTime
                 FROM contracts Group BY contracts.template_id ORDER BY MinCreatTime) TEM
                 WHERE TEM.MinCreatTime BETWEEN ", Time1, " AND ", Time2)
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
  d = c(d, temp)
}

k <- unlist(d)
k <- as.vector(k)
Template_ID <- k


##################### Find all IDs on Inkdit ####################
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = DBNAME, user = USER, password = PASSWORD)
AllID <- dbGetQuery(conn, "SELECT id FROM users")
dbDisconnect(conn)
AllID <- as.matrix(AllID)
AllID <- as.numeric(AllID)

##################### Deactivated Users ####################
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = DBNAME, user = USER, password = PASSWORD)
DisactivedUser <- dbGetQuery(conn, "SELECT id FROM users_deactivated")
dbDisconnect(conn)
DisactivedUser <- as.matrix(DisactivedUser)
DisactivedUser <- as.numeric(DisactivedUser)

##################### Deactivated Organization members ####################
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = DBNAME, user = USER, password = PASSWORD)
DisactivedMembers <- dbGetQuery(conn, "SELECT user_id FROM organization_members_deactivated")
dbDisconnect(conn)
DisactivedMembers <- as.matrix(DisactivedMembers)
DisactivedMembers <- as.numeric(DisactivedMembers)

##################### Deactivated Organization members ####################
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = DBNAME, user = USER, password = PASSWORD)
SigningID <- dbGetQuery(conn, "SELECT user_id FROM signings")
dbDisconnect(conn)
SigningID <- as.matrix(SigningID)
SigningID <- t(SigningID)
SigningID <- SigningID[1,]
SigningID <- as.numeric(SigningID)

##################### Organization members ID ####################
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = DBNAME, user = USER, password = PASSWORD)
OrganizationMemberID <- dbGetQuery(conn, "SELECT user_id FROM organization_members")
dbDisconnect(conn)
OrganizationMemberID <- as.matrix(OrganizationMemberID)
OrganizationMemberID <- t(OrganizationMemberID)
OrganizationMemberID <- OrganizationMemberID[1,]
OrganizationMemberID <- as.numeric(OrganizationMemberID)




##################### New switcher ID #######################
# library(RPostgreSQL)
# source("stopkey.R")
# 
# 
# TimeGen <- function(Year = 2011, Month = 1, Day = 1){
#   return(paste("'", as.character(Year), "-", as.character(Month), "-", as.character(Day), "'", sep=""))
# }
# 
# Interval_FreqCount <- function(Time1, Time2){
#   QueryC = paste("SELECT creator_id FROM
#                  (SELECT signings.signer_id, MIN(signings.created_at) MinSignTime
#                  FROM signings Group BY signings.signer_id ORDER BY MinSignTime) AS TEM1
#                  INNER JOIN
#                  (SELECT contracts.creator_id, MIN(contracts.created_at) MinCreatTime
#                  FROM contracts Group BY contracts.creator_id ORDER BY MinCreatTime) AS TEM2
#                  ON TEM1.signer_id = TEM2.creator_id
#                  WHERE MinCreatTime BETWEEN", Time1, " AND ", Time2, "AND MinCreatTime > MinSignTime")
#   drv <- dbDriver("PostgreSQL")
#   conn <- dbConnect(drv, dbname = DBNAME, user = USER, password = PASSWORD)
#   result_temp <- dbGetQuery(conn, QueryC)
#   dbDisconnect(conn)
#   return(result_temp)
# }
# 
# 
# source("Time_Interval_Generator.R")
# Year1 = 2011
# Month1 = 10
# Year2 = 2016
# Month2 = 10
# A = Time_Generator(c(Year1, Month1), c(Year2, Month2))
# 
# d = 0
# for(i in 1:(nrow(A)-1)){
#   temp = Interval_FreqCount(TimeGen(A[i, 1], A[i, 2]), TimeGen(A[i+1, 1], A[i+1, 2]))
#   d = c(d, temp)
# }
# 
# e <- unlist(d)
# e <- as.vector(e)
# e <- e[2:length(e)]
# SwitcherID <- e
# 
# ##################### Paying user ###########################
#   QueryC = paste("SELECT user_id FROM plans")
#   drv <- dbDriver("PostgreSQL")
#   conn <- dbConnect(drv, dbname = DBNAME, user = USER, password = PASSWORD)
#   y <- dbGetQuery(conn, QueryC)
#   dbDisconnect(conn)
#   y <- as.matrix(y)
#   y <- as.numeric(y)
#   PayingID <- y
#   write.csv(k, file = "G:\\Codes\\Text_Mining\\R script_v3\\CreatorList.csv")
#   
#   
#   
# ############################ Account ID ##############################
#   library(RPostgreSQL)
# 
#     QueryC = paste("SELECT TEM1.id FROM
#                   (SELECT id, created_at, last_login_at
#                    FROM users
#                    WHERE last_login_at BETWEEN '2010-10-1' AND '2018-10-1') AS TEM1
#                    WHERE TEM1.created_at BETWEEN '2011-10-1' AND '2018-10-1' ORDER BY TEM1.id")
#     drv <- dbDriver("PostgreSQL")
#     conn <- dbConnect(drv, dbname = DBNAME, user = USER, password = PASSWORD)
#     p <- dbGetQuery(conn, QueryC)
#     dbDisconnect(conn)
# 
#     p <- as.matrix(p)
#     AccountLoginID <- as.numeric(p)
#     
# ########################### Strip information #############################
#     library(RPostgreSQL)
#     
#     QueryC = paste("SELECT creator_id, SUM(amount) FROM
#                   (SELECT creator_id, contract_id AS CID1
#                    FROM contracts) AS TEM1
#                    INNER JOIN
#                    (SELECT contract_id AS CID2, amount, created_at
#                    FROM stripe_payments) AS TEM2
#                    ON TEM1.CID1 = TEM2.CID2
#                    GROUP BY creator_id")
#     drv <- dbDriver("PostgreSQL")
#     conn <- dbConnect(drv, dbname = DBNAME, user = USER, password = PASSWORD)
#     q <- dbGetQuery(conn, QueryC)
#     dbDisconnect(conn)
#     q <- q[, 1]
#     q <- as.matrix(q)
#     StripeID <- as.numeric(q)
    
############### The CreatorID that are not in the AccountID ###############
    InterPart <- intersect(OrganizationMemberID, Creating_User_ID)
    Kankan <- setdiff(CreatorID, InterPart)
    
############### Count the number of contracts that created by the organization members ############    
    
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
      for(j in 1:(length(OrganizationMemberID))){
        UserID <- OrganizationMemberID[j]
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
    