rm(list = ls())
library(RPostgreSQL)
source("User_Frequency.R")
UserList <- FrequentUser(1000)
UserList1 <- as.vector(UserList[,1])
NoContract <- UserList[,2]

Connections <- vector(length = length(UserList1))
No = 1

for (creatorID in UserList1){
QueryC = paste("SELECT COUNT(DISTINCT TEM1. signer_id) FROM (SELECT TEM.creator_id, TEM.signer_id, TEM.created_at FROM (SELECT contracts.creator_id, signings.signer_id, contracts.created_at FROM contracts, signings WHERE contracts.contract_id = signings.contract_id ORDER BY created_at) AS TEM WHERE TEM.creator_id =", creatorID, ") as TEM1")
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
Connections[No] <- as.numeric(dbGetQuery(conn, QueryC))
No <- No + 1
dbDisconnect(conn)
}

dataxx <- rbind(Connections, NoContract)
labels <- c("No. Connections", "No. Contract")
barplot(dataxx) 
