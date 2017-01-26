FrequentUser <- function(Freq){
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
result <- dbGetQuery(conn, "SELECT * FROM (select created_by_organization_id, count(*) AS totalNum from contracts group by created_by_organization_id) AS foo ORDER BY totalNum DESC")
dbDisconnect(conn)

dim(result[2])
max(result[2])
aa = quantile(t(result[2]), probs = seq(0, 1, by= 0.01))

plot(1:nrow(result), cumsum(t(result[2]))/sum(t(result[2])))
aa
barplot(aa)

Frequent_user = subset(result, result[2] >= Freq)
a <- Frequent_user[2:nrow(Frequent_user),]
b <- a[!is.na(a[,1]),]
return(b)
}

#######################3
RangeUser <- function(Freq1, Freq2){
Userlong <- FrequentUser(min(c(Freq1,Freq2)))
Usershort <- FrequentUser(max(c(Freq1,Freq2)))
UserlongID <- Userlong[,1]
UsershortID <- Usershort[,1]
range <- setdiff(UserlongID, UsershortID)
final <- Userlong[Userlong[,1] %in% range,]
return(final)

}

##########################################################################
OrganizationUser <- function(){
  library(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
  result <- dbGetQuery(conn, "SELECT created_by_organization_id FROM (
  SELECT    contracts.id         AS contract_id, 
                       signings.user_id     AS signing_user_id, 
                       signings.created_at  AS signed_at_date,
                       COALESCE(template_creating_user_id, creating_user_id) AS creator_user_id,
                       created_by_organization_id AS created_by_organization_id
                       FROM      contracts LEFT JOIN signings 
                       ON signings.contract_id = contracts.id
  ) AS TEM
                       GROUP BY created_by_organization_id ORDER BY created_by_organization_id")
  result = result[!is.na(result)]
  return(result)
  }





####################### 4

SwitchUser <- function(){
  library(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
  result <- dbGetQuery(conn, "SELECT created_by_organization_id FROM
(SELECT created_by_organization_id, MIN(signed_at_date) MinCreateTime FROM (
                       SELECT    contracts.id         AS contract_id, 
                       signings.user_id     AS signing_user_id, 
                       signings.created_at  AS signed_at_date,
                       COALESCE(template_creating_user_id, creating_user_id) AS creator_user_id,
                       created_by_organization_id AS created_by_organization_id
                       FROM      contracts LEFT JOIN signings 
                       ON signings.contract_id = contracts.id
  ) AS TEM
                       GROUP BY created_by_organization_id ORDER BY created_by_organization_id) AS TEM3
                       INNER JOIN
                       (SELECT organization_id AS signed_by_organization_id, MIN(signed_at_date) MinSignTime FROM (
                       (
                       SELECT    contracts.id         AS contract_id, 
                       signings.user_id     AS signing_user_id, 
                       signings.created_at  AS signed_at_date,
                       COALESCE(template_creating_user_id, creating_user_id) AS creator_user_id,
                       created_by_organization_id AS created_by_organization_id
                       FROM      contracts LEFT JOIN signings 
                       ON signings.contract_id = contracts.id
                       ) AS TEM1 INNER JOIN 
                       (
                       SELECT organization_id, user_id
                       FROM organization_members
                       ) AS TEM2
                       ON TEM1.signing_user_id = TEM2.user_id
                       AND TEM1.created_by_organization_id <> TEM2.organization_id
                       )
                       GROUP BY signed_by_organization_id ORDER BY MinSignTime
                       ) AS TEM4
                       ON TEM3.created_by_organization_id = TEM4.signed_by_organization_id
                       WHERE MinCreateTime > MinSignTime AND MinCreateTime BETWEEN '2011-1-1' AND '2018-6-1' 
                       ORDER BY created_by_organization_id")
  dbDisconnect(conn)
  result <- as.numeric(as.matrix(result))
  
  SwitchList = 0
  InfluencerList = 0
  for(ID in result){
    library(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
    result1 <- dbGetQuery(conn, paste("SELECT created_by_organization_id
FROM (
                         SELECT    contracts.id         AS contract_id, 
                         signings.user_id     AS signing_user_id, 
                         signings.created_at  AS signed_at_date,
                         COALESCE(template_creating_user_id, creating_user_id) AS creator_user_id,
                         created_by_organization_id AS created_by_organization_id
                         FROM      contracts LEFT JOIN signings 
                         ON signings.contract_id = contracts.id
    ) AS TEM1 INNER JOIN 
                         (
                         SELECT organization_id, user_id
                         FROM organization_members
                         ) AS TEM2
                         ON TEM1.signing_user_id = TEM2.user_id
                         WHERE organization_id = ", ID ,"ORDER BY signed_at_date"))
    dbDisconnect(conn)
    b <- as.numeric(result1[1,1])
    print(c(ID, b))
    if(!is.na(b)){
    if(b == 4 | b == 6 ){
    }else{SwitchList <- c(SwitchList,ID)
          InfluencerList <- c(InfluencerList, b)}
    }
  }
  finalresult <- rbind(InfluencerList, SwitchList)
  finalresult <- finalresult[,2:ncol(finalresult)]
  finalresult <- t(finalresult)
  return(finalresult)
}





####################### Switch user frequency #########
IDFrequency <- function(ID){
  library(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
  result <- dbGetQuery(conn, paste("SELECT count(*) FROM contracts WHERE created_by_organization_id = ", ID))
  dbDisconnect(conn)
  
  b <- as.matrix(result)
  return(as.numeric(b[,1]))
  }