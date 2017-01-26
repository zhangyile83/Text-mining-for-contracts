rm(list = ls())
library(RPostgreSQL)

libs <- c('tm','wordcloud','Rgraphviz')
lapply(libs,require, character.only = TRUE)

drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
result <- dbGetQuery(conn, "SELECT content FROM contracts 
                     WHERE created_by_organization_id = 222")
result = t(result)

cleanFun <- function(htmlString) {
  htmlString<-(gsub("<.*?>", "", htmlString))
  htmlString<-(gsub("&#+", "", htmlString))
  htmlString<-(gsub("x[A-Z]+", "", htmlString))
  htmlString<-(gsub("x[0-9]+", "", htmlString))
  
  return(gsub("\n", "", htmlString))
  
}

kankan <- cleanFun(result)
View(kankan[1])
