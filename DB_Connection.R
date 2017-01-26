rm(list = ls())
library(RPostgreSQL)
library(RWeka)
setwd("G:\\Codes\\Text_Mining\\R script_v3")
source("Stopwords.R")
source("User_Frequency.R")
source("Type_classifier.R")

libs <- c('tm','Rgraphviz','stringr','NLP','openNLP')
lapply(libs,require, character.only = TRUE)

# UserIDLIst <- RangeUser(100,10000)[,1]
# IDList <- SwitchUser()
# InfluencerList <- IDList[,1]
# UserIDLIst <- IDList[,2]
UserIDLIst <- OrganizationUser()

# UserFrequency <- RangeUser(100, 10000)[,2]

DB <- c("No.", "InfluencerID","OrganizationID", "Type", "Total Created Contracts", rep("Words & Aver. Freq.", 20))


Index = 1
NumCount = 0
for(UserID in UserIDLIst){
  NumCount <- NumCount + 1
  print(Index)
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = "Inkdit", user = "postgres", password = "314159")
QueryC = paste("SELECT content FROM contracts WHERE created_by_organization_id =", UserID)
result <- dbGetQuery(conn, QueryC)
result = t(result)
dbDisconnect(conn)


cleanFun <- function(htmlString) {
  htmlString<-(gsub("<.*?>", "", htmlString))
  return(gsub("\n", "", htmlString))
  
}

kankan <- cleanFun(result)
View(kankan)

kankan <-  Corpus(VectorSource(kankan))

tagPOS <-  function(x, ...) { 
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  return(POStags)
}

myStpwdlist =  StopWordsList()

cleanCorpus <- function(corpus){
  corpus_cl <- tm_map(corpus,content_transformer(gsub),pattern = '[[:punct:]]',replacement = ' ')
  corpus_cl <- tm_map(corpus_cl,stripWhitespace)
  corpus_cl <- tm_map(corpus_cl,content_transformer(tolower))
  corpus_cl <- tm_map(corpus_cl,removeWords,c(myStpwdlist,stopwords('english')))
  corpus_cl <- tm_map(corpus_cl,removeNumbers)
  #  corpus_cl <- tm_map(corpus_cl, stemDocument)
  
  result <- corpus_cl
}
kankan2 = cleanCorpus(kankan)
dtm <- DocumentTermMatrix(kankan2)

dim_dtm <- dim(dtm)
if(min(dim_dtm)<1) next;
TDM = inspect(dtm[1:dim_dtm[1], 1:dim_dtm[2]])
TDM <- t(TDM)

TDM1 <- TDM[,which(as.vector(colSums(TDM))>30)]
if(is.matrix(TDM1)){
beforePOS <- row.names(TDM1)
afterPOS <- tagPOS(paste(beforePOS,collapse = ' '))

POS_word.non_idx <- which(str_detect(afterPOS,'^N'))
POS_word.non <- beforePOS[POS_word.non_idx]
POS_word.vrb_idx <- which(str_detect(afterPOS,'^V'))
POS_word.vrb <- beforePOS[POS_word.vrb_idx]
remainPOS <- beforePOS[union(POS_word.non_idx,POS_word.vrb_idx)]

TDM1 <- TDM1[row.names(TDM1) %in% remainPOS,]

if(is.null(dim(TDM1))) next;
TDM2 <- as.matrix(rowMeans(TDM1[which(as.vector(rowMeans(TDM1))>=1),]))
TDM2 <- TDM2[order(-TDM2[,1]),]
TDM2 <- as.matrix(TDM2)

LB <- min(nrow(TDM2), 20)
if(LB<1) next;
TDM3 <- TDM2[1:LB,]
TDM3 <- as.matrix(TDM3)

TDM3 <- t(TDM3)
# value <- c(Index, InfluencerList[NumCount], UserID, Type_classifier(colnames(TDM3)), IDFrequency(UserID), colnames(TDM3))
value <- c(Index, UserID, Type_classifier(colnames(TDM3)), IDFrequency(UserID), colnames(TDM3))


value1 <- c("", "", "", "", as.numeric(TDM3))
value2 <- c(rep("", 23))

DB<- rbind(DB, value, value1)

Index <- Index + 1

View(TDM3)
}else if(length(TDM1)>0){
  beforePOS <- as.character(unlist(attributes(TDM1)))
  afterPOS <- tagPOS(paste(beforePOS,collapse = ' '))
  
  POS_word.non_idx <- which(str_detect(afterPOS,'^N'))
  POS_word.non <- beforePOS[POS_word.non_idx]
  POS_word.vrb_idx <- which(str_detect(afterPOS,'^V'))
  POS_word.vrb <- beforePOS[POS_word.vrb_idx]
  remainPOS <- beforePOS[union(POS_word.non_idx,POS_word.vrb_idx)]
  
  TDM1 <- TDM1[beforePOS %in% remainPOS]
  
  TDM2 <- as.numeric(TDM1)
  TDM1 <- TDM1[order(-TDM2)]
  
  LB <- min(length(TDM1), 20)
  if(LB<1) next;
  TDM3 <- TDM1[1:LB]
  TDM3Names <- as.character(unlist(attributes(TDM3)))
  
  value <- c(Index, UserID, Type_classifier(TDM3Names), IDFrequency(UserID), TDM3Names)
  
  value1 <- c("", "", "", "", as.numeric(TDM3))
  value2 <- c(rep("", 23))
  
  DB<- rbind(DB, value, value1)
  
  Index <- Index + 1
  
  View(TDM3)
}else{}
}


# TDM3 <- as.matrix(TDM2[1:20,])
# 
# View(remainPOS)
# TDM1
write.csv(DB, file = "G:\\Codes\\Text_Mining\\R script_v3\\DB3.csv")
# write.csv(TDM1, file = "G:\\Codes\\Text_Mining\\R script_v3\\kankan1.csv")
# n