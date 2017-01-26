rm(list = ls())
libs <- c('tm','stringr')
lapply(libs,require,character.only = TRUE)
options(stringsAsFactors = FALSE)


setwd("G:/Codes/Text_Mining/R script_Yile")
A = read.csv("TDM77.csv", header = TRUE)

c <- matrix(nrow(A[1]), ncol = 1)

  # c[, 1] = A[, 1]
  temp = str_detect(A[,1], "^[a-z]+$")
  temp1 = as.matrix(temp)
  c = temp1
  B = A[c,];

  Row_index = 1
  gap = 1
  RecorderIndex = 0
while (Row_index<nrow(B)){  
  NextC = TRUE
  ii = 1
  print(ii)
  while (NextC == TRUE){
    templateWord = B[Row_index, 1]
    if(nchar(templateWord) < 6) break
    nextWord = B[Row_index+ii, 1]
    print(templateWord)
    print(nextWord)
    if(length(grep(templateWord,nextWord))>0) {
      # Operation here
      B[Row_index,2:ncol(B)] = B[Row_index,2:ncol(B)] + B[Row_index+ii, 2:ncol(B)] 
      RecorderIndex = c(RecorderIndex, Row_index+ii)
      ii = ii + 1
#      print(ii)
    } else {NextC = FALSE}
    print(NextC)
  }
  Row_index = Row_index + 1
  
  sprintf("the gap is %d", gap)
  print(Row_index)
}
  
  RecorderIndex = RecorderIndex[2:length(RecorderIndex)]
  allIndex = 1:nrow(B)
  selectIndex = setdiff(allIndex, RecorderIndex)
  B_select <- B[selectIndex,]

 write.csv(B_select, file = "G:\\Codes\\Text_Mining\\R script_Yile\\A3_temp.csv")
 