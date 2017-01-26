rm(list = ls())
A2 <- read.csv('G:/Codes/Text_Mining/R script_Yile/A2_temp.csv')

temp1 = A2[1, 3:ncol(A2)]
temp2 = A2[2, 3:ncol(A2)]
temp = temp1 + temp2

a <- "Hello"
b <- "Hesllossaaaa"
setdiff(b, a)

library(rindex)
strcmp(b, a)
