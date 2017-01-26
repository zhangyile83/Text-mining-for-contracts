Time_Generator <- function(start_date, end_date){
Time_list = matrix(nrow = ((end_date[1]-start_date[1]-1)*12 + 12-start_date[2]+1 + end_date[2]), ncol = 2)
RowNum = 1
  for(j in start_date[1]:end_date[1]){
    if(j == start_date[1]) {RowNum_temp = start_date[2]:12}
    else if(j == end_date[1]) RowNum_temp = 1:end_date[2]
            else RowNum_temp = 1:12
     for(i in RowNum_temp){
        Time_list[RowNum, 1] = j
        Time_list[RowNum, 2] = i
        RowNum = RowNum + 1
     }
  }
result <- Time_list
}