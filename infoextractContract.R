# This R script is written to conduct the zipcode and money quantity matching in the contracts

# Extract the zip code and amount of money in the contracts
infoextractContract <- function(txt_vector){
  cont.zipcode <- list() # Store the zipcodes mentioned in each contract
  cont.money <- list() # Store the amounts of money mentioned in each contract
  cont.date <- list() # Store the dates mentioned in each contract
  cont.tel <- list() # Store the tel numbers mentioned in each contract
  for (i in 1:length(txt_vector)){
    temp.money <- str_extract_all(txt_vector[i],'\\$[0-9]+,*[0-9]*') # Use regular expression to match the amount of money
    temp2.money <- removePunctuation(temp.money[[1]])
    cont.money[[i]] <- temp.money
    
    temp.zipcode.can <- str_extract_all(txt_vector[i],'[a-z][0-9][a-z] *[0-9][a-z][0-9]')
    temp.zipcode.usa <- str_extract_all(txt_vector[i],'[0-9]{5}-+[0-9]{4}')
    cont.zipcode[[i]] <- c(temp.zipcode.can[[1]],temp.zipcode.usa[[1]])
    
    temp.date.num <- str_extract_all(txt_vector[i],'[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}')
    temp.date.word <- str_extract_all(txt_vector[i],'[a-z]+ +[0-9]{1,2}[a-z]*, +[0-9]{2,4}')
    cont.date[[i]] <- c(temp.date.num,temp.date.word) 
    # Still may not correct if the format of dates are different
    
    temp.tel <- str_extract_all(txt_vector[i],'[0-9]{3}-* *[0-9]{3}-* *[0-9]{4}')
    cont.tel[[i]] <- temp.tel
  }
  cont.info <- list(cont.money,cont.zipcode,cont.tel,cont.date) # Note the sequence
  return(cont.info)
}
cont.info <- infoextractContract(txt_vector)
