# Code 241 for washroom 
Type_classifier <- function(Keywords){
  
  Media <- c("media", "photo", "photography", "photograph", "photographer","photographs", "producer", "producers", "subscriber", "subscribers", "video", "videos","tape","tapes", "artist", "artists", "model","models", "music", "music", "publication", "publications", "publisher", "publishers","facebook", "musician")
  Safety <- c("safety", "hazard", "hazards","excavation", "fire", "injury", "death")
  Employment <-c("employment", "contractor",  "contractors", "salary", "bonus", "employee", "employer", "employees", "employers")
  Activity <- c("activity", "event", "events", "trip" ,"camp", "travel","game", "games", "camping", "parade", "meeting", "conference", "show", "ceremony", "wedding", "marriage")
  Education <- c("education", "school", "lesson", "lessons", "yoga", "teacher", "teachers", "class", "dance", "student", "students")
  Business <- c("business", "information", "service", "project", "projects", "services", "privacy", "contact", "policy", "company", "product", "payment",  "companies", "products", "payments", "escrow", "client", "clients", "invoice")  
  BusinessCopyRight <- c("Business: copyright", "copyright", "copy", "patent", "copys", "patents")
  BusinessRental <- c("Business: rental", "rent", "rental", "rentals", "lessee", "lease", "lessor", "apartment", "apartments", "studio" ,"studios")
  BusinessTransaction <- c("BusinessTransaction", "payment", "payments", "invoice", "bank", "vendor", "vendors")
  BusinessAdvertisement <- c("BusinessAdvertisement", "advertisement", "advertisements", "ads")
  Medical <- c("medical", "symptom", "medicine", "medicines", "treat", "cure", "diagnose", "drug", "drugs", "cancer")

  Type <- 0
  
  if(length(intersect(Media, Keywords))){
    Type <- Media[1]
  }else if(length(intersect(Safety, Keywords))){
    Type <- Safety[1]
  }else if(length(intersect(Employment, Keywords))){
    Type <- Employment[1]
  }else if(length(intersect(Activity, Keywords))){
    Type <- Activity[1]
  }else if(length(intersect(Education, Keywords))){
    Type <- Education[1]
  }else if(length(intersect(Business, Keywords))){
    Type <- Business[1]
  }else if(length(intersect(BusinessCopyRight, Keywords))){
    Type <- BusinessCopyRight[1]
  }else if(length(intersect(BusinessRental, Keywords))){
    Type <- BusinessRental[1]
  }else if(length(intersect(BusinessTransaction, Keywords))){
    Type <- BusinessTransaction[1]
  }else if(length(intersect(BusinessAdvertisement, Keywords))){
    Type <- BusinessAdvertisement[1]
  }else if(length(intersect(Medical, Keywords))){
    Type <- Medical[1]
  }else{
    Type <- "NA"
  }
  
  return(Type)
}

