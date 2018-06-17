pid <- seq(1,27307,1)
location <- rep(NA,27307)
yearbuilt <- rep(NA,27307)
totval <- rep(NA,27307)
bedrooms <- rep(NA, 27307)
bathrooms <- rep(NA, 27307)
halfbaths <- rep(NA, 27307)

data <- data.frame(pid, location, yearbuilt, totval, bedrooms, bathrooms, halfbaths)

for (ii in 1:27307){
  file <- paste(paste("VisionAppraisal/newdata2017/",as.character(ii),sep = ""),".html",sep = "")
  
  if (file.exists(file)){
    x <- scan(file, what="", sep="\n")
    
    # location
    locationline <- grep("MainContent_lblLocation", x)
    location <- gsub("<[^<>]*>", "", x[locationline])
    location <- trimws(location, "both")
    data$location[ii] <- location
    
    # yearbuilt
    # uses the earliest year on file
    yearline <- grep("_lblYearBuilt", x)
    year <- gsub("<[^<>]*>", "", x[yearline])
    year <- gsub(" ", "", year, fixed = TRUE)
    if (min(year) != "" && min(year) != 0){
      data$yearbuilt[ii] <- min(year)
    }
    
    
    # totval
    valueline <- grep("MainContent_lblGenAppraisal", x) # seems like there would be two numbers?
    value <- gsub("<[^<>]*>", "", x[valueline[2]])
    value <- gsub("[^a-zA-Z0-9]", "", value)
    data$totval[ii] <- value
    
    # bedrooms
    # if sections of house exist, we sum all bedroom numbers
    bedroomline <- grep("Total Bedrooms|Total Bedrms|Ttl Bedrms", x)
    bedroom <- gsub("<[^<>]*>", "", x[bedroomline])
    bedroom <- gsub("[^0-9]", "", bedroom)
    if (bedroom != ""){
      data$bedrooms[ii] <- sum(as.integer(bedroom))
    }
    # Check 8435. Should be something like 64, but I'm getting NA D: 
    
    # bathrooms
    # if sections of house exist, we sum all bathroom numbers 
    # if bathrooms have 0.5, we save them as half bathrooms
    half <- 0
    bathroomline <- grep("Total Bthrms|Total Baths|Ttl Bathrms", x)
    bathroom <- gsub("<[^<>]*>", "", x[bathroomline])
    bathroom <- gsub("[^0-9.]", "", bathroom)
    if (bathroom != ""){
      data$bathrooms[ii] <- sum(as.integer(bathroom))
      if (as.numeric(bathroom) - as.integer(bathroom) > 0){
        half <- sum(as.numeric(bathroom) - as.integer(bathroom) > 0, na.rm = TRUE)
      }
    }
    
    # halfbaths
    # if sections of house exist, we sum all halfbath numbers
    # if bathrooms had 0.5 above, we add those too
    halfbathline <- grep("Total Half Baths|Ttl Half Bths", x)
    if (length(halfbathline) != 0){
      halfbath <- gsub("<[^<>]*>", "", x[halfbathline])
      halfbath <- gsub("[^0-9.]", "", halfbath)
      if (halfbath != ""){
        data$halfbaths[ii] <- sum(as.integer(halfbath)) + sum(half)
      }
    }else{
      if (half != 0){
        data$halfbaths[ii] <- sum(half)
      }
    }
    
    
    
    # zone
    # MainContent_lblZone
    # gsub Zone out ?
    
    # neighborhood
    # MainContent_lblNbhd
    
    # landval
    # MainContent_lblLndAppr
    
    # garagesqft
    # <td>UGR</td><td>Garage Under</td>
    # <td>FGR</td><td>Garage</td>
    # not too sure if that's all there is
    # GARAGE-AVE ??
    
    
  }
}

zone <- rep(NA, 27307)
for (ii in 1:27307){
  file <- paste(paste("newdata2017/",as.character(ii),sep = ""),".html",sep = "")
  
  if (file.exists(file)){
    x <- scan(file, what="", sep="\n")
    
    zoneline <- grep("MainContent_lblZone", x)
    z <- x[zoneline]
    z <- gsub("<[^<>]*>", "", z)
    z <- gsub("\t", "", z)
    z <- gsub("Zone", "", z)
    zone[ii] <- z
  }
}
# things to consider
# AIRP v. AIRPORT
# what to do with combined zones, like BA/RM2 and IH/RM1 



neighborhood <- rep(NA, 27307)
for (ii in 1:27307){
  file <- paste(paste("newdata2017/",as.character(ii),sep = ""),".html",sep = "")
  
  if (file.exists(file)){
    x <- scan(file, what="", sep="\n")
    
    neighborhoodline <- grep("MainContent_lblNbhd", x)
    nh <- x[neighborhoodline]
    nh <- gsub("<[^<>]*>", "", nh)
    nh <- gsub("\t", "", nh)
    nh <- gsub("Neighborhood", "", nh)
    if (nh != ""){
      neighborhood[ii] <- nh
    }
  }
}



landval <- rep(NA, 27307)
for (ii in 1:27307){
  file <- paste(paste("newdata2017/",as.character(ii),sep = ""),".html",sep = "")
  
  if (file.exists(file)){
    x <- scan(file, what="", sep="\n")
    
    valueline <- grep("MainContent_lblLndAppr", x)
    if (length(valueline) != 0){
      value <- x[valueline]
      value <- gsub("<[^<>]*>", "", value)
      value <- gsub("[^0-9]", "", value)
      landval[ii] <- as.integer(value)
    }
  }
}



garage <- rep(NA, 27307)
for (ii in 1:27307){
  file <- paste(paste("newdata2017/",as.character(ii),sep = ""),".html",sep = "")
  
  if (file.exists(file)){
    x <- scan(file, what="", sep="\n")
    
    garagesqft <- 0
    garageline1 <- grep("Garage", x)
    if (length(garageline1) != 0){
      garage1 <- x[garageline1 + 1]
      garage1 <- gsub("[^0-9]+", "", garage1)
      garagesqft <- sum(as.integer(garage1), na.rm = TRUE)
    }
    garageline2 <- grep("GARAGE", x)
    if (length(garageline2) != 0){
      garage2 <- x[garageline2]
      garage2 <- gsub("<[^<>]*>", "", garage2)
      garage2 <- gsub(".*;([0-9]+) S.F..*", "\\1", garage2)
      garagesqft <- garagesqft + sum(as.integer(garage2), na.rm = TRUE)
    }
    
    if (garagesqft != 0){
      garage[ii] <- garagesqft
    }
    
  }
}








zone <- rep(NA, 27307)
neighborhood <- rep(NA, 27307)
landval <- rep(NA, 27307)
garage <- rep(NA, 27307)

for (ii in 1:27307){
  file <- paste(paste("newdata2017/",as.character(ii),sep = ""),".html",sep = "")
  
  if (file.exists(file)){
    x <- scan(file, what="", sep="\n")
    
    # zone
    zoneline <- grep("MainContent_lblZone", x)
    z <- x[zoneline]
    z <- gsub("<[^<>]*>", "", z)
    z <- gsub("\t", "", z)
    z <- gsub("Zone", "", z)
    zone[ii] <- z
    
    # neighborhood
    neighborhoodline <- grep("MainContent_lblNbhd", x)
    nh <- x[neighborhoodline]
    nh <- gsub("<[^<>]*>", "", nh)
    nh <- gsub("\t", "", nh)
    nh <- gsub("Neighborhood", "", nh)
    if (nh != ""){
      neighborhood[ii] <- nh
    }
    
    # landval
    valueline <- grep("MainContent_lblLndAppr", x)
    if (length(valueline) != 0){
      value <- x[valueline]
      value <- gsub("<[^<>]*>", "", value)
      value <- gsub("[^0-9]", "", value)
      landval[ii] <- as.integer(value)
    }
    
    # garagesqft
    garagesqft <- 0
    garageline1 <- grep("Garage", x)
    if (length(garageline1) != 0){
      garage1 <- x[garageline1 + 1]
      garage1 <- gsub("[^0-9]+", "", garage1)
      garagesqft <- sum(as.integer(garage1), na.rm = TRUE)
    }
    garageline2 <- grep("GARAGE", x)
    if (length(garageline2) != 0){
      garage2 <- x[garageline2]
      garage2 <- gsub("<[^<>]*>", "", garage2)
      garage2 <- gsub(".*;([0-9]+) S.F..*", "\\1", garage2)
      garagesqft <- garagesqft + sum(as.integer(garage2), na.rm = TRUE)
    }
    if (garagesqft != 0){
      garage[ii] <- garagesqft
    }
    
  }
}










write.csv(data, "dataclean.csv", row.names = FALSE)


