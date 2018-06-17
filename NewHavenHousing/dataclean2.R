#Phoebe Cheung, Margaret Luo, & Pei Tao


these <- 1:27307

housing.matrix <- matrix(NA,27307, 41)
colnames(housing.matrix) <- c("pid", "location", "yearbuilt", "totval", 
                              "bedrooms", "bathrooms", "halfbaths", "address", 
                              "saledate1", "saleprice1", "saleowner1", 
                              "saledate2", "saleprice2", "saleowner2", 
                              "saledate3", "saleprice3", "saleowner3", 
                              "saledate4", "saleprice4", "saleowner4", 
                              "saledate5", "saleprice5", "saleowner5", 
                              "sqft", "replcost", "pctgood", "style", "model", 
                              "grade", "occupancy", "actype", "bathstyle", 
                              "kstyle", "exval", "acres", "zone", "neighborhood",
                              "landval", "garagesqft", "bedrooms9plus", 
                              "multibuilding")
Housing <- as.data.frame(housing.matrix) 


for (i in these) {
  Housing$pid[i] <- i
  cat(i, "\n")
  filename <- file.path('VisionAppraisal/newdata2017',paste(i, '.html', sep=""))
  
  if (file.exists(filename)){
    tempx <- scan(filename, what="",sep="\n")
    
    # zone
    zoneline <- grep("MainContent_lblZone", tempx)
    z <- tempx[zoneline]
    z <- gsub("<[^<>]*>", "", z)
    z <- gsub("\t", "", z)
    z <- gsub("Zone", "", z)
    Housing$zone[i] <- z
    
    # neighborhood
    neighborhoodline <- grep("MainContent_lblNbhd", tempx)
    nh <- tempx[neighborhoodline]
    nh <- gsub("<[^<>]*>", "", nh)
    nh <- gsub("\t", "", nh)
    nh <- gsub("Neighborhood", "", nh)
    if (nh != ""){
      Housing$neighborhood[i] <- nh
    }
    
    # landval
    valueline <- grep("MainContent_lblLndAppr", tempx)
    if (length(valueline) != 0){
      value <- tempx[valueline]
      value <- gsub("<[^<>]*>", "", value)
      value <- gsub("[^0-9]", "", value)
      Housing$landval[i] <- as.integer(value)
    }
    
    # garagesqft
    garagesqft <- 0
    garageline1 <- grep("Garage", tempx)
    if (length(garageline1) != 0){
      garage1 <- tempx[garageline1 + 1]
      garage1 <- gsub("[^0-9]+", "", garage1)
      garagesqft <- sum(as.integer(garage1), na.rm = TRUE)
    }
    garageline2 <- grep("GARAGE", tempx)
    if (length(garageline2) != 0){
      garage2 <- tempx[garageline2]
      garage2 <- gsub("<[^<>]*>", "", garage2)
      garage2 <- gsub(".*;([0-9]+) S.F..*", "\\1", garage2)
      garagesqft <- garagesqft + sum(as.integer(garage2), na.rm = TRUE)
    }
    if (garagesqft != 0){
      Housing$garagesqft[i] <- garagesqft
    }
    
    #Building location
    LOCthisline <- grep("MainContent_lblLocation", tempx)
    LOCclean <- gsub("<[^<>]*>", "", tempx[LOCthisline])
    Housing$location[i] <- trimws(LOCclean)
    
    #year built
    BUILTthisline <- grep("_lblYearBuilt", tempx)
    BUILTclean <- gsub("<[^<>]*>", "", tempx[BUILTthisline])
    
    #If there is more than one year built, reports the oldest date
    Housing$yearbuilt[i] <- min(as.numeric(BUILTclean))
    
    #Total appraisal value
    TOTVALthisline <- grep("MainContent_lblGenAppraisal", tempx)
    TOTVALclean <- trimws(gsub("<[^<>]*>", "", tempx[TOTVALthisline[2]]))
    Housing$totval[i] <- as.numeric(gsub("[^0-9]","",TOTVALclean))
    
    #Total number of bedrooms (numeric)
    BEDthisline <- grep("To*ta*l Be*dro*ms", tempx, value = FALSE)
    BEDclean <- gsub("<[^<>]*>", "", tempx[BEDthisline])
    BEDvalue <- gsub("[^0-9]","",BEDclean)
    
    #Loops through the BEDvalue vector to report the number of bedrooms or NA.
    #The if statement checks BEDvalue to make sure it is not an empty string. 
    #If there is more than one numeric object in the BEDvalue, the sum of the 
    #values are reported.
    #Decided to report 9 as a numeric value isntead of "9+" as a string.
    for (j in 1:length(BEDvalue)) {
      if (BEDvalue[j] != "") {
        Housing$bedrooms[i] <- sum(as.numeric(BEDvalue), na.rm = TRUE)
      }
    }
    
    #Is there a field with 9+ bedrooms?
    if (grepl("\\+", BEDclean)) {
      Housing$bedrooms9plus[i] <- T
    } else {
      Housing$bedrooms9plus[i] <- F}
    
    #Total number of full bathrooms
    BATHthisline <- grep("To*ta*l Ba*thr*o*m*s", tempx)
    BATHclean <- gsub("<[^<>]*>", "", tempx[BATHthisline])
    
    #Checks to see if the word Half is used in the bathrooom line and converts
    #"1 Half" to 0.5. 
    if (grepl("Half", BATHclean[1])) {
      BATHvalue <- 0.5
    } else {
      BATHvalue <- gsub("[^0-9./]","",BATHclean)
    }
    
    #Sets and Resets TOTBATHvalue and ADDHALFBATH
    TOTBATHvalue <- NA
    ADDHALFBATH <- NA
    
    #Loops through the BATHvalue vector to check if each element contains a 
    #a value and then checks if that value is a fraction. If the value is not
    #a fraction in a string, a TOTBATHvalue is updated to include the BATHvalue.
    #If the value is a fraction in a string, the fraction is parsed, turned into
    #a numeric decimal, and TOTBATHvalue is updated. If BATHvalue is empty, 
    #nothing is updated in the data frame.
    for (j in 1:length(BATHvalue)) {
      if (BATHvalue[j] != "") {
        if (BATHvalue[j] != "1/2") {
          TOTBATHvalue <- sum(TOTBATHvalue, as.numeric(BATHvalue[j]), 
                              na.rm = TRUE)
        } else {
          BATHvaluenofrac <- lapply(BATHvalue, function(x) eval(parse(text=x)))
          TOTBATHvalue <- sum(TOTBATHvalue, as.numeric(BATHvaluenofrac), 
                              na.rm = TRUE)
        }
      }
    }
    
    #If TOTBATHvalue is an integer, it is reported in the data frame. 
    #If TOTBATHvalue is not an integer, TOTBATHvalue contains .5, 0.5 will be 
    #subracted from the value reported into the data frame for number of full 
    #bathrooms and instead is saved as 1 half bath to be used later.
    if (!is.na(TOTBATHvalue)) {
      if (as.integer(TOTBATHvalue) == TOTBATHvalue) {
        Housing$bathrooms[i] <- TOTBATHvalue
        ADDHALFBATH <- NA
      } else {
        Housing[i,6] <- TOTBATHvalue - .5
        ADDHALFBATH <- 1
      }
    }
    
    #Total number of half baths
    HALFBATHthisline <- grep("To*ta*l Half Ba*ths", tempx)
    
    #If a line that holds the half bath information exists, the line will be 
    #removed of any html and non-numeric characters and saved as HALFBATHvalue. 
    if (length(HALFBATHthisline) != 0) {
      HALFBATHclean <- gsub("<[^<>]*>", "", tempx[HALFBATHthisline])
      HALFBATHvalue <- gsub("[^0-9]","",HALFBATHclean)
      
      #Loops through HALFBATHvalue, and if HALFBATHvalue is not an empty string, 
      #the sum of the values will be added to the cell in the data frame for 
      #half baths. 
      for (j in 1:length(HALFBATHvalue)) {
        if (HALFBATHvalue[j] != "") {
          Housing$halfbaths[i] <- sum(as.numeric(HALFBATHvalue),
                                      na.rm = TRUE)
        }
      }
    }
    
    #Adds Half Bath info from Full Bath coding into data frame to existing amount
    if (!is.na(ADDHALFBATH)) {
      Housing$halfbaths[i] <- sum(as.numeric(ADDHALFBATH),Housing[i,7], 
                                  na.rm = TRUE)
    }
    
    
    
    #Finding Owner Address
    addressline <- grep("MainContent_lblAddr1", tempx)
    addressclean0 <- gsub("<br>", " ", tempx[addressline])
    addressclean1 <- gsub("<[^<>]*>", "", addressclean0)
    addressonly <- gsub("\t\t\tAddress", "", addressclean1)
    Housing$address[i] <- addressonly
    
    #Sale History/Ownership History
    for (j in 1:5) {
      infoline <- grep("Ownership History", tempx)
      eachinfoline <- infoline[2] + 2*(1+j)
      eachinfovector <- unlist(strsplit(tempx[eachinfoline], split = "align"))
      eachinfoclean <- gsub("<[^<>]*>", "", eachinfovector)
      saledate <- eachinfoclean[length(eachinfoclean)]
      saledateclean <- trimws(gsub("=\"center\">", "", saledate))
      if (saledateclean != "\t\t") {
        if (saledateclean != "&nbsp;") {
          if (saledateclean != "") {
            Housing[i,3*(2+j)] <- saledateclean            
          }
        }
      }
      
      saleprice <- eachinfoclean[2]
      salepriceclean <- gsub("[^0-9]", "", saleprice)
      Housing[i,(3*(2+j))+1] <- as.numeric(salepriceclean)
      
      saleowner <- eachinfoclean[1]
      if (trimws(saleowner) != "") {
        saleownerclean0 <- gsub("\t\t\t\t", "", saleowner)
        saleownerclean1 <- gsub("<td ", "", saleownerclean0)
        saleownerclean2 <- gsub("&amp;", "&", saleownerclean1)
        Housing[i,(3*(2+j))+2] <- saleownerclean2
      }
    }
    
    #Living area in square feet
    sqftline <- grep("MainContent_ctl01_lblBldArea", tempx)
    sqftclean0 <- gsub("<[^<>]*>", "", tempx[sqftline])
    sqftclean1 <- gsub("[^0-9]", "", sqftclean0)
    Housing$sqft[i] <- as.numeric(sqftclean1)
    
    #Replacement Cost
    replcostline <- grep("MainContent_ctl01_lblRcn", tempx)
    replcostclean0 <- gsub("<[^<>]*>", "", tempx[replcostline[1]])
    replcostclean1 <- gsub("[^0-9]", "", replcostclean0)
    Housing$replcost[i] <- as.numeric(replcostclean1)
    
    #Multibuilding
    buildingcountthisline <- grep("MainContent_lblBldCount", tempx)
    buildingcountclean <- gsub("<[^<>]*>", "", tempx[buildingcountthisline])
    if (as.numeric(buildingcountclean) > 1) {
      Housing$multibuilding[i] <- T
    } else {
      Housing$multibuilding[i] <- F}
    
    #Percent Good
    ph = grepl("lblPctGood",tempx)
    if (sum(ph) == 1){
      scrapeit=tempx[grepl("lblPctGood",tempx)]
      scrapeit=sub(".*lblPctGood\">", "", scrapeit)
      scrapeit = sub("</span>.*","",scrapeit)
      scrapeit=as.numeric(scrapeit)
      Housing$pctgood[i]=scrapeit
    }
    else {Housing$pctgood[i]=NA}
    
    ph=grepl("<td>Style",tempx)
    if (sum(ph)==1){
      scrapeit=tempx[grepl("<td>Style",tempx)]
      scrapeit=sub(".*</td><td>", "", scrapeit)
      scrapeit = sub("</td.*","",scrapeit)
      Housing$style[i]=scrapeit
    } else {Housing$style[i]=NA}
    
    ph=grepl("<td>Model</td><td>",tempx)
    if (sum(ph)==1){
      scrapeit=tempx[grepl("<td>Model</td><td>",tempx)]
      scrapeit=sub(".*</td><td>", "", scrapeit)
      scrapeit = sub("</td.*","",scrapeit)
      if (trimws(scrapeit) != "") {
        Housing$model[i]=scrapeit 
      }
    }
    
    ph=grepl("<td>Grade:</td><td>",tempx)
    if (sum(ph)==1){
      scrapeit=tempx[grepl("<td>Grade:</td><td>",tempx)]
      scrapeit=sub(".*</td><td>", "", scrapeit)
      scrapeit = sub("</td.*","",scrapeit)
      if (trimws(scrapeit) != "") {
        Housing$grade[i]=scrapeit
      }
    }
    
    ph=grepl("<td>Occupancy</td><td>",tempx)
    if (sum(ph)==1){
      scrapeit=tempx[grepl("<td>Occupancy</td><td>",tempx)]
      scrapeit=sub(".*</td><td>", "", scrapeit)
      scrapeit = sub("</td.*","",scrapeit)
      Housing$occupancy[i]=as.numeric(scrapeit)
    } else {Housing$occupancy[i]=NA}
    
    ph=grepl("AC Type:</td><td>",tempx)
    if (sum(ph)==1){
      scrapeit=tempx[grepl("AC Type:</td><td>",tempx)]
      scrapeit=sub(".*</td><td>", "", scrapeit)
      scrapeit = sub("</td.*","",scrapeit)
      if (trimws(scrapeit) != "") {
        Housing$actype[i]=scrapeit
      }
    }
    
    ph=grepl("Bath Style:</td><td>",tempx)
    if (sum(ph)==1){
      scrapeit=tempx[grepl("Bath Style:</td><td>",tempx)]
      scrapeit=sub(".*</td><td>", "", scrapeit)
      scrapeit = sub("</td.*","",scrapeit)
      if (trimws(scrapeit) != "") {
        Housing$bathstyle[i]=scrapeit
      }
    }
    
    ph=grepl("Kitchen Style:</td><td>",tempx)
    if (sum(ph)==1){
      scrapeit=tempx[grepl("Kitchen Style:</td><td>",tempx)]
      scrapeit=sub(".*</td><td>", "", scrapeit)
      scrapeit = sub("</td.*","",scrapeit)
      if (trimws(scrapeit) != "") {
        Housing$kstyle[i]=scrapeit
      }
    }
    
    ph = grepl("MainContent_lblLndAcres\">",tempx)
    if (sum(ph)==1){
      scrapeit=tempx[grepl("MainContent_lblLndAcres\">",tempx)]
      scrapeit=sub(".*Acres\">", "", scrapeit)
      scrapeit = sub("</span.*","",scrapeit)
      Housing$acres[i]=as.numeric(scrapeit)
    } else {Housing$acres[i]=NA}
    
    ph = grepl("Extra Features$",tempx)
    if (sum(ph)==1){
      scrapeit=tempx[(grep("Extra Features$",tempx)+4)]
      if (sum(grepl("\\$",scrapeit))==1){
        scrapeit=sub(".*\\$", "", scrapeit)
        scrapeit = sub(",","",scrapeit)
        scrapeit = sub("</td>.*","",scrapeit)
        Housing$exval[i]=scrapeit
      } else {
        scrapeit=tempx[(grep("Extra Features$",tempx)+5)]
        if (sum(grepl("No Data for Extra Features",scrapeit))>=1){
          Housing$exval[i]="No Data for Extra Features"
        } else {Housing$exval[i]=NA}
      }
    } else {Housing$exval[i]=NA}
    
    ph = grepl("lblPctGood",tempx)
    if (sum(ph) > 1){
      scrapeit=tempx[grepl("lblPctGood",tempx)]
      scrapeit=sub(".*lblPctGood\">", "", scrapeit)
      scrapeit = sub("</span>.*","",scrapeit)
      scrapeit=as.numeric(scrapeit)
      Housing$pctgood[i]=mean(scrapeit,na.rm = TRUE)
    } else {Housing$pctgood[i]=Housing$pctgood[i]}
    
    ph=grepl("<td>Style",tempx)
    if (sum(ph)>1){
      scrapeit=tempx[grepl("<td>Style",tempx)]
      scrapeit=sub(".*</td><td>", "", scrapeit)
      scrapeit = sub("</td.*","",scrapeit)
      scrapeit1=scrapeit[1]
      for (j in 2:length(scrapeit)){
        scrapeit1=paste(scrapeit1, scrapeit[j], sep=", ")
      }
      Housing$style[i]=scrapeit1
    } else {Housing$style[i]=Housing$style[i]}
    
    ph=grepl("<td>Model</td><td>",tempx)
    if (sum(ph)>1){
      scrapeit=tempx[grepl("<td>Model</td><td>",tempx)]
      scrapeit=sub(".*</td><td>", "", scrapeit)
      scrapeit = sub("</td.*","",scrapeit)
      scrapeit1=scrapeit[1]
      for (j in 2:length(scrapeit)){
        scrapeit1=paste(scrapeit1,scrapeit[j],sep=", ")
      }
      Housing$model[i]=scrapeit1
    } else {Housing$model[i]=Housing$model[i]}
    
    ph=grepl("<td>Grade:</td><td>",tempx)
    if (sum(ph)>1){
      scrapeit=tempx[grepl("<td>Grade:</td><td>",tempx)]
      scrapeit=sub(".*</td><td>", "", scrapeit)
      scrapeit = sub("</td.*","",scrapeit)
      scrapeit1=scrapeit[1]
      for (j in 2:length(scrapeit)){
        scrapeit1=paste(scrapeit1, scrapeit[j], sep=", ")
      }
      Housing$grade[i]=scrapeit1
    } else {Housing$grade[i]=Housing$grade[i]}
    
    ph=grepl("<td>Occupancy</td><td>",tempx)
    if (sum(ph)>1){
      scrapeit=tempx[grepl("<td>Occupancy</td><td>",tempx)]
      scrapeit=sub(".*</td><td>", "", scrapeit)
      scrapeit = sub("</td.*","",scrapeit)
      scrapeit=sum(as.numeric(scrapeit),na.rm = TRUE)
      Housing$occupancy[i]=scrapeit
    } else {Housing$occupancy[i]=Housing$occupancy[i]}
    
    ph=grepl("AC Type:</td><td>",tempx)
    if (sum(ph)>1){
      scrapeit=tempx[grepl("AC Type:</td><td>",tempx)]
      scrapeit=sub(".*</td><td>", "", scrapeit)
      scrapeit = sub("</td.*","",scrapeit)
      scrapeit1=scrapeit[1]
      for (j in 2:length(scrapeit)){
        scrapeit1=paste(scrapeit1,scrapeit[j],sep=", ")
      }
      Housing$actype[i]=scrapeit1
    } else {Housing$actype[i]=Housing$actype[i]}
    
    ph=grepl("Bath Style:</td><td>",tempx)
    if (sum(ph)>1){
      scrapeit=tempx[grepl("Bath Style:</td><td>",tempx)]
      scrapeit=sub(".*</td><td>", "", scrapeit)
      scrapeit = sub("</td.*","",scrapeit)
      if (scrapeit[1]=="None"){scrapeit1=as.character("No Style")}
      else {scrapeit1=scrapeit[1]}
      scrapeit1=scrapeit[i]
      for (j in 2:length(scrapeit)){
        scrapeit1=paste(scrapeit1,scrapeit[j],sep=", ")
      }
      Housing$bathstyle[i]=scrapeit1
    } else {Housing$bathstyle[i]=Housing$bathstyle[i]}
    
    ph=grepl("Kitchen Style:</td><td>",tempx)
    if (sum(ph)>1){
      scrapeit=tempx[grepl("Kitchen Style:</td><td>",tempx)]
      scrapeit=sub(".*</td><td>", "", scrapeit)
      scrapeit = sub("</td.*","",scrapeit)
      if (scrapeit[1]=="None"){scrapeit1=as.character("No Style")}
      else {scrapeit1=scrapeit[1]}
      scrapeit1=scrapeit[1]
      for (j in 2:length(scrapeit)){
        scrapeit1=paste(scrapeit1,scrapeit[j],sep=", ")
      }
      Housing$kstyle[i]=scrapeit1
    } else {Housing$kstyle[i]=Housing$kstyle[i]}
    
  }
}    

Housing$bathstyle[grep("&nbsp;",Housing$bathstyle)]=sub("&nbsp;","No Data",
                                                        Housing$bathstyle[grep("&nbsp;",Housing$bathstyle)])
Housing$kstyle[grep("&nbsp;",Housing$kstyle)]=sub("&nbsp;","No Data",
                                                  Housing$kstyle[grep("&nbsp;",Housing$kstyle)])





write.csv(Housing, "dataclean2.csv", row.names = FALSE, as.is=TRUE)
