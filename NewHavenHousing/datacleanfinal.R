### Phoebe Cheung, Margaret Luo, Pei Tao
### Homework 4


### Read csv from our original scrape of the data
original <- read.csv('hw3_ptc24_ml2484_pt347.csv')


### Read in everyone's submissions to begin creating a Master Data Frame
folder <- "hw3_submit"
files <- dir(folder, pattern=".csv")

alldf <- list()
for (i in 1:length(files)) {
  netid <- gsub(".*_hw3_(.*).csv", "\\1", files[i])
  netid <- gsub("(.*)-[0-9]", "\\1", netid)
  alldf[[netid]] <- read.csv(paste0(folder, "/", files[i]), as.is=TRUE)
}


### Master scrape of all 8 solutions, choose what we believe to be the best 
### solution 
# preferences for:
checks <- c("address", "saledate1", "saleprice1", "saleowner1", "saledate2",
            "saleprice2", "saleowner2", "saledate3", "saleprice3", "saleowner3",
            "saledate4", "saleprice4", "saleowner4", "saledate5", "saleprice5",
            "saleowner5", "sqft", "replcost")

for (i in 1:length(checks)) {
  thiscol <- checks[i]
  
  allsols <- sapply(alldf, function(df) df[,thiscol])
  
  #address: want values sx67 has for the commas in the address, trims leading ws
  #when no street number
  if (i == 1) {
    address <- trimws(allsols[,6])
  }
  
  #saledate 1: yq.. preference to keep in format mm/dd/yyyy
  if (i == 2) {
    saledate1 <- allsols[,3]
  }
  
  #saleprice1
  if (i == 3) {
    saleprice1 <- allsols[,1]
  }
  
  #saleowner1: phh26.. changes &amp to '&' while also removing trailing white space
  # and then cleaned up extra info before ";" in certain PIDS like 4763
  if (i == 4) {
    saleowner1 <- NULL
    bestchoice <- allsols[,1]
    cleanbestchoice <- strsplit(bestchoice, ";")
    for (j in 1:length(cleanbestchoice)) {
      cat(j, "\n")
      if (length(cleanbestchoice[j][[1]]) == 2) {
        saleowner1[j] <- cleanbestchoice[j][[1]][2]
      } else {
        saleowner1[j] <- cleanbestchoice[j][[1]][1]
      }
    }
  }
  
  #saledate2: yq76.. perference to keep in format mm/dd/yyyy
  if (i == 5) {
    saledate2 <- allsols[,3]
  }
  
  #saleprice2
  if (i == 6) {
    saleprice2 <- allsols[,1]
  }
  
  #saleowner2: trim trailing ws of sx67 because sx67 doesn't contain extra info 
  #before ";" in certain pids
  if (i == 7) {
    saleowner2 <- trimws(allsols[,6])
  }
  
  #saledate3: yq76.. preference to keep in format mm/dd/yyyy
  if (i == 8) {
    saledate3 <- allsols[,3]
  }
  
  #saleprice3
  if (i == 9) {
    saleprice3 <- allsols[,1]
  }
  
  #saleowner3: ttrim trailing ws of sx67 because sx67 doesn't contain extra info 
  #before ";" in certain pids
  if (i == 10) {
    saleowner3 <- trimws(allsols[,6])
  }
  
  #saledate4: yq76.. preference to keep in format mm/dd/yyyy
  if (i == 11) {
    saledate4 <- allsols[,3]
  }
  
  #saleprice4
  if (i == 12) {
    saleprice4 <- allsols[,1]
  }
  
  #saleowner4: ttrim trailing ws of sx67 because sx67 doesn't contain extra info 
  #before ";" in certain pids
  if (i == 13) {
    saleowner4 <- trimws(allsols[,6])
  }
  
  #saledate5: yq76.. preference to keep in format mm/dd/yyyy
  if (i == 14) {
    saledate5 <- allsols[,3]
  }
  
  #saleprice5
  if (i == 15) {
    saleprice5 <- allsols[,1]
  }
  
  
  #saleowner5: trim trailing ws of sx67 because sx67 doesn't contain extra info 
  #before ";" in certain pids
  if (i == 16) {
    saleowner5 <- trimws(allsols[,6])
  }
  
  #living square feet: chose zl392.. because summed living area across all
  #buildings/section if more than one building/section
  if (i == 17) {
    sqft <- allsols[,5]
  }
  
  #replacement cost: chose zl392.. because summed replacement avlue across all 
  #buildings/sections if more than one building/section
  if (i == 18) {
    replcost <- allsols[,5]
  }
}

# preferences for pctgood, acres, exval, occupancy, occupancy, location
modal_table = NULL
modalpctgood = NULL
for (i in 1:27307){
  for (j in 1:length(alldf)){
    modal_table[j] = alldf[[j]]$pctgood[i]}
  modalpctgood[i] = names(table(modal_table,useNA="always")[
    order(table(modal_table,useNA="always"),decreasing=T)])[1]
}

modal_table=NULL
modalacres = NULL
for (i in 1:27307){
  for (j in 1:length(alldf)){
    modal_table[j] = alldf[[j]]$acres[i]}
  modalacres[i] = names(table(modal_table,useNA="always")[
    order(table(modal_table,useNA="always"),decreasing=T)])[1]
}

modal_table=NULL
modalexval = NULL
for (i in 1:27307){
  for (j in 1:length(alldf)){
    modal_table[j] = alldf[[j]]$exval[i]}
  modalexval[i] = names(table(modal_table,useNA="always")[
    order(table(modal_table,useNA="always"),decreasing=T)])[1]
}

modal_table=NULL
modaloccupancy = NULL
for (i in 1:27307){
  for (j in 1:length(alldf)){
    modal_table[j] = alldf[[j]]$occupancy[i]}
  modaloccupancy[i] = names(table(modal_table,useNA="always")[
    order(table(modal_table,useNA="always"),decreasing=T)])[1]
}

modal_table=NULL
modallocation = NULL
for (i in 1:27307){
  for (j in 1:length(alldf)){
    modal_table[j] = alldf[[j]]$location[i]}
  modallocation[i] = names(table(modal_table,useNA="always")[
    order(table(modal_table,useNA="always"),decreasing=T)])[1]
}


### preferences for: yearbuilt, totval, bedrooms, bathrooms, halfbaths, zone,
### neighborhood, #landval, garagesqft
data <- matrix(nrow=27307, ncol=10, NA)
data <- as.data.frame(data)
colnames(data) <- c("pid", "yearbuilt", "totval", "bedrooms", "bathrooms", "halfbaths", 
                    "zone", "neighborhood", "landval", "garagesqft")
data$pid <- seq(1,27307,1)

# get column names
colnames(alldf[[1]])

# yearbuilt
thiscol <- "yearbuilt"
colnames(alldf[[1]])[18] <- "yearbuilt"
allyearbuilt <- sapply(alldf, function(df) df[,thiscol])
v <- apply(allyearbuilt,
           1, function(y) length(unique(y)))
table(v)
temp <- rep(NA, 27307)
for (ii in 1:27307){
  tableyear <- table(allyearbuilt[ii,], useNA = "always")
  temp[ii] <- as.integer(names(tableyear[order(tableyear, decreasing=T)])[1])
}
data$yearbuilt <- temp

# totval
thiscol <- "totval"
alltotval <- sapply(alldf, function(df) df[,thiscol])
v <- apply(alltotval,
           1, function(y) length(unique(y)))
table(v)
data$totval <- alltotval[,1]

# bedrooms
thiscol <- "bedrooms"
allbedrooms <- sapply(alldf, function(df) df[,thiscol])
v <- apply(allbedrooms,
           1, function(y) length(unique(y)))
table(v)
data$bedrooms <- allbedrooms[,4]

# bathrooms
thiscol <- "bathrooms"
colnames(alldf[[1]])[21] <- "bathrooms"
allbathrooms <- sapply(alldf, function(df) df[,thiscol])
v <- apply(allbathrooms,
           1, function(y) length(unique(y)))
table(v)
temp <- rep(NA, 27307)
for (ii in 1:27307){
  tablebathrooms <- table(allbathrooms[ii,], useNA = "always")
  temp[ii] <- as.integer(names(tablebathrooms[order(tablebathrooms, decreasing=T)])[1])
}
data$bathrooms <- temp
# to check which of ours differ from the mode, use the below line
# which(allbathrooms[,4] != temp)

# halfbaths
thiscol <- "halfbaths"
allhalfbaths <- sapply(alldf, function(df) df[,thiscol])
v <- apply(allhalfbaths,
           1, function(y) length(unique(y)))
table(v)
temp <- rep(NA, 27307)
for (ii in 1:27307){
  tablehalfbaths <- table(allhalfbaths[ii,], useNA = "always")
  temp[ii] <- as.integer(names(tablehalfbaths[order(tablehalfbaths, decreasing=T)])[1])
}
data$halfbaths <- temp
# which(allhalfbaths[,4] != temp)

# zone
thiscol <- "zone"
allzone <- sapply(alldf, function(df) df[,thiscol])
v <- apply(allzone,
           1, function(y) length(unique(y)))
table(v)
data$zone <- allzone[,1]

# neighborhood
thiscol <- "neighborhood"
allneighborhood <- sapply(alldf, function(df) df[,thiscol])
v <- apply(allneighborhood,
           1, function(y) length(unique(y)))
table(v)
temp <- rep(NA, 27307)
for (ii in 1:27307){
  tableneighborhood <- table(allneighborhood[ii,], useNA = "always")
  temp[ii] <- names(tableneighborhood[order(tableneighborhood, decreasing=T)])[1]
}
data$neighborhood <- temp
# which(allneighborhood[,4] != temp)

# landval
thiscol <- "landval"
alllandval <- sapply(alldf, function(df) df[,thiscol])
v <- apply(alllandval,
           1, function(y) length(unique(y)))
table(v)
temp <- rep(NA, 27307)
for (ii in 1:27307){
  tablelandval <- table(alllandval[ii,], useNA = "always")
  temp[ii] <- names(tablelandval[order(tablelandval, decreasing=T)])[1]
}
data$landval <- temp
# which(alllandval[,4] != temp)

# garagesqft
thiscol <- "garagesqft"
allgarage <- sapply(alldf, function(df) df[,thiscol])
v <- apply(allgarage,
           1, function(y) length(unique(y)))
table(v)
data$garagesqft <- allgarage[,5]


###Putting all of our preference soltuions into a Master Dataframe
MasterData <- data.frame(pid = seq(1:27307), address = address, 
                         saledate1 = saledate1, saleprice1 = saleprice1,
                         saleowner1 = saleowner1, saledate2 = saledate2, saleprice2 = saleprice2,
                         saleowner2 = saleowner2, saledate3 = saledate3, saleprice3 = saleprice3,
                         saleowner3 = saleowner3, saledate4 = saledate4, saleprice4 = saleprice4,
                         saleowner4 = saleowner4, saledate5 = saledate5, saleprice5 = saleprice5,
                         saleowner5 = saleowner5, sqft = sqft, replcost = replcost, 
                         pctgood = modalpctgood, acres = modalacres, exval = modalexval, 
                         occupancy = modaloccupancy)
MasterData$location <- modallocation
MasterData$yearbuilt <- data$yearbuilt
MasterData$totval <- data$totval
MasterData$bedrooms <- data$bedrooms
MasterData$bathrooms <- data$bathrooms
MasterData$halfbaths <- data$halfbaths
MasterData$zone <- data$zone
MasterData$neighborhood <- data$neighborhood
MasterData$landval <- data$landval
MasterData$garagesqft <- data$garagesqft
MasterData$style <- original$style
MasterData$model <- original$model
MasterData$grade <- original$grade
MasterData$actype <- original$actype
MasterData$bathstyle <- original$bathstyle
MasterData$kstyle <- original$kstyle
MasterData$bedrooms9plus <- original$bedrooms9plus
MasterData$multibuilding <- original$multibuilding


### To run the geocoder to get match and long/lat information
locations <- MasterData$location
pid <- 1:27307
city <- "New Haven"
state <- "CT"

geodata <- data.frame(pid = pid,
                          location=locations,
                          city=city,
                          state=state,
                          zip="")

for (i in 1:27){
  write.csv(geodata[1:1000,],paste("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4/parsing",
                                   i,".csv",sep=""),row.names=F)
  geodata=geodata[1001:dim(geodata)[1],]
}
write.csv(geodata[1:dim(geodata)[1],],paste(
  "/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4/parsing",
  "28",".csv",sep=""),row.names=F)

rm(geodata)

url <- "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch?form"

for (i in 1:28){
  f=paste("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4/parsing",i,".csv",sep="")
  req <- POST(url, body=list(addressFile = upload_file(f),
                             benchmark = "Public_AR_Census2010",
                             vintage = "Census2010_Census2010"),
              encode = "multipart")
  content(req, "text", encoding = "UTF-8")
  
  # easier to write the output into a .csv file and then read it in
  outfile <- tempfile(fileext = ".csv")
  writeLines(content(req, "text", encoding = "UTF-8"), outfile)
  
  v <- read.csv(outfile, header=FALSE, as.is=TRUE,
                col.names = c("pid", "address", "match1", "match2", "address_match",
                              "latlong", "lineid", "side", "state", "county", "tract", "block") )
  
  v=v[-which(v$V1=="pid"),]
  v=v[order(as.numeric(v$V1)),]
  write.csv(v,paste("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4/v",i,".csv",sep=""),
            row.names=F)
}

# Creates dataframe for the information that the geocoder website provides
govdata=NULL
for ( i in 1:28){
  ph=read.csv(paste("/Volumes/SSG SSD T3/ssd_statgrad/DS2_625/hw4/v",i,".csv",sep=""))
  govdata=rbind(govdata,ph)
}
colnames(govdata) <- c("pid", "address", "match1", "match2", "address_match",
                       "latlong", "lineid", "side", "state", "county", "tract", "block")
latlonglist=strsplit(as.character(govdata$latlong),split=",")

latitude=NULL
longitude=NULL
for (i in 1:length(latlonglist)){
  if (length(latlonglist[[i]])>=1){
    latitude[i] = latlonglist[[i]][1]
    longitude[i] = latlonglist[[i]][2]
  }
  else {
    latitude[i] = NA
    longitude[i] = NA
  } 
}


### Put the geocoder info for match1, match2, latitude, longitude into Master
### Data Frame
MasterData$match1=govdata$match1
MasterData$match2=govdata$match2
MasterData$latitude=latitude
MasterData$longitude=longitude

write.csv(MasterData, "hw4_ptc24_ml2484_pt347.csv", row.names = F)
