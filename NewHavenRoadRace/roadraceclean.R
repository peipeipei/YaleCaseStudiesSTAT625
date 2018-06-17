# New Haven Road Race 20k

twenty2017 <- "http://www.newhavenroadrace.org/wp-content/uploads/2015/04/NHRR-2017-20k-results-update-3.txt"
twenty2016 <- "http://www.newhavenroadrace.org/wp-content/uploads/2015/04/NH16-20k-Overall-update5.txt"
twenty2015 <- "http://www.newhavenroadrace.org/wp-content/uploads/2015/04/new-haven-20K-results.txt"
twenty2014 <- "http://www.jbsports.com/wp-content/uploads/RESULTS-20K-overall.txt"
twenty2013 <- "http://www.jbsports.com/wp-content/uploads/RESULTS-NH13-20k1.txt"
twenty2012 <- "http://www.newhavenroadrace.org/wp-content/uploads/2015/04/NH12-20k-overall.txt"

htmls <- c(twenty2017, twenty2016, twenty2015, twenty2014, twenty2013)

Sys.setlocale('LC_ALL', 'C')

getDF <- function(year){
  html <- htmls[2018-year]
  read <- readLines(html)
  
  equalLine <- grep("=", read)
  
  colnames<- read[equalLine - 1]
  colnames <- unlist(strsplit(colnames, " +"))
  
  if (year <= 2015){
    isNameCol <- which(colnames == "Name")
    if (length(isNameCol) > 0){
      colnames <- colnames[-isNameCol]
    } 
  }
  
  equalsigns <- read[equalLine]
  widths <- diff(c(1, gregexpr(" ", equalsigns)[[1]]))
  
  return(read.fwf(file=html, widths=widths, col.names=colnames, skip=equalLine))
}

df2017 <- getDF(2017)
df2016 <- getDF(2016)
df2015 <- getDF(2015)
df2014 <- getDF(2014)
df2013 <- getDF(2013)

cols <- c("Place", "Name", "City", "Div", "X10ksplit", "Time", "Pace", "Nettime", "Sex", "Age")
actualCols <- c("Event", "Year", "Name", "Age", "Sex", "Div", "Nettime")

clean <- function(yearData, df){
  
  length <- NA
  if (yearData > 2015){
    length <- nrow(df)
  }else{
    length <- nrow(df) - 1
  }
  
  event <- rep("20k", length)
  year <- rep(yearData, length)
  
  name <- rep(NA, length)
  sex <- rep(NA, length)
  div <- rep(NA, length)
  age <- rep(NA, length)
  
  if (yearData > 2015){
    name <- df$Name
    div <- df$Div.1
    sex[which(grepl("M", df$Div.1))] <- "M"
    sex[which(grepl("F", df$Div.1))] <- "F"
  } else {
    name <- (paste(trimws(df$First[1:length]), trimws(df$Last[1:length]), sep=" "))
    sex <- df$Sex[1:length]
    div <- df$Div.1[1:length]
    age <- df$Age[1:length]
  }
  
  if (yearData <= 2013){
    divtemp <- trimws(as.character(df$Div.1)[1:length])
    div <- paste(substr(divtemp,1,3), substr(divtemp,4,5), sep="-")
  }
  
  nettime_temp <- trimws(df$Nettime)
  nettime <- rep(NA, length)
  for (ii in 1:length){
    timetemp <- unlist(strsplit(nettime_temp[ii],":"))
    len <- length(timetemp)
    seconds <- as.integer(timetemp[len]) / 60
    minutes <- as.integer(timetemp[len-1])
    if (len-2 == 1){
      minutes <- minutes + 60*as.integer(timetemp[len-2])
    }
    nettime[ii] <- minutes + seconds
  }
  
  return(data.frame(Event=event, Year=year, Name=name, Age=age, Sex=sex,
                    Div=div, Nettime=nettime))
}

clean2017 <- clean(2017, df2017)
clean2016 <- clean(2016, df2016)
clean2015 <- clean(2015, df2015)
clean2014 <- clean(2014, df2014)
clean2013 <- clean(2013, df2013)

fiveYears <- rbind(clean2017,rbind(clean2016,rbind(clean2015,rbind(clean2014, clean2013))))
fiveYears$Div <- trimws(as.character(fiveYears$Div))
fiveYears$Name <- trimws(as.character(fiveYears$Name))
fiveYears$Sex <- trimws(as.character(fiveYears$Sex))

write.csv(fiveYears, "20k.csv", row.names=FALSE)

namesUnaged <- unique(fiveYears$Name[which(fiveYears$Year == 2017 | fiveYears$Year == 2016)])
namesAged <- unique(fiveYears$Name[which(fiveYears$Year == 2013 | fiveYears$Year == 2014 | fiveYears$Year == 2015)])
namesToGetAges <- intersect(namesUnaged, namesAged)

for (ii in 1:length(namesToGetAges)){
  tempdf <- fiveYears[which(fiveYears$Name == namesToGetAges[ii]),] # not sure if this is needed
  
  rows <- which(fiveYears$Name == namesToGetAges[ii])
  earliestYear <- fiveYears$Year[rows[length(rows)]]
  earliestAge <- fiveYears$Age[rows[length(rows)]]
  
  for (jj in 1:length(rows)-1){
    row <- rows[jj]
    fiveYears$Age[row] <- earliestAge + (fiveYears$Year[row] - earliestYear)
  }
}

# set Runner Unknown age to NA
fiveYears$Age[which(fiveYears$Name == "Runner Unknown")] <- NA

write.csv(fiveYears, "fiveYears.csv", row.names=FALSE)


top2017 <- sort(fiveYears$Nettime[which(fiveYears$Year == 2017)])[1:3]
top2016 <- sort(fiveYears$Nettime[which(fiveYears$Year == 2016)])[1:3]
top2015 <- sort(fiveYears$Nettime[which(fiveYears$Year == 2015)])[1:3]
top2014 <- sort(fiveYears$Nettime[which(fiveYears$Year == 2014)])[1:3]
top2013 <- sort(fiveYears$Nettime[which(fiveYears$Year == 2013)])[1:3]

baddf <- data.frame(years = c(rep(2017,3), rep(2016,3), rep(2015,3), rep(2014,3), rep(2013,3)), 
                    times = c(top2017, top2016, top2015, top2014, top2013))
plot(baddf$times ~ baddf$years)

library(ggplot2)
ggplot(fiveYears, aes(Year, Nettime, colour=Div)) + geom_point(position = "jitter")
men <- fiveYears[which(fiveYears$Sex == "M"),]
women <- fiveYears[which(fiveYears$Sex == "F"),]
ggplot(men, aes(Year, Nettime, colour=Div)) + geom_point(position = "jitter")
ggplot(women, aes(Year, Nettime, colour=Div)) + geom_point(position = "jitter")

# gonna use the stupid idea that the nettimes are already sorted by year in the main df
what <- c(which(fiveYears$Year == 2017 & fiveYears$Sex == "M")[1:3],
          which(fiveYears$Year == 2017 & fiveYears$Sex == "F")[1:3],
          which(fiveYears$Year == 2016 & fiveYears$Sex == "M")[1:3],
          which(fiveYears$Year == 2016 & fiveYears$Sex == "F")[1:3],
          which(fiveYears$Year == 2015 & fiveYears$Sex == "M")[1:3],
          which(fiveYears$Year == 2015 & fiveYears$Sex == "F")[1:3],
          which(fiveYears$Year == 2014 & fiveYears$Sex == "M")[1:3],
          which(fiveYears$Year == 2014 & fiveYears$Sex == "F")[1:3],
          which(fiveYears$Year == 2013 & fiveYears$Sex == "M")[1:3],
          which(fiveYears$Year == 2013 & fiveYears$Sex == "F")[1:3])
top3df <- fiveYears[what,]
ggplot(top3df, aes(Year, Nettime, color=Sex)) + geom_point()

ggplot(men, aes(Div, Nettime, colour=Year)) + geom_point(position = "jitter")
ggplot(women, aes(Div, Nettime, colour=Year)) + geom_point(position = "jitter")


unsure <- fiveYears[which(fiveYears$Name %in% namesToGetAges),]
ggplot(unsure[which(unsure$Sex=="F"),], aes(Year, Nettime, group=Name, color=Div)) + geom_line()
ggplot(unsure[which(unsure$Sex=="M"),], aes(Year, Nettime, group=Name, color=Div)) + geom_line()



