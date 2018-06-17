##################### 
# New Haven Road Race
#####################

# By now, you should have individually scraped versions of the road race
# data for at least 5 years.

# Your task today is to work in groups of 3 to produce ONE amazing visualization
# of the dataset. Try to work with someone new!

# Please have one person project their screen to a TV. By 2pm, be ready to have
# your plot completed and projected on the screen. 

# Some inspiration:
# http://www.nytimes.com/interactive/2012/08/05/sports/olympics/the-100-meter-dash-one-race-every-medalist-ever.html



################################# 
# Homework: Consider the question of whether repeat runners improve/regress 
#           over time. How does one quantify this improvement on an individual
#           and/or group level? Be sure to account for other variables that
#           might be relevant to this conversation.

# Nothing to submit. Be prepared to present a linear model that addresses this
# question next class.

data <- read.csv("fiveYears.csv", as.is=TRUE)
uniqueNames <- unique(data$Name)
repeatRunners <- data.frame(Event=as.Date(character()),
                            Year=integer(), 
                            Name=character(), 
                            Age=integer(),
                            Sex=character(),
                            Div=character(),
                            Nettime=integer()) 

for (ii in 1:length(uniqueNames)){
  name <- uniqueNames[ii]
  temp <- data[which(data$Name == name),]
  if (nrow(temp) > 1){
    years <- unique(temp$Year)
    if (length(years) == nrow(temp)){
      repeatRunners <- rbind(repeatRunners,temp)
    }
  }
}

write.csv(repeatRunners, "repeatRunners.csv", row.names=FALSE)

################################# 

repeatRunners <- read.csv("repeatRunners.csv", as.is=TRUE)

temps <- data.frame(Year=c(2017, 2016, 2015, 2014, 2013, 2012), 
                    Temp=c(68, 70, 72, 80, 74, 70),
                    Humidity=c(73, 51, 74, 82, 92, 79))

integer0toNA <- function(x) {
  if (length(x) == 0)
    return(NA)
  return(x)
}


data_byRunners <- data.frame(matrix(ncol = 25, nrow = length(unique(repeatRunners$Name))))
x <- c("Name", "Sex", "Age", "Div", 
       "LastRace", "PreviousRace1", "PreviousRace2", "PreviousRace3", "PreviousRace4", "NumRaces",
       "LastYear", "LastTemp", "LastHumidity",
       "PreviousYear1", "PreviousTemp1", "PreviousHumidity1",
       "PreviousYear2", "PreviousTemp2", "PreviousHumidity2",
       "PreviousYear3", "PreviousTemp3", "PreviousHumidity3",
       "PreviousYear4", "PreviousTemp4", "PreviousHumidity4")
colnames(data_byRunners) <- x

names <- unique(repeatRunners$Name)
for (i in 1:length(names)) {
  data_byRunners$Name[i] <- names[i]
  data_byRunners$Sex[i] <- repeatRunners$Sex[which(repeatRunners$Name == names[i])][1]
  data_byRunners$Age[i] <- repeatRunners$Age[which(repeatRunners$Name == names[i])][1]
  data_byRunners$Div[i] <- repeatRunners$Div[which(repeatRunners$Name == names[i])][1]
  data_byRunners$NumRaces[i] <- sum(repeatRunners$Name == names[i])
  
  
  temp_times <- repeatRunners$Nettime[which(repeatRunners$Name == names[i])]
  data_byRunners$LastRace[i] <- temp_times[1]
  data_byRunners$PreviousRace1[i] <- temp_times[2]
  data_byRunners$PreviousRace2[i] <- temp_times[3]
  data_byRunners$PreviousRace3[i] <- temp_times[4]
  data_byRunners$PreviousRace4[i] <- temp_times[5]
  
  temp_rows <- repeatRunners[which(repeatRunners$Name == names[i]),]
  temp_years <- temp_rows$Year
  data_byRunners$LastYear[i] <- temp_years[1]
  data_byRunners$PreviousYear1[i] <- temp_years[2]
  data_byRunners$PreviousYear2[i] <- temp_years[3]
  data_byRunners$PreviousYear3[i] <- temp_years[4]
  data_byRunners$PreviousYear4[i] <- temp_years[5]
  
  data_byRunners$LastTemp[i] <- temps[which(temps$Year == temp_years[1]),]$Temp
  data_byRunners$LastHumidity[i] <- temps[which(temps$Year == temp_years[1]),]$Humidity
  
  data_byRunners$PreviousTemp1[i] <- temps[which(temps$Year == temp_years[2]),]$Temp
  data_byRunners$PreviousHumidity1[i] <- temps[which(temps$Year == temp_years[2]),]$Humidity
  
  data_byRunners$PreviousTemp2[i] <- integer0toNA(temps[which(temps$Year == temp_years[3]),]$Temp)
  data_byRunners$PreviousHumidity2[i] <- integer0toNA(temps[which(temps$Year == temp_years[3]),]$Humidity)
  
  data_byRunners$PreviousTemp3[i] <- integer0toNA(temps[which(temps$Year == temp_years[4]),]$Temp)
  data_byRunners$PreviousHumidity3[i] <- integer0toNA(temps[which(temps$Year == temp_years[4]),]$Humidity)
  
  data_byRunners$PreviousTemp4[i] <- integer0toNA(temps[which(temps$Year == temp_years[5]),]$Temp)
  data_byRunners$PreviousHumidity4[i] <- integer0toNA(temps[which(temps$Year == temp_years[5]),]$Humidity)
  
}

write.csv(data_byRunners, "data_byRunners.csv", row.names = FALSE)

################################# 

data_byRunners <- read.csv("data_byRunners.csv", as.is=TRUE)
data_byRunners$Difference <- data_byRunners$LastRace - data_byRunners$PreviousRace1

lm.1 <- lm(LastRace ~ PreviousRace1 + Sex, data = data_byRunners)
summary(lm.1)
plot(lm.1$residuals)

lm.2 <- lm(sqrt(LastRace) ~ PreviousRace1 + Sex, data = data_byRunners)
summary(lm.2)
plot(lm.2$residuals)

lm.3 <- lm(log(LastRace) ~ PreviousRace1 + Sex, data = data_byRunners)
summary(lm.3)
plot(lm.3$residuals)

lm.4 <- lm(LastRace ~ PreviousRace1 +PreviousRace2 + Sex, data = data_byRunners)
summary(lm.4)
plot(lm.4$residuals)

lm.5 <- lm(log(LastRace) ~ PreviousRace1 +PreviousRace2 + Sex, data = data_byRunners)
summary(lm.5)
plot(lm.5$residuals)

lm.6 <- lm(log(LastRace) ~ PreviousRace1 +PreviousRace2 + Sex + NumRaces, data = data_byRunners)
summary(lm.6)
plot(lm.6$residuals)

# try doing difference in nettime (probably want to log it?)

lm.7 <- lm(Difference ~ PreviousRace1 + PreviousTemp1 + PreviousHumidity1
           + Sex + NumRaces, 
           data = data_byRunners)
summary(lm.7)
plot(lm.6$residuals)

