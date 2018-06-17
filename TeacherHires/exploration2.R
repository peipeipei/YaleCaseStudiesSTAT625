hires <- read.csv("TeacherHires.csv",as.is=TRUE)

hires[hires == "N/A"] <- NA
hires[hires == " N/A"] <- NA

# interviewed
# hired 
# experience 
# ASK SUSAN IF "yes*" in hired IS ANYTHING DIFFERENT
for (ii in 1:length(appdate)){
  # interviewed
  if(grepl("yes",hires$interviewed[ii])){
    hires$interviewed[ii] <- "yes"
  }
  # hired
  if (grepl("yes", hires$hired[ii])){
    hires$hired[ii] <- "yes"
  }
  # experience
  if(grepl("month",hires$experience[ii])){
    experienceMonths <- as.numeric(gsub("([0-9]+).*$", "\\1", hires$experience[ii]))
    hires$experience[ii] <- experienceMonths/12
  }
  if(grepl("years",hires$experience[ii])){
    hires$experience[ii] <- as.numeric(gsub("([0-9]+).*$", "\\1", hires$experience[ii]))
  }
  # hired into 0 and 1
  if (hires$hired[ii] == "yes"){
    hires$hired_01[ii] <- 1
  }
  if (hires$hired[ii] == "no"){
    hires$hired_01[ii] <- 0
  }
}
hires$experience <- as.numeric(hires$experience)


# appdate
appdate <- strsplit(hires$appdate,"/")
appyear <- rep(NA, length(appdate))
appmonth <- rep(NA, length(appdate))
for (ii in 1:length(appdate)){
  appyear[ii] <- appdate[[ii]][3]
  appmonth[ii] <- appdate[[ii]][1]
}
appyear[which(appyear == 2004)] <- "04"
hires$appyear <- appyear
hires$appmonth <- appmonth
hires$appmonth_abrv <- month.abb[as.numeric(appmonth)]

# GPAs
hires$GPA.u <- as.numeric(hires$GPA.u)
hires$GPA.g <- as.numeric(hires$GPA.g)
hires[which(hires$GPA.g > 4.5),]$GPA.g <- NA

# MA
hires[which(grepl("yes",hires$MA)),]$MA <- "yes"

# substitute
hires[which(grepl("no",hires$substitute)),]$substitute <- "no"

# teaching
hires[which(grepl("yes",hires$teaching)),]$teaching <- "yes"

# workkids
hires[which(grepl("no",hires$workkids)),]$workkids <- "no"

# residence
hires$residence <- factor(hires$residence)

hires <- hires[which(!is.na(hires$interviewed) & !is.na(hires$hired) & !is.na(hires$age)),]
