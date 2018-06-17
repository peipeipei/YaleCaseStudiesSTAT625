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




full <- glm(hired_01 ~ age + sex + MA + substitute + experience + teaching + workkids + volunteer, 
            data = hires, family = binomial)
null <- glm(hired_01 ~ 1, data = hires, family = binomial)
step(null, scope=list(lower=null, upper=full), direction="forward") # hired_01 ~ volunteer + workkids
step(full, data=hires, direction="backward") # ?
step(null, scope = list(upper=full), data=hires, direction="both") # hired_01 ~ volunteer + workkids

temp.glm <- glm(formula = hired_01 ~ volunteer + workkids, family = binomial, 
                data = hires)



full.agegroup <- glm(hired_01 ~ agegroup + sex + MA + substitute + experience + 
                       teaching + workkids + volunteer + residence, 
                     data = hires, family = binomial)
null.agegroup <- glm(hired_01 ~ 1, data = hires, family = binomial)
step(null.agegroup, scope=list(lower=null.agegroup, upper=full.agegroup), direction="forward") 
  # hired_01 ~ residence + workkids
step(full.agegroup, data=hires, direction="backward") 
  # hired_01 ~ agegroup + sex + MA + experience + workkids + volunteer + residence
step(null.agegroup, scope = list(upper=full), data=hires, direction="both")
  # hired_01 ~ volunteer + workkids

temp.glm <- glm(formula = hired_01 ~ agegroup + sex + MA + experience + workkids + volunteer + residence, 
                family = binomial, data = hires)
temp.glm2 <- glm(hired_01 ~ agegroup + residence,
                 family = binomial, data = hires)
summary(temp.glm2)



female <- hires[which(hires$sex == "Female"),]
full.female <- glm(hired_01 ~ agegroup + residence + MA + substitute + experience + teaching + workkids + volunteer, 
            data = female, family = binomial)
null.female <- glm(hired_01 ~ 1, data = female, family = binomial)
step(null.female, scope=list(lower=null.female, upper=full.female), direction="forward") 
# hired_01 ~ hires + workkids
step(full.female, data=female, direction="backward") 
# hired_01 ~ agegroup + residence + MA + substitute + experience + workkids + volunteer
step(null, scope = list(upper=full), data=female, direction="both") 
# hired_01 ~ volunteer + workkids



interviewed <- hires[which(hires$interviewed == "yes"),]
full.interviewed <- glm(hired_01 ~ agegroup + residence + MA + substitute + experience + teaching + workkids + volunteer, 
                   data = interviewed, family = binomial)
null.interviewed <- glm(hired_01 ~ 1, data = interviewed, family = binomial)
step(null.interviewed, scope=list(lower=null.interviewed, upper=full.interviewed), direction="forward")
step(full.interviewed, data=interviewed, direction="backward") 
step(null.interviewed, scope = list(upper=full.interviewed), data=interviewed, direction="both") 

interview.glm <- glm(hired_01 ~ agegroup + residence + experience, 
                     data = interviewed, family = binomial)
summary(interview.glm)

hired.glm <- glm(hired_01 ~ agegroup,
                 data = hires, family = binomial)
summary(hired.glm)

# below shows that age group does not matter once they are interviewed
interview_simple.glm <- glm(hired_01 ~ agegroup, 
                     data = interviewed, family = binomial)
summary(interview_simple.glm)


temp <- glm(hired_01 ~ age + sex,
            data = hires, family=binomial)
summary(temp)


# gonna try some bootstrapping
# sample 1: those who were hired
# sample 2: those we were NOT hired
# bootstrap might just be good for confidence intervals though... might want to use permutation test
YEShired_age <- hires[which(hires$hired == "yes" & !is.na(hires$age)),]$age
NOhired_age <- hires[which(hires$hired == "no" & !is.na(hires$age)),]$age
test_statistic <- mean(YEShired_age) - mean(NOhired_age)
allages <- c(YEShired_age, NOhired_age)
totlength <- length(YEShired_age) + length(NOhired_age)
N <- 1000
bootstrap <- rep(0,N)
for (ii in 1:N){
  sample <- sample(allages,totlength,replace=T)
  yes_mean <- mean(sample[1:length(YEShired_age)])
  no_mean <- mean(sample[(length(YEShired_age)+1):totlength])
  bootstrap[ii] <- yes_mean - no_mean
}

pvalue <- length(which(test_statistic > bootstrap))/N # once it was 0.01, 0.015

lowerbound <- quantile(bootstrap, 0.025) # about -2.75ish
upperbound <- quantile(bootstrap, 0.975) # about 2.688ish 
# so 0 is within the bound


# let's do a permutation test... 
permutate <- rep(0,N)
for (ii in 1:N){
  sample <- sample(allages,totlength,replace=F)
  yes_mean <- mean(sample[1:length(YEShired_age)])
  no_mean <- mean(sample[(length(YEShired_age)+1):totlength])
  permutate[ii] <- yes_mean - no_mean
}
pvalue_permute <- length(which(test_statistic > permutate))/N #0.002






# bootstrap
# just females
YEShired_age_f <- hires[which(hires$hired == "yes" & !is.na(hires$age) & hires$sex == "Female"),]$age
NOhired_age_f <- hires[which(hires$hired == "no" & !is.na(hires$age)  & hires$sex == "Female"),]$age
test_statistic_f <- mean(YEShired_age_f) - mean(NOhired_age_f)
allages_f <- c(YEShired_age_f, NOhired_age_f)
totlength_f <- length(YEShired_age_f) + length(NOhired_age_f)
N_f <- 1000
bootstrap_f <- rep(0,N_f)
for (ii in 1:N_f){
  sample <- sample(allages_f,totlength_f,replace=T)
  yes_mean <- mean(sample[1:length(YEShired_age_f)])
  no_mean <- mean(sample[(length(YEShired_age_f)+1):totlength_f])
  bootstrap_f[ii] <- yes_mean - no_mean
}
pvalue_f <- length(which(test_statistic_f > bootstrap_f))/N_f # 0.027, 0.035


# let's do some graphs 

# interview age v non-interview age
# hired age v non-hired age 
# interviewed + hired age v. interview + non-hired

boxplot(age ~ interviewed, data=hires)
mean(hires[which(hires$interviewed == "yes"),]$age, na.rm = TRUE) # 30.61947
mean(hires[which(hires$interviewed == "no"),]$age, na.rm = TRUE) # 33.03509

boxplot(age ~ hired, data=hires)
mean(hires[which(hires$hired == "yes"),]$age, na.rm = TRUE) # 30.07042
mean(hires[which(hires$hired == "no"),]$age, na.rm = TRUE) # 32.90964

boxplot(age ~ hired, data=hires[which(hires$interviewed=="yes"),])
mean(hires[which(hires$hired == "yes" & hires$interviewed=="yes"),]$age, na.rm = TRUE) # 30.07042
mean(hires[which(hires$hired == "no" & hires$interviewed=="yes"),]$age, na.rm = TRUE) # 31.54762

boxplot(age ~ sex, data=hires[which(hires$hired == "yes"),])
mean(hires[which(hires$hired == "yes" & hires$sex=="Female"),]$age, na.rm = TRUE) # 29.90909
mean(hires[which(hires$hired == "no" & hires$sex=="Male"),]$age, na.rm = TRUE) # 36.35185


#### DO STUFF ABOUT WOMEN V MEN





# H0 = those who are interviewed are from the same population as those who are not
interviewed <- hires[which(hires$interviewed == "yes" & !is.na(hires$age)),]
not_interviewed <- hires[which(hires$interviewed == "no" & !is.na(hires$age)),]
test_statistic_interview <- mean(interviewed$age) - mean(not_interviewed$age)
allages_interview <- c(interviewed$age, not_interviewed$age)
totlength_interview <- length(interviewed$age) + length(not_interviewed$age)
N_interview <- 1000
permutate_interview <- rep(0,N_interview)
for (ii in 1:N_interview){
  sample_interview <- sample(allages_interview,totlength_interview,replace=F)
  yes_mean <- mean(sample_interview[1:length(interviewed$age)])
  no_mean <- mean(sample_interview[(length(interviewed$age)+1):totlength_interview])
  permutate_interview[ii] <- yes_mean - no_mean
}
pvalue_interview <- length(which(test_statistic_interview > permutate_interview))/N_interview #0.01ish


# H0 = given they were interviewed, those who were hired are in the same population as those who are not
interview_hired <- hires[which(hires$interviewed == "yes" & hires$hired == "yes" & !is.na(hires$age)),]
interview_nothired <- hires[which(hires$interviewed == "yes" & hires$hired == "no" & !is.na(hires$age)),]
test_statistic_ih <- mean(interview_hired$age) - mean(interview_nothired$age)
allages_ih <- c(interview_hired$age, interview_nothired$age)
totlength_ih <- length(interview_hired$age) + length(interview_nothired$age)
N_ih <- 1000
permutate_ih <- rep(0,N_ih)
for (ii in 1:N_ih){
  sample_ih <- sample(allages_ih,totlength_ih,replace=F)
  yes_mean <- mean(sample_ih[1:length(interview_hired$age)])
  no_mean <- mean(sample_ih[(length(interview_hired$age)+1):totlength_ih])
  permutate_ih[ii] <- yes_mean - no_mean
}
pvalue_ih <- length(which(test_statistic_ih > permutate_ih))/N_ih #0.174



# test proportions of those who are interviewed from older v younger





# test proportions of those who are hired from older v younger 


# graph of (proportion younger, proportion hired) and (proportion older, proportion hired), draw y=x line






boxplot(age ~ sex, data=hires[which(hires$hired == "yes"),])
mean(hires[which(hires$hired == "yes" & hires$sex=="Female"),]$age, na.rm = TRUE) # 29.90909
mean(hires[which(hires$hired == "no" & hires$sex=="Male"),]$age, na.rm = TRUE) # 36.35185


males <- hires[which(hired)]



