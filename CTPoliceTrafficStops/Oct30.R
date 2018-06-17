################
# CT Police Traffic Stops
#################

# Today

# I'd expect that we're all somewhat familiar with the dataset now. For today,
# we'll be working in groups of 3 to tackle this dataset together. The focal
# point for our analysis will be on racial discrimination. You should first talk
# about what are some ways of measuring racial discrimination based on the
# variables in the dataset. Then apply statistical modeling to examine whether
# there is evidence for discrimination, and if so, which race of individuals
# experience the most discrimination.

# You will have to submit a report on your findings in one week. Spend the time
# in class today to work on modeling/analysis/outlining your report.

library(tidyverse) 
library(lubridate)
library(ggplot2)
library(nnet)

CT_police <- readRDS("CT_police.rds")

# let's do something very similar to the tutorial
# search rate for each police 

data <- CT_police[CT_police$driver_race %in% c("White", "Black", "Hispanic", "Asian"),]

summary_stats <- function(data, race){
  race <- race
  
  if (race != "all"){
    data <- data[which(data$driver_race == race),]
  }
  n_stops <- nrow(data)
  n_searches <- sum(data$search_conducted)
  n_hits <- sum(data$contraband_found)
  search_rate <- n_searches / n_stops
  hit_rate <- n_hits / n_searches
  
  return(data.frame(race, n_stops, n_searches, n_hits, search_rate, hit_rate))
}

all_rates <- summary_states(data, "all")
white_rates <- summary_stats(data, "White")
black_rates <- summary_stats(data, "Black")
asian_rates <- summary_stats(data, "Asian")
hispanic_rates <- summary_stats(data, "Hispanic")
other_rates <- summary_stats(data, "Other")



# Using the code from the tutorial

summary_stats <- function(search_conducted, contraband_found) {
  n_stops     = length(search_conducted)
  n_searches  = sum(search_conducted)
  n_hits      = sum(contraband_found)
  search_rate = n_searches / n_stops
  hit_rate    = n_hits / n_searches
  return(data.frame(n_stops, n_searches, n_hits, search_rate, hit_rate))
}

basic_summary_statistics_by_race = data %>% 
  group_by(driver_race) %>% 
  do(summary_stats(.$search_conducted, .$contraband_found))
basic_summary_statistics_by_race

basic_summary_statistics_by_race_and_police = data %>% 
  filter(!is.na(county_name)) %>%
  group_by(driver_race, officer_id) %>%
  do(summary_stats(.$search_conducted, .$contraband_found))


data_for_plot <- basic_summary_statistics_by_race_and_police %>%
  filter(driver_race == 'White') %>% 
  right_join(basic_summary_statistics_by_race_and_police %>% filter(driver_race != 'White'), by='officer_id')

# plot search rates. 
max_val = max(basic_summary_statistics_by_race_and_police$search_rate) * 1.05
search_plot = ggplot(data_for_plot) + 
  # specify data we want to plot
  geom_point(aes(x = search_rate.x, y = search_rate.y, size = n_stops.y)) + 
  # make one subplot for each minority race group
  facet_grid(.~driver_race.y) + 
  # add a diagonal line to indicate parity
  geom_abline(slope = 1, intercept = 0, linetype='dashed') +   
  scale_x_continuous('White search rate', limits=c(0, max_val), labels = scales::percent, expand=c(0,0)) + 
  scale_y_continuous('Minority search rate', limits=c(0, max_val), labels = scales::percent, expand=c(0,0)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="none") + 
  scale_size_area(max_size=5)
search_plot


# plotting officers who seem to have a definite bias
# say, search rate is 1.1 more for minorities
data_highsearchrate <- data_for_plot[which(data_for_plot$search_rate.y/data_for_plot$search_rate.x > 3),]
max_val_hitrate = max(data_highsearchrate$hitrate) * 1.05
hit_plot = ggplot(data_highsearchrate) + 
  # specify data we want to plot
  geom_point(aes(x = hit_rate.x, y = hit_rate.y, size = n_stops.y)) + 
  # make one subplot for each minority race group
  facet_grid(.~driver_race.y) + 
  # add a diagonal line to indicate parity
  geom_abline(slope = 1, intercept = 0, linetype='dashed') +   
  scale_x_continuous('White hit rate', limits=c(0, max_val), labels = scales::percent, expand=c(0,0)) + 
  scale_y_continuous('Minority hit rate', limits=c(0, max_val), labels = scales::percent, expand=c(0,0)) + 
  theme_bw(base_size=15) + 
  theme(legend.position="none") + 
  scale_size_area(max_size=5)
hit_plot

glm.1 <- glm(search_conducted ~ driver_gender + driver_age + driver_race + stop_outcome + 
           county_name, data=data, family=binomial())
glm.2 <- glm(search_conducted ~ driver_gender + driver_age + driver_race + county_name, 
             data=data, family=binomial())
summary(glm.2)



# no idea how useful this glm is...
data$Arrest <- data$stop_outcome == "Arrest"
glm.3 <- glm(Arrest ~ driver_gender + driver_age + driver_race + county_name, 
             data=data, family=binomial())
summary(glm.3)


data$driver_race.f <- factor(data$driver_race)
levels(data$driver_race.f) # "Asian"    "Black"    "Hispanic" "White" 
data$driver_race.f <- factor(data$driver_race.f,levels(data$driver_race.f)[c(4,1:3)]) # "White"    "Asian"    "Black"    "Hispanic"


# https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
# just speeding
# race factor: Asian, Black, Hispanic, White
data_speeding <- data[which(data$violation == "Speeding"),]
multi.speeding <- multinom(stop_outcome ~ driver_gender + driver_age + driver_race + county_name, 
                           data=data_speeding)
summary(multi.speeding)
z_multi.speeding <- summary(multi.speeding)$coefficients/summary(multi.speeding)$standard.errors
p_multi.speeding <- (1 - pnorm(abs(z_multi.speeding), 0, 1)) * 2

# includes speeding
data_hasSpeeding <- data[which(grepl("Speeding",data$violation)),]
multi.hasSpeeding <- multinom(stop_outcome ~ driver_gender + driver_age + driver_race + county_name, 
                           data=data_hasSpeeding) 
summary(multi.hasSpeeding)
z_multi.hasSpeeding <- summary(multi.hasSpeeding)$coefficients/summary(multi.hasSpeeding)$standard.errors
p_multi.hasSpeeding <- (1 - pnorm(abs(z_multi.hasSpeeding), 0, 1)) * 2



multi.speeding_nocounty <- multinom(stop_outcome ~ driver_gender + driver_age + driver_race.f, 
                           data=data_speeding)
summary(multi.speeding_nocounty)
z_multi.speeding_nocounty <- summary(multi.speeding_nocounty)$coefficients/summary(multi.speeding_nocounty)$standard.errors
p_multi.speeding_nocounty <- (1 - pnorm(abs(z_multi.speeding_nocounty), 0, 1)) * 2













multi.speeding_race <- multinom(stop_outcome ~ driver_race.f, data=data_speeding)
summary(multi.speeding_race)
z_multi.speeding_race <- summary(multi.speeding_race)$coefficients/summary(multi.speeding_race)$standard.errors
p_multi.speeding_race <- (1 - pnorm(abs(z_multi.speeding_race), 0, 1)) * 2

data_ticket_verbal <- data_speeding[which(data_speeding$stop_outcome == "Ticket" | data_speeding$stop_outcome == "Verbal Warning"),]
data_ticket_verbal$stop_outcome <- factor(data_ticket_verbal$stop_outcome, levels=c("Verbal Warning", "Ticket"))
data_ticket_verbal$driver_race <- factor(data_ticket_verbal$driver_race, levels=c("White", "Asian", "Hispanic", "Black"))
glm.ticket_verbal <- glm(stop_outcome ~ driver_race, 
                         data=data_ticket_verbal,
                         family=binomial())
summary(glm.ticket_verbal)

data_summons_verbal <- data_speeding[which(data_speeding$stop_outcome == "Summons" | data_speeding$stop_outcome == "Verbal Warning"),]
data_summons_verbal$stop_outcome <- factor(data_summons_verbal$stop_outcome, levels=c("Verbal Warning", "Summons"))
data_summons_verbal$driver_race <- factor(data_summons_verbal$driver_race, levels=c("White", "Asian", "Hispanic", "Black"))
glm.summons_verbal <- glm(stop_outcome ~ driver_race, 
                         data=data_summons_verbal,
                         family=binomial())
summary(glm.summons_verbal)

data_summons_ticket <- data_speeding[which(data_speeding$stop_outcome == "Summons" | data_speeding$stop_outcome == "Ticket"),]
data_summons_ticket$stop_outcome <- factor(data_summons_ticket$stop_outcome, levels=c("Ticket", "Summons"))
data_summons_ticket$driver_race <- factor(data_summons_ticket$driver_race, levels=c("White", "Asian", "Hispanic", "Black"))
glm.summons_ticket <- glm(stop_outcome ~ driver_race, 
                          data=data_summons_ticket,
                          family=binomial())
summary(glm.summons_ticket)









glm.5 <- glm(search_conducted ~ driver_gender + driver_age + relevel(factor(data$driver_race), ref=4) + county_name, 
             data=data, family=binomial())
summary(glm.5)

glm.6 <- glm(search_conducted ~ driver_gender + driver_age + relevel(factor(data$driver_race), ref=4), 
             data=data, family=binomial())
summary(glm.6)






# get baseline number for each country
# clean country population data
countydata1 <- read.csv("countydata1.csv", as.is=TRUE)
countydata2 <- read.csv("countydata2.csv", as.is=TRUE)
countydata1 <- countydata1[,c(3,5,7,9,11,13)]
countydata2 <- countydata2[,c(3,5)]
countydata <- cbind(countydata1,countydata2)
countydata <- countydata[c(1,20,14,16,19,18),]
row.names(countydata) <- c("Population", "White", "Black", "Asian", "Hispanic", "Multi")
colnames(countydata) <- c("Hartford", "Middlesex", "Litchfield", "Tolland", "Fairfield", "NewHaven", "Windham", "NewLondon")
countydata <- t(countydata)
countydata <- gsub("%|,", "", countydata)
countydata <- as.data.frame(countydata)

as.numeric.factor <- function(x) {
  as.numeric(levels(x))[x]
}
countydata[,1:6] <- lapply(countydata[,1:6], as.numeric.factor)

for (i in 1:nrow(countydata)){
  other <- 100 - countydata$White[i] - countydata$Black[i] - countydata$Asian[i] - countydata$Hispanic[i] + countydata$Multi[i]
  countydata$Other[i] <- other
}




