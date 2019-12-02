set.seed(67982193)
path <- "/home/dnivog/PaaniLoo/STAT 841/Project/speed-dating/speeddating.csv"

## Read the data as character data frame
speed_dating <- read.csv(path, colClasses=c(rep("character",123)), na.string = "?")

# Remove has_null and wave fields
# Remove the fields which are bucketed nominal values for the numeric fields
remove_fields <- c("has_null","wave","d_d_age","d_importance_same_race", "d_importance_same_religion",
                   "d_pref_o_attractive", "d_pref_o_sincere", "d_pref_o_intelligence", "d_pref_o_funny", "d_pref_o_ambitious", "d_pref_o_shared_interests",
                   "d_attractive_o", "d_sincere_o", "d_intelligence_o", "d_funny_o", "d_ambitious_o", "d_shared_interests_o",
                   "d_attractive_important", "d_sincere_important", "d_intelligence_important", "d_funny_important", "d_ambitious_important", "d_shared_interests_important",
                   "d_attractive", "d_sincere", "d_intelligence", "d_funny", "d_ambitious", "d_shared_interests",
                   "d_attractive_partner", "d_sincere_partner", "d_intelligence_partner", "d_funny_partner", "d_ambitious_partner", "d_shared_interests_partner",
                   "d_sports",	"d_tvsports",	"d_exercise",	"d_dining",	"d_museums",	"d_art",	"d_hiking",	"d_gaming",	"d_clubbing",	"d_reading",
                   "d_tv",	"d_theater",	"d_movies",	"d_concerts",	"d_music",	"d_shopping",	"d_yoga",
                   "d_interests_correlate", "d_expected_happy_with_sd_people",	"d_expected_num_interested_in_me",	"d_expected_num_matches",	"d_like",	"d_guess_prob_liked")
speed_dating <- speed_dating[ , !(names(speed_dating) %in% remove_fields)]

## Convert the data fields to factor and numeric as needed
speed_dating[,1]<-as.factor(speed_dating[,1])
speed_dating[,5]<-as.factor(speed_dating[,5])
speed_dating[,6]<-as.factor(speed_dating[,6])
speed_dating[,7]<-as.factor(speed_dating[,7])
speed_dating[,10]<-as.factor(speed_dating[,10])
speed_dating[,64]<-as.factor(speed_dating[,64])
speed_dating[,65]<-as.factor(speed_dating[,65])
speed_dating[,66]<-as.factor(speed_dating[,66])
speed_dating[,c(2:4,8,9,11:63)]<-as.numeric(unlist(speed_dating[,c(2:4,8,9,11:63)]))

## Deal with NA values

# Get the mode for factor predictors
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Replace NA value with mean for numeric
for(i in c(2:4,8,9,11:63)) {
  speed_dating[ , i][is.na(speed_dating[ , i])] <- mean(speed_dating[ , i], na.rm = TRUE)
}

# Replace NA values with mode for factor
for(i in c(1,5:7,10,64:66)) {
  speed_dating[ , i][is.na(speed_dating[ , i])] <- getmode(speed_dating[ , i])
}

# Check if anymore NA values exist
na_count <-sapply(speed_dating, function(y) sum(length(which(is.na(y)))))
(na_count <- data.frame(na_count))

library(fmsb)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(grid)
library(gridBase)
library(scales)
library(ggplot2)

# kisko kya pasand hai
d1 <- select(speed_dating, gender, ends_with("_important")) %>% select(-starts_with(("d_")))
d1[, c(2:7)] <- as.numeric(unlist(d1[, c(2:7)]))
d2 <-  
  d1 %>% 
  group_by(gender) %>%
  summarise(
    Attractive = mean(attractive_important),
    Sincere = mean(sincere_important),
    Intelligent = mean(intelligence_important),
    Funny = mean(funny_important),
    Ambitious = mean(ambitious_important),
    Same_int = mean(shared_interests_important)
  )

d3 <- data.frame(
  quality = names(d2[,-1]),
  values = unlist(d2[1,-1], use.names=FALSE),
  gender = rep("female", 6)
)
d4 <- data.frame(
  quality = names(d2[,-1]),
  values = unlist(d2[2,-1], use.names=FALSE),
  gender = rep("male", 6)
)

d6 <- rbind(d3, d4)
d6

ggplot(d6, aes(fill=gender, y=values, x=quality)) + 
  geom_bar(position="dodge", stat="identity")+
coord_flip()


# Rating given to oneself vs rating given by partner for males and females
ratings_by_partner <- select(speed_dating, gender, sincere_o, intelligence_o, 
                             attractive_o, funny_o, ambitious_o)
ratings_by_self <- select(speed_dating, gender, sincere, intelligence,
                          attractive, funny, ambitious)

names(ratings_by_partner) <- names(ratings_by_self)


head(ratings_by_partner)
mean(ratings_by_partner[, "attractive"])

average_ratings_by_partner <- ratings_by_partner %>% 
  group_by(gender) %>%
  summarise(
    Attractive = mean(attractive),
    Sincere = mean(sincere),
    Intelligent = mean(intelligence),
    Funny = mean(funny),
    Ambitious = mean(ambitious)
  )

average_ratings_by_self <- ratings_by_self %>% 
  group_by(gender) %>%
  summarise(
    Attractive = mean(attractive),
    Sincere = mean(sincere),
    Intelligent = mean(intelligence),
    Funny = mean(funny),
    Ambitious = mean(ambitious)
  )
average_ratings_by_partner

ratings_male_partner <- data.frame(
)
for (row in 2:ncol(average_ratings_by_partner)){
  ratings_male_partner[row-1, "quality"] <- names(average_ratings_by_partner)[row]
  ratings_male_partner[row-1, "value"] <- average_ratings_by_partner[2, row]
  ratings_male_partner[row-1, "rater"] <- "partner"
}

ratings_male_self <- data.frame(
)
for (row in 2:ncol(average_ratings_by_self)){
  ratings_male_self[row-1, "quality"] <- names(average_ratings_by_self)[row]
  ratings_male_self[row-1, "value"] <- average_ratings_by_self[2, row]
  ratings_male_self[row-1, "rater"] <- "self"
}
ratings_male <- rbind(ratings_male_partner, ratings_male_self)

ratings_female_partner <- data.frame(
)
for (row in 2:ncol(average_ratings_by_partner)){
  ratings_female_partner[row-1, "quality"] <- names(average_ratings_by_partner)[row]
  ratings_female_partner[row-1, "value"] <- average_ratings_by_partner[1, row]
  ratings_female_partner[row-1, "rater"] <- "partner"
}

ratings_female_self <- data.frame(
)
for (row in 2:ncol(average_ratings_by_self)){
  ratings_female_self[row-1, "quality"] <- names(average_ratings_by_self)[row]
  ratings_female_self[row-1, "value"] <- average_ratings_by_self[1, row]
  ratings_female_self[row-1, "rater"] <- "self"
}
ratings_female <- rbind(ratings_female_partner, ratings_female_self)

ggplot(ratings_male, aes(fill=rater, y=value, x=quality)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip()

ggplot(ratings_female, aes(fill=rater, y=value, x=quality)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip()

# Acceptance Rejection rate
male <- 0
female <- 0
male_said_yes <- 0
male_partner_said_yes <- 0
female_said_yes <- 0
female_partner_said_yes <- 0

male_said_no <- 0
male_partner_said_no <- 0
female_said_no <- 0
female_partner_said_no <- 0

for (row in 1:nrow(speed_dating)){
  if (speed_dating$gender[row] == "female"){
    female <- female + 1
    if (speed_dating$decision[row] == 1){
      female_said_yes <- female_said_yes + 1 
    }
    else {
      female_said_no <- female_said_no + 1
    }
    if (speed_dating$decision_o[row] == 1){
      female_partner_said_yes <- female_partner_said_yes + 1
    }
    else {
      female_partner_said_no <- female_partner_said_no + 1
    }
  }
  else {
    male <- male + 1
    if (speed_dating$decision[row] == 1){
      male_said_yes <- male_said_yes + 1 
    }
    else {
      male_said_no <- male_said_no + 1
    }
    if (speed_dating$decision_o[row] == 1){
      male_partner_said_yes <- male_partner_said_yes + 1
    }
    else {
      male_partner_said_no <- male_partner_said_no + 1
    }
  }
}

dd <- data.frame(
  gender = c("male", "male", "female", "female"),
  value = c(45, 55, 35, 65),
  type = c("said_yes", "said_no", "said_yes", "said_no")
)

# no yes rate for male and females
ggplot(dd, aes(fill=type, y=value, x=gender)) + 
  geom_bar(position="fill", stat="identity") 

# optimism vs pessisim in men and women
male_optimism <- 0
male_pessimism <- 0
male_realism <- 0
female_optimism <- 0
female_pessimism <- 0
female_realism <- 0

for (row in 1:nrow(speed_dating)){
  if (speed_dating$gender[row] == "female"){
    if (speed_dating$decision[row] == 1 && speed_dating$decision_o[row] == 0){
      female_optimism <- female_optimism + 1
    }
    if (speed_dating$decision[row] == 0 && speed_dating$decision_o[row] == 1){
      female_pessimism <- female_pessimism + 1
    }
    if (speed_dating$decision[row] == speed_dating$decision_o[row]){
      female_realism <- female_realism + 1
    }
  }
  else {
    if (speed_dating$decision[row] == 1 && speed_dating$decision_o[row] == 0){
      male_optimism <- male_optimism + 1
    }
    if (speed_dating$decision[row] == 0 && speed_dating$decision_o[row] == 1){
      male_pessimism <- male_pessimism + 1
    }
    if (speed_dating$decision[row] == speed_dating$decision_o[row]){
      male_realism <- male_realism + 1
    }
  }
}