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
speed_dating$field <- tolower(speed_dating$field)

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
library(gridExtra)
library(grid)
library(reshape2)

table(speed_dating$field)
#age distribution
ggplot(speed_dating, aes(fill=gender, age)) + 
  geom_histogram()

nrow(speed_dating[speed_dating$gender=="female",])

# mean age by gender
mean_age_male <- mean(speed_dating[speed_dating$gender == "male", ]$age)
mean_age_female <- mean(speed_dating[speed_dating$gender == "female", ]$age)
dd <- data.frame(
  mean_age = c(mean_age_male, mean_age_female),
  gender = c("male", "female")
)

ggplot(dd, aes(fill=gender, x=gender, y=mean_age)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(data=dd,aes(label=round(mean_age, digits = 2)),vjust=-0.2)

summary(speed_dating$race)
# mean age by race
mean_age_ECA     <-  mean(speed_dating[speed_dating$race == "European/Caucasian-American",]$age)
mean_age_Asian   <-  mean(speed_dating[speed_dating$race == "'Asian/Pacific Islander/Asian-American'",]$age)
mean_age_Black   <-  mean(speed_dating[speed_dating$race == "'Black/African American'",]$age)
mean_age_Latino  <-  mean(speed_dating[speed_dating$race == "'Latino/Hispanic American'",]$age)
mean_age_Other   <-  mean(speed_dating[speed_dating$race == "Other",]$age)

dd <- data.frame(
  race = c(
    "White",
    "Asian",
    "Black",
    "Latino",
    "Other"
  ),
  mean_age = c(
    mean_age_ECA,  
    mean_age_Asian,
    mean_age_Black, 
    mean_age_Latino,
    mean_age_Other 
  )
)

ggplot(dd, aes(fill=race, x=race, y=mean_age)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(data=dd,aes(label=round(mean_age, digits = 2)),vjust=-0.2)

# age by race and gender
male_mean_age_ECA     <-  mean(speed_dating[speed_dating$gender == "male" & speed_dating$race == "European/Caucasian-American",]$age)
male_mean_age_Asian   <-  mean(speed_dating[speed_dating$gender == "male" & speed_dating$race == "'Asian/Pacific Islander/Asian-American'",]$age)
male_mean_age_Black   <-  mean(speed_dating[speed_dating$gender == "male" & speed_dating$race == "'Black/African American'",]$age)
male_mean_age_Latino  <-  mean(speed_dating[speed_dating$gender == "male" & speed_dating$race == "'Latino/Hispanic American'",]$age)
male_mean_age_Other   <-  mean(speed_dating[speed_dating$gender == "male" & speed_dating$race == "Other",]$age)

dd <- data.frame(
  race = c(
    "White",
    "Asian",
    "Black",
    "Latino",
    "Other"
  ),
  mean_age = c(
    male_mean_age_ECA,  
    male_mean_age_Asian,
    male_mean_age_Black, 
    male_mean_age_Latino,
    male_mean_age_Other 
  )
)

ggplot(dd, aes(fill=race, x=race, y=mean_age)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(data=dd,aes(label=round(mean_age, digits = 2)),vjust=-0.2)+
  ggtitle("Age distribution across different races for men")

female_mean_age_ECA     <-  mean(speed_dating[speed_dating$gender == "female" & speed_dating$race == "European/Caucasian-American",]$age)
female_mean_age_Asian   <-  mean(speed_dating[speed_dating$gender == "female" & speed_dating$race == "'Asian/Pacific Islander/Asian-American'",]$age)
female_mean_age_Black   <-  mean(speed_dating[speed_dating$gender == "female" & speed_dating$race == "'Black/African American'",]$age)
female_mean_age_Latino  <-  mean(speed_dating[speed_dating$gender == "female" & speed_dating$race == "'Latino/Hispanic American'",]$age)
female_mean_age_Other   <-  mean(speed_dating[speed_dating$gender == "female" & speed_dating$race == "Other",]$age)

dd <- data.frame(
  race = c(
    "White",
    "Asian",
    "Black",
    "Latino",
    "Other"
  ),
  mean_age = c(
    female_mean_age_ECA,  
    female_mean_age_Asian,
    female_mean_age_Black, 
    female_mean_age_Latino,
    female_mean_age_Other 
  )
)

ggplot(dd, aes(fill=race, x=race, y=mean_age)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(data=dd,aes(label=round(mean_age, digits = 2)),vjust=-0.2)


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

# kisko kya pasand hai race wise
summary(speed_dating$race)
"European/Caucasian-American",
"Asian/Pacific Islander/Asian-American",
"Black/African American",
"Latino/Hispanic American",
"Other"
dd <- data.frame(
  quality = c(),
  value = c(),
  race = c()
)

for (r in c("European/Caucasian-American",
            "'Asian/Pacific Islander/Asian-American'",
            "'Black/African American'",
            "'Latino/Hispanic American'",
            "Other")){
  d1 <- summarise(
    speed_dating[speed_dating$race == r, ],
    Attractive = mean(attractive_important),
    Sincere = mean(sincere_important),
    Intelligent = mean(intelligence_important),
    Funny = mean(funny_important),
    Ambitious = mean(ambitious_important),
    Same_int = mean(shared_interests_important)
  )
  d2 <- data.frame(
    quality = names(d1),
    value = unlist(d1[1,], use.names=FALSE),
    race = rep(r, 6)
  )
  dd <- rbind(dd, d2)
}
dd

ggplot(dd, aes(fill=race, y=value, x=quality)) + 
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
  value = c(male_said_yes/male, male_said_no/male, 
            female_said_yes/female, female_said_no/female),
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

dd <- data.frame(
  type = c("optimism", "optimism", "pessisism", "pessisism"),
  value = c(male_optimism/male, female_optimism/female, 
            male_pessimism/male, female_pessimism/female),
  gender = c("male", "female", "male", "female")
)

# no yes rate for male and females
ggplot(dd, aes(fill=gender, y=value, x=type)) + 
  geom_bar(position="fill", stat="identity") 

# most tharki race
ECA <- sum(speed_dating$race == "European/Caucasian-American")
Asian <- sum(speed_dating$race == "'Asian/Pacific Islander/Asian-American'")
Black <- sum(speed_dating$race == "'Black/African American'")
Latino <- sum(speed_dating$race == "'Latino/Hispanic American'")
Other <- sum(speed_dating$race == "Other")

ECA_said_yes <- nrow(speed_dating[speed_dating$race == "European/Caucasian-American" & speed_dating$decision == 1,])
Asian_said_yes <- nrow(speed_dating[speed_dating$race == "'Asian/Pacific Islander/Asian-American'" & speed_dating$decision == 1,])
Black_said_yes <- nrow(speed_dating[speed_dating$race == "'Black/African American'" & speed_dating$decision == 1,])
Latino_said_yes <- nrow(speed_dating[speed_dating$race == "'Latino/Hispanic American'" & speed_dating$decision == 1,])
Other_said_yes <- nrow(speed_dating[speed_dating$race == "Other" & speed_dating$decision == 1,])

dd <- data.frame(
  race = c(
    "European/Caucasian-American",
     "Asian/Pacific Islander/Asian-American",
     "Black/African American",
     "Latino/Hispanic American",
     "Other"
    ),
  yes_percentage = c(
    ECA_said_yes/ECA*100,
    Asian_said_yes/Asian*100,
    Black_said_yes/Black*100,
    Latino_said_yes/Latino*100,
    Other_said_yes/Other*100
  )
)

ggplot(dd, aes(fill=race, y=yes_percentage, x=race)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x=element_blank()) 

# most tharki race by gender

ECA_male <- nrow(speed_dating[speed_dating$race == "European/Caucasian-American" & speed_dating$gender == "male",])
Asian_male <- nrow(speed_dating[speed_dating$race == "'Asian/Pacific Islander/Asian-American'" & speed_dating$gender == "male",])
Black_male <- nrow(speed_dating[speed_dating$race == "'Black/African American'" & speed_dating$gender == "male",])
Latino_male <- nrow(speed_dating[speed_dating$race == "'Latino/Hispanic American'" & speed_dating$gender == "male",])
Other_male <- nrow(speed_dating[speed_dating$race == "Other" & speed_dating$gender == "male",])

ECA_male_said_yes <- nrow(speed_dating[speed_dating$race == "European/Caucasian-American" & speed_dating$gender == "male" & speed_dating$decision == 1,])
Asian_male_said_yes <- nrow(speed_dating[speed_dating$race == "'Asian/Pacific Islander/Asian-American'" & speed_dating$gender == "male" & speed_dating$decision == 1,])
Black_male_said_yes <- nrow(speed_dating[speed_dating$race == "'Black/African American'" & speed_dating$gender == "male" & speed_dating$decision == 1,])
Latino_male_said_yes <- nrow(speed_dating[speed_dating$race == "'Latino/Hispanic American'" & speed_dating$gender == "male" & speed_dating$decision == 1,])
Other_male_said_yes <- nrow(speed_dating[speed_dating$race == "Other" & speed_dating$gender == "male" & speed_dating$decision == 1,])

dd <- data.frame(
  race = c(
    "Male European/Caucasian-American",
    "Male Asian/Pacific Islander/Asian-American",
    "Male Black/African American",
    "Male Latino/Hispanic American",
    "Male Other"
  ),
  yes_percentage = c(
    ECA_male_said_yes/ECA_male*100,
    Asian_male_said_yes/Asian_male*100,
    Black_male_said_yes/Black_male*100,
    Latino_male_said_yes/Latino_male*100,
    Other_male_said_yes/Other_male*100
  )
)
dd
ggplot(dd, aes(fill=race, y=yes_percentage, x=race)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x=element_blank()) 


ECA_female <- nrow(speed_dating[speed_dating$race == "European/Caucasian-American" & speed_dating$gender == "female",])
Asian_female <- nrow(speed_dating[speed_dating$race == "'Asian/Pacific Islander/Asian-American'" & speed_dating$gender == "female",])
Black_female <- nrow(speed_dating[speed_dating$race == "'Black/African American'" & speed_dating$gender == "female",])
Latino_female <- nrow(speed_dating[speed_dating$race == "'Latino/Hispanic American'" & speed_dating$gender == "female",])
Other_female <- nrow(speed_dating[speed_dating$race == "Other" & speed_dating$gender == "female",])

ECA_female_said_yes <- nrow(speed_dating[speed_dating$race == "European/Caucasian-American" & speed_dating$gender == "female" & speed_dating$decision == 1,])
Asian_female_said_yes <- nrow(speed_dating[speed_dating$race == "'Asian/Pacific Islander/Asian-American'" & speed_dating$gender == "female" & speed_dating$decision == 1,])
Black_female_said_yes <- nrow(speed_dating[speed_dating$race == "'Black/African American'" & speed_dating$gender == "female" & speed_dating$decision == 1,])
Latino_female_said_yes <- nrow(speed_dating[speed_dating$race == "'Latino/Hispanic American'" & speed_dating$gender == "female" & speed_dating$decision == 1,])
Other_female_said_yes <- nrow(speed_dating[speed_dating$race == "Other" & speed_dating$gender == "female" & speed_dating$decision == 1,])

dd <- data.frame(
  race = c(
    "Female European/Caucasian-American",
    "Female Asian/Pacific Islander/Asian-American",
    "Female Black/African American",
    "Female Latino/Hispanic American",
    "Female Other"
  ),
  yes_percentage = c(
    ECA_female_said_yes/ECA_male*100,
    Asian_female_said_yes/Asian_female*100,
    Black_female_said_yes/Black_female*100,
    Latino_female_said_yes/Latino_female*100,
    Other_female_said_yes/Other_female*100
  )
)
dd
ggplot(dd, aes(fill=race, y=yes_percentage, x=race)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x=element_blank()) 

# acceptance with age group
age1 <- nrow(speed_dating[speed_dating$age < 20,])
age2 <- nrow(speed_dating[speed_dating$age >= 20 & speed_dating$age < 25,])
age3 <- nrow(speed_dating[speed_dating$age >= 25 & speed_dating$age < 30,])
age4 <- nrow(speed_dating[speed_dating$age >= 30 & speed_dating$age < 35,])
age5 <- nrow(speed_dating[speed_dating$age >= 35 & speed_dating$age < 40,])
age6 <- nrow(speed_dating[speed_dating$age >= 40,])

age1_said_yes <- nrow(speed_dating[speed_dating$age < 20 & speed_dating$decision == 1,])
age2_said_yes <- nrow(speed_dating[speed_dating$age >= 20 & speed_dating$age < 25 & speed_dating$decision == 1,])
age3_said_yes <- nrow(speed_dating[speed_dating$age >= 25 & speed_dating$age < 30 & speed_dating$decision == 1,])
age4_said_yes <- nrow(speed_dating[speed_dating$age >= 30 & speed_dating$age < 35 & speed_dating$decision == 1,])
age5_said_yes <- nrow(speed_dating[speed_dating$age >= 35 & speed_dating$age < 40 & speed_dating$decision == 1,])
age6_said_yes <- nrow(speed_dating[speed_dating$age >= 40 & speed_dating$decision == 1,])

dd <- data.frame(
  age_group = c("<20", "20-25", "25-30", "30-35", "35-40", ">40"),
  yes_percentage = c(
    age1/age1_said_yes*100,
    age2/age2_said_yes*100,
    age3/age3_said_yes*100,
    age4/age4_said_yes*100,
    age5/age5_said_yes*100,
    age6/age6_said_yes*100
  )
)

ggplot(dd, aes(fill=age_group, y=yes_percentage, x=age_group)) + 
  geom_bar(position="dodge", stat="identity")

# acceptance with age group for men
age1_male <- nrow(speed_dating[speed_dating$age < 20 & speed_dating$gender == "male",])
age2_male <- nrow(speed_dating[speed_dating$age >= 20 & speed_dating$age < 25 & speed_dating$gender == "male",])
age3_male <- nrow(speed_dating[speed_dating$age >= 25 & speed_dating$age < 30 & speed_dating$gender == "male",])
age4_male <- nrow(speed_dating[speed_dating$age >= 30 & speed_dating$age < 35 & speed_dating$gender == "male",])
age5_male <- nrow(speed_dating[speed_dating$age >= 35 & speed_dating$age < 40 & speed_dating$gender == "male",])
age6_male <- nrow(speed_dating[speed_dating$age >= 40 & speed_dating$gender == "male",])

age1_male_said_yes <- nrow(speed_dating[speed_dating$age < 20 & speed_dating$decision == 1 & speed_dating$gender == "male",])
age2_male_said_yes <- nrow(speed_dating[speed_dating$age >= 20 & speed_dating$age < 25 & speed_dating$decision == 1 & speed_dating$gender == "male",])
age3_male_said_yes <- nrow(speed_dating[speed_dating$age >= 25 & speed_dating$age < 30 & speed_dating$decision == 1 & speed_dating$gender == "male",])
age4_male_said_yes <- nrow(speed_dating[speed_dating$age >= 30 & speed_dating$age < 35 & speed_dating$decision == 1 & speed_dating$gender == "male",])
age5_male_said_yes <- nrow(speed_dating[speed_dating$age >= 35 & speed_dating$age < 40 & speed_dating$decision == 1 & speed_dating$gender == "male",])
age6_male_said_yes <- nrow(speed_dating[speed_dating$age >= 40 & speed_dating$decision == 1 & speed_dating$gender == "male",])

dd <- data.frame(
  age_group = c("<20", "20-25", "25-30", "30-35", "35-40", ">40"),
  yes_percentage = c(
    age1_male/age1_male_said_yes*100,
    age2_male/age2_male_said_yes*100,
    age3_male/age3_male_said_yes*100,
    age4_male/age4_male_said_yes*100,
    age5_male/age5_male_said_yes*100,
    age6_male/age6_male_said_yes*100
  )
)

ggplot(dd, aes(fill=age_group, y=yes_percentage, x=age_group)) + 
  geom_bar(position="dodge", stat="identity")

# acceptance with age group for women
age1_female <- nrow(speed_dating[speed_dating$age < 20 & speed_dating$gender == "female",])
age2_female <- nrow(speed_dating[speed_dating$age >= 20 & speed_dating$age < 25 & speed_dating$gender == "female",])
age3_female <- nrow(speed_dating[speed_dating$age >= 25 & speed_dating$age < 30 & speed_dating$gender == "female",])
age4_female <- nrow(speed_dating[speed_dating$age >= 30 & speed_dating$age < 35 & speed_dating$gender == "female",])
age5_female <- nrow(speed_dating[speed_dating$age >= 35 & speed_dating$age < 40 & speed_dating$gender == "female",])
age6_female <- nrow(speed_dating[speed_dating$age >= 40 & speed_dating$gender == "female",])

age1_female_said_yes <- nrow(speed_dating[speed_dating$age < 20 & speed_dating$decision == 1 & speed_dating$gender == "female",])
age2_female_said_yes <- nrow(speed_dating[speed_dating$age >= 20 & speed_dating$age < 25 & speed_dating$decision == 1 & speed_dating$gender == "female",])
age3_female_said_yes <- nrow(speed_dating[speed_dating$age >= 25 & speed_dating$age < 30 & speed_dating$decision == 1 & speed_dating$gender == "female",])
age4_female_said_yes <- nrow(speed_dating[speed_dating$age >= 30 & speed_dating$age < 35 & speed_dating$decision == 1 & speed_dating$gender == "female",])
age5_female_said_yes <- nrow(speed_dating[speed_dating$age >= 35 & speed_dating$age < 40 & speed_dating$decision == 1 & speed_dating$gender == "female",])
age6_female_said_yes <- nrow(speed_dating[speed_dating$age >= 40 & speed_dating$decision == 1 & speed_dating$gender == "female",])

dd <- data.frame(
  age_group = c("<20", "20-25", "25-30", "30-35", "35-40", ">40"),
  yes_percentage = c(
    age1_female/age1_female_said_yes*100,
    age2_female/age2_female_said_yes*100,
    age3_female/age3_female_said_yes*100,
    age4_female/age4_female_said_yes*100,
    age5_female/age5_female_said_yes*100,
    age6_female/age6_female_said_yes*100
  )
)

ggplot(dd, aes(fill=age_group, y=yes_percentage, x=age_group)) + 
  geom_bar(position="dodge", stat="identity")

# hobbies comparison

sports <-  ggplot(speed_dating, aes(sports)) +
  geom_density(aes(fill=gender), alpha=0.5) 

movies <-  ggplot(speed_dating, aes(movies)) +
  geom_density(aes(fill=gender), alpha=0.5) 

music <-  ggplot(speed_dating, aes(music)) +
  geom_density(aes(fill=gender), alpha=0.5) 

yoga <- ggplot(speed_dating, aes(yoga)) +
  geom_density(aes(fill=gender), alpha=0.5) 

shopping <-  ggplot(speed_dating, aes(shopping)) +
  geom_density(aes(fill=gender), alpha=0.5) 

gaming <- ggplot(speed_dating, aes(gaming)) +
  geom_density(aes(fill=gender), alpha=0.5) 

art <- ggplot(speed_dating, aes(art)) +
  geom_density(aes(fill=gender), alpha=0.5) 

dining <- ggplot(speed_dating, aes(dining)) +
  geom_density(aes(fill=gender), alpha=0.5) 

reading <- ggplot(speed_dating, aes(reading)) +
  geom_density(aes(fill=gender), alpha=0.5) 

grid.arrange(
  sports, movies, music,
  yoga, shopping, reading,
  gaming, art, dining,
  ncol=3, nrow=3,
  top = "Sira is sira"
)



# occupation analysis
speed_dating$field <- tolower(speed_dating$field)
occ <- count (speed_dating, "field")
sorted_occ <- occ[order(-occ$freq),]
top_occ <- head(sorted_occ, 10)

ggplot(top_occ, aes(x=field, y=freq)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# hobbies correlation
hobbies <- speed_dating[, 40:55]
cormat<-signif(cor(hobbies),2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color="white") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#music means concerts. And tv means tvsports. And art means muesuems etc/

hobbies_male <- speed_dating[speed_dating$gender == "male", 40:55]
cormat<-signif(cor(hobbies_male),2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color="white") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hobbies_male <- speed_dating[speed_dating$gender == "female", 40:55]
cormat<-signif(cor(hobbies_male),2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color="white") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


length(names(speed_dating))

dd <- data.frame(
  race = c(
    "White",
    "Asian",
    "Black",
    "Latino",
    "Other"
  ),
  mean_age = c(
    nrow(speed_dating[speed_dating$race[]])
  )
)
ggplot(speed_dating, aes(fill=race, x=race)) + 
  geom_histogram(stat="count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


occ <- count (speed_dating, "field")
sorted_occ <- occ[order(occ$freq, decreasing=F),]
top_occ <- head(sorted_occ, 10)

ggplot(top_occ, aes(x=field, y=freq)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Popular fields of work/study")

ggplot(speed_dating, aes(x=field)) + 
  geom_histogram(stat="count")+
  ggtitle("Popular fields of work/study")

length(unique(speed_dating$field))

age1_female <- nrow(speed_dating[speed_dating$age < 20 & speed_dating$gender == "female",])


#optimism vs pessisim correct vaala
male_optimism <- nrow(speed_dating[speed_dating$like>=5 & 
        speed_dating$decision_o == 0 &
          speed_dating$gender == "male",])
female_optimism <- nrow(speed_dating[speed_dating$like>=5 & 
        speed_dating$decision_o == 0 &
        speed_dating$gender == "female",])
male_pessimism <- nrow(speed_dating[speed_dating$like<5 & 
                                     speed_dating$decision_o == 1 &
                                     speed_dating$gender == "male",])
female_pessimism <- nrow(speed_dating[speed_dating$like<5 & 
                                       speed_dating$decision_o == 1 &
                                       speed_dating$gender == "female",])
male <- nrow(speed_dating[speed_dating$gender == "male",])
female <- nrow(speed_dating[speed_dating$gender == "female",])
male_realism <- male - male_optimism - male_pessimism
female_realism <- female - female_optimism - female_pessimism
dd <- data.frame(
  type = c("optimism", "optimism", "pessisism", "pessisism",
           "realism", "realism"),
  value = c(male_optimism/male, female_optimism/female, 
            male_pessimism/male, female_pessimism/female,
            male_realism/male, female_realism/female),
  gender = c("male", "female", "male", "female", "male", "female")
)

# no yes rate for male and females
ggplot(dd, aes(fill=gender, y=value, x=type)) + 
  geom_bar(position="fill", stat="identity") 

summary(speed_dating$gender)
dd <- data.frame(
  quality = c(),
  value = c(),
  gender = c()
)


# Rating given to oneself vs rating given by partner for males and females

ggplot(ratings_male, aes(fill=rater, y=value, x=quality)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip()+
  ggtitle("Ratings given by men to themselves vs partner rating")
