set.seed(67982193)

## Read the data as character data frame
speed_dating <- read.csv("path\\to\\speeddating.csv",colClasses=c(rep("character",123)),na.string = "?")

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
