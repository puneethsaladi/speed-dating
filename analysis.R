set.seed(67982193)
speed <- read.csv("/home/dnivog/PaaniLoo/STAT 841/Project/speeddating.csv")
summary(speed)
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
d1 <- select(speed, gender, ends_with("_important")) %>% select(-starts_with(("d_")))
d1[, c(2:7)] <- as.numeric(unlist(d1[, c(2:7)]))
d2 <-  
  d1 %>% 
  group_by(gender) %>%
  summarise(
    Attractive = mean(attractive_important),
    Sincere = mean(sincere_important),
    Intelligent = mean(intellicence_important),
    Funny = mean(funny_important),
    Ambitious = mean(ambtition_important),
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
ratings_by_partner <- select(speed, gender, ends_with("_o")) %>% select(-starts_with(("pref_"))) %>% select(-starts_with(("d_")))
names(d1)
ratings_by_self <- select(speed, gender, sinsere)