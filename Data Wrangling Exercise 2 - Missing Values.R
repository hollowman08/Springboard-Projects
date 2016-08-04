suppressMessages(library(dplyr))
suppressMessages(library(data.table))
library(tidyr)

##Load titanic_original dataset
titanic <- fread("titanic_original.csv")

##Explore dataset
head(titanic)
tail(titanic)
summary(titanic)

##Replace blank cells in embarked with "S"
titanic_embarked <- which(titanic$embarked == "")
titanic$embarked[titanic_embarked] <- "S"

##Replace NA values in age with mean age
titanic_age <- which(is.na(titanic$age))
titanic$age[titanic_age] <- round(mean(titanic$age, na.rm = TRUE),0)
summary(titanic)
titanic$age <- as.numeric(titanic$age)

##Replace blank cells in lifeboat with "NA"
titanic_boat <- which(titanic$boat == "")
titanic$boat[titanic_boat] <- "NA"

##Add new column has_cabin_number
titanic1 <- titanic %>% 
     mutate(has_cabin_number = 0)

##Add 1 to rows that have cabin numbers
titanic_has_cabin <- which(!titanic1$cabin == "")
titanic1$has_cabin_number[titanic_has_cabin] <- 1

write.csv(titanic1, "titanic_clean.csv")
