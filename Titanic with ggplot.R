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

##Replace blank cells in lifeboat with "NA"
titanic_boat <- which(titanic$boat == "")
titanic$boat[titanic_boat] <- "NA"

##Add new column has_cabin_number
titanic$has_cabin_number <- ifelse(titanic$cabin == "", 0, 1)

##Add 1 to rows that have cabin numbers

##Adding Data Visualization
# Use ggplot() for the first instruction
library(ggplot2)
titanic_sex <- which(titanic$sex != "")
titanic$sex <- titanic$sex[titanic_sex]
ggplot(titanic, aes(x = factor(pclass), fill = factor(sex))) +
     geom_bar(position = "dodge")


# Use ggplot() for the second instruction
ggplot(titanic, aes(x = factor(pclass), fill = factor(sex))) +
     geom_bar(position = "dodge") + 
     facet_grid(". ~ survived")

# Position jitter (use below)
posn.j <- position_jitter(0.5, 0)

# Use ggplot() for the last instruction
ggplot(titanic, aes(x = factor(pclass), y = age, col = factor(sex))) +
     geom_jitter(size = 3, aes(alpha = 0.5), position = posn.j) + 
     facet_grid(". ~ survived")



write.csv(titanic, "titanic_clean.csv")
