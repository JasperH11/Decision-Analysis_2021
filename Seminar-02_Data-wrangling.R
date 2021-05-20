# Seminar 02 - Data wrangling 

library(tidyverse)
browseVignettes("tidyverse")

# Section 1: Follow along ----

library(readr)

# Load file 
urlfile = "https://raw.githubusercontent.com/CWWhitney/teaching_R/master/participants_data.csv"
participants_data <- read_csv(url(urlfile))

# Show first six rows
head(participants_data)

# Column names 
names(participants_data)

# Structure
str(participants_data)

# Show each entry of variable 'age'
participants_data$age



# Section 2: Task on 'diamonds' data set ----
library(ggplot2)


# Task 1: Select carant and price
diamonds$carat
diamonds$price


# Task 2: Filter carat > 0.5
filter(diamonds, carat > 0.5)


# Task 3: Rename 'price' as 'cost'
diamonds <- rename(diamonds, cost = price)
names(diamonds)

object <- 
  
  # Task 4: Name 'expensive' if price > mean of price, if otherwise name 'cheap'
  diamonds <- mutate(diamonds, category = ifelse(cost<(mean(cost)),"expensive", "cheap"))


# Task 5: split into cheap and expensive using group_by()
group_by(diamonds, category)


# Task 6: summarize()
summarize(diamonds, mean(carat), sum(cost)) 
summary(diamonds)

