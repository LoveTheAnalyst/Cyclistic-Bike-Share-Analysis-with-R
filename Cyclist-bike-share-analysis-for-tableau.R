#1
#install and load packages
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("lubridate")
library(lubridate)
install.packages("ggplot2")
library(ggplot2)

#2
#import files... Each line represents each month
cyclist_2021_05 <- read_csv("cyc_05_2021.csv")
cyclist_2021_06 <- read_csv("cyc_06_2021.csv")
cyclist_2021_07 <- read_csv("cyc_07_2021.csv")
cyclist_2021_08 <- read_csv("cyc_08_2021.csv")
cyclist_2021_09 <- read_csv("cyc_09_2021.csv")
cyclist_2021_10 <- read_csv("cyc_10_2021.csv")
cyclist_2021_11 <- read_csv("cyc_11_2021.csv")
cyclist_2021_12 <- read_csv("cyc_12_2021.csv")
cyclist_2022_01 <- read_csv("cyc_01_2022.csv")
cyclist_2022_02 <- read_csv("cyc_02_2022.csv")
cyclist_2022_03 <- read_csv("cyc_03_2022.csv")
cyclist_2022_04 <- read_csv("cyc_04_2022.csv")


#3
#check the dataset
head(cyclist_2021_05)
str(cyclist_2021_05)

#4
#check column names December 2021 as an example
colnames(cyclist_2021_12)

#5
#combine the 12 months
year_cyclist <- rbind(cyclist_2021_05, cyclist_2021_06, cyclist_2021_07, cyclist_2021_08, cyclist_2021_09, 
                      cyclist_2021_10, cyclist_2021_11, cyclist_2021_12, cyclist_2022_01, cyclist_2022_02,
                      cyclist_2022_03, cyclist_2022_04)
View(year_cyclist)

#6
#check column names
colnames(year_cyclist)

#7
#convert ride_length data type to numeric
year_cyclist$ride_length <- as.numeric(year_cyclist$ride_length)

#8
#confirm the data type
class(year_cyclist$ride_length)
View(year_cyclist)

#9
#divide ride_length by 60 to convert to minutes
year_cyclist$ride_length <- (year_cyclist$ride_length)/60
View(year_cyclist)

#10
#statistic summary of all dataset
summary(year_cyclist)

#11
#statistic summary of the ride_length column
summary(year_cyclist$ride_length)


#TRENDS OBSERVED
#casual ride_plan have higher mean than member ride_plan

#12
#average ride_length for members and casual riders.
average_ride_length_for_riders <-  
  year_cyclist %>%
  group_by(ride_plan) %>%
  summarise(average_ride_length = mean(ride_length))
View(average_ride_length_for_riders)

#13
#maximum ride_length for members and casual riders.
max_ride_length_for_riders <-  
  year_cyclist %>%
  group_by(ride_plan) %>%
  summarise(max_ride_length = max(ride_length))
View(max_ride_length_for_riders)

#14
#minimum ride_length for members and casual riders.
min_ride_length_for_riders <-  
  year_cyclist %>%
  group_by(ride_plan) %>%
  summarise(min_ride_length = min(ride_length))
View(min_ride_length_for_riders)

#15
#number of rides for ALL users by day_of_week..... in tibble
count(year_cyclist)

#16
#average ride_length GROUP BY day_of_week and ride_plan
avg_ride_len_by_dow_plan<-
  year_cyclist %>%
  group_by(day_of_week, ride_plan) %>%
  summarise(average_ride_length = mean(ride_length))
View(avg_ride_len_by_dow_plan)


#17
#number of rides by day_of_week
no_of_riders_by_dow <-
  year_cyclist %>%
  group_by(day_of_week, ride_plan) %>%
  count()
View(no_of_riders_by_dow)


#18
#number of rides by ride type
no_of_riders_by_ride_type <-
  year_cyclist %>%
  group_by(rideable_type, ride_plan) %>%
  count()
View(no_of_riders_by_ride_type)


#19
#average ride group by rideable_type
avg_ride_by_rideable_type<-
  year_cyclist %>%
  group_by(rideable_type, ride_plan) %>%
  summarise(average_ride_length = mean(ride_length))
View(avg_ride_by_rideable_type)


#20
#number of users by ride_plan
no_of_users <- count(year_cyclist,ride_plan) 
View(no_of_users)




#-----------------------------------------------------------------------#

#visualizations


#N.B: Further analysis can be done to gain more insights and visualizations 

#1
#Title: Number of riders: Casual vs Member",
#subtitle = "From May 2021 to April 2022")

#2
#Title: Number of rides per Rideable Type"
#subtitle = "From May 2021 to April 2022")

#3
#Number of rides per Day of Week



