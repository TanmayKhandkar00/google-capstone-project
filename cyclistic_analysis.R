#change working directory
setwd("C:/Users/Tanmay Khandkar/Desktop/Trip_data")


#load packages
library(tidyverse)  #for data import and wrangling
library(lubridate)  #for date functions
library(ggplot2)    #for visualizations
library(hms)        #time
library(janitor)
library(data.table) #exporting data frame

#read the csv files, these files reflect data of a full year from Jan 23 to Dec 23
data_2023_01 <- read_csv("202301-divvy-tripdata.csv")
data_2023_02 <- read_csv("202302-divvy-tripdata.csv")
data_2023_03 <- read_csv("202303-divvy-tripdata.csv")
data_2023_04 <- read_csv("202304-divvy-tripdata.csv")
data_2023_05 <- read_csv("202305-divvy-tripdata.csv")
data_2023_06 <- read_csv("202306-divvy-tripdata.csv")
data_2023_07 <- read_csv("202307-divvy-tripdata.csv")
data_2023_08 <- read_csv("202308-divvy-tripdata.csv")
data_2023_09 <- read_csv("202309-divvy-tripdata.csv")
data_2023_10 <- read_csv("202310-divvy-tripdata.csv")
data_2023_11 <- read_csv("202311-divvy-tripdata.csv")
data_2023_12 <- read_csv("202312-divvy-tripdata.csv")

#examine individual dataframes
str(data_2023_01)
str(data_2023_02)
str(data_2023_03)
str(data_2023_04)
str(data_2023_05)
str(data_2023_06)
str(data_2023_07)
str(data_2023_08)
str(data_2023_09)
str(data_2023_10)
str(data_2023_11)
str(data_2023_12)

#merge all dataframes into a single dataframe
tripData_2023 <- rbind(data_2023_01,data_2023_02,data_2023_03,data_2023_04,
                       data_2023_05,data_2023_06,data_2023_07, data_2023_08,
                       data_2023_09,data_2023_10,data_2023_11,data_2023_12)


#Add ride_length column for calculations
tripData_2023$ride_length <- difftime(tripData_2023$ended_at, tripData_2023$started_at, units = "mins" )

#create new data frame
tripData_2023_v2 <- tripData_2023

#Add columns that list the date, month, day, year time and hour of each ride for calculations
tripData_2023_v2$date <- as.Date(tripData_2023_v2$started_at)
tripData_2023_v2$day <- format(as.Date(tripData_2023_v2$date),"%d")
tripData_2023_v2$month <- format(as.Date(tripData_2023_v2$date), "%m")
tripData_2023_v2$year <- format(as.Date(tripData_2023_v2$date), "%Y")
tripData_2023_v2$day_of_week <- format(as.Date(tripData_2023_v2$date), "%A")
tripData_2023_v2$time <- format(as_hms(tripData_2023_v2$started_at), "%H:%M:%S")
tripData_2023_v2$hour <- hour(as_hms(tripData_2023_v2$started_at))                                    

#Drop the start and end lat and long, start station id and end station id columns as they are not important for our analysis
tripData_2023_v2 <- tripData_2023_v2 %>% 
  select(-c(start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))


#Remove NA(or Null) values
tripData_2023_v2 <- na.omit(tripData_2023_v2)
#Remove Duplicates
tripData_2023_v2 <- tripData_2023_v2 %>%
                      distinct()
#Remove values containing zero or negative ride length
tripData_2023_v2 <- tripData_2023_v2[!(tripData_2023_v2$ride_length<=0),]

#In order to conduct analysis on seasonal trend, we'll add a new column to mention the season next to each month
tripData_2023_v2 <- tripData_2023_v2 %>% mutate(season =
                                                case_when(month== "03" ~ "Spring",
                                                          month== "04" ~ "Spring",
                                                          month== "05" ~ "Spring",
                                                          month== "06" ~ "Summer",
                                                          month== "07" ~ "Summer",
                                                          month== "08" ~ "Summer",
                                                          month== "09" ~ "Fall",
                                                          month== "10" ~ "Fall",
                                                          month== "11" ~ "Fall",
                                                          month== "12" ~ "Winter",
                                                          month== "01" ~ "Winter",
                                                          month== "02" ~ "Winter"
                                                          ) )


#Analysis and Visualisation
#Average ride length per member type
tripData_2023_v2 %>% 
  group_by(member_casual) %>%
  summarise(average_ride_length = mean(ride_length), min_ride_length = min(ride_length), max_ride_length = max(ride_length))

#--------------Number of rides------------------
#total number of rides
nrow(tripData_2023_v2)

#Number of rides per member type
tripData_2023_v2 %>% 
  group_by(member_casual) %>%
  count(member_casual)

#Number of rides per bike type and number of bikes per type
tripData_2023_v2 %>%
  group_by(rideable_type)%>%
  count(rideable_type)

tripData_2023_v2 %>%
  group_by(member_casual,rideable_type)%>%
  count(rideable_type)

#Number of rides per day of week
tripData_2023_v2 %>%
  group_by(member_casual) %>%
  count(day_of_week) 

#Total rides and average ride length
tripData_2023_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), average_ride_length = mean(ride_length))%>%  
  arrange(member_casual, day_of_week)

#Number of rides per hour
tripData_2023_v2 %>%
  group_by(member_casual, hour)%>%
  count(hour) %>%
  print(n=48)
#Total rides per hour
tripData_2023_v2 %>%
  group_by(hour) %>%
  count(hour) %>%
  print(n=24)

#Number of rides per month
tripData_2023_v2 %>%
  group_by(member_casual, month)%>%
  count(month)%>%
  print(n=24)

#Number of rides per day of month
tripData_2023_v2 %>%
  group_by(member_casual) %>%
  count(day) %>%
  print (n=62)
  
#Number of rides per start station name, to find out most and least popular stations among users
tripData_2023_v2 %>%
  group_by(start_station_name,member_casual) %>%
  count(start_station_name) %>%
  arrange(desc(n))

#Number of rides per season
tripData_2023_v2 %>%
  group_by(season, member_casual) %>%
  count(season)
  

#Calculate average, total, max, min ride length for members and casuals according to different parameters
#Ride length per day of week
tripData_2023_v2 %>% 
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_length = mean(ride_length), min_ride_length = min(ride_length), max_ride_length = max(ride_length))

#Ride length per hour
tripData_2023_v2 %>% 
  group_by(hour, member_casual) %>%
  summarise(average_ride_length = mean(ride_length), min_ride_length = min(ride_length), max_ride_length = max(ride_length)) %>%
  print(n=48)

#Ride length per season
tripData_2023_v2 %>% 
  group_by(season, member_casual) %>%
  summarise(average_ride_length = mean(ride_length), min_ride_length = min(ride_length), max_ride_length = max(ride_length)) 
  
#Ride length per day of month
tripData_2023_v2 %>% 
  group_by(day, member_casual) %>%
  summarise(average_ride_length = mean(ride_length), min_ride_length = min(ride_length), max_ride_length = max(ride_length)) %>%
  print(n=62)
  

#Create new df
cyclistic_trip_data_2023 <- tripData_2023_v2
#change the month column from number to name of month
cyclistic_trip_data_2023 <- cyclistic_trip_data_2023 %>% mutate(month =
                                                  case_when(month== "01" ~ "January",
                                                            month== "02" ~ "February",
                                                            month== "03" ~ "March",
                                                            month== "04" ~ "April",
                                                            month== "05" ~ "May",
                                                            month== "06" ~ "June",
                                                            month== "07" ~ "July",
                                                            month== "08" ~ "August",
                                                            month== "09" ~ "September",
                                                            month== "10" ~ "October",
                                                            month== "11" ~ "November",
                                                            month== "12" ~ "December"
                                                  ) )

#Remove column not needed (started_at, ended_at, rideId, end_station_name, )
cyclistic_trip_data_2023 <- cyclistic_trip_data_2023 %>%
  select(-c(started_at, ended_at, ride_id, end_station_name))

#Save new df for visualization in Tableau
fwrite(cyclistic_trip_data_2023, "cyclistic-tripdata-2023.csv")
render("cyclistic_analysis.R", output_format = "html_document")