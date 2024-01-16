######################### Load packages##################################

library("tidyverse")
library("ggplot2")
library("lubridate")
#======================================================================#
                        #Import and Read datasets
setwd("C:/Users/geler/Desktop/Code/CaseStudy1/Divvy_Trips_2019_Q1")
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
setwd("C:/Users/geler/Desktop/Code/CaseStudy1/Divvy_Trips_2020_Q1")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

#=====================================================================#

colnames(q1_2019)
colnames(q1_2020)

q1_2019 <- rename(q1_2019,
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,  
                  ended_at = end_time,  
                  start_station_name = from_station_name, 
                  start_station_id = from_station_id, 
                  end_station_name = to_station_name, 
                  end_station_id = to_station_id, 
                  member_casual = usertype
                  )

#======================================================================#
                        #Check for inconsistencies

str(q1_2019)
str(q1_2020)

#=======================================================================#
               #Convert ride_id And rideable_type to character

q1_2019 <- mutate(q1_2019,
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type)
                  )

#=======================================================================#
                              #Merge

df_trips <- full_join(q1_2019, q1_2020)

#======================================================================#
                    #Delete unnecessary columns

df_trips <- select(df_trips,-gender, -birthyear, -start_lat, -start_lng, 
                   -end_lat, -end_lng, -tripduration)
#=======================================================================#
                    #Prepare data: Clean and Add
#Inspect

colnames(df_trips)
nrow(df_trips)
dim(df_trips)
str(df_trips)
summary(df_trips)

#Solve problems

#1 col member casual need to be edited to have only 2values member & casual

df_trips$member_casual[df_trips$member_casual=="Subscriber"]<-"member"
df_trips$member_casual[df_trips$member_casual=="Customer"]<-"casual"
table(df_trips$member_casual)  #to check the code's result

#2 We need to have year, month and day cols to aggregate

df_trips$date <- as.Date(df_trips$started_at, format = "%Y/%m/%d")
df_trips$year <- year(df_trips$date)
df_trips$month <- month(df_trips$date)
df_trips$day <- day(df_trips$date)
df_trips$day_of_week <- weekdays(df_trips$date)

#3 Add ride_length

df_trips<- mutate(df_trips, ride_length = ended_at - started_at)
str(df_trips)
is.factor(df_trips$ride_length)
df_trips$ride_length <- as.numeric(as.character(df_trips$ride_length))
is.numeric(df_trips$ride_length)
#4 Remove >0 ride_length | start station name = HQ QR

v2_df_trips = df_trips[!(df_trips$ride_length < 0
                         | df_trips$start_station_name == "HQ QR"),]

#####################################################################
#+++++++++++++++++++++++++++ANALYSIS++++++++++++++++++++++++++++++++#
#####################################################################

summary(v2_df_trips) # MIN, MEDIAN, MEAN, MAX

# members VS. casual users

aggregate(v2_df_trips$ride_length ~ v2_df_trips$member_casual, FUN = mean)
aggregate(v2_df_trips$ride_length ~ v2_df_trips$member_casual, FUN = max)
aggregate(v2_df_trips$ride_length ~ v2_df_trips$member_casual, FUN = min)
aggregate(v2_df_trips$ride_length ~ v2_df_trips$member_casual, FUN = median)

#avg trip time per day : member VS. casual

v2_df_trips$day_of_week <- 
  ordered(v2_df_trips$day_of_week,
          levels = 
            c("Sunday", "Monday", "Tuesday", "Wednesday",
              "Thursday", "Friday", "Saturday"))

#ridership data based on usertype and weekdays

analyze_df <-v2_df_trips %>% mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(number_of_ride = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) 

#############################################################################
#+++++++++++++++++++++++++++++++Visualize++++++++++++++++++++++++++++++++++#
#############################################################################

#number of rides by rider

ggplot(analyze_df) +
  geom_col(mapping = aes(x = weekday, y = number_of_ride, fill = member_casual))


#average duration

ggplot(analyze_df) + 
  geom_col(mapping = aes(x = weekday, y = average_duration, fill = member_casual))
