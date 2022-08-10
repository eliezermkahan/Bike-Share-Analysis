# Install and load packages to be used in analysis
install.packages("tidyverse")
install.packages("lubridate")
install.packages("hms")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(hms)
library(ggplot2)
library(scales)

##Data collection ##

# Import most recent 12 months of data: May '21 - April '22
m1 <- read.csv("./cyclisticCsvFiles/202105-divvy-tripdata.csv")
m2 <- read.csv("./cyclisticCsvFiles/202106-divvy-tripdata.csv")
m3 <- read.csv("./cyclisticCsvFiles/202107-divvy-tripdata.csv")
m4 <- read.csv("./cyclisticCsvFiles/202108-divvy-tripdata.csv")
m5 <- read.csv("./cyclisticCsvFiles/202109-divvy-tripdata.csv")
m6 <- read.csv("./cyclisticCsvFiles/202110-divvy-tripdata.csv")
m7 <- read.csv("./cyclisticCsvFiles/202111-divvy-tripdata.csv")
m8 <- read.csv("./cyclisticCsvFiles/202112-divvy-tripdata.csv")
m9 <- read.csv("./cyclisticCsvFiles/202201-divvy-tripdata.csv")
m10 <- read.csv("./cyclisticCsvFiles/202202-divvy-tripdata.csv")
m11 <- read.csv("./cyclisticCsvFiles/202203-divvy-tripdata.csv")
m12 <- read.csv("./cyclisticCsvFiles/202204-divvy-tripdata.csv")

# Inspect data + combine into single data frame (df)
# Column names and structures need to match
colnames(m1)
colnames(m2)
colnames(m3)
colnames(m4)
colnames(m5)
colnames(m6)
colnames(m7)
colnames(m8)
colnames(m9)
colnames(m10)
colnames(m11)
colnames(m12)

str(m1)
str(m2)
str(m3)
str(m4)
str(m5)
str(m6)
str(m7)
str(m8)
str(m9)
str(m10)
str(m11)
str(m12)

## Wrangle Data ##

# Stack individual data frames into one big df
all_rides <- rbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)
str(all_rides)

# Add data to prepare for analysis
# Add the columns - date, month, day, year, day_of_week -> derived from 'started_at' column
# These columns allow us to aggregate ride data for each day, month, year, etc
all_rides$date <- as.Date(all_rides$started_at)
all_rides$month <- format(as.Date(all_rides$date),"%m")
all_rides$day <- format(as.Date(all_rides$date),"%d")
all_rides$year <- format(as.Date(all_rides$date),"%Y")
all_rides$day_of_week <- format(as.Date(all_rides$date),"%A")

# Calculate ride length from 'started_at' and 'ended_at' columns
# Add columns ride_length_hours and ride_length_hms (hour, min, sec)
all_rides$started_at <- ymd_hms(all_rides$started_at)
all_rides$ended_at <- ymd_hms(all_rides$ended_at)
all_rides$ride_length_hours <- as.numeric(difftime(all_rides$ended_at,all_rides$started_at, units = 'hours'))
all_rides$ride_length_hms <- hms(hours= as.numeric(all_rides$ride_length_hours))

# Check for negative or zero ride length values
# Inspect for blank fields
table(sign(all_rides$ride_length_hours))     
table(all_rides$start_station_name == "")
table(all_rides$start_station_name == "" & all_rides$start_station_id == "")
table(all_rides$end_station_name == "")
table(all_rides$end_station_name == "" & all_rides$end_station_id == "")
table(all_rides$end_station_name == "" & all_rides$start_station_name == "")

# Remove 'bad' data
# Create new version (v2) of df, because deleting data
# Filter out trips greater than 24 hrs and less than 60 seconds
# Filter out rows containing 'NA' fields
# Filter out rows with blank values
# Drop ride_id column - unnecessary + save memory
# Add ride_length minutes column for calculations
# Amend rideable_type column to only have classic bike and electric bike - docked bike was renamed to classic bike in April 2020
all_rides_v2 <- all_rides %>%
    filter(
      !is.na(all_rides$year),
      ride_length_hours < 24,
      ride_length_hours > .01667,
      start_station_name != "",
      start_station_id != "",
      end_station_name != "",
      end_station_id != ""
    )
all_rides_v2$ride_id <- NULL
all_rides_v2$ride_length_min <-all_rides_v2$ride_length_hours*60
all_rides_v2$rideable_type[all_rides_v2$rideable_type == "docked_bike"] <- "classic_bike"  
  
## Analyze data ##

summary(all_rides_v2$ride_length_min)

# Compare Members vs Casual - ride length
aggregate(all_rides_v2$ride_length_min ~ all_rides_v2$member_casual, FUN = mean)
aggregate(all_rides_v2$ride_length_min ~ all_rides_v2$member_casual, FUN = median)
aggregate(all_rides_v2$ride_length_min ~ all_rides_v2$member_casual, FUN = max)
aggregate(all_rides_v2$ride_length_min ~ all_rides_v2$member_casual, FUN = min)

# Compare average daily ride time - Member vs Casual
aggregate(all_rides_v2$ride_length_min ~ all_rides_v2$member_casual + all_rides_v2$day_of_week, FUN = mean)

# Correctly order day_of_week field
all_rides_v2$day_of_week <- ordered(all_rides_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# analyze ridership data by type and weekday
all_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%           # creates weekday field using wday()
  group_by(member_casual, weekday) %>%                           # groups by user-type and weekday
  summarise(number_of_rides = n()							                   # calculates the number of rides and average duration 
            ,average_duration = mean(ride_length_min)) %>%       # calculates the average duration
  arrange(weekday, member_casual)								                 # sorts

## Visualize Data ##

# Visualize the number of rides by rider type - weekday
all_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
   geom_col(position = "dodge")+
   labs(x="Weekday", y="Number of Rides", title="Daily Ride Count")+
   scale_y_continuous(labels = number)+
   scale_fill_discrete(name="Legend", labels=c("Casual","Member"))

# Visualize the number of rides by rider type - Month
all_rides_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
   geom_col(position = "dodge")+
   labs(x="Month", y="Number of Rides", title="Monthly Ride Count")+
   scale_y_continuous(labels = number)+
   scale_fill_discrete(name="Legend", labels=c("Casual","Member"))


# Visualize average ride duration - weekday
all_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length_min)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
   geom_col(position = "dodge")+
   labs(x="Weekday", y="Average Trip Duration (min)", title="Average Trip Duration")+
   scale_fill_discrete(name="Legend", labels=c("Casual","Member"))

# Visualize average ride duration - month
all_rides_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length_min)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(x="Month", y="Average Trip Duration (min)", title="Average Trip Duration")+
  scale_fill_discrete(name="Legend", labels=c("Casual","Member"))

# visualize preferred bike type 
all_rides_v2 %>%
  group_by(member_casual, rideable_type)%>%
  summarise(number_of_rides = n())%>%
  ggplot(aes(x= rideable_type, y= number_of_rides, fill = member_casual))+
   geom_col(position="dodge")+
   labs(x="Bicycle Type", y="Number of Rides", title = "Bike Preference")+
   scale_fill_discrete(name="Legend", labels=c("Casual","Member"))

# Visualize daily Usage - number of rides*hours per day
all_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length_min)) %>%
  mutate(total_usage = ((number_of_rides)*(average_duration/60))) %>%
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = total_usage, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(x="Weekday", y="Total Bicycle Usage (hr)", title="Total Daily Usage")+
  scale_fill_discrete(name="Legend", labels=c("Casual","Member"))


# Visualize monthly Usage - number of rides*hours per month
all_rides_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length_min)) %>%
  mutate(total_usage = ((number_of_rides)*(average_duration/60))) %>%
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = total_usage, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(x="Month", y="Total Bicycle Usage (hr)", title="Total Monthly Usage")+
  scale_fill_discrete(name="Legend", labels=c("Casual","Member"))

# Visualize hourly ridership for weekends/weekdays (comment out days)
all_rides_v2 %>% 
  mutate(date= format(as.POSIXct(all_rides_v2$started_at),format = "%H")) %>% 
  filter(#day_of_week != "Sunday",
          day_of_week != "Monday",
          day_of_week != "Tuesday",
          day_of_week != "Wednesday",
          day_of_week != "Thursday",
         #day_of_week != "Friday",
         #day_of_week != "Saturday"
         ) %>%
  group_by(date, member_casual) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, date)  %>% 
  ggplot(aes(x = date, y = number_of_rides,
             group= member_casual, color = member_casual)) +
  geom_point(size=2)+
  geom_line(size=1.5)+
  labs(x="Time of Day", y="Number of Rides", title="Bike Rides per Hour", subtitle = "Weekends")+
  scale_y_continuous(labels = number)+
  scale_fill_discrete(name="Legend", labels=c("Casual","Member"))


