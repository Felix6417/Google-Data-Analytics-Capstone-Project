library(tidyverse)
library(dplyr)
library(lubridate) # This will come in handy in dealing with dates.
library(hms)
library(readxl) # Reading csv files into R.
library(data.table)


jan22_df <- read_excel("C:\\Users\\USER\\Desktop\\202201-divvy-tripdata.xlsx") 
feb22_df <- read_excel("C:\\Users\\USER\\Desktop\\202202-divvy-tripdata.xlsx") 
mar22_df <- read_excel("C:\\Users\\USER\\Desktop\\202203-divvy-tripdata.xlsx") 
apr22_df <- read_excel("C:\\Users\\USER\\Desktop\\202204-divvy-tripdata.xlsx") 
may22_df <- read_excel("C:\\Users\\USER\\Desktop\\202205-divvy-tripdata.xlsx")
jun22_df <- read_excel("C:\\Users\\USER\\Desktop\\202206-divvy-tripdata.xlsx")
jul22_df <- read_excel("C:\\Users\\USER\\Desktop\\202207-divvy-tripdata.xlsx")
aug22_df <- read_excel("C:\\Users\\USER\\Desktop\\202208-divvy-tripdata.xlsx")
sep22_df <- read_excel("C:\\Users\\USER\\Desktop\\202209-divvy-publictripdata.xlsx") 
oct22_df <- read_excel("C:\\Users\\USER\\Desktop\\202210-divvy-tripdata.xlsx")
nov22_df <- read_excel("C:\\Users\\USER\\Desktop\\202211-divvy-tripdata.xlsx")
dec22_df <- read_excel("C:\\Users\\USER\\Desktop\\202212-divvy-tripdata.xlsx")

# With all files successfully imported into R, we merge them into a single file.

cyclist1_df <- rbind(apr22_df, aug22_df, dec22_df, feb22_df)
cyclist2_df <- rbind(jul22_df, jun22_df, mar22_df, may22_df)
cyclist3_df <- rbind(nov22_df, oct22_df, sep22_df, jan22_df)
cyclist4_df <- rbind(cyclist1_df, cyclist2_df, cyclist3_df, jan22_df)
cyclist5_df <- rbind(cyclist4_df, jan22_df)
remove(apr22_df, aug22_df, dec22_df, feb22_df, jan22_df, jun22_df, jul22_df, mar22_df, may22_df, nov22_df, oct22_df, sep22_df)
remove(cyclist1_df, cyclist2_df, cyclist3_df)


cyclists <- cyclist5_df

# Next, i create a column for length of each ride.before then unwanted columns were removed.
cyclists <- cyclist5_df %>% 
  select(-c(start_time, end_time, ride_duration, week_day, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))

cyclists$ride_length <- difftime(cyclist5_df$ended_at, cyclist5_df$started_at, units = "mins")

# Having created a column for length of each ride more columns were created for Day of the week, month, season, time of day, etc.

cyclists$date <- as.Date(cyclist5_df$started_at)
cyclists$day_of_week <- wday(cyclist5_df$started_at)
cyclists$day_of_week <- format(as.Date(cyclists$date), "%A")
cyclists$month <- format(as.Date(cyclists$date), "%m")
cyclists$day <- format(as.Date(cyclists$date), "%d")
cyclists$year <- format(as.Date(cyclists$date), "%Y")
cyclists$time <- format(as.Date(cyclists$date), "%H:%M:%S")
cyclists$time <- as_hms((cyclist5_df$started_at))
cyclists$hour <- hour(cyclists$time)

# time of Day column

cyclists <- cyclists %>%  mutate(time_of_day = 
                                   case_when(hour == "0" ~ "Night",
                                             hour == "1" ~ "Night",
                                             hour == "2" ~ "Night",
                                             hour == "3" ~ "Night",
                                             hour == "4" ~ "Night",
                                             hour == "5" ~ "Night",
                                             hour == "6" ~ "Morning",
                                             hour == "7" ~ "Morning",
                                             hour == "8" ~ "Morning",
                                             hour == "9" ~ "Morning",
                                             hour == "10" ~ "Morning",
                                             hour == "11" ~ "Morning",
                                             hour == "12" ~ "Afternoon",
                                             hour == "13" ~ "Afternoon",
                                             hour == "14" ~ "Afternoon",
                                             hour == "15" ~ "Afternoon",
                                             hour == "16" ~ "Afternoon",
                                             hour == "17" ~ "Afternoon",
                                             hour == "18" ~ "Evening",
                                             hour == "19" ~ "Evening",
                                             hour == "20" ~ "Evening",
                                             hour == "21" ~ "Evening",
                                             hour == "22" ~ "Evening",
                                             hour == "23" ~ "Evening")
)


cyclists <- cyclists %>% mutate(season = 
                                  case_when( month == "03" ~ "Spring",
                                             month == "04" ~ "Spring",
                                             month == "05" ~ "Spring",
                                             month == "06" ~ "Summer",
                                             month == "07" ~ "Summer",
                                             month == "08" ~ "Summer",
                                             month == "09" ~ "Fall",
                                             month == "10" ~ "Fall",
                                             month == "11" ~ "Fall",
                                             month == "12" ~ "Winter",
                                             month == "01" ~ "Winter",
                                             month == "02" ~ "Winter")
)
                                           


# At this stage i want to do a final cleaning to make the data ready for error free analysis, I remove double entries, missing data and zero ride_lengths.
cyclists <- distinct(cyclists)
cyclists <- na.omit(cyclists)
cyclists <- cyclists[!(cyclists$ride_length <= 0),]

# Data is now perfectly cleaned and ready for Analysis!

# Total riders by category

cyclists %>% 
  group_by(member_casual) %>% 
  count(member_casual)

# total Bike types ridden
cyclists %>% 
  group_by(rideable_type) %>% 
  count(rideable_type)

# distribution of Riders by Bike types
cyclists %>% 
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

# Total Monthly rides
cyclists %>% 
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 30)

#Week Day with rides distribution
cyclists %>% 
  group_by(member_casual) %>% 
  count(day_of_week)

# Time of the day ride distribution
cyclists %>% 
  group_by(time_of_day) %>% 
  count(time_of_day)
cyclists %>% 
  group_by(member_casual) %>% 
  count(time_of_day)

# Total rides by Different times of the day
#Morning
cyclists %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#Afternoon
cyclists %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#Evening
cyclists %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

# Night
cyclists %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

cyclists %>% 
  group_by(month) %>% 
  count(month)

#Total ride distribution by Seasons
#Summer
cyclists %>% 
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  count(season)
#Spring
cyclists %>% 
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  count(season)
#Fall
cyclists %>% 
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  count(season)
#Winter
cyclists %>% 
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  count(season)
# Total Seasonal Distribution of rides
cyclists %>% 
  group_by(season, member_casual) %>% 
  count(season)
cyclists %>% 
  group_by(season) %>% 
  count(season)

#Average of the total ride length by different groupings

cyclists_avg <- mean(cyclists$ride_length)
print(cyclists_avg)

# Average Ride_length by Rider Type

cyclists %>% 
  group_by(member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
cyclists %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
cyclists %>% 
  group_by(rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
cyclists %>% 
  group_by(hour, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
cyclists %>% 
  group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
cyclists %>% 
  group_by(time_of_day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
cyclists %>% 
  group_by(time_of_day) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclists %>% 
  group_by(time_of_day) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclists %>% 
  group_by(time_of_day) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclists %>% 
  group_by(time_of_day) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
cyclists %>% 
  group_by(day_of_week, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclists %>% 
  group_by(day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
cyclists %>% 
  group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
cyclists %>% 
  group_by(month, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclists %>% 
  group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
cyclists %>% 
  group_by(month, rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclists %>% 
  group_by(season, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclists %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

cyclists %>% 
  group_by(season, rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


# In other to be able to use this data in tableau, we need to add a column which will contain the names of the months.

cyclists <- cyclists %>% mutate(month = 
                                  case_when( month == "01" ~ "January",
                                             month == "02" ~ "February",
                                             month == "03" ~ "March",
                                             month == "04" ~ "April",
                                             month == "05" ~ "May",
                                             month == "06" ~ "June",
                                             month == "07" ~ "July",
                                             month == "08" ~ "August",
                                             month == "09" ~ "September",
                                             month == "10" ~ "October",
                                             month == "11" ~ "November",
                                             month == "12" ~ "December")
)


Finally, I download the dataframe as a .csv file I can use on tableau.

write.csv(cyclists, "C:\\Users\\USER\\Desktop\\final_project_data.csv", row.names=FALSE)