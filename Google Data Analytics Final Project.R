# we Begin by loading all the libraries we will be needing for cleaning, sorting and analysing the data.
library(tidyverse)
library(dplyr)
library(lubridate) # This will come in handy in dealing with dates.
library(hms)
library(readxl) # Reading csv files into R.
library(data.table)
# Import the csv files.

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
cyclist3_df <- rbind(nov22_df, oct22_df, sep22_df)
cyclist4_df <- rbind(cyclist1_df, cyclist2_df, cyclist3_df)
remove(apr22_df, aug22_df, dec22_df, feb22_df, jan22_df, jun22_df, jul22_df, mar22_df, may22_df, nov22_df, oct22_df, sep22_df)
remove(cyclist1_df, cyclist2_df, cyclist3_df)

# Now we have a dataframe that contains all the years data as cyclist4_df

cyclist <- cyclist4_df
# Next we calulate the length of rides subtracting ended_time from started time and converting it to minutes.

cyclist$ride_length <- difftime(cyclist_date$ended_at, cyclist_date$started_at, units = "mins")

# 