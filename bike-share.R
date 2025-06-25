# Data analysis to help stakeholders design better marketing strategies
# Focus in answering the question: "How do annual members and casual riders use 
# Cyclistic bikes differently?"

# Ideas: Rides length and rides count per membership type and per day of the 
# week and month.

# Setting up the libraries
library(tidyverse) # wrangle data
library(conflicted) # manage conflicts
library(lubridate) # manage date time
library(janitor) # manage cleaning
library(stringr) # manage string operations
library(skimr) # summary functions
 
# Setting defaults
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Collecting data
dir_csv <- "./raw_data" # directory path with raw data containing all Divvy rides 
                        # from 05/2024 to 04/2025
files_csv <- list.files(path = dir_csv, full.names = TRUE) # listing files
dfs_list <- lapply(files_csv, read.csv) # Creating a list of data frames

# Inspecting the data frames
#for (df in dfs_list) {
#  print(colnames(df))
#  print(str(df))
#}

# Joining data frames
all_trips <- bind_rows(dfs_list)
#skim(all_trips) # Inspecting all trips joined

# Data Cleaning and Transformation
  # Correcting structural errors
    # Transforming column types
      # All data types are consistent to each variable, creating two columns of
      # start and end time rides as datetime type
all_trips$started_at_dt <- ymd_hms(all_trips$started_at)
all_trips$ended_at_dt <- ymd_hms(all_trips$ended_at)
all_trips$started_at_dt_date <- date(all_trips$started_at_dt)
all_trips$started_at_dt_ym <- floor_date(all_trips$started_at_dt_date, "month")
    # Removing leading and trailing spaces of string type variables
all_trips <- all_trips %>% 
  mutate_if(is.character,str_trim)
    # Treating blank values
all_trips <- all_trips %>% 
  mutate(
    across(where(is.character), ~ na_if(.x, ""))
  )

  # Cleaning null rows and columns
#skim(all_trips) # Looking through data, all information about duration of the 
                # trips are complete. However, there are some inconsistencies in
                # latitude and longitude of stations and missing values in 6 
                # variables. So, let's keep two data frames, one with all data 
                # and one excluding NA's.
all_trips_wo_na <- all_trips %>% 
  drop_na()

  # Checking for duplicates
#is_duplicated <- duplicated(all_trips)
#all_trips[is_duplicated,]
    # There aren't any duplicated values
  
  # Feature engineering
    # Creating ride length and day of week variables to improve insights
all_trips$ride_length <- as.numeric(difftime(all_trips$ended_at_dt, 
                                  all_trips$started_at_dt, units = "mins"))
all_trips_wo_na$ride_length <- as.numeric(difftime(all_trips_wo_na$ended_at_dt,
                                        all_trips_wo_na$started_at_dt, 
                                        units = "mins"))
all_trips$weekday <- wday(all_trips$started_at_dt, 
                          label = TRUE, 
                          abbr = FALSE, 
                          locale = "English")
all_trips_wo_na$weekday <- wday(all_trips_wo_na$started_at_dt, 
                                label = TRUE, 
                                abbr = FALSE,
                                locale = "English")

  # Validating outliers and invalid observations
#skim(all_trips)
#skim(all_trips_wo_na)
    # There are some negative ride lengths.
filtered_all_trips <- all_trips %>% 
  filter(all_trips$ride_length > 0)
filtered_all_trips_wo_na <- all_trips_wo_na %>% 
  filter(all_trips_wo_na$ride_length > 0)
#skim(filtered_all_trips_wo_na$ride_length)
    # Negative ride lengths removed, but there are very brief rides.
#View(filtered_all_trips_wo_na)
    # A lot of very brief rides start and end at the same station, maybe, due to 
    # error or member wrong disengage. Very high right skewed data, it makes 
    # difficult to infer maintenance or user error. Very high and low rides 
    # length can deceive how users are using the service. Removing outliers.
filtered_all_trips_wo_na <- filtered_all_trips_wo_na %>% 
  mutate(log_ride_length = log(ride_length))
log_mean_ride_length <- mean(filtered_all_trips_wo_na$log_ride_length)
log_sd_ride_length <- sd(filtered_all_trips_wo_na$log_ride_length)
loW_outlier = log_mean_ride_length - 3*log_sd_ride_length
upp_outlier = log_mean_ride_length + 3*log_sd_ride_length
filtered_all_trips_wo_na_v2 <- filtered_all_trips_wo_na %>% 
  filter(log_ride_length >= loW_outlier & log_ride_length <= upp_outlier)
    # Still there are a lot of same start and end station rides, removing all 
    # rides with the same start and end station less than the first quartile.
filtered_all_trips_wo_na_v3 <- filtered_all_trips_wo_na_v2 %>% 
  filter(!(ride_length <= 5.85 & start_station_name == end_station_name))
#skim(filtered_all_trips_wo_na_v3$ride_length)

# Data exploration and Insights
  # The variables of interest are station(i.e. start_station_name), rideable_type,
  # member_casual(membership), ride_length, weekday and started_at_dt. Creating 
  # a dataframe assigned with variables of interest using rides with complete 
  # information
all_tripsv2 <- filtered_all_trips_wo_na_v3[c("ride_id", 
                                    "start_station_name", 
                                    "rideable_type",
                                    "member_casual",
                                    "ride_length",
                                    "weekday",
                                    "started_at_dt",
                                    "started_at_dt_date",
                                    "started_at_dt_ym")]
 # Descriptive analysis
skim(all_tripsv2)
  # How do members and casual riders use bikes differently?
by_date <- all_tripsv2 %>% group_by(started_at_dt_ym, member_casual)
by_weekday <- all_tripsv2 %>% group_by(weekday, member_casual)
  # By count, average, median, maximum and minimum ride length
by_date_tbl <- by_date %>% 
  summarise(
    number_rides = n(),
    avg_ride_length = mean(ride_length),
    median_ride_length = median(ride_length),
    max_ride_length = max(ride_length),
    min_ride_length = min(ride_length),
  ) %>% 
  mutate(pct_rides = (number_rides / sum(number_rides)) * 100)
by_weekday_tbl <- by_weekday %>% 
  summarise(
    number_rides = n(),
    avg_ride_length = mean(ride_length),
    median_ride_length = median(ride_length),
    max_ride_length = max(ride_length),
    min_ride_length = min(ride_length),
  ) %>% 
  mutate(pct_rides = (number_rides / sum(number_rides)) * 100)
  # Viz
    # Monthly stats
      # By number of rides
chart1 <- ggplot(by_date_tbl, 
       aes(x = started_at_dt_ym)) +
  geom_col(aes(y = number_rides, 
               fill = member_casual),
               position = "dodge") + 
  geom_line(aes(y = pct_rides * 10000,
                color = member_casual)) +
  scale_y_continuous(
    "Number of rides",
    sec.axis = sec_axis(~ . / 10000, name = "Percent per membership")
  ) +
  labs(x = "Month and Year",
       fill = "Membership",
       color = "Membership",
       title = "Number of Rides per Month",
       subtitle = "Chart 1")
      # By Average ride length
chart2 <- ggplot(by_date_tbl, 
       aes(x = started_at_dt_ym)) +
  geom_col(aes(y = avg_ride_length,
               fill = member_casual),
           position = "dodge") + 
  labs(x = "Month and Year",
       y = "Average Ride length",
       fill = "Membership",
       title = "Average Ride Length per Month",
       subtitle = "Chart 2")
    # Day of the week stats
      # By number of rides
chart3 <- ggplot(by_weekday_tbl, 
       aes(x = weekday)) +
  geom_col(aes(y = number_rides, 
               fill = member_casual),
           position = "dodge") + 
  labs(x = "Day of the Week",
       y = "Number of Rides",
       fill = "Membership",
       title = "Number of Rides per Day of the Week",
       subtitle = "Chart 3")
      # By Average ride length
chart4 <- ggplot(by_weekday_tbl, 
       aes(x = weekday)) +
  geom_col(aes(y = avg_ride_length,
               fill = member_casual),
           position = "dodge") + 
  labs(x = "Day of the Week",
       y = "Average Ride length",
       fill = "Membership",
       title = "Average Ride Length per Day of the Week",
       subtitle = "Chart 4")
# Plotting charts
chart1
chart2
chart3
chart4

# Analysis of the charts:

# Chart 1: There is an increase of rides when the weather is warmer and a 
# decrease when it's colder, the percent amount of rides per members are higher
# when the weather is colder, but lower when it's warmer. There are an increase
# of rides by casual members when the weather is warmer.

# Chart 2: Casual members have the longest average ride length per month.

# Chart 3: Members ride mostly on work days and casual members ride on weekends.
# Casual members ride increase from friday to sunday.

# Chart 4: Casual members increase their rides length on weekends and members 
# keep the average almost the same during all week.

# Opinion:

# Cyclist can offer monthly subscriptions for weekend passes starting on friday 
# going until sunday to increase their members base.
