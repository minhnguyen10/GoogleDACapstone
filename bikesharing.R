#Load packages
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(dplyr)
library(ggplot2)
#file_list <- list.files()

#ldf <- lapply(file_list , read.csv)

#df_final <- do.call("rbind", ldf)

#write.csv(df_final,'tripdata_202008_202107.csv')

df <- read.csv('tripdata_202008_202107.csv')
head(df)

str(df)

colnames(df)
df_final <- df
#Process
df_final[, "started_at"] <- lapply("started_at", function(d) lubridate::ymd_hms(df_final[,d]))
df_final[, "ended_at"] <- lapply("ended_at", function(d) lubridate::ymd_hms(df_final[,d]))
str(df_final)

df_final$ride_length <- with(df_final, as.numeric(difftime(ended_at,started_at,units="hours")))
#There is likely be an error, so I just remove all rows that have duration 
#larger than 12 hours = 720 mins
df_final <- df_final %>%
  filter(ride_length <= 24 & ride_length > 0 )

df_final$day_of_week <- wday(as.Date(df_final$started_at))
head(df_final)

df_final$year <-year(as.Date(df_final$started_at))

df_final$month <-month(as.Date(df_final$started_at))

nm1 <- setNames(rep(c("Winter", "Spring", "Summer", "Fall"),
                    each = 3), month.name)
nm2 <- setNames(rep(c("Jan", "Feb", "Mar", "Apr",'May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),
                           each = 1), month.name)
df_final <- df_final %>% 
  mutate(Season = nm1[month])
df_final <- df_final %>% 
  mutate(Month_name = nm2[month])


#Select needed col
colnames(df_final)
trimmed_df <- df_final %>% select('rideable_type','start_station_name',
         'member_casual','ride_length','day_of_week','year','month','Month_name','Season')
#Save the trimmed_df
#write.csv(trimmed_df,'trimmed_tripdata_202008_202107.csv')

#Following the suggested steps from the instruction of the challenge
#Run a few calculations in one file to get a better sense of the data layout:
#Calculate the mean of ride_length 
#Overall mean ride_length
mean(trimmed_df$ride_length)
#to minutes
#Calculate the max ride_length
max(trimmed_df$ride_length)

#Calculate the mode of day of week

#Calculate the average ride_length for member and casual riders
#average_ride_length_sumary <- 
trimmed_df %>%
  group_by(member_casual) %>%
  summarise(average_ride_lenth=mean(ride_length),
            total_ride_lenth=sum(ride_length),
            number_of_ride= n()) %>%
  ggplot(aes(x=average_ride_lenth,y=member_casual)) +
  geom_bar(stat = "identity", width=0.2,position=position_dodge()) +
  coord_flip()+
  labs(title="Comparison of Average ride length by Member type for bikesharing",
       caption=paste0("Data from: 08/2020 to 07/2021"),
       x="Average ride length",
       y="Member vs Casual rider")

trimmed_df %>%
  group_by(member_casual) %>%
  summarise(average_ride_lenth=mean(ride_length),
            total_ride_lenth=sum(ride_length),
            number_of_ride= n()) %>%
  ggplot(aes(x=number_of_ride,y=member_casual)) +
  geom_bar(stat = "identity", width=0.2,position=position_dodge()) +
  coord_flip()+
  labs(title="Comparison of Number of ride by Member type for bikesharing",
       caption=paste0("Data from: 08/2020 to 07/2021"),
       x="Number of ride",
       y="Member vs Casual rider")

trimmed_df %>%
  group_by(member_casual) %>%
  summarise(average_ride_lenth=mean(ride_length),
            total_ride_lenth=sum(ride_length),
            number_of_ride= n()) %>%
  ggplot(aes(x=number_of_ride,y=member_casual)) +
  geom_bar(stat = "identity", width=0.2,position=position_dodge()) +
  coord_flip()+
  labs(title="Comparison of Total of ride length by Member type for bikesharing",
       caption=paste0("Data from: 08/2020 to 07/2021"),
       x="Total of ride length",
       y="Member vs Casual rider")

#By season

trimmed_df %>%
  group_by(Month_name) %>%
  summarise(average_ride_lenth=mean(ride_length),
            total_ride_lenth=sum(ride_length),
            number_of_ride= n()) %>%
  mutate(Month_name = fct_relevel(Month_name, 
                                  'Jan', 'Feb', 'Mar', 'Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')) %>%
  ggplot(aes(x=number_of_ride,y=Month_name)) +
  geom_bar(stat = "identity", width=0.5,position=position_dodge()) +
  coord_flip() +
  labs(title="Comparison of Total ride length by Season for bikesharing",
       caption=paste0("Data from: 08/2020 to 07/2021"),
       x="Total ride length",
       y="Number of Bookings")

# by month and member type

trimmed_df %>%
  group_by(member_casual,Month_name) %>%
  summarise(average_ride_lenth=mean(ride_length),
            total_ride_lenth=sum(ride_length),
            number_of_ride= n()) %>%
  mutate(Month_name = fct_relevel(Month_name,
                                  'Jan', 'Feb', 'Mar', 'Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')) %>%
  ggplot(aes(x=total_ride_lenth,y=Month_name,fill=member_casual)) +
  geom_bar(stat = "identity", width=0.5,position=position_dodge()) +
  coord_flip() +
  labs(title="Comparison of Total ride length by month and member type for bikesharing",
       caption=paste0("Data from: 08/2020 to 07/2021"),
       legend="member - casual",
       x="Total ride length",
       y="Number of Bookings")


trimmed_df %>%
  group_by(member_casual,Season) %>%
  summarise(average_ride_lenth=mean(ride_length),
            total_ride_lenth=sum(ride_length),
            number_of_ride= n()) %>%
  mutate(Season = fct_relevel(Season, 'Spring','Summer','Fall','Winter')) %>%
  ggplot(aes(x=number_of_ride,y=Season,fill=member_casual)) +
  geom_bar(stat = "identity", width=0.5,position=position_dodge()) +
  coord_flip() +
  labs(title="Comparison of Total ride length by Season and member type for bikesharing",
       caption=paste0("Data from: 08/2020 to 07/2021"),
       legend="member - casual",
       x="Total ride length",
       y="Number of Bookings")

#By ride type 
trimmed_df %>%
  group_by(rideable_type) %>%
  summarise(average_ride_lenth=mean(ride_length),
            total_ride_lenth=sum(ride_length),
            number_of_ride= n()) %>%
  # mutate(Season = fct_relevel(Season, 'Spring','Summer','Fall','Winter')) %>%
  ggplot(aes(x=total_ride_lenth,y=rideable_type)) +
  geom_bar(stat = "identity", width=0.2,position=position_dodge()) +
  coord_flip() +
  labs(title="Comparison of Total ride length by Season and member type for bikesharing",
       caption=paste0("Data from: 08/2020 to 07/2021"),
       legend="member - casual",
       x="Total ride length",
       y="Bike type")

#member
trimmed_df %>%
  group_by(member_casual,rideable_type) %>%
  summarise(average_ride_lenth=mean(ride_length),
            total_ride_lenth=sum(ride_length),
            number_of_ride= n()) %>%
  #mutate(Season = fct_relevel(Season, 'Spring','Summer','Fall','Winter')) %>%
  ggplot(aes(x=number_of_ride,y=rideable_type,fill=member_casual)) +
  geom_bar(stat = "identity", width=0.5,position=position_dodge()) +
  coord_flip() +
  labs(title="Comparison of Total ride length by Season and member type for bikesharing",
       caption=paste0("Data from: 08/2020 to 07/2021"),
       legend="member - casual",
       x="Total ride length",
       y="Bike type")

filter_df <- trimmed_df %>%
  group_by(month) %>%
  summarise(average_ride_lenth=mean(ride_length),
            total_ride_lenth=sum(ride_length),
            number_of_ride= n())
ggplot(data = filter_df,aes(x = month, y = total_ride_lenth)) +
  geom_line()+
  geom_point()
#Comparing the number of member vs casual over time Visualize
count_over_time_sumary <- trimmed_df %>%
  count(year, member_casual, sort = FALSE)
head(count_over_time_sumary)
#Comparing the ride_length of member vs casual in different year. Visualize
#Comparing the number of ride of member vs casual in different day. Visualize




