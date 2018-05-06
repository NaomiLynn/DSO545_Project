# load data:
table07 = read.csv("2017-fordgobike-tripdata.csv")
table0801 = read.csv("201801_fordgobike_tripdata.csv")
table0802 = read.csv("201802_fordgobike_tripdata.csv")
table0803 = read.csv("201803_fordgobike_tripdata.csv")

# append data together into 1 table:
data = rbind(table07,table0801,table0802,table0803)

# save file to .csv
write.csv(data, file = "ford_bike_data.csv")


### Heatmap to see the busiest time 

library(lubridate)
library(dplyr)
library(ggplot2)

data$start_time = ymd_hms(data$start_time)
data$end_time = ymd_hms(data$end_time)

# data1 is for creating the heatmap
data1 = data%>%
  mutate(day = wday(start_time, label = T, abbr = F),
         hour = hour(start_time)) %>%
  group_by(day, hour) %>%
  summarise(count = n())

ggplot(data1, aes(x = factor(day), y = factor(hour), fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  xlab("") +
  ylab("Hour of the Day") +
  ggtitle("Heatmap of Ford GoBike Rental Day V.S. Hour") +
  theme(legend.position="none")

# Results: People rent bikes mostly at 8am and 5pm.




### Linda's Codes 05/02 20:40PM

## First, create a temporary dataset, adding new columns as "outflow" and "inflow":
data_temp = data %>%
  mutate(day = wday(start_time, label = T, abbr = F),
         start_hour = hour(start_time),
         end_hour = hour(end_time),
         outflow = 1,
         inflow = 1)

## Find out top 10 stations that are out of stock on Monday, 8am:

# Step1 - count the outflow and inflow at each station, at 8am:
data_8am = data_temp %>%
  group_by(day, start_station_name)%>%
  summarise(outflow_count = sum(ifelse
                                (start_hour == 8, 
                                  outflow, 
                                  0)),
            inflow_count = sum(ifelse
                               (end_hour == 8, 
                                 inflow, 
                                 0))) 

# Step2 - find out top 10 stations that are out of stock on Monday, 8am:
# (Can change weekdays)
monday_8am = data_8am %>%
  filter(day == 'Monday') %>%
  mutate(bikes_needed = outflow_count - inflow_count) %>%
  select(day, start_station_name, bikes_needed) %>%
  arrange(desc(bikes_needed)) %>%
  slice(seq_len(10))

ggplot(monday_8am, aes(reorder(x = start_station_name,-bikes_needed), y = bikes_needed)) +
  geom_col() +
  ggtitle("Top 10 Out of Stock Stations on Monday, 8AM") +
  xlab("Stations") +
  ylab("Bikes Needed")

# San Francisco Caltrain Station 2: 21 bikes 
monday = data_temp %>%
  filter(day == "Monday", 
         start_station_name == "San Francisco Caltrain Station 2 (Townsend St at 4th St)") %>%
  mutate(available_bikes_num = 21 - outflow_count)


### Andy's Codes 05/02 23:08PM

### 1. Mapping out the top 10 tourist attractions and existing Ford GoBike stations at SF to identify if there're
### potential areas to set up new stations.

library(maps)
library(ggmap)

sf_attraction=read.csv("SF_top10_attractions.csv")
SF_Map=qmap("San Franciso city", zoom=12)

start_bike_station=data%>%
  select(start_station_name,
         start_station_latitude,start_station_longitude)%>%
  group_by(start_station_name,start_station_latitude,start_station_longitude)%>%
  summarise()

end_bike_station=data%>%
  select(end_station_name,
         end_station_latitude,end_station_longitude)%>%
  group_by(end_station_name,end_station_latitude,end_station_longitude)%>%
  summarise()

SF_Map+
  geom_point(data=start_bike_station,
             aes(x=start_station_longitude,y=start_station_latitude),size=1,color="black")+
  geom_point(data=end_bike_station,
             aes(x=end_station_longitude,y=end_station_latitude),size=1,color="black")+
  geom_point(data=sf_attraction,
             aes(x=longitude,y=latitude),size=3,color="red",shape=17)

### Maintenance plan
## Plotting out the condition of the bikes by the end of the 1st quarter of 2018 by following segments:
## 1. Usage hour > 60 hours --> Not recommend to use
## 2. Usage hour between 40 and 60 hours --> Acceptable
## 3. Usage hour < 40 hours --> Recommend to use

bike_usage_q1=data%>%
  filter(end_time>="2018-01-01" & end_time<="2018-03-31")%>%
  select(duration_sec,bike_id)%>% 
  group_by(bike_id)%>%
  summarize(usage_hour=sum(duration_sec)/3600)%>%
  mutate(Condition=ifelse(usage_hour>60, "Not recommend to use",
                                                    ifelse(usage_hour>=40 & usage_hour<= 60, "Acceptable",
                                                           ifelse(usage_hour<40, "Recommend to use","NA"))))

## List of bikes' condition by the end of Q1, 2018
ggplot(bike_usage_q1,aes(Condition))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)+
  scale_y_continuous(labels=scales::percent)+
  labs(title = "Condition of the bikes by the end of Q1, 2018",y = "Percent", x = "Condition")

## Where're the bikes that are not recommended to use located?

bike_not_rec=bike_usage_q1%>%
  select(bike_id,Condition)%>%
  filter(Condition=="Not recommend to use")

bike_not_rec_detail=merge(bike_not_rec,data,by="bike_id")%>%
  filter(end_time>="2018-01-01" & end_time<="2018-03-31")%>%
  group_by(bike_id)%>%
  select(bike_id,Condition,end_time,end_station_name,end_station_latitude,end_station_longitude)%>%
  arrange(bike_id,desc(end_time))%>%
  do(slice(., 1))

SF_Map+
  geom_point(data=start_bike_station,aes(x=start_station_longitude,y=start_station_latitude),size=1,color="black")+
  geom_point(data=end_bike_station,aes(x=end_station_longitude,y=end_station_latitude),size=1,color="black")+
  geom_point(data=bike_not_rec_detail,aes(x=end_station_longitude,y=end_station_latitude),size=1,color="red")



### Jack's code ###

### 1.2 Potential Out-of-stock Stations ###
### Find the stations most likely to be out of stock at 8am or 5pm on 1 weekday
# Cautious: must divide by the number of weeks of this dataframe
# to find the average delta (bike inflow - bike outflow) of a station at 8am of 1 weekday


## find out how many weeks from 2017/01/01 to 2018/03/31 the timeframe of this dataframe
start_date = min(data$start_time)
# start_date = 2017-06-28
end_date = max(data$start_time)
# end_date = 2018-03-31

myDates = seq(from = start_date, to = end_date, by = "days")
number_of_weeks = length(which(wday(myDates, label = TRUE)=="Wed"))
number_of_weeks
# There are 40 Wednesdays (or say 40 weeks) in our dataframe
# 39 Mondays, but doesn't impact our analysis much


## create an intermediate dataframe to calculate delta  (=inflow-outflow) 
## of a station at 8am
# create new columns "inflow" and "outflow" and assign 1 for futher calculation

weekday_8am_int = data %>%
  mutate(day = wday(start_time, label = T, abbr = F),
         start_hour = hour(start_time),
         end_hour = hour(end_time),
         outflow = 1,
         inflow = 1,
         hour = 8) %>% # create outflow/inflow to calculate delta (=inflow-outflow) of a station 
  filter(start_hour == 8| end_hour == 8, # keep only start or end at 8am
         day %in% c("Monday", "Tuesday", "Wednesday",
                    "Thursday", "Friday")) %>%
  select(day, start_hour, end_hour, hour, start_station_name, end_station_name, outflow, inflow)
# intentionally stops at this intermediate dataframe to double check


## create aggregated delta of each station at 8am
weekday_8am_int2 = weekday_8am_int %>%
  group_by(day, hour, start_station_name, end_station_name) %>%
  summarise(outflow_count = sum(ifelse
                                (start_hour == 8, 
                                  outflow, 
                                  0)),
            inflow_count = sum(ifelse
                               (end_hour == 8, 
                                 inflow, 
                                 0))) # %>% 
# mutate(delta = inflow_count - outflow_count)

# not sure why mutate doesn't work here, so just seperate the code here
weekday_8am_int2$delta = weekday_8am_int2$inflow_count - weekday_8am_int2$outflow_count

weekday_8am = weekday_8am_int2 %>%
  group_by(day, hour, start_station_name) %>%
  summarize(rental = sum(outflow_count)/number_of_weeks,
            return = sum(inflow_count)/number_of_weeks,
            delta = sum(delta)/number_of_weeks)%>%
  arrange(delta) 
# checked : highest delta mostly happens on Wednesday
# 1) San Francisco Ferry Building (Harry Bridges Plaza), Wednesday 
# 2) San Francisco Caltrain Station 2 (Townsend St at 4th St), Wednesday
# 3) Berry St at 4th St, Tuesday
# 4) Steuart St at Market St, Monday
# 5) Howard St at Beale St, Monday


## plot the top5 most likely out of bikes stations
weekday_8am %>%
  group_by(start_station_name) %>%
  summarize(delta = min(delta)) %>%
  arrange(delta) %>%
  slice(1:5) %>%
  ggplot(aes(x= reorder(start_station_name,-delta) , y= -delta )) +
  geom_col(fill = c("darkblue")) +
  coord_flip() + 
  ggtitle("Top5 stations most likely to be short of bikes at 8am on a weekday") +
  xlab("") + 
  ylab("The number of bikes (average within 2017/06 - 2018/03)") + 
  theme_bw() + # theme_xxx can select different color background 
  geom_text( aes(label= paste(round(-delta, digits=1))),
             position = 'dodge',
             hjust = +1.5,
             color = 'white')


## plot the top5 most rental stations  
weekday_8am %>%
  group_by(start_station_name) %>%
  summarize(rental = max(rental)) %>%
  arrange(desc(rental)) %>%
  slice(1:5) %>%
  ggplot(aes(x= reorder(start_station_name, rental) , y= rental )) +
  geom_col(fill = c("darkblue")) +
  coord_flip() + 
  ggtitle("Top5 stations that has most rental at 8am on a weekday") +
  xlab("") + 
  ylab("The number of bikes (average within 2017/06 - 2018/03)") +
  theme_bw() + # theme_xxx can select different color background 
  geom_text(aes(label= paste(round(rental, digits=1))),
            position = 'dodge',
            hjust = +1.5,
            color = 'white')



## plot the top5 most return stations  
weekday_8am %>%
  group_by(start_station_name) %>%
  summarize(return = max(return)) %>%
  arrange(desc(return)) %>%
  slice(1:5) %>%
  ggplot(aes(x= reorder(start_station_name, return) , y= return )) +
  geom_col(fill = c("darkblue")) +
  coord_flip() + 
  ggtitle("Top5 stations that has most return at 8am on a weekday") +
  xlab("") + 
  ylab("The number of bikes (average within 2017/06 - 2018/03)") +
  theme_bw() + # theme_xxx can select different color background 
  geom_text(aes(label= paste(round(return, digits=1))),
            position = 'dodge',
            hjust = +1.5,
            color = 'white')


## what can be further worked on in #2
## find out the peak month and only analyze within that month 
## to avoid average out due to low-demand season (perhaps winter)



### 5.1.5 Program Utilization along the past three quarters ###
### Goal: find out program usage in summer, fall, and winnter

# Assumption: We use the number of different bikes that are rented within this time period to
# represent the number of bikes needed of different seasons.
# Because the data don't have full month of June, 2017, we define summer = July~Sep,
# fall = Oct~Dec, spring = Jan~Mar. The season definition also seems to be more appropriate
# based on SF's temperature and percipitation.


## find out the number of Wednesdays in summer, fall, winter
## to do average 
summer_start = ymd("2017-07-01")
summer_end = ymd("2017-09-30")
fall_start = ymd("2017-10-01")
fall_end = ymd("2017-12-31")
winter_start = ymd("2018-01-01")
winter_end = ymd("2018-03-31")

summer_du = seq(from = summer_start, to = summer_end, by = "days")
num_weeks_summer = length(which(wday(summer_du, label = TRUE)=="Wed"))
fall_du = seq(from = fall_start, to = fall_end, by = "days")
num_weeks_fall = length(which(wday(fall_du, label = TRUE)=="Wed"))
winter_du = seq(from = winter_start, to = winter_end, by = "days")
num_weeks_winter = length(which(wday(winter_du, label = TRUE)=="Wed"))

library(tidyr)

## Find out the total program utilization ride (total bike ride time) across seaons

season_ride = data %>%
  mutate(month = month(start_time)) %>% 
  mutate(season = ifelse(month >=7 & month <=9,
                         "summer",
                         ifelse(month >=10 & month <=12,
                                "fall",
                                "winter")))  %>% 
  group_by(season, user_type) %>%
  summarise(ride_time = sum(duration_sec)) %>%
  spread(key = user_type, value = ride_time) %>%
  mutate(Total = Customer + Subscriber) %>%
  gather(Customer:Total , key = "user_type", value = "ride_time")

# change the order of seaons to summer, fall, winter for plotting purpose
season_ride$season = factor(season_ride$season)
lev = levels(season_ride$season)
lev = lev[c(2,1,3)]
season_ride$season = factor(season_ride$season, levels = lev)

# line plot of bike usage time by seasons and user types
ggplot(season_ride,aes(season, y = (ride_time/(60*60)), 
                       group=user_type,
                       col = user_type)) +
  geom_line() + 
  xlab("") +
  ylab("Total bike usage time") +
  ggtitle("Bike usage time by seasons and user types") +
  scale_y_continuous(name = "Bike usage hours (000's)",
                     breaks = seq(30000, 80000, by=10000), # notice that breaks still at the actual value
                     label = seq(30, 80, by=10)) +
  theme_bw()



## Bike usage hours can be further broken down to (# of rentals) x (avg ride time/rental)
## This breakdown can allow us to under the driver of the trend and the real reasons

season_ride_bd = data %>%
  mutate(month = month(start_time)) %>% 
  mutate(season = ifelse(month >=7 & month <=9,
                         "summer",
                         ifelse(month >=10 & month <=12,
                                "fall",
                                "winter")))  %>% 
  group_by(season, user_type) %>%
  summarise(ride_time = sum(duration_sec) , num_of_rentals = n()) %>%
  mutate(avg_ride_time = ride_time/num_of_rentals)

## plot number of rentals in each season

season_ride_bd_num = season_ride_bd %>%
  select(season, user_type, num_of_rentals) %>%
  spread(key = user_type, value = num_of_rentals) %>%
  mutate(Total = Customer + Subscriber) %>%
  gather(Customer:Total , key = "user_type", value = "num_of_rentals") 

# change the order of seaons to summer, fall, winter for plotting purpose
season_ride_bd_num$season = factor(season_ride_bd_num$season)
lev_bd = levels(season_ride_bd_num$season)
lev_bd = lev_bd[c(2,1,3)]
season_ride_bd_num$season = factor(season_ride_bd_num$season, levels = lev_bd)

ggplot(season_ride_bd_num, aes(season, y = num_of_rentals, 
                         group=user_type,
                         col = user_type)) +
  geom_line() + 
  xlab("") +
  ylab("Number of bike rentals") +
  ggtitle("Number of bike rentals by seasons and user types") +
  scale_y_continuous(name = "Number of bike rentals (000's)",
                     breaks = seq(50000, 300000, by=50000), 
                     label = seq(50, 300, by=50)) +
  theme_bw()



## plot avg ride time per rental in each season

season_avg_time = season_ride_bd %>%
  select(season, user_type, avg_ride_time)%>%
  spread(key = user_type, value = avg_ride_time)%>%
  gather(Customer:Subscriber , key = "user_type", value = "avg_ride_time") 

# create a "Total" user avg ride time directly from raw data
# then stack to the dataframe season_avg_time

season_avg_time_total = data %>%
  mutate(month = month(start_time)) %>% 
  mutate(season = ifelse(month >=7 & month <=9,
                         "summer",
                         ifelse(month >=10 & month <=12,
                                "fall",
                                "winter")),
         user_type = "Total")  %>% 
  group_by(season, user_type) %>%
  summarise(ride_time = sum(duration_sec) , num_of_rentals = n()) %>%
  mutate(avg_ride_time = ride_time/num_of_rentals) %>%
  select(season, user_type, avg_ride_time)

season_avg_time_total$season = factor(season_avg_time_total$season)

# stack 2 tables together

season_avg_time = rbind(season_avg_time ,season_avg_time_total)

# change the order of seaons to summer, fall, winter for plotting purpose
season_avg_time$season = factor(season_avg_time$season)
lev_time = levels(season_avg_time$season)
lev_time = lev_time[c(2,1,3)]
season_avg_time$season = factor(season_avg_time$season, levels = lev_time)
season_avg_time$user_type = factor(season_avg_time$user_type)

# finally plot the avg ride time per rental in each season
ggplot(season_avg_time, aes(season, y = avg_ride_time/60, 
                               group=user_type,
                               col = user_type)) +
  geom_line() + 
  xlab("") +
  ylab("Ride time (min)") +
  ggtitle("Average ride time per rental by seasons and user types") +
  theme_bw()


