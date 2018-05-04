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


### Find the most busiest stations at 8am and 5pm on "one" weekday
### Cautious: must divided by the number of days to find the average delta of a station at 8am of 1 weekday

## find out 

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
# intentionally creating this intermediate dataframe to double check


weekday_8am = weekday_8am_int %>%
  group_by(day, hour, start_station_name, end_station_name) %>%
  summarise(outflow_count = sum(ifelse
                                (start_hour == 8, 
                                  outflow, 
                                  0)),
            inflow_count = sum(ifelse
                               (end_hour == 8, 
                                 inflow, 
                                 0))) 

# %>%
#  mutate(delta = inflow_count - outflow_count)

weekday_8am$delta = weekday_8am$inflow_count - weekday_8am$outflow_count

weekday_8am = weekday_8am %>%
  group_by(day, hour, start_station_name) %>%
  summarize(delta = sum(delta)) %>%
  arrange(delta) 
# checked : highest delta  happens on Wednesday, s
# 1) San Francisco Ferry Building (Harry Bridges Plaza), Wednesday 
# 2) San Francisco Caltrain Station 2 (Townsend St at 4th St), Wednesday
# 3) Berry St at 4th St, Tuesday




mutate(outflow_count = i)

############### new ideas ##############
# renters during weekdays are usually subscriber; weekends most one-time
# -> 2 different customer segments
#


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

<<<<<<< HEAD
### Andy's Codes 05/02 23:08PM

### 1. Mapping out the top 10 tourist attractions and existing Ford GoBike stations at SF to identify if there're
### potential areas to set up new stations.

library(maps)
library(ggplot2)
library(dplyr)
library(ggmap)


table07 = read.csv("2017-fordgobike-tripdata.csv")
table0801 = read.csv("201801_fordgobike_tripdata.csv")
table0802 = read.csv("201802_fordgobike_tripdata.csv")
table0803 = read.csv("201803_fordgobike_tripdata.csv")

data = rbind(table07,table0801,table0802,table0803)
write.csv(data, file = "ford_bike_data.csv")

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

###

library(maps)
library(ggplot2)
library(dplyr)
library(ggmap)
library(lubridate)

table07 = read.csv("2017-fordgobike-tripdata.csv")
table0801 = read.csv("201801_fordgobike_tripdata.csv")
table0802 = read.csv("201802_fordgobike_tripdata.csv")
table0803 = read.csv("201803_fordgobike_tripdata.csv")

data = rbind(table07,table0801,table0802,table0803)
write.csv(data, file = "ford_bike_data.csv")
SF_Map=qmap("San Franciso city", zoom=12)

### Maintenance plan
## Plotting out the condition of the bikes by the end of the 1st quarter of 2018 by following segments:
## 1. Usage hour > 60 hours --> Not recommend to use
## 2. Usage hour between 40 and 60 hours --> Acceptable
## 3. Usage hour < 40 hours --> Recommend to use

data$start_time = ymd_hms(data$start_time)
data$end_time = ymd_hms(data$end_time)

bike_usage_q1=data%>%
  filter(end_time>="2018-01-01" & end_time<="2018-03-31")%>%
  select(duration_sec,bike_id)%>% 
  group_by(bike_id)%>%
  summarize(usage_hour=sum(duration_sec)/3600)


bike_usage_q1=mutate(bike_usage_q1,Condition=ifelse(usage_hour>60, "Not recommend to use",
                                                    ifelse(usage_hour>=40 & usage_hour<= 60, "Acceptable",
                                                           ifelse(usage_hour<40, "Recommend to use","NA"))))

## Chart- condition of the bikes by the end of Q1, 2018
ggplot(bike_usage_q1,aes(Condition))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25)+
  scale_y_continuous(labels=scales::percent)+
  labs(title = "Condition of the bikes by the end of Q1, 2018",y = "Percent", x = "Condition")

## The station of the bikes that are not recommended to use
#Pending...still thinking how to do it...

