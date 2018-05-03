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

# the busiest times are at 8am and 5pm


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
