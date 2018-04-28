# load data:
table07 = read.csv("2017-fordgobike-tripdata.csv")
table0801 = read.csv("201801_fordgobike_tripdata.csv")
table0802 = read.csv("201801_fordgobike_tripdata.csv")
table0803 = read.csv("201801_fordgobike_tripdata.csv")

# append data together into 1 table:
data = rbind(table07,table0801,table0802,table0803)

# save file to .csv
write.csv(data, file = "ford_bike_data.csv")


## Heatmap

library(lubridate)
library(dplyr)
library(ggplot2)

data$start_time = ymd_hms(data$start_time)

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
  ggtitle("Heatmap of Ford GoBike Day V.S. Hour") +
  theme(legend.position="none")

----
## test 