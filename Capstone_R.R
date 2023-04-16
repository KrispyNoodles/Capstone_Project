
#installing packages not now
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("maps")
install.packages("ggthemes")
install.packages("ggmap")
install.packages("parsedate")
install.packages("chron")
install.packages("ggpmisc")
install.packages("png")
install.packages("grid")

#installing libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(ggthemes)
library(ggmap)
library(parsedate)
library(lubridate)
library(chron)
library(ggpmisc)
library(png)
library(grid)

#Process and Prepare stage
#Importing data using read.csv
month_1 <- read.csv("C:/Users/user/Desktop/Dataset of 1 year/202101-divvy-tripdata.csv")
month_2 <- read.csv("C:/Users/user/Desktop/Dataset of 1 year/202102-divvy-tripdata.csv")
month_3 <- read.csv("C:/Users/user/Desktop/Dataset of 1 year/202103-divvy-tripdata.csv")
month_4 <- read.csv("C:/Users/user/Desktop/Dataset of 1 year/202104-divvy-tripdata.csv")
month_5 <- read.csv("C:/Users/user/Desktop/Dataset of 1 year/202105-divvy-tripdata.csv")
month_6 <- read.csv("C:/Users/user/Desktop/Dataset of 1 year/202106-divvy-tripdata.csv")
month_7 <- read.csv("C:/Users/user/Desktop/Dataset of 1 year/202107-divvy-tripdata.csv")
month_8 <- read.csv("C:/Users/user/Desktop/Dataset of 1 year/202108-divvy-tripdata.csv")
month_9 <- read.csv("C:/Users/user/Desktop/Dataset of 1 year/202109-divvy-tripdata.csv")
month_10 <- read.csv("C:/Users/user/Desktop/Dataset of 1 year/202110-divvy-tripdata.csv")
month_11 <- read.csv("C:/Users/user/Desktop/Dataset of 1 year/202111-divvy-tripdata.csv")
month_12 <- read.csv("C:/Users/user/Desktop/Dataset of 1 year/202112-divvy-tripdata.csv")

#removing empty columns in month_11
month_11 <- month_11 %>%
  select(-(X))

#combination of all month into one data frame
total_month <- rbind(month_1, month_2, month_3, month_4, month_5, month_6, month_7, month_8, month_9, month_10, month_11, month_12)

#removing months after combining
remove(month_1, month_2, month_3, month_4, month_5, month_6, month_7, month_8, month_9, month_10, month_11, month_12)

#filtering out the empty spaces in these cells, tests bikes and invalid station ids that are less than 11 characters
total_month <- total_month %>%
  filter(start_station_name!="") %>% 
  filter(start_station_id!="") %>% 
  filter(end_station_name!="") %>%
  filter(started_at!="") %>%
  filter(end_station_id!="")

View(total_month)

#removing data that contains test rides
total_month <- total_month %>%
  filter(end_station_id != "Hubbard Bike-checking (LBS-WH-TEST)")

#removing data that invalid id in either start or end station
total_month <- total_month %>%
  filter(nchar(start_station_id) > 11) %>% 
  filter(nchar(end_station_id) > 11)

#removing duplicates
total_month <- total_month %>%
  distinct(ride_id, .keep_all = TRUE)

#sampling from the total data
#https://www.surveymonkey.com/mp/sample-size-calculator/ population size: 1,314,506, confidence level =99%, margin of error 1%
sample_month <- sample_n(total_month, size = 16433)

#separating the date and time to two separate columns
sample_month[c('started_at_date', 'started_at_time')] <- str_split_fixed(sample_month$started_at, ' ', 2)
sample_month[c('ended_at_date', 'ended_at_time')] <- str_split_fixed(sample_month$ended_at, ' ', 2)

#Code adapted from ChatGPT. adding ":00" to the back of all strings in the time columns that are 4 or 5 characters long
sample_month$ended_at_time <- ifelse(nchar(sample_month$ended_at_time) %in% c(4, 5), paste0(sample_month$ended_at_time, ":00"), sample_month$ended_at_time)
sample_month$started_at_time <- ifelse(nchar(sample_month$started_at_time) %in% c(4, 5), paste0(sample_month$started_at_time, ":00"), sample_month$started_at_time)

#adding a 0 in front of all strings that are 7 characters, such that "8:12:00" becomes "08:12:00"
sample_month$ended_at_time <- ifelse(nchar(sample_month $ended_at_time) %in% c(7), paste0("0", sample_month$ended_at_time), sample_month$ended_at_time)
sample_month$started_at_time <- ifelse(nchar(sample_month $started_at_time) %in% c(7), paste0("0", sample_month$started_at_time), sample_month$started_at_time)

#converting the dates into the correct ymd format
sample_month$ended_at_date <- parse_date_time(sample_month$ended_at_date, orders = c("dmy", "ymd"))
sample_month$ended_at_date <- format(sample_month$ended_at_date, format = "%Y-%m-%d")

#concatenating the date and time back to the original columns
sample_month$ended_at <- str_c(sample_month$ended_at_date, ' ', sample_month$ended_at_time)
sample_month$started_at <- str_c(sample_month$started_at_date, ' ', sample_month$started_at_time)

#converting to a date time data type
sample_month$ended_at <- as.POSIXct(sample_month$ended_at, format="%Y-%m-%d %H:%M:%S")
sample_month$started_at <- as.POSIXct(sample_month$started_at, format="%Y-%m-%d %H:%M:%S")

#creating a mins_rental column to count the time difference between the start and end timing of a rental
sample_month$mins_rental <- round(as.numeric(difftime(sample_month$ended_at, sample_month$started_at, units = "mins")),digits = 0)

#creating a weekday column that starts from Sunday
sample_month$weekday <- wday(sample_month$started_at_date, label=TRUE, abbr=FALSE)

#creating a month column
sample_month$month <- month(sample_month$started_at_date, label=TRUE) 

View(sample_month)
str(sample_month)

#Analyse Stage
#creation of two data frames that contains the member and casual rider data separately
member_data <- sample_month %>%
  filter(sample_month$member_casual == "member")

casual_data <- sample_month %>%
  filter(sample_month$member_casual == "casual")

#generation of the bar chart
ggplot(sample_month, aes(x=weekday, fill=member_casual)) +
  geom_bar() +
  scale_fill_manual(values = c("#004989", "#FD151B")) +
  labs(title="Total Cyclist On Weekdays", x = "Weekdays", y = "No. of riders",
       subtitle ="Total cyclist across the weekdays for the year 2021", 
       caption='Data collected by Motivate International Inc.') +
  annotate("rect", xmin=0.5, xmax=1.5, ymin=0, ymax=2579, alpha=0.2, color="black", fill="black") +
  annotate("rect", xmin=6.5, xmax=7.5, ymin=0, ymax=2897, alpha=0.2, color="black", fill="black") +
  geom_text(aes(label = ..count..), stat='count', position=position_stack(vjust = 0.5), colour = "white")

#generation of the bar chart
ggplot(sample_month, aes(x=month, fill=member_casual)) +
  geom_bar() +
  scale_fill_manual(values = c("#004989", "#FD151B")) +
  labs(title="Total Cyclist On Different Months", x = "Months", y = "No. of riders",
       subtitle ="Total cyclist across the months for the year 2021", 
       caption='Data collected by Motivate International Inc.') +
  geom_text(aes(label = ..count..), stat='count', position=position_stack(vjust = 0.5), colour = "white") +
  annotate("rect", xmin=4.5, xmax=10.5, ymin=0, ymax=2579, alpha=0.2, color="black", fill="black")

#calculation of mean and max
mean_max <- data.frame(Dataset = c("sample_month", "member_data", " casual_data "),
                      Mean = c(round(mean(sample_month$mins_rental), digits=2), round(mean(member_data$mins_rental), digits=2), round(mean(casual_data$mins_rental), digits=2)),
                      Max = c(max(sample_month$mins_rental), max(member_data$mins_rental), max(casual_data$mins_rental)))

#generation of the bar chart
ggplot(sample_month, aes(x=mins_rental, fill=member_casual)) +
  geom_bar() +
  scale_fill_manual(values = c("#004989", "#FD151B")) +
  labs(title="Total Rental Timing Of Cyclists", x = "Rental Time (mins)", y = "No. of riders",
       subtitle ="Samples of riders and their rental timing for the year 2021", 
       caption='Data collected by Motivate International Inc.') +
  xlim(0,250)+ ylim(0,800)+
  annotate("rect", xmin=50, xmax=250, ymin=0, ymax=100, alpha=0.2, color="black", fill="black") +
  annotate(geom = "table",
           x = 150,
           y = 600,
           label = list(mean_max))

#types of map type “terrain”, “terrain-background”, “terrain-labels”, “terrain-lines”, “toner”, “toner-2010”, “toner-2011”, 
#“toner-background”, “toner-hybrid”, “toner-labels”, “toner-lines”, “toner-lite”, “watercolor”

#creation of Chicago map with the assistance of Open Street Map)
Chicago_map <- get_stamenmap(
  bbox = c(left = -87.7336, bottom = 41.8042, right = -87.4640, top = 41.9200),
  maptype = "terrain-lines",
  zoom = 13
)

#creation of line_data for both member and casual riders separately
line_data_member <- data.frame(
  x = c(member_data$start_lng, member_data$end_lng),
  y = c(member_data$start_lat, member_data$end_lat)
)
line_data_casual <- data.frame(
  x = c(casual_data$start_lng, casual_data$end_lng),
  y = c(casual_data$start_lat, casual_data$end_lat)
)

#creating a value to read the north symbol image
arrow <- readPNG("C:\\Users\\user\\Downloads\\arrow.png")


#generation of the map plot
ggmap(Chicago_map) +
  geom_path(data = line_data_member, aes(x = x, y = y, color = "Member"), size = 0.01, alpha = 0.3) +
  geom_path(data = line_data_casual, aes(x = x, y = y, color = "Casual"), size = 0.01, alpha = 0.2) +
  labs(title = "Analysis Of Rider's Path", x = "Latitude", y = "Longtitude",
       subtitle ="Samples of riders starting and ending stations connected in Chicago for the year 2021", 
       caption='Data collected by Motivate International Inc.
       Graph by D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2.
       The R Journal, 5(1), 144-161. URL http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf') +
  geom_text(aes(x = -87.56, y = 41.875, label = "Chicago"), size = 8) +
  annotate("rect", xmin = -87.66, xmax = -87.61, ymin = 41.86 , ymax = 41.91, alpha=0.2, color="black", fill="black") +
  annotate("rect", xmin = -87.64, xmax = -87.59, ymin = 41.81 , ymax = 41.85, alpha=0.2, color="black", fill="black") +
  scale_color_manual(values = c("Member" = "#FD151B", "Casual" = "#004989"),
                     name = "Type") +
  coord_cartesian() +
  theme(legend.key.size = unit(1.5, "lines")) +
  annotation_custom(rasterGrob(arrow, interpolate = TRUE),
                    xmin = -87.45, xmax = -87.55, 
                    ymin = 41.825 , ymax = 41.85)

save(sample_month,file="s_data.Rda")
load("s_data.Rda")

plot(barchart1)
plot(barchart2)
plot(barchart3)
plot(map)
