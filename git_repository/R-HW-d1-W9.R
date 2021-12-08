library(tidyverse)
install.packages('tidycharts')
library(tidycharts)
library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(patchwork)
library(ggside)
library(highcharter)
library(plotly)
library(data.table)
library(glue)
library(rlang)
library(skimr)
library(inspectdf)
library(naniar)
library(DataExplorer)
library(lares)
library(highcharter)
library(dlookr)
library(esquisse)
library(hrbrthemes) 
library(plotly) 
library(tidyquant)

# Question_1

data1 <- fread('Airlines.csv')
data2 <- fread('Airports.csv')
data3 <- fread('Flights.csv')

# Question_2

flight_airport <- merge(data3,data2,
                        by.x = 'flights_ID', by.y = 'ID',
                        all.x=F)
flight_airport %>% view()

flight_airport_airline <- merge(flight_airport, data1,
                                by.x = 'AIRLINE', by.y = 'AIRLINE',
                                all.x=T)

flight_airport_airline %>% view()
df <- copy(flight_airport_airline) 
df %>% view()

# Question_3

df$AIR_TIME %>% unique(na.rm=T)

# Question_4

data3 %>% arrange(DEPARTURE_TIME) %>% arrange(desc(DEPARTURE_DELAY))

# Question_5

data3 %>% view()
data3 %>% transmute(at_ad = paste(ARRIVAL_TIME, '<->', ARRIVAL_DELAY)) %>% head(5)

# Question_6

data3 %>% ggplot(aes(FLIGHT_NUMBER)) +
  geom_histogram(color= ft_cols$red, fill='Blue') + stat_bin(bins=500) +
  labs(title='Flight number distribution',
       x = 'Flight number',
       y = 'Say') + theme_modern_rc()

# Question_7

data3 %>% ggplot(aes(FLIGHT_NUMBER)) + geom_bar(aes(fill=AIRLINE),
                                                position=position_dodge())

# Question_8

data3 %>% ggplot(aes(SCHEDULED_DEPARTURE, DEPARTURE_TIME)) + 
  geom_point(na.rm=T, alpha=10, aes(color=AIRLINE)) + 
  labs(x = 'Scheduled Departure',
       y = 'Departure Time',
       title = 'Scatterplot')

# Question_9

data3 %>% ggplot() + geom_miss_point(aes(ARRIVAL_DELAY, ARRIVAL_TIME), na.rm=F) +
  scale_colour_manual(name = "Flights data", values = c("blue", "green"))   










































