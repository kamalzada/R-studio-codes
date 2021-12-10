library(vioplot)
library(tidyverse)
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
library(skimr)
library(inspectdf)
library(naniar)
library(DataExplorer)
library(highcharter)
library(hrbrthemes) 
library(plotly)

# Question_1

df <- fread('Superstore_data.csv')
df %>%  glimpse()
df$`Order Date` <- as.Date(df$`Order Date`) 
df$`Ship Date` <- as.Date(df$`Ship Date`) 

# Question_2

colnames(df) <- gsub(' ', '_', colnames(df)) 
df %>% names()

# Question_3

df %>% ggplot(aes(Sales,Profit)) + geom_point() 

# Question_4

df %>% ggplot(aes(Sales,Profit)) + geom_point() + 
  geom_smooth(se=FALSE)

# Question_5

df %>% ggplot(aes(Sales,Profit)) + geom_point(aes(color=Segment, size=Discount)) + 
  geom_smooth(se=FALSE)

# Question_6

df %>% ggplot(aes(Sales,Profit)) + geom_point(aes(color=Region)) + 
  facet_grid(Category~. , scales='free')

# Question_7

df %>% ggplot(aes(Sales)) + geom_histogram(aes(fill=Category), bins=30)+
scale_x_continuous(limit=c(10,4000))

# Question_8

df %>% ggplot(aes(Sales, Profit)) + geom_boxplot()
p <- df %>% ggplot(aes(Sales)) + geom_boxplot()
s <- df %>% ggplot(aes(Profit)) + geom_boxplot()
p+s

# Question_9

df %>% ggplot(aes(Quantity)) + geom_histogram(aes(fill=Ship_Mode), bins=30) +
  facet_grid(Category~. , scales='free')

# Question_10

df %>% ggplot(aes(Sales, Profit)) + geom_violin()

# Question_11

df %>% ggplot(aes(Sales, Profit)) + geom_violin() +
  labs(title='Shipping by Product Category')








