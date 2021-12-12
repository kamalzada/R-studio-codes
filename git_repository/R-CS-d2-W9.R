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
library(readxl)

df <- read_xlsx('MO14-Round-1-Dealing-With-Data-Workbook.xlsx',
            na='', sheet=2, col_names=F)
colnames(df) <- 'col'
df %>% view()

df$col = gsub('_', '', df$col)
df$col = gsub(' ', '', df$col)
df %>% view()

df$hour <- substr(df$col, 1, 4)
df$hour = gsub('[0-9]$', '', df$hour)
df$hour = gsub('F', '', df$hour)
df$hour = gsub('S', '', df$hour)
df$hour = gsub('T', '', df$hour)
df$hour = gsub('W', '', df$hour)
df$hour = gsub('MM', 'M', df$hour)
df %>% view()

df$col = gsub('^[0-9]', '', df$col)
df$col = gsub('^[0-9]', '', df$col)
df %>% view()

for (i in 1:15){
  df$col = gsub('^[A-z]', '', df$col)
}

df$col = gsub('rd', '', df$col)
df$col = gsub('nd', '', df$col)
df$col = gsub('th', '', df$col)
df$col = gsub('st', '', df$col)

df$date <- substr(df$col, 1, 11)

df$date = gsub('0$', '', df$date)
df$date = gsub('1$', '', df$date)
df$date = gsub('2$', '', df$date)
df$date = gsub('3$', '', df$date)
df$date = gsub('44', '4', df$date)
df$date = gsub('5$', '', df$date)
df$date = gsub('6$', '', df$date)
df$date = gsub('7$', '', df$date)
df$date = gsub('8$', '', df$date)
df$date = gsub('9$', '', df$date)
df$date %>% view()

library(lubridate)
df$tarix <- dmy(df$date)
df %>% view()
df <- df %>% select(-date)
df$weekdays <-  weekdays(df$tarix)
df %>% view()

df$energy <- substr(df$col, nchar(df$col)-7, nchar(df$col))
df$col = gsub('2014', '', df$col)
df$col = gsub('^[0-9]', '', df$col)
df$col = gsub('-', '', df$col)
df$col = gsub('[A-z]', '', df$col)
df$col = gsub('kwh', '', df$col)
df$energy = gsub('kwh', '', df$col)
df %>% view()
df <- df %>% select(-col)
df <- df %>% rename(energy_kwh = energy) 
df$energy_kwh <- as.character(df$energy_kwh)
df %>% view()















