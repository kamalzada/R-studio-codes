library(tidyverse) 
library(data.table)
library(rstudioapi)
library(skimr)
library(inspectdf)
library(mice)
library(plotly)
library(highcharter)
library(recipes) 
library(caret) 
library(purrr) 
library(graphics) 
library(Hmisc) 
library(glue)

df <- ggplot2::mpg %>% as.data.frame()
df %>% view()
colSums(is.na(df)) # no null values exist
df %>% inspect_na()
df %>% skim()

# this variable is numeric but not continuous and has very few unique values, so we consider it categorical
df$cyl %>% unique() 

df_num <- df %>% select_if(is.numeric) %>% filter(cyl)
df_num %>% view()
df_cat <- df %>% select_if(is.character)
df_cat %>% view()

for (col in 1:ncol(df_cat)){
  df_cat[[col]] <- as.numeric(as.factor(df_cat[[col]]))
}
df_cat <- cbind(cyl=df$cyl, df_cat) %>% as.data.frame()

df <- cbind(df_num, df_cat) %>% as.data.frame()
df <- df %>% scale()
df %>% view()

h2o.init()
h2o_data <- df %>% as.h2o()
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.85, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]
df %>% names()
target <- 'Life_expectancy'
features <- df %>% select(-Life_expectancy) %>% names()

model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)

model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))







