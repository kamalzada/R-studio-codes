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
library(h2o)
library(patchwork)

df = fread('crimes.csv')
colSums(is.na(df)) # no null values
df %>% inspect_na

# Question_1

target <- 'ViolentCrimesPerPop'
features <- df %>% select(-ViolentCrimesPerPop) %>% names()

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = df)

glm %>% summary()

coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
features <- features[!features %in% coef_na]

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = df)

glm %>% summary()

while(glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[1] >= 1.5){
  afterVIF <- glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[-1] %>% names()
  f <- as.formula(paste(target, paste(afterVIF, collapse = " + "), sep = " ~ "))
  glm <- glm(f, data = df)
}

glm %>% faraway::vif() %>% sort(decreasing = T) %>% names() -> features 

df <- df %>% select(ViolentCrimesPerPop,features)

# Question_2

df <- df %>% scale() %>% as.data.frame
df %>% view()

# Question_3

h2o.init()

h2o_data <- df %>% as.h2o()

h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.85, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- 'ViolentCrimesPerPop'
features <- df %>% select(-ViolentCrimesPerPop) %>% names()

# Question_4

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

y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred %>% glimpse()

# Question_5
residuals = test$ViolentCrimesPerPop - y_pred$predict
RMSE = sqrt(mean(residuals^2))
RMSE

y_test_mean = mean(test$ViolentCrimesPerPop)
y_test_mean

tss = sum((test$ViolentCrimesPerPop - y_test_mean)^2) 
tss
rss = sum(residuals^2) 
rss

# Question_6

y_pred_train <- model %>% h2o.predict(newdata = train) %>% as.data.frame()

train_set <- train %>% as.data.frame()
residuals = train_set$ViolentCrimesPerPop - y_pred_train$predict

RMSE_train = sqrt(mean(residuals^2))
y_train_mean = mean(train_set$ViolentCrimesPerPop)

tss = sum((train_set$ViolentCrimesPerPop - y_train_mean)^2)
rss = sum(residuals^2)

R2_train = 1 - (rss/tss); R2_train

n <- train_set %>% nrow() 
k <- features %>% length() 
Adjusted_R2_train = 1-(1-R2_train)*((n-1)/(n-k-1))
Adjusted_R2_train


























