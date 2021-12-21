library(tidyverse)
library(skimr)
library(tidyr)
library(dplyr)
library(inspectdf)
library(caret)
library(glue)
library(highcharter)
library(h2o)
library(scorecard)
library(data.table)
library(qdapRegex)

raw <- fread('Churn_Modelling (1).csv')
raw %>% glimpse()
df <- copy(raw)
df %>% view()

df <- df %>% select(-RowNumber, -CustomerId, -Surname, -Geography)
df %>% describe()
df %>% view()
names(df) <- tolower(names(df)) 
sapply(df, function(x) length(unique(x))) 

# -- 0 value forms over 30% of the overall values in the Balance column
df %>% group_by(balance) %>% summarise(say = n()) %>% head()
( df$balance %>% table() %>% max() ) / ( df %>% count() ) # 36%

df <- df %>% rename(target=exited)
colSums(is.na(df)) # no null values

df$gender <-  df$gender %>% as.factor() %>% as.integer() 
class(df$gender)
df$gender %>% table()
df %>% describe()

# --- let's find the number of duplicates
df %>% duplicated() %>% sum()

# --- let's handle the outliers and scale our dataset
df_numeric <- df %>% select(age, estimatedsalary, creditscore, balance)
df_categoric <- df %>% select(-age, -estimatedsalary, -creditscore, -balance, -target)
df_numeric %>% head()
df_categoric %>% head() 

num_vars <- colnames(df_numeric)
for_vars <- c()
for (b in 1:length(num_vars)) {
  OutVals <- boxplot(df_numeric[[num_vars[b]]], plot=F)$out
  if(length(OutVals)>0){
    for_vars[b] <- num_vars[b]
  }
}
for_vars <- for_vars %>% as.data.frame() %>% drop_na() %>% pull(.) %>% as.character()
for_vars %>% length()

for (o in for_vars) {
  OutVals <- boxplot(df_numeric[[o]], plot=F)$out
  mean <- mean(df_numeric[[o]],na.rm=T)
  
  o3 <- ifelse(OutVals>mean,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]
  o1 <- ifelse(OutVals<mean,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]
  
  val3 <- quantile(df_numeric[[o]],0.75,na.rm = T) + 1.5*IQR(df_numeric[[o]],na.rm = T)
  df_numeric[which(df_numeric[[o]] %in% o3),o] <- val3
  
  val1 <- quantile(df_numeric[[o]],0.25,na.rm = T) - 1.5*IQR(df_numeric[[o]],na.rm = T)
  df_numeric[which(df_numeric[[o]] %in% o1),o] <- val1
}

df <- cbind(df_numeric, df_categoric) %>% as.data.frame()
df <- scale(df)
df <- cbind(df, raw %>% select(Exited)) %>% as.data.frame()
df <- df %>% rename(target=Exited)
df %>% view()
features <- df %>% select(-target) %>% names()
target <- df %>% select(target) %>% names() 

# --- Building Logistical Regression model 
h2o.init() 
df_h2o <- df %>% as.h2o()
df_h2o <- df_h2o %>% h2o.splitFrame(ratios = 0.85, seed = 123)
train <- df_h20[[1]]
valid <- df_h20[[2]]

model <- h2o.glm(
  x = features, y = target, family = "binomial", 
  training_frame = train, validation_frame = valid,
  nfolds = 10, seed = 123, remove_collinear_columns = T,
  balance_classes = T, lambda = 0, compute_p_values = T)

pred <- model %>% h2o.predict(newdata = valid) %>% 
  as.data.frame() %>% select(p1,predict) 

model %>%
  h2o.auc(train = T,
          valid = T,
          xval = T) %>%
  as_tibble() %>%
  round(2) %>%
  mutate(data = c('train','test','cross_val')) %>%
  mutate(gini = 2*value-1) %>%
  select(data,auc=value,gini)

#We observe that it did better on the test set
model %>% h2o.performance(newdata = valid) %>%
  h2o.find_threshold_by_max_metric('f1')

model %>% h2o.performance(newdata = train) %>%
  h2o.find_threshold_by_max_metric('f1')








