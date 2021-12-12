library(vioplot)
library(tidyverse)
library(tidycharts)
library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(patchwork)
library(ggside)
library(plotly)
library(data.table)
library(DataExplorer)
library(hrbrthemes) 
library(plotly)  

# Question_1

data1 <- fread('manager survey data.csv')
data2 <- fread('employee survey data.csv')
data3 <- fread('general data.csv')

general_employee <- merge(data3, data2, 
                          by.x = 'EmployeeID', by.y = 'EmployeeID',
                          all.x = T)

df <- merge(general_employee, data1, 
            by.x = 'EmployeeID', by.y = 'EmployeeID',
            all.x = T)

df %>% view()
colSums(is.na(df)) # identifying number of null values each column has
is.na(df) %>% sum() # total number of null values
is.na(df) %>% which() # null values' positions
df[!complete.cases(df)] %>% view() # rows that contain at least 1 na value

# Question_2

df$EmployeeID <- as.factor(df$EmployeeID)
class(df$EmployeeID)

# Question_3

df %>% view()
colSums(is.na(df)) %>% as.data.frame() %>% filter(.>0) # columns containing na values
df %>% select(MonthlyIncome, NumCompaniesWorked, PercentSalaryHike, TotalWorkingYears ,      
              YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrMager, EnvironmentSatisfaction,
              JobSatisfaction, WorkLifeBalance) %>% head() 

for(i in 1:ncol(df)){
  df[is.na(df[[i]]), i] <- median(df[[i]], na.rm = TRUE)
}
colSums(is.na(df)) # no column has null values now

# Question_4

data <- fread('HR.csv')
data %>% view()

# Question_5

unique_values <- vector()
for (col in colnames(data)){
  unique_values <- append(unique_values, length(unique(data[[col]])))
}
column_unique <- cbind(columns=colnames(data), unique_values) %>% as.data.frame()
column_unique %>% view()
data$left <-  as.factor(data$left)
data$Work_accident <-  as.factor(data$Work_accident)
data$promotion_last_5years <-  as.factor(data$promotion_last_5years)
data$salary <-  as.factor(data$salary)

# Question_6

for(i in 1:ncol(data)){
  data[is.na(data[[i]]), i] <- median(data[[i]], na.rm = TRUE)
}
colSums(is.na(data))

# Question_7

data %>% mutate(experience=case_when(time_spend_company>3 ~ 'Experienced',
                          time_spend_company<=3 ~ 'Inexperienced')) %>% view()

# Question_8
data$left <- as.numeric(data$left) # conversion back to numeric because factor type cannot be summed
data %>% group_by(Department) %>% summarise(say=sum(left)) %>% 
filter(say==max(say)) 

# Question_9

data %>% glimpse()
lr <- data %>% group_by(salary) %>% summarise(say=sum(left)) %>% as.data.frame()
ggplot(data=lr, aes(x=salary, y=say)) + geom_bar(stat='identity', fill='blue')  

# Question_10

data %>% filter(time_spend_company>3, last_evaluation>0.72,
                number_project>4, left==1) %>% view()

















