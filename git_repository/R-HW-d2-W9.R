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

# these two appear to be numeric but have very few unique values
# so we'll fill na values with mode and the rest with median
df$WorkLifeBalance %>% unique() 
df$EnvironmentSatisfaction %>% unique()

for (col in colnames(df)){
  if 
}

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



# Question_7

data %>% mutate(experience=case_when(time_spend_company>3 ~ 'Experienced',
                          time_spend_company<=3 ~ 'Inexperienced')) %>% view()

# Question_8
data %>% group_by(Department) %>% summarise(number=sum(left)) %>% 
filter(number==max(number))

# Question_9

data %>% glimpse()
lr <- data %>% group_by(salary) %>% summarise(say=sum(left)) %>% as.data.frame()
ggplot(data=lr, aes(x=salary, y=say)) + geom_bar(stat='identity', fill='blue')  

# Question_10

data %>% filter(time_spend_company>3, last_evaluation>0.72,
                number_project>4, left==1) %>% view()

















