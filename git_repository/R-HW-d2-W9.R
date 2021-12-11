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






















