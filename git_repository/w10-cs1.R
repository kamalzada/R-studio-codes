library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(inspectdf)
library(skimr)
library(tidyverse) 
library(data.table)
library(mice)
library(plotly)
library(graphics) 
library(shinythemes)

injuries <- fread("injuries.csv")
products <- fread("products.csv")
population <- fread("population.csv")

injuries %>% inspect_na()
products %>% inspect_na()
population %>% inspect_na()

injuries$prod_code %>% 
  as.factor() %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  .[1,1] -> pc

products$title[products$prod_code == pc]
stair_step <- injuries %>% filter(prod_code == 1842)

#1st analysis
stair_step %>% count(diag, sort = T) 

#2nd analysis
stair_step %>% count(body_part, sort = T) 

#3rd analysis
stair_step %>% count(location, sort = T) 

#4th analysis
merge(stair_step %>% count(age, sex), population, 
        by = c("age", "sex"), 
        all.x = T) %>% 
mutate(rate = n / population * 10000) %>% 
ggplot(aes(age, rate, colour = sex)) + 
geom_line() + 
labs(y = "Injuries per 10,000 people") 

#User Interface

ui <- fluidPage(titlePanel('Case Study Web App'), 
                theme=shinytheme('cyborg'),
                
                fluidRow(
                  column(10, selectInput('code', label='Product', products$title))
                ),
                fluidRow(
                  column(4, tableOutput('diag')),
                  column(4, tableOutput('body_part')),
                  column(4, tableOutput('location'))
                  ),
                fluidRow(
                  column(12, plotOutput('age_sex'))
                )
                )

#Server 

server <- function(input, output, session){
  data <- reactive(merge(injuries, products,
                         by='prod_code', all.x=T) %>% filter(title==input$code))
  output$diag <- renderTable(data() %>% count(diag, sort=T))
  output$body_part <- renderTable(data() %>% count(body_part, sort=T))
  output$location <- renderTable(data() %>% count(location, sort=T))
  output$age_sex <- renderPlot({
    
    merge(data() %>% count(age, sex), population, 
          by = c("age", "sex"), 
          all.x = T) %>% 
      mutate(rate = n / population * 10000) %>% 
      ggplot(aes(age, n, colour = sex)) + 
      geom_line() + 
      labs(y = "Injuries per 10,000 people") 
  }, res=96)

  }

shinyApp(ui, server)
