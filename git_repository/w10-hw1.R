library(shiny)
library(tidyverse)
library(tidycharts)
library(tidyr)
library(dplyr)
library(data.table)
library(patchwork)
library(plotly)
library(shinythemes)
library(inspectdf)

# Question_1
df <- fread('Future-500.csv')

# Question_2-9
ui <- fluidPage(
  theme=shinytheme('cerulean'),
  titlePanel(title = "Exploratory Data Analysis"),
  textOutput("text"),
  verbatimTextOutput("code"),
  tableOutput("static"), 
  selectInput ("type", label = "Which type of variable?", choices = c("Numeric", "Character")),
  tableOutput("dynamic"), 
  plotOutput("plot_na"),
  plotOutput("plot_cor"),
  plotOutput('bar'),
  numericInput('n', 'Number of Samples',1, min=1, max=200),
  actionButton('b', 'Ok'),
  tableOutput('l'),

  verbatimTextOutput("summary") 
)
server <- function(input, output, session) {
  output$text <- renderText({"Glimpse of Future-500 data:"})
  output$static <- renderTable(df %>% glimpse() %>% head()) 
  output$dynamic = renderUI({
    if (input$type=='Numeric'){
      renderTable(df %>% skimr::skim() %>% filter(skim_type=='numeric'))
    }
    else
      renderTable(df %>% skimr::skim() %>% filter(skim_type=='character'))
    })
  output$plot_na <- renderPlot(
    plot(df %>% inspect_na() %>% show_plot()))
  output$plot_cor <- renderPlot(
    plot(df %>% inspect_cor() %>% show_plot()))
  output$bar <- renderPlot(
    plot(df %>% group_by(Inception, Industry) %>% summarise(total=sum(Profit)) %>% 
           ggplot(aes(Inception, total)) + geom_bar(stat='Identity', position = position_dodge(width = 0.8), binwidth = 25) + 
           facet_wrap(~Industry) + scale_y_continuous() + scale_x_continuous() + 
           labs(title='Total Profit per Industry throghout the years') + xlab('years') + ylab('total profit') )
    )
  data <- eventReactive(input$b, { 
    df %>% group_by(Inception, Industry) %>% summarise(total=sum(Profit)) %>% arrange(desc(total)) %>% head(input$n)
    })
  output$l <- renderTable({
    data()
    }
    
  )  
  
}  
shinyApp(ui, server)    


  
         