#shinytesting

library(DT)
library(dplyr)
library(tidyr)
library(shiny)
library(shinythemes)



ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Local Climate News Article Database"),
                p("A place to find articles written by 1Earth journalists.This database currently contains articles from the following journalists:"),
                em("-Liz McLaughlin, Adam Wagner, John Deem, Gareth McGrath, David Boraks, Emily Jones,Drew Kann, Marisa Mecke"),
                h4("To search within a date range, use the widget below. To filter by Author, use the search bar above the 'Key Author' column. To search via key word, use the global search bar to the right."),
                fluidRow(
                  column(4,wellPanel(
                    dateRangeInput('dateRange',
                                   label = 'Filter Articles by Date',
                                   start = as.Date('2019-01-01') , end = as.Date('2023-06-01')
                    )
                  ))),
               fluidRow(DTOutput("dynamic",))
)
server <- function(input, output, session) {
  data <- read.csv("fulldata2.csv")
  data = subset(data, select = -c(X))
  output$dynamic <- renderDT(
    data  %>% filter(Date.Published >= input$dateRange[1] & Date.Published <= input$dateRange[2]),
    options = list(pageLength = 10, autoWidth=TRUE),
    filter='top', escape=FALSE)
    
}

shinyApp(ui, server)


data  %>% filter(`Date Published` >= input$dateRange[1] & `Date Published` <= input$dateRange[2]),



data,options = list(pageLength = 10, autoWidth=TRUE),
filter='top', escape=FALSE)
