#shinytesting

library(DT)
library(dplyr)
library(tidyr)
library(shiny)
library(shinythemes)

#load data

data <- read.csv("fulldata_4.csv")
data = subset(data, select = -c(X))

  
#UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Local Climate News Article Database"),
                p("A place to find articles from journalists in the U.S. Southeast that focus on issues of climate and the environment.This database currently contains articles from the following journalists:"),
                em("-Liz McLaughlin, Adam Wagner, John Deem, Gareth McGrath, David Boraks, Emily Jones,Drew Kann, Marisa Mecke"),
                br(),
                br(),
                p(HTML("To search within a <strong><em>date range</em></strong>, use the widget below. To filter by <strong><em>Author</em></strong>, use the search bar above the 'Key Author' column. 
                   To search via <strong><em>keyword</em></strong>, use the global search bar to the lower right.
                  The button below will download a .csv of the table with the filters you have selected", align="center")),
                fluidRow(
                  column(6,wellPanel(
                    dateRangeInput('dateRange',
                                   label = 'Filter Articles by Date',
                                   start = as.Date('2019-01-01') , end = as.Date('2023-06-01')
                    )
                  )),
                  column(6,downloadButton('downLoadFilter',"Download the filtered data"))),
                  
               fluidRow(
                        DTOutput("dynamic",))
)

#SERVER
server <- function(input, output, session) {
  output$dynamic <- renderDT(
    data  %>% filter(Date.Published >= input$dateRange[1] & Date.Published <= input$dateRange[2]),
    options = list(pageLength = 10, autoWidth=TRUE),
    filter='top', escape=FALSE)
  
  thedata <- reactive(data)
  
  
  output$downLoadFilter <- downloadHandler(
    filename = function() {
      paste('Filtered data-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(thedata()[input[["dynamic_rows_all"]], ],file)
    }
  )
    
}

shinyApp(ui, server)



