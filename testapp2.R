#shinytesting

library(DT)
library(dplyr)
library(tidyr)
library(shiny)
library(shinythemes)
library(shinyjs)

#try to add password protection
# Define the correct username and password
correct_username <- "user"
correct_password <- "elmo"

#load data

data <- read.csv("fulldata2.csv")
data = subset(data, select = -c(X))


#UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                shinyjs::useShinyjs(),
                div(id = "login_page",
                    textInput("username", "Username"),
                    passwordInput("password", "Password"),
                    actionButton("login_button", "Login")
                ),
                hidden(
                div(id = "main_page",
                titlePanel("Local Climate News Article Database"),
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
                )
)

server <- function(input, output, session) {
  observeEvent(input$login_button, {
    if (input$username == correct_username && input$password == correct_password) {
      shinyjs::hide("login_page")
      shinyjs::show("main_page")
    } else {
      showModal(modalDialog(
        title = "Error",
        "Incorrect username or password. Please try again.",
        easyClose = TRUE
      ))
    }
  })
}

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



