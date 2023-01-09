#shinytesting

library(DT)
library(dplyr)
library(tidyr)
library(shiny)
library(shinythemes)



ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Local Climate News Article Database"),
                p("A place to find articles written by 1Earth-funded journalists"),
                DTOutput("dynamic")
)
server <- function(input, output, session) {
  data <- read.csv("fulldata.csv")
  data = subset(data, select = -c(X))
  output$dynamic <- renderDT(data, options = list(pageLength = 10, autoWidth=TRUE),
                                    filter='top', escape=FALSE)
}

shinyApp(ui, server)

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("")
                tabsetPanel(
                  tabPanel("Home", #MainPage ####
                           titlePanel("Environmental Enforcement Reports"),
                           sidebarLayout(
                             sidebarPanel(
                               p("Looking for a report? Start here (below) or on the map (right)"),
                               varSelectInput("region", label= "What geography are you interested in?",
                                              geography),
                               selectInput("specific", label= "Select a geography",
                                           choices=NULL),
                               textOutput("result"),
                               uiOutput("link")
                             ),
                             mainPanel(
                               leafletOutput("map")
                             )
                           )
                  ),
                  tabPanel("Make a Report", #Report-Making Page ####
                           h4(textOutput("makereport")),
                           actionButton("yes", "Yes!"),
                           br(),
                           br(),
                           h5("No, I'd like to choose another place. (Select below)"),
                           varSelectInput("region2", label= "What geography are you interested in?",
                                          geography),
                           selectInput("specific2", label= "Select a geography",
                                       choices=NULL),
                           imageOutput("kermit"),
                           textOutput("answerno")
                  ),
                  tabPanel("About this Project", #About Page ####
                           p("This app isn't done yet so I don't have much to share here")
                  )
                )
)