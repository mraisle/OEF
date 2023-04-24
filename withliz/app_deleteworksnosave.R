library(DT)
library(dplyr)
library(tidyr)
library(shiny)
library(shinythemes)
library(shinyjs)

# Load data
data_file <- "fulldata_4.csv"
if (file.exists(data_file)) {
  data <- read.csv(data_file)
} else {
  data <- read.csv("data/fulldata_4.csv")
}

#data = subset(data, select = -c(X))

# UI
ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinytheme("cerulean"),
  titlePanel("Local Climate News Article Database"),
  p("A place to find articles from journalists in the U.S. Southeast that focus on issues of climate and the environment.This database currently contains articles from the following journalists:"),
  em("-Liz McLaughlin, Adam Wagner, John Deem, Gareth McGrath, David Boraks, Emily Jones,Drew Kann, Marisa Mecke"),
  br(),
  br(),
  p(HTML("To search within a <strong><em>date range</em></strong>, use the widget below. To filter by <strong><em>Author</em></strong>, use the search bar above the 'Key Author' column. 
                   To search via <strong><em>keyword</em></strong>, use the global search bar to the lower right.
                  The button below will download a .csv of the table with the filters you have selected", align = "center")),
  fluidRow(
    column(6, wellPanel(
      dateRangeInput('dateRange',
                     label = 'Filter Articles by Date',
                     start = as.Date('2019-01-01'), end = as.Date('2023-06-01')
      )
    )),
    column(6, downloadButton('downLoadFilter', "Download the filtered data"))
  ),
  
  fluidRow(
    column(6, actionButton("delete_rows", "Delete Selected Rows")),
    column(6, actionButton("save_changes", "Save Changes"))
  ),
  
  fluidRow(
    DTOutput("dynamic")
  )
)

# SERVER
server <- function(input, output, session) {
  saved_data_file <- "saved_fulldata_4.csv"
  
  # Load saved data or the original data if no saved data exists
  if (file.exists(saved_data_file)) {
    initial_data <- read.csv(saved_data_file)
  } else {
    initial_data <- data
  }
  
  thedata <- reactiveVal(initial_data)
  
  
  output$dynamic <- renderDT(
    datatable(thedata() %>% filter(Date.Published >= input$dateRange[1] & Date.Published <= input$dateRange[2]),
              options = list(pageLength = 10, autoWidth = TRUE),
              filter = 'top', escape = FALSE,
              editable = list(target = "cell", disable = list(columns = c(0)))
    )
  )
  
  observeEvent(input$delete_rows, {
    selected_rows <- input$dynamic_rows_selected
    if (!is.null(selected_rows)) {
      new_data <- thedata()[-selected_rows, ]
      thedata(new_data)
    }
  })
  
  observeEvent(input$save_changes, {
    write.csv(thedata(), data_file, row.names = FALSE)
  })
  
  proxy <- dataTableProxy("dynamic")
  
  observeEvent(input$dynamic_cell_edit, {
    info <- input$dynamic_cell_edit
    str(info)
    i <- info$row
    j <- info$col + 1
    v <- info$value
    thedata()[i, j] <<- DT::coerceValue(v, thedata()[i, j])
    replaceData(proxy, thedata(), resetPaging = FALSE)
  })
  
  output$downLoadFilter <- downloadHandler(
    filename = function() {
      paste('Filtered data-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      filtered_data <- thedata() %>%
        filter(Date.Published >= input$dateRange[1] & Date.Published <= input$dateRange[2]) %>%
        .[input[["dynamic_rows_all"]], ]
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
  # Save data to a file when the session ends
  session$onSessionEnded(function() {
    write.csv(thedata(), saved_data_file, row.names = FALSE)
  })
  
  # Load saved data when a new session starts
  observe({
    if (file.exists(saved_data_file)) {
      thedata(read.csv(saved_data_file))
    } else {
      thedata(data)
    }
  })
}

shinyApp(ui, server)

    
  