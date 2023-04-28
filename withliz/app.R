library(DT)
library(dplyr)
library(tidyr)
library(shiny)
library(shinythemes)
library(shinyjs)
library(RSQLite)
library(DBI)

# Load data
#data_file <- "fulldata_April2023.csv"
#if (file.exists(data_file)) {
 # data <- read.csv(data_file)
#} else {
 # data <- read.csv("fulldata_April2023.csv")
#}

#data = subset(data, select = -c(X, X.1))

# Create a SQLite database to store the table data
con <- dbConnect(SQLite(), "table_data.sqlite")

if (!dbExistsTable(con, "data_table")) {
  data_file <- "fulldata_April2023.csv"
  data <- read.csv(data_file)
  data = subset(data, select = -c(X, X.1, All.Authors))
  dbWriteTable(con, "data_table", data, overwrite = TRUE)
}

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
  thedata <- reactiveVal()
  
  # Load data from the SQLite database
  observe({
    data_from_db <- dbReadTable(con, "data_table")
    thedata(data_from_db)
  })
  
  
  output$dynamic <- renderDT(
    datatable(
      thedata() %>% filter((is.na(Date.Published)) | (Date.Published >= input$dateRange[1] & Date.Published <= input$dateRange[2])),
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
        filter((is.na(Date.Published)) | (Date.Published >= input$dateRange[1] & Date.Published <= input$dateRange[2])) %>%
        .[input[["dynamic_rows_all"]], ]
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
  # Save data to the SQLite database every 60 seconds
  autoSaveInterval <- 30 * 1000 # 30 seconds in milliseconds
  autoSave <- reactiveTimer(autoSaveInterval)
  observe({
    autoSave()
    dbWriteTable(con, "data_table", thedata(), overwrite = TRUE)
  })
}

shinyApp(ui, server)

    
  