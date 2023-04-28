library(DT)
library(dplyr)
library(tidyr)
library(shiny)
library(shinythemes)
library(shinyjs)
library(RSQLite)
library(DBI)
library(shinymanager)
library(scrypt)

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

password <- scrypt::hashPassword("bolt4ever!")

# data.frame with credentials info
credentials <- data.frame(
  user = c("puffins", "meg", "diogo", "1earth"),
  password = password,
  is_hashed_password = TRUE,
  # comment = c("alsace", "auvergne", "bretagne"), %>% 
  stringsAsFactors = FALSE
)


# Create a SQLite database to store the table data
con <- dbConnect(SQLite(), "table_data.sqlite", autoconnect = TRUE)

if (!dbExistsTable(con, "data_table")) {
  data_file <- "fulldata_April2023.csv"
  data <- read.csv(data_file)
  data = subset(data, select = -c(X, X.1, All.Authors))
  dbWriteTable(con, "data_table", data, overwrite = TRUE)
}

# UI
ui <- secure_app(head_auth = tags$script(inactivity),
                 fluidPage(
                   tags$style(HTML("
  .custom-button:not(:disabled) {
    color: black !important;
    border-color: #F200FF !important;
  }
  .custom-button:hover {
    background-color: #F200FF !important;
  }
  .custom-button {
    display: block;
    margin: 0 auto;
  }
  .custom-button2:not(:disabled) {
    color: black !important;
    border-color: #00FF7F !important;
  }
  .custom-button2:hover {
    background-color: #00FF7F !important;
  }
  .custom-button2 {
    display: block;
    margin: 0 auto;
  }
")),
  shinyjs::useShinyjs(),
  theme = shinytheme("cerulean"),
  titlePanel("Local Climate News Article Database"),
  p("A place to find articles from journalists in the U.S. Southeast that focus on issues of climate and the environment.This database currently contains articles from the following journalists:"),
  em("-Liz McLaughlin, Adam Wagner, John Deem, Gareth McGrath, David Boraks, Emily Jones,Drew Kann, Marisa Mecke, Gautama Mehta, and Meris Lutz"),
  br(),
  br(),
  p(HTML("To search within a <strong><em>date range</em></strong>, use the widget below. To filter by <strong><em>Author</em></strong>, use the search bar above the 'Key Author' column. 
                   To search via <strong><em>keyword</em></strong>, use the global search bar to the lower right or the search bars above the 'Article Title' or 'Preview' columns.
                  The button below will download a .csv of the table with the filters you have selected.")),
  br(),
  p(HTML("<strong><em>Important Notes</em></strong>")),
  p(HTML("To delete rows, click which rows you would like to delete. Selected rows will appear in blue. If you wish to unselect a row, simply click it again and it will return to a white background. When you click the 'Delete Selected Rows' button,
  all of the rows selected in blue will be removed from the table. You can verify this by scrolling to the bottom of the page - in the bottom left
  corner it will indicate how many rows are now in the table. 
         If you deleted rows by mistake, you can press the 'Undo Delete' button. <em>NB - THIS BUTTON WILL ONLY UNDO THE MOST RECENT DELETION.</em> If you made an error and would like to restore rows, please contact Megan.")),
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
    column(6, actionButton("delete_rows", "Delete Selected Rows",class = "custom-button")),
    column(6, actionButton("undo_delete", "Undo Delete", class = "custom-button2")),
  ),
  br(),
  fluidRow(
    DTOutput("dynamic")
  )
)
)



# SERVER
server <- function(input, output, session) {
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
   data_from_db <- dbReadTable(con, "data_table")
  thedata <- reactiveValues(data = data_from_db)
  previous_data <- reactiveVal()
  
  output$dynamic <- renderDT(
    datatable(
      thedata$data %>% filter((is.na(Date.Published)) | (Date.Published >= input$dateRange[1] & Date.Published <= input$dateRange[2])),
      options = list(pageLength = 10, autoWidth = TRUE),
      filter = 'top', escape = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0)))
    )
  )
  
  observeEvent(input$delete_rows, {
    selected_rows <- input$dynamic_rows_selected
    if (!is.null(selected_rows)) {
      previous_data(thedata$data) # Store the current dataset before deletion
      new_data <- thedata$data[-selected_rows, ]
      thedata$data <- new_data
      dbWriteTable(con, "data_table", new_data, overwrite = TRUE) # Remove deleted rows from the database
    }
  })
  
  observeEvent(input$undo_delete, {
    if (!is.null(previous_data())) {
      thedata$data <- previous_data() # Restore the previous dataset
      previous_data(NULL) # Clear previous_data to prevent multiple undos
      dbWriteTable(con, "data_table", thedata$data, overwrite = TRUE) # Update the database with the restored data
    }
  })
  
  proxy <- dataTableProxy("dynamic")
  
  observeEvent(input$dynamic_cell_edit, {
    info <- input$dynamic_cell_edit
    str(info)
    i <- info$row
    j <- info$col + 1
    v <- info$value
    thedata$data[i, j] <<- DT::coerceValue(v, thedata$data[i, j])
    replaceData(proxy, thedata$data, resetPaging = FALSE)
  })
  
  output$downLoadFilter <- downloadHandler(
    filename = function() {
      paste('Filtered data-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      filtered_data <- thedata$data %>%
        filter((is.na(Date.Published)) | (Date.Published >= input$dateRange[1] & Date.Published <= input$dateRange[2])) %>%
        .[input[["dynamic_rows_all"]], ]
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
  # Save data to the SQLite database every 60 seconds
  autoSaveInterval <- 45 * 1000 # 45 seconds in milliseconds
  autoSave <- reactiveTimer(autoSaveInterval)
  observe({
    autoSave()
    dbWriteTable(con, "data_table", thedata$data, overwrite = TRUE)
  })
}


shinyApp(ui, server)

    
  