library(shiny)
library(DT)

# Server
server <- function(input, output) {
  
  # Auxiliary function
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  # Reactive
  getListUnder <- reactive({
    df <- mtcars
    df$Delete <- shinyInput(actionButton, nrow(df),'delete_',label = "Delete",icon=icon("trash"),
                            style = "color: red;background-color: white",
                            onclick = paste0('Shiny.onInputChange( \"delete_button\" , this.id, {priority: \"event\"})'))
    
    df$ID <- seq.int(nrow(df))
    return(df)
  })
  
  # Assign reactive data.frame to reactiveValues
  values <- reactiveValues(df = NULL)
  values$df <- isolate({getListUnder()})
  
  # When press delete_button, remove row
  observeEvent( input$delete_button, {
    selectedRow <- as.numeric(strsplit(input$delete_button, "_")[[1]][2])
    values$df <<- subset(values$df, ID!=selectedRow)
  })
  
  # Render data.table
  output$print_mtcars <- renderDataTable({
    final_df <- values$df
    table <- final_df %>%
      DT::datatable(filter = "top", rownames = F
                    ,extensions = 'FixedColumns'
                    ,options = list(pageLength = 10,scrollX = TRUE,
                                    fixedColumns = list(leftColumns = 2)),
                    escape=F)
  })
  
  # Download filtered data as CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("mtcars-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      filtered_rows <- input$print_mtcars_rows_all
      write.csv(values$df[filtered_rows, !colnames(values$df) %in% c("Delete", "ID")], file, row.names = FALSE)
    }
  )
}

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
                  column(6,downloadButton("downloadData", "Download Data as CSV"))),
                
                fluidRow(
                  DT::dataTableOutput("dynamic"))
)


# Run the application
shinyApp(ui = ui, server = server)
