library(shiny)

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
      DT::datatable(filter = "none", rownames = F
                    ,extensions = 'FixedColumns'
                    ,options = list(pageLength = 10,scrollX = TRUE,
                                    fixedColumns = list(leftColumns = 2)),
                    escape=F)
  })
}


# UI
ui <- fluidPage(
  titlePanel("Delete rows"),
  mainPanel(
    DT::dataTableOutput("print_mtcars")
  )
)


# Run the application
shinyApp(ui = ui, server = server)
