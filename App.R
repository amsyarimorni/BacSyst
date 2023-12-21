# app.R
# Install required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl")
}

# Load the required libraries
library(shiny)
library(readxl)
library(writexl)

# Define relative paths for Excel_1 and a temporary result file
excel_1_path <- "data/MasterData.xlsx"
result_temp_path <- tempfile(fileext = ".xlsx")

# Path to the Guideline.txt file
guideline_path <- "Guideline.txt"

# Path to the Species_query.xlsx file
species_query_path <- "SpeciesQueryTemplate/Species_query.xlsx"

# Define the UI
ui <- fluidPage(
  titlePanel(HTML("<div style='display: flex; align-items: center;'>
    BacSyst v.1.1
  </div>")),
  sidebarLayout(
    sidebarPanel(
      fileInput("excel_2_upload", "Upload Species Query"),
      HTML("<p>Species name is case sensitive</p>"),
      
      actionButton("run_button", "Run"),
      HTML("<br>"),
      HTML("<br>"),
      downloadButton("download_button", "Download Result"),
      HTML("<br>"),
      HTML("<br>"),
      downloadButton("download_template_button", "SpeciesQuery Template"),
      HTML("<br>"),
      HTML("<br>"),
      actionButton("guideline_button", "ReadMe"),
      br(),
      HTML("1. Download template</p>"),
      HTML("2. Fill in species name</p>"),
      HTML("3. Save file</p>"),
      HTML("4. Upload saved file</p>"),
      HTML("5. Run</p>"),
      HTML("<p>This package is in BETA mode, comments are highly appreciated</p>"),
      
    ),
    mainPanel(
      tableOutput("result_table"),
      verbatimTextOutput("readme_text"),
      verbatimTextOutput("status_message")  # Display status message
    )
  )
)

# Define the server
server <- function(input, output, session) {
  
  vlookup_result <- eventReactive(input$run_button, {
    if (is.null(input$excel_2_upload))
      return(NULL)
    
    # Read Excel_1 and Excel_2
    excel_1 <- read_xlsx(excel_1_path)
    excel_2 <- read_xlsx(input$excel_2_upload$datapath)
    
    # Perform VLOOKUP based on the "Species" column
    vlookup_result <- merge(excel_2, excel_1, by = "Species", all.x = TRUE)
    
    # Replace NAs with "NO MATCH"
    vlookup_result[is.na(vlookup_result)] <- "NO MATCH"
    
    return(vlookup_result)
  })
  
  status_message <- reactiveVal("")  # Initialize a reactive value for status message
  
  observeEvent(vlookup_result(), {
    # When the query is completed, update the status message
    status_message("Your query is completed, Database update: Sept 2023. - Morni M. A.")
  })
  
  observe({
    if (!is.null(vlookup_result())) {
      # Save the VLOOKUP result to a temporary Excel file
      write_xlsx(vlookup_result(), result_temp_path)
    }
  })
  
  output$result_table <- renderTable({
    vlookup_result()
  })
  
  # Define the action to open the Guideline.txt file
  observeEvent(input$guideline_button, {
    readme_content <- readLines(guideline_path)
    output$readme_text <- renderText({
      paste(readme_content, collapse = "<br>")
    })
  })
  
  output$status_message <- renderText({
    status_message()  # Display the status message
  })
  
  output$download_button <- downloadHandler(
    filename = function() {
      "Result.xlsx"
    },
    content = function(file) {
      # Copy the temporary result file to the download path
      file.copy(result_temp_path, file)
    }
  )
  
  # Define the action to download the template
  output$download_template_button <- downloadHandler(
    filename = function() {
      "Species_query_template.xlsx"
    },
    content = function(file) {
      # Copy the template file to the download path
      file.copy(species_query_path, file)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)

