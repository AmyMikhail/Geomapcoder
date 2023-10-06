############################################
# EXAMPLE FOR UPLOADING SHAPEFILES:


library(shiny)
library(shinyFiles)

### UI
ui <- fluidPage(
  shinyDirButton('directory_select', 'Select a directory', title='Select a directory'),
  textOutput('directory_name')
)

### Server
server <- function(input, output, session) {
  volumes <- getVolumes()
  shinyDirChoose(input, 'directory_select', roots=volumes, session=session)
  dirname <- reactive({parseDirPath(volumes, input$directory_select)})
  
  ## Observe input dir. changes
  observe({
    if(!is.null(dirname)){
      print(dirname())
      output$directory_name <- renderText(dirname())
      ## Load files here
      # csv <- read.csv(paste0(dirname(), '/filename1.csv'))
      # rdata <- load(paste0(dirname(), '/filename2.Rdata'))
      # etc.
    }
  })
}

shinyApp(ui = ui, server = server)



# Alternative strategy ----------------------------------------------------

library(shiny)
library(tcltk)

ui <- fluidPage(
  titlePanel("Shiny Example"),
  helpText("Location of Example Input File"),
  verbatimTextOutput("printFilePath"),
  actionButton("inputFileButton", "Select File")
)

server <- function(input, output) {      
  observeEvent(input$inputFileButton, {
    selectedFilePath <-
      tk_choose.files(caption = "Select the Example Input File")
    output$printFilePath <- renderPrint(selectedFilePath)
    assign("selectedFilePath", selectedFilePath, .GlobalEnv)
  })      
}

shinyApp(ui, server)
