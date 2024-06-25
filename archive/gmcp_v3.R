library(shiny)
library(leaflet)
library(shinyFiles)
library(sf)

ui <- fluidPage(
  shinyFilesButton("shapefile_button", "Select Shapefile", "Choose", "upload"),
  checkboxInput("show_shapefile", "Show Shapefile", FALSE),
  leafletOutput("map")
)

server <- function(input, output, session) {
  shinyFileChoose(input, "shapefile_button", roots = c(home = "~"), filetypes = c("", "shp"))
  
  shapefile_path <- reactive({
    req(input$shapefile_button)
    fp <- parseSavePath(c(home = "~"), input$shapefile_button)$datapath
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  observe({
    if (input$show_shapefile) {
      if (!is.null(input$shapefile_button)) {
        shapefile <- sf::st_read(shapefile_path())
        
        leafletProxy("map") %>%
          addPolygons(data = shapefile)
      }
    }
  })
}

shinyApp(ui, server)