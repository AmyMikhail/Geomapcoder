# Check if pacman is installed, install if not:
if (!require("pacman")) install.packages("pacman")

# Use pacman::p_load to load required packages from CRAN:
pacman::p_load(shiny, 
               shinyjs,
               shinyFiles,
               DT,
               leaflet, 
               tmaptools,
               geodata, 
               sf)

# Create the app:
ui <- fluidPage(
  useShinyjs(),
  titlePanel(textOutput("title")),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, align = "center",
               actionButton("en_btn", "English", style = "background-color: #00008B; color: white; font-weight: bold;"),
               actionButton("fr_btn", "Français", style = "background-color: #00008B; color: white; font-weight: bold;"),
               actionButton("es_btn", "Español", style = "background-color: #00008B; color: white; font-weight: bold;")
        )
      ),
      br(),
      textInput("search", textOutput("search_label")),
      actionButton("submit_search", textOutput("submit_btn"), style = "background-color: #006400; color: white; font-weight: bold;"),
      br(),
      shinyFilesButton("shapefile", "Select Shapefile", "Please select a shapefile", multiple = FALSE),
      uiOutput("region_select"),
      br(),
      downloadButton("downloadData", textOutput("download_btn"), style = "background-color: #00008B; color: white; font-weight: bold; width: 100%;")
    ),
    mainPanel(
      leafletOutput("map", height = 400),
      DT::dataTableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data frame to store user-entered locations
  locations <- reactiveVal(data.frame(name = character(), lat = numeric(), lon = numeric(), region = character(), stringsAsFactors = FALSE))
  
  # Reactive expression to get map based on user input
  map_data <- eventReactive(input$submit_search, {
    search_term <- input$search
    if (nchar(search_term) > 0) {
      tryCatch({
        geocoded <- tmaptools::geocode_OSM(q = search_term, as.data.frame = TRUE)
        if (nrow(geocoded) > 0) {
          list(lat = geocoded$lat[1], lon = geocoded$lon[1])
        } else {
          showNotification(textOutput("location_not_found"), type = "error")
          return(NULL)
        }
      }, error = function(e) {
        showNotification(textOutput("geocoding_error"), type = "error")
        return(NULL)
      })
    } else {
      NULL
    }
  })
  
  # Render the map
  output$map <- renderLeaflet({
    map_data <- map_data()
    if (!is.null(map_data)) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = map_data$lon, lat = map_data$lat, zoom = 10) %>%
        addMarkers(data = locations(), lng = ~lon, lat = ~lat, popup = ~name)
    }
  })
  
  # Handle click events on the map
  map_clicks <- reactiveValues(lat = NULL, lon = NULL)
  
  observeEvent(input$map_click, {
    click_lat <- input$map_click$lat
    click_lon <- input$map_click$lng
    
    # Find the region for the clicked location
    region <- findRegion(click_lat, click_lon)
    
    showModal(modalDialog(
      title = textOutput("enter_name"),
      textInput("location_name", textOutput("location_name_label"), ""),
      footer = tagList(
        modalButton(textOutput("cancel_btn")),
        actionButton("add_location", textOutput("add_btn"))
      )
    ))
    
    # Set the region name in the modal dialog
    observeEvent(region, {
      updateTextInput(session, "location_name", value = paste0(region, " - "))
    }, once = TRUE)
  })
  
  # Reset the location name input when the modal is shown
  observeEvent(input$location_name, {
    updateTextInput(session, "location_name", value = "")
  }, once = TRUE)
  
  # Add location only when the "Add Location" button is clicked
  observeEvent(input$add_location, {
    location_name <- input$location_name
    if (nchar(location_name) > 0) {
      region <- findRegion(map_clicks$lat, map_clicks$lon)
      new_locations <- rbind(locations(), data.frame(name = location_name, lat = map_clicks$lat, lon = map_clicks$lon, region = region, stringsAsFactors = FALSE))
      locations(new_locations)
      removeModal()
    } else {
      showNotification(textOutput("enter_name_error"), type = "error")
    }
  })
  
  # Render the table
  output$table <- DT::renderDataTable({
    datatable(locations(), options = list(scrollX = TRUE), rownames = FALSE, colnames = colnames_lang())
  })
  
  # Download data as CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("locations_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data.frame(
        name = locations()$name,
        lat = format(locations()$lat, scientific = FALSE),
        lon = format(locations()$lon, scientific = FALSE),
        region = locations()$region
      ), file, row.names = FALSE)
    }
  )
  
  # Language selection
  language <- reactiveVal("en")
  
  observeEvent(input$en_btn, {
    language("en")
    updateText()
  })
  
  observeEvent(input$fr_btn, {
    language("fr")
    updateText()
  })
  
  observeEvent(input$es_btn, {
    language("es")
    updateText()
  })
  
  updateText <- function() {
    output$title <- renderText({
      switch(language(), en = "Location Map Explorer", fr = "Explorateur de carte de localisation", es = "Explorador de mapas de ubicación")
    })
    output$search_label <- renderText({
      switch(language(), en = "Search for a location:", fr = "Rechercher un lieu :", es = "Buscar una ubicación:")
    })
    output$submit_btn <- renderText({
      switch(language(), en = "Submit", fr = "Soumettre", es = "Enviar")
    })
    output$download_btn <- renderText({
      switch(language(), en = "Download Data", fr = "Télécharger les données", es = "Descargar datos")
    })
    output$location_not_found <- renderText({
      switch(language(), en = "Location not found. Please try again.", fr = "Lieu non trouvé. Veuillez réessayer.", es = "Ubicación no encontrada. Inténtalo de nuevo.")
    })
    output$geocoding_error <- renderText({
      switch(language(), en = "Error geocoding location. Please try again.", fr = "Erreur de géocodage du lieu. Veuillez réessayer.", es = "Error al geocodificar la ubicación. Inténtalo de nuevo.")
    })
    output$enter_name <- renderText({
      switch(language(), en = "Enter Location Name", fr = "Entrez le nom du lieu", es = "Ingrese el nombre de la ubicación")
    })
    output$location_name_label <- renderText({
      switch(language(), en = "Location Name:", fr = "Nom du lieu :", es = "Nombre de la ubicación:")
    })
    output$cancel_btn <- renderText({
      switch(language(), en = "Cancel", fr = "Annuler", es = "Cancelar")
    })
    output$add_btn <- renderText({
      switch(language(), en = "Add Location", fr = "Ajouter un lieu", es = "Agregar ubicación")
    })
    output$enter_name_error <- renderText({
      switch(language(), en = "Please enter a location name.", fr = "Veuillez entrer un nom de lieu.", es = "Por favor ingrese un nombre de ubicación.")
    })
    output$shapefile_label <- renderText({
      switch(language(), en = "Select Shapefile", fr = "Sélectionnez le fichier shapefile", es = "Seleccionar archivo shapefile")
    })
  }
  
  colnames_lang <- reactive({
    switch(language(),
           en = c("Name", "Latitude", "Longitude", "Region"),
           fr = c("Nom", "Latitude", "Longitude", "Région"),
           es = c("Nombre", "Latitud", "Longitud", "Región")
    )
  })
  
  # Reactive variable to store the uploaded shapefile
  shapefile <- reactiveVal(NULL)
  
  shinyFileChoose(input, "shapefile", roots = getVolumes(), filetypes = c("", "shp"))
  
  observeEvent(input$shapefile, {
    path <- parseDirPath(getVolumes(), input$shapefile)
    shp <- st_read(path)
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = shp)
    })
  })
  
  updateText()
}

shinyApp(ui, server)