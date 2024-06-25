# Check for and load required packages:
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  shiny,
  leaflet,
  tmaptools,
  sf,
  shinyFiles,
  here,
  DT,
  rio,
  shiny.i18n
)

# Initialize translator
i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("en")

ui <- fluidPage(
  titlePanel(
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      img(src = "geomapcoder_iconv3.png",
          height = '6%',
          width = '10%'),
      tags$span(style = "color:darkblue", 
                "| GeomapcodeR version 1.0 |"),
      div(
        style = "display: flex;",
        actionButton("lang_en", "English", style = "background-color: #00008B; color: white; font-weight: bold; margin-right: 5px;"),
        actionButton("lang_fr", "Français", style = "background-color: #00008B; color: white; font-weight: bold; margin-right: 5px;"),
        actionButton("lang_es", "Español", style = "background-color: #00008B; color: white; font-weight: bold; margin-right: 5px;"),
        actionButton("lang_ru", "Русский", style = "background-color: #00008B; color: white; font-weight: bold; margin-right: 5px;"),
        actionButton("lang_ar", "العربية", style = "background-color: #00008B; color: white; font-weight: bold;")
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      textInput("country", i18n$t("Enter a country name:"), "France"),
      actionButton("search", i18n$t("Search")),
      br(), br(),
      uiOutput("shapefile_button"),
      br(),
      uiOutput("column_selector_name"),
      br(),
      uiOutput("column_selector_code"),
      checkboxInput("show_polygons", i18n$t("Show Polygons"), FALSE)
    ),
    mainPanel(
      leafletOutput("map", height = 400),
      br(),
      DTOutput("locations_table"),
      br(),
      uiOutput("download_button")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value for current language
  current_language <- reactiveVal("en")
  
  # Language selection
  observeEvent(input$lang_en, { current_language("en"); i18n$set_translation_language("en") })
  observeEvent(input$lang_fr, { current_language("fr"); i18n$set_translation_language("fr") })
  observeEvent(input$lang_es, { current_language("es"); i18n$set_translation_language("es") })
  observeEvent(input$lang_ru, { current_language("ru"); i18n$set_translation_language("ru") })
  observeEvent(input$lang_ar, { current_language("ar"); i18n$set_translation_language("ar") })
  
  # Update UI text when language changes
  observe({
    lang <- current_language()
    updateTextInput(session, "country", label = i18n$t("Enter a country name:"))
    updateActionButton(session, "search", label = i18n$t("Search"))
    updateCheckboxInput(session, "show_polygons", label = i18n$t("Show Polygons"))
  })
  
  # Render shinyFiles button with translated label
  output$shapefile_button <- renderUI({
    shinyFilesButton("shapefile", i18n$t("Choose Shapefile"), i18n$t("Please select a .shp file"), multiple = FALSE)
  })
  
  # Render download button with translated label
  output$download_button <- renderUI({
    downloadButton("download_csv", i18n$t("Download Results"))
  })
  
  # Turn off scientific notation
  options(scipen = 999)
  
  # Initialize map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      addLayersControl(
        overlayGroups = c("Markers"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # React to country search
  observeEvent(input$search, {
    country <- input$country
    coords <- geocode_OSM(country, as.data.frame = TRUE)
    
    if (!is.null(coords) && nrow(coords) > 0) {
      leafletProxy("map") %>%
        setView(lng = coords$lon, lat = coords$lat, zoom = 8)
    } else {
      showNotification(i18n$t("Country not found"), type = "error")
    }
  })
  
  # Set up shinyFiles
  volumes <- c(Home = fs::path_home())
  shinyFileChoose(input, "shapefile", roots = volumes, session = session)
  
  # Read shapefile
  shapefile <- reactiveVal(NULL)
  
  observeEvent(input$shapefile, {
    file_selected <- input$shapefile
    if (is.integer(file_selected)) return(NULL)
    
    path <- shinyFiles::parseFilePaths(volumes, file_selected)
    if (nrow(path) == 0) return(NULL)
    
    full_path <- here(path$datapath)
    
    tryCatch({
      sf_object <- sf::st_read(full_path, options = "ENCODING=UTF-8")
      shapefile(sf_object)
      showNotification(i18n$t("Shapefile loaded successfully"), type = "message")
    }, error = function(e) {
      showNotification(paste(i18n$t("Error reading shapefile:"), e$message), type = "error")
    })
  })
  
  # Create column selector UI for region name
  output$column_selector_name <- renderUI({
    req(shapefile())
    column_names <- names(shapefile())
    selectInput("region_column", i18n$t("Select Region Name Column:"), choices = column_names)
  })
  
  # Create column selector UI for region code
  output$column_selector_code <- renderUI({
    req(shapefile())
    column_names <- names(shapefile())
    selectInput("code_column", i18n$t("Select Region Code Column:"), choices = column_names)
  })
  
  # Add polygons to map
  observe({
    req(shapefile())
    req(input$show_polygons)
    
    leafletProxy("map") %>%
      clearGroup("Polygons") %>%
      addPolygons(data = shapefile(), fillColor = "red", fillOpacity = 0.5, weight = 1, group = "Polygons")
  })
  
  # Remove polygons when checkbox is unchecked
  observe({
    if (!input$show_polygons) {
      leafletProxy("map") %>% clearGroup("Polygons")
    }
  })
  
  # Store selected locations
  selected_locations <- reactiveVal(data.frame(
    ID = character(),
    Latitude = numeric(),
    Longitude = numeric(),
    Region = character(),
    Code = character(),
    stringsAsFactors = FALSE
  ))
  
  # Handle map clicks
  observeEvent(input$map_click, {
    click <- input$map_click
    showModal(modalDialog(
      title = i18n$t("Enter Location ID"),
      textInput("location_id", i18n$t("Location ID:")),
      footer = tagList(
        modalButton(i18n$t("Cancel")),
        actionButton("save_location", i18n$t("Save"))
      )
    ))
  })
  
  # Save location when modal is submitted
  observeEvent(input$save_location, {
    req(input$location_id)
    click <- input$map_click
    
    # Find the region and code using st_contains
    region_name <- i18n$t("Unknown")
    region_code <- i18n$t("Unknown")
    if (!is.null(shapefile()) && !is.null(input$region_column) && !is.null(input$code_column)) {
      tryCatch({
        point <- st_point(c(click$lng, click$lat)) %>% st_sfc(crs = st_crs(shapefile()))
        region <- st_contains(shapefile(), point, sparse = FALSE)
        if (any(region)) {
          region_name <- shapefile()[[input$region_column]][which(region)]
          region_code <- shapefile()[[input$code_column]][which(region)]
        }
      }, error = function(e) {
        showNotification(paste(i18n$t("Error determining region:"), e$message), type = "warning")
      })
    }
    
    new_location <- data.frame(
      ID = input$location_id,
      Latitude = as.numeric(click$lat),
      Longitude = as.numeric(click$lng),
      Region = region_name,
      Code = region_code,
      stringsAsFactors = FALSE
    )
    
    current_locations <- selected_locations()
    updated_locations <- rbind(current_locations, new_location)
    selected_locations(updated_locations)
    
    leafletProxy("map") %>%
      addMarkers(lng = click$lng, lat = click$lat, popup = input$location_id, group = "Markers")
    
    removeModal()
  })
  
  # Display locations table
  output$locations_table <- renderDT({
    locations <- selected_locations()
    colnames(locations) <- c(
      i18n$t("ID"),
      i18n$t("Latitude"),
      i18n$t("Longitude"),
      i18n$t("Region"),
      i18n$t("Code")
    )
    datatable(locations, 
              options = list(
                pageLength = 5,
                language = list(
                  search = i18n$t("Search:"),
                  lengthMenu = i18n$t("Show _MENU_ entries"),
                  info = i18n$t("Showing _START_ to _END_ of _TOTAL_ entries"),
                  paginate = list(
                    first = i18n$t("First"),
                    last = i18n$t("Last"),
                    `next` = i18n$t("Next"),
                    previous = i18n$t("Previous")
                  )
                )
              )
    )
  })
  
  # Download handler for CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("selected_locations_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Ensure full precision for latitude and longitude
      locations <- selected_locations()
      locations$Latitude <- format(locations$Latitude, nsmall = 10, scientific = FALSE)
      locations$Longitude <- format(locations$Longitude, nsmall = 10, scientific = FALSE)
      
      # Translate column names
      colnames(locations) <- c(
        i18n$t("ID"),
        i18n$t("Latitude"),
        i18n$t("Longitude"),
        i18n$t("Region"),
        i18n$t("Code")
      )
      
      # Export CSV using rio::export with BOM
      rio::export(locations, file, format = "csv", fileEncoding = "UTF-8-BOM", bom = TRUE)
    }
  )
}

shinyApp(ui, server)