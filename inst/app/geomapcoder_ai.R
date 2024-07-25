# Check for and load required packages:
if (!require(pacman))
  install.packages("pacman")
pacman::p_load(shiny,
               leaflet,
               tmaptools,
               sf,
               shinyFiles,
               here,
               DT,
               rio,
               shiny.i18n)

# Initialize translator
i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("en")

# Create UI
ui <- fluidPage(titlePanel(
  div(
    style = "display: flex; justify-content: space-between; align-items: center;",
    img(
      src = "geomapcoder_iconv3.png",
      height = '6%',
      width = '10%'
    ),
    tags$span(style = "color:darkblue", "| GeomapcodeR version 1.0 |"),
    div(
      style = "display: flex;",
      actionButton("lang_en", "English", style = "background-color: #00008B; color: white; font-weight: bold; margin-right: 5px;"),
      actionButton("lang_fr", "Français", style = "background-color: #00008B; color: white; font-weight: bold; margin-right: 5px;"),
      actionButton("lang_es", "Español", style = "background-color: #00008B; color: white; font-weight: bold; margin-right: 5px;"),
      actionButton("lang_ru", "Русский", style = "background-color: #00008B; color: white; font-weight: bold; margin-right: 5px;"),
      actionButton("lang_ar", "العربية", style = "background-color: #00008B; color: white; font-weight: bold;")
    )
  )
), sidebarLayout(
  sidebarPanel(
    uiOutput("country_input"),
    uiOutput("search_button"),
    br(),
    br(),
    uiOutput("shapefile_button"),
    br(),
    uiOutput("column_selector_name"),
    br(),
    uiOutput("column_selector_code"),
    uiOutput("show_polygons_checkbox")
  ),
  mainPanel(
    leafletOutput("map", height = 400),
    br(),
    DTOutput("locations_table"),
    br(),
    uiOutput("download_button")
  )
))

# Create server:
server <- function(input, output, session) {
  # Reactive value for current language
  current_language <- reactiveVal("en")
  
  # Language selection
  observeEvent(input$lang_en, {
    current_language("en")
    i18n$set_translation_language("en")
  })
  observeEvent(input$lang_fr, {
    current_language("fr")
    i18n$set_translation_language("fr")
  })
  observeEvent(input$lang_es, {
    current_language("es")
    i18n$set_translation_language("es")
  })
  observeEvent(input$lang_ru, {
    current_language("ru")
    i18n$set_translation_language("ru")
  })
  observeEvent(input$lang_ar, {
    current_language("ar")
    i18n$set_translation_language("ar")
  })
  
  # Reactive translations
  translations <- reactive({
    lang <- current_language()
    list(
      enter_country = i18n$t("Enter a country name:"),
      search = i18n$t("Search"),
      show_polygons = i18n$t("Show Polygons"),
      choose_shapefile = i18n$t("Choose Shapefile"),
      select_shapefile = i18n$t("Please select a .shp file"),
      select_region_name = i18n$t("Select Region Name Column:"),
      select_region_code = i18n$t("Select Region Code Column:"),
      download_results = i18n$t("Download Results"),
      id = i18n$t("ID"),
      latitude = i18n$t("Latitude"),
      longitude = i18n$t("Longitude"),
      region = i18n$t("Region"),
      code = i18n$t("Code"),
      no_data = i18n$t("No data available in table"),
      markers = i18n$t("Markers"),
      search_dt = i18n$t("Search:"),
      show_entries = i18n$t("Show _MENU_ entries"),
      showing_entries = i18n$t("Showing _START_ to _END_ of _TOTAL_ entries"),
      first = i18n$t("First"),
      last = i18n$t("Last"),
      next_page = i18n$t("Next"),
      previous = i18n$t("Previous"),
      enter_location_id = i18n$t("Enter Location ID"),
      location_id = i18n$t("Location ID:"),
      cancel = i18n$t("Cancel"),
      save = i18n$t("Save"),
      unknown = i18n$t("Unknown")
    )
  })
  
  # UI Outputs
  output$country_input <- renderUI({
    textInput("country", translations()$enter_country, "France")
  })
  
  output$search_button <- renderUI({
    actionButton("search", translations()$search)
  })
  
  output$show_polygons_checkbox <- renderUI({
    checkboxInput("show_polygons", translations()$show_polygons, value = FALSE)
  })
  
  output$shapefile_button <- renderUI({
    shinyFilesButton(
      "shapefile",
      translations()$choose_shapefile,
      translations()$select_shapefile,
      multiple = FALSE
    )
  })
  
  output$column_selector_name <- renderUI({
    req(shapefile())
    column_names <- names(shapefile())
    selectInput("region_column",
                translations()$select_region_name,
                choices = column_names)
  })
  
  output$column_selector_code <- renderUI({
    req(shapefile())
    column_names <- names(shapefile())
    selectInput("code_column",
                translations()$select_region_code,
                choices = column_names)
  })
  
  output$download_button <- renderUI({
    downloadButton("download_csv", translations()$download_results)
  })
  
  # Turn off scientific notation
  options(scipen = 999)
  
  # Initialize map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addLayersControl(
        overlayGroups = c(translations()$markers),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Update map language
  observe({
    t <- translations()
    leafletProxy("map") %>%
      clearControls() %>%
      addLayersControl(overlayGroups = c(t$markers),
                       options = layersControlOptions(collapsed = FALSE))
  })
  
  # React to country search
  observeEvent(input$search, {
    country <- input$country
    coords <- geocode_OSM(country, as.data.frame = TRUE)
    
    if (!is.null(coords) && nrow(coords) > 0) {
      leafletProxy("map") %>%
        setView(lng = coords$lon,
                lat = coords$lat,
                zoom = 8)
    } else {
      showNotification(translations()$country_not_found, type = "error")
    }
  })
  
  # Set up shinyFiles
  volumes <- c(Home = fs::path_home())
  shinyFileChoose(input, "shapefile", roots = volumes, session = session)
  
  # Read shapefile
  shapefile <- reactiveVal(NULL)
  
  observeEvent(input$shapefile, {
    file_selected <- input$shapefile
    if (is.integer(file_selected))
      return(NULL)
    
    path <- shinyFiles::parseFilePaths(volumes, file_selected)
    if (nrow(path) == 0)
      return(NULL)
    
    full_path <- here(path$datapath)
    
    tryCatch({
      sf_object <- sf::st_read(full_path, options = "ENCODING=UTF-8")
      shapefile(sf_object)
      showNotification(translations()$shapefile_loaded, type = "message")
    }, error = function(e) {
      showNotification(paste(translations()$error_reading_shapefile, e$message),
                       type = "error")
    })
  })
  
  # Handle polygon display
  observe({
    req(shapefile())
    if (isTruthy(input$show_polygons)) {
      leafletProxy("map") %>%
        clearGroup("Polygons") %>%
        addPolygons(
          data = shapefile(),
          fillColor = "red",
          fillOpacity = 0.5,
          weight = 1,
          group = "Polygons"
        )
    } else {
      leafletProxy("map") %>% clearGroup("Polygons")
    }
  })
  
  # Store selected locations
  selected_locations <- reactiveVal(
    data.frame(
      ID = character(),
      Latitude = numeric(),
      Longitude = numeric(),
      Region = character(),
      Code = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # Handle map clicks
  observeEvent(input$map_click, {
    click <- input$map_click
    t <- translations()
    showModal(modalDialog(
      title = t$enter_location_id,
      textInput("location_id", t$location_id),
      footer = tagList(
        modalButton(t$cancel),
        actionButton("save_location", t$save)
      )
    ))
  })
  
  # Save location when modal is submitted
  observeEvent(input$save_location, {
    req(input$location_id)
    click <- input$map_click
    t <- translations()
    
    # Find the region and code using st_contains
    region_name <- t$unknown
    region_code <- t$unknown
    if (!is.null(shapefile()) &&
        !is.null(input$region_column) && !is.null(input$code_column)) {
      tryCatch({
        point <- st_point(c(click$lng, click$lat)) %>% st_sfc(crs = st_crs(shapefile()))
        region <- st_contains(shapefile(), point, sparse = FALSE)
        if (any(region)) {
          region_name <- shapefile()[[input$region_column]][which(region)]
          region_code <- shapefile()[[input$code_column]][which(region)]
        }
      }, error = function(e) {
        showNotification(paste(t$error_determining_region, e$message), type = "warning")
      })
    }
    
    # Add ID, GPS and region of clicked locations to a table:
    new_location <- data.frame(
      ID = input$location_id,
      Latitude = as.numeric(click$lat),
      Longitude = as.numeric(click$lng),
      Region = region_name,
      Code = region_code,
      stringsAsFactors = FALSE
    )
    
    # Update table with additional clicked locations:
    current_locations <- selected_locations()
    updated_locations <- rbind(current_locations, new_location)
    selected_locations(updated_locations)
    
    # Update map with markers for clicked locations:
    leafletProxy("map") %>%
      addMarkers(
        lng = click$lng,
        lat = click$lat,
        popup = input$location_id,
        group = translations()$markers
      )
    
    removeModal()
  })
  
  # Display locations table
  output$locations_table <- renderDT({
    locations <- selected_locations()
    t <- translations()
    colnames(locations) <- c(t$id, t$latitude, t$longitude, t$region, t$code)
    datatable(locations, options = list(
      pageLength = 5,
      language = list(
        search = t$search_dt,
        lengthMenu = t$show_entries,
        info = t$showing_entries,
        zeroRecords = t$no_data,
        emptyTable = t$no_data,
        paginate = list(
          first = t$first,
          last = t$last,
          'next' = t$next_page,
          previous = t$previous
        )
      )
    ))
  })
  
  # Download handler for CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("selected_locations_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Ensure full precision for latitude and longitude
      locations <- selected_locations()
      locations$Latitude <- format(locations$Latitude,
                                   nsmall = 10,
                                   scientific = FALSE)
      locations$Longitude <- format(locations$Longitude,
                                    nsmall = 10,
                                    scientific = FALSE)
      
      # Translate column names
      t <- translations()
      colnames(locations) <- c(t$id, t$latitude, t$longitude, t$region, t$code)
      
      # Export CSV using rio::export with BOM
      rio::export(
        locations,
        file,
        format = "csv",
        fileEncoding = "UTF-8-BOM",
        bom = TRUE
      )
    }
  )
}

shinyApp(ui, server)