# Load required packages
library(shiny)
library(leaflet)
library(tmaptools)
library(DT)
library(shinyFiles)
library(sf)
library(shinyjs)

# Define UI
ui <- fluidPage(
  useShinyjs(), # Enable shinyjs
  
  # Language buttons
  fluidRow(
    column(2, align = "left",
           actionButton("lang_en", "English", style = "background-color: darkblue; color: white; font-weight: bold;"),
           actionButton("lang_fr", "Français", style = "background-color: darkblue; color: white; font-weight: bold;"),
           actionButton("lang_es", "Español", style = "background-color: darkblue; color: white; font-weight: bold;"),
           actionButton("lang_ru", "Русский", style = "background-color: darkblue; color: white; font-weight: bold;"),
           actionButton("lang_ar", "العربية", style = "background-color: darkblue; color: white; font-weight: bold;")
    )
  ),
  
  # Search bar and map
  fluidRow(
    column(4,
           textInput("search_bar", h4("Search for a country or region:"), value = ""),
           actionButton("submit_search", "Submit"),
           checkboxInput("upload_shapefile", "Upload a shapefile?"),
           shinyjs::hidden(
             div(
               id = "shapefile_inputs",
               shinyFilesButton("shapefile", "Choose shapefile", title = "Upload a shapefile", multiple = TRUE),
               selectInput("region_name_col", "Select column with region names:", choices = NULL),
               selectInput("region_code_col", "Select column with region codes:", choices = NULL)
             )
           )
    ),
    column(8, leafletOutput("map", height = 500))
  ),
  
  # Table
  fluidRow(
    column(8, DT::dataTableOutput("table"))
  ),
  
  # Download button
  fluidRow(
    column(8, downloadButton("download_data", "Download Data"))
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    lang = "en",
    data = data.frame(Location = character(), Longitude = numeric(), Latitude = numeric()),
    shapefile = NULL
  )
  
  # Language translation
  translate <- eventReactive(c(input$lang_en, input$lang_fr, input$lang_es, input$lang_ru, input$lang_ar), {
    lang <- switch(rv$lang,
                   "en" = list(search_bar = "Search for a country or region:",
                               submit = "Submit",
                               upload_shapefile = "Upload a shapefile?",
                               region_name_col = "Select column with region names:",
                               region_code_col = "Select column with region codes:",
                               download_data = "Download Data"),
                   "fr" = list(search_bar = "Rechercher un pays ou une région:",
                               submit = "Soumettre",
                               upload_shapefile = "Télécharger un shapefile?",
                               region_name_col = "Sélectionnez la colonne avec les noms de région:",
                               region_code_col = "Sélectionnez la colonne avec les codes de région:",
                               download_data = "Télécharger les données"),
                   "es" = list(search_bar = "Buscar un país o región:",
                               submit = "Enviar",
                               upload_shapefile = "¿Cargar un shapefile?",
                               region_name_col = "Seleccione la columna con nombres de región:",
                               region_code_col = "Seleccione la columna con códigos de región:",
                               download_data = "Descargar datos"),
                   "ru" = list(search_bar = "Поиск страны или региона:",
                               submit = "Отправить",
                               upload_shapefile = "Загрузить shapefile?",
                               region_name_col = "Выберите столбец с названиями регионов:",
                               region_code_col = "Выберите столбец с кодами регионов:",
                               download_data = "Скачать данные"),
                   "ar" = list(search_bar = "ابحث عن دولة أو منطقة:",
                               submit = "إرسال",
                               upload_shapefile = "تحميل ملف شكل؟",
                               region_name_col = "حدد العمود الذي يحتوي على أسماء المناطق:",
                               region_code_col = "حدد العمود الذي يحتوي على رموز المناطق:",
                               download_data = "تنزيل البيانات")
    )
    lang
  })
  
  # Update language
  observeEvent(input$lang_en, {rv$lang <- "en"})
  observeEvent(input$lang_fr, {rv$lang <- "fr"})
  observeEvent(input$lang_es, {rv$lang <- "es"})
  observeEvent(input$lang_ru, {rv$lang <- "ru"})
  observeEvent(input$lang_ar, {rv$lang <- "ar"})
  
  # Search for country/region
  observeEvent(input$submit_search, {
    location <- geocode_OSM(input$search_bar, as.data.frame = TRUE)
    if (nrow(location) > 0) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        setView(lng = location$lon, lat = location$lat, zoom = 6)
    }
  })
  
  # Upload shapefile
  observeEvent(shinyFileChoose(input, "shapefile", "home"), {
    shp_files <- parseDirPath(roots = getVolumes(), shinyFileChoose(input, "shapefile", "home"))
    rv$shapefile <- st_read(shp_files$datapath)
    updateSelectInput(session, "region_name_col", choices = names(rv$shapefile))
    updateSelectInput(session, "region_code_col", choices = names(rv$shapefile))
  })
  
  # Show/hide shapefile inputs
  observeEvent(input$upload_shapefile, {
    if (input$upload_shapefile) {
      shinyjs::show("shapefile_inputs")
    } else {
      shinyjs::hide("shapefile_inputs")
      rv$data <- data.frame(Location = character(), Longitude = numeric(), Latitude = numeric())
    }
  })
  
  # Add shapefile to map
  observeEvent(input$region_name_col, {
    if (!is.null(rv$shapefile)) {
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data = rv$shapefile,
                    fillColor = "transparent",
                    color = "red",
                    weight = 2,
                    popup = ~get(input$region_name_col),
                    group = "regions")
    }
  })
  
  # Register location
  observeEvent(input$map_click, {
    click <- input$map_click
    if (!is.null(click)) {
      showModal(modalDialog(
        title = "Enter location name",
        textInput("location_name", "Name:", ""),
        actionButton("register_location", "Register"),
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$register_location, {
    name <- input$location_name
    click <- input$map_click
    lon <- click$lng
    lat <- click$lat
    
    if (input$upload_shapefile && !is.null(rv$shapefile)) {
      region_name <- rv$shapefile[st_contains(rv$shapefile, st_sfc(st_point(c(lon, lat)), crs = st_crs(rv$shapefile))), input$region_name_col]
      region_code <- rv$shapefile[st_contains(rv$shapefile, st_sfc(st_point(c(lon, lat)), crs = st_crs(rv$shapefile))), input$region_code_col]
      rv$data <- rbind(rv$data, data.frame(Location = name, Longitude = lon, Latitude = lat, region_name, region_code, stringsAsFactors = FALSE))
    } else {
      rv$data <- rbind(rv$data, data.frame(Location = name, Longitude = lon, Latitude = lat, stringsAsFactors = FALSE))
    }
    
    removeModal()
  })
  
  # Display table
  output$table <- renderDT({
    if (input$upload_shapefile && !is.null(rv$shapefile)) {
      colnames(rv$data) <- c("Location", "Longitude", "Latitude", "region_name", "region_code")
    } else {
      colnames(rv$data) <- c("Location", "Longitude", "Latitude")
    }
    rv$data
  }, options = list(pageLength = 5, width = "100%"))
  
  # Download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$data, file, row.names = FALSE)
    }
  )
  
  # Display map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)