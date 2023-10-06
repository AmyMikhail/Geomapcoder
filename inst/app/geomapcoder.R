#########################################
# GEOMAPCODER 1.0
#########################################

# This shiny app is for geocoding and labeling locations on an interactive map.
# It has six elements:
# - a. Panel box with usage instructions
# - b. Static text input box to choose location to set map view to
# - c. Interactive leaflet map
# - d. Pop-up shiny alert text box to enter label or ID for a selected location
# - e. Table of results with label/ID, latitude and longitude for each point
# - f. Download button to download the table of results as a .csv file

# Note 1: instructions are currently in French, English may be added in future.
# Note 2: If the map view is set to Haiti, admin level 4 polygons are overlaid.
# Note 3: The admin level 4 polygons for Haiti take a while to load (~ 10 s).



# Prepare R environment ---------------------------------------------------


# Check if pacman is installed, install if not:
if (!require("pacman")) install.packages("pacman")

# Use pacman::p_load to load required packages from CRAN:
pacman::p_load(shiny,
               shinyalert,
               shinyjs,
               shinyFiles,
               shiny.i18n,
               fs,
               DT,
               here, 
               rio, 
               dplyr,
               purrr,
               sf,
               tmaptools,
               leaflet)

# Use pacman to install and load packages from Github:
pacman::p_load_gh("datasketch/shi18ny")


# Specify URL source for leaflet map awesome markers:
icon_url <- "https://raw.githubusercontent.com/rstudio/leaflet/main/docs/libs/leaflet/images/marker-icon.png"


# Create empty data.frame to hold results:
tab <- na.omit(data.frame(ID = NA_character_, 
                          Latitude = NA_real_,
                          Longitude = NA_real_))


# Create UI ---------------------------------------------------------------

ui <- function(request){
  
  fluidPage(
  
  # Use shiny alert package to link pop-up labels with GPS points:
  useShinyalert(force = TRUE),
  
  # Use shinyjs package to conditionally hide some user inputs:
  shinyjs::useShinyjs(),
  
  #########################################
  # CREATE TITLE AND INSTRUCTIONS
  
  
  titlePanel(div(img(src = "geomapcoder_iconv3.png",
                     height = '6%',
                     width = '10%'),
                 tags$span(style = "color:darkblue", 
                           "| geomapcodeR version 1.0 |")
                 )),
  
  sidebarLayout(
    
    sidebarPanel(width = 5,
      
      h3("Instructions", style = "color:darkblue; margin-top: 6px;"), 
      
      p("Cette application permet de localiser et de géocoder des adresses 
        (retrouver les points SPG) sur une carte interactive."),
      
      tags$ol(
        
        tags$li("Pour commencer, entrez la région ou le pays d'intérêt dans la 
                boite ci-dessous et appuyez sur le bouton", 
                tags$b("Envoyer"), 
                "."),
        
        tags$li("Indiquez si vous souhaitez inclure un niveau administratif en 
                cochant la case et téléchargez les shapefiles 
                correspondantes."),
        
        tags$li("Sur la carte, appuyez sur le bouton +/- pour effectuer un zoom."), 
        
        tags$li("Double-cliquez sur l'emplacement qui vous intéresse."),
        
        tags$li("Dans la fenêtre contextuelle qui s'affiche, saisissez un nom 
                ou un identifiant unique pour cet emplacement."), 
        
        tags$li("Cliquez sur le bouton", 
                tags$b("Valider"),
                "pour soumettre et enregistrer votre saisie."),
        
        tags$li("Répétez les étapes 1 à 6 pour autant d'adresses que 
                nécessaire."),
        
        tags$li("Cliquez sur le bouton",
                tags$b("Télécharger les résultats"),
                "pour enregistrer un fichier .csv avec les points SPG."), 
        
        tags$li("Cliquez sur le bouton", 
                tags$b("Télécharger la carte"), 
                "pour enregistrer une carte statique des emplacements marqués.")
        
        ),
      
      p(""),
      
      
      #########################################
      # CREATE INPUTS TO SET MAP VIEW
      
      fluidRow(
        
        column(width = 7,
               
               # Add label for checkbox:
               p(tags$b("Lieu à rechercher :"), 
                 style = "color:darkblue"), 
               
               p(""),
               
               # Add free text box for inputting region and country:
               textInput(inputId = "placename", 
                         label = NULL, 
                         value = "", 
                         width = "100%",
                         placeholder = "Région et pays d'intérêt"), 
               
               
               p(""),
               
               # Add label for checkbox:
               p(tags$b("Inclure un région administratif ?"), 
                 style = "color:darkblue"), 
               
               ), 
        
        column(width = 5,
               
               align = "right",
               
               p("", style = "margin-bottom: 30px;"),
               

               # Add an action button:
               actionButton(inputId = "submit_placename", 
                            label = "Envoyer", 
                            class = "btn-primary", 
                            style = "background-color: darkred"),
               
               
               p("", style = "margin-bottom: 18px;"), 
               
               # Add check-box for region shapefiles:
               checkboxInput(inputId = "addregions", 
                             label = NULL, 
                             value = FALSE)
               )
        
      ), 
      
      # Hide shapefile uploader unless admin regions box is ticked:
      fluidRow(
        
        column(width = 7,
               
               # Uploader for admin region shapefile
               shinyjs::hidden(
                 shinyFilesButton(id = "addshapefile",
                                  label = "Choisir un fichier shapefile..",
                                  title = "Choisir le fichier shapefile pour les régions administratives :",
                                  multiple = TRUE,
                                  viewtype = "detail",
                                  class = "btn-primary",
                                  style = "background-color: darkred")),
                 
                 # Add line break:
                 p(id = "extra1", ""),
               
               # Ask user to select the column in shapefile containing region names:
               shinyjs::hidden(
                 selectInput(inputId = "shapefile_names",
                             label = "Nom des régions (sélectionner) :",
                             choices = "")),
               
               ),
        
        
        column(width = 5, 
               align = "right",
               
               # Add line break:  
               p(id = "extra2", ""),
               
               # Add line break:
               p(id = "extra3", "", style = "margin-bottom: 22px;"),
                 
               # Ask user to select the column in shapefile containing region pcodes:
               shinyjs::hidden(
                 selectInput(inputId = "shapefile_pcodes",
                             label = "Codes des régions (sélectionner) :",
                             choices = ""))
               
               )

      ),

      
      fluidRow(
        
        column(width = 7, 
               
               # Add download button for results file:
               downloadButton(outputId = "download_resultstab", 
                              label = "Télécharger les résultats", 
                              class = "btn-primary", 
                              style = "background-color: darkblue")
               
               
               ), 
        
        column(width = 5, 
               
               align = "right",
               
               # Add download button for static map of points:
               downloadButton(outputId = "download_staticmap", 
                              label = "Télécharger la carte", 
                              class = "btn-primary", 
                              style = "background-color: darkblue")
               
               
               )
        
        
        
      )
      
      ),
    
    
    #########################################
    # CREATE OUTPUTS FOR MAIN PANNEL
    
      
    mainPanel(width = 7,
      
      # Add the map to the output:
      leafletOutput("map"), 
      
      # Add a text output to say if region and country were found:
      tags$em(span(textOutput(
        outputId = "placefinder_result"), 
        style = "color:darkred")), 
      
      # Add line break before table:
      br(),
              
      # Add the results table to the output:
      DTOutput("tab"), 
              
      )
    )
  )
}



# Create server -----------------------------------------------------------

server <- function(input, output, session) {
  
  #########################################
  # CONDITIONALLY SHOW SHAPEFILE UPLOADER
  
  
  # Make shapefile uploader appear conditionally on user checking "add regions":
  observeEvent(input$addregions, {
    
    shinyjs::toggle(id = "addshapefile", condition = input$addregions)
    
  })
  
  # Make column selectors appear conditionally on shapefile upload:
  observeEvent(input$addshapefile, {
    
    shinyjs::toggle(id = "shapefile_names", condition = input$addshapefile)
    
    shinyjs::toggle(id = "shapefile_pcodes", condition = input$addshapefile)
    
  })
  

  #########################################
  # CONDITIONALLY IMPORT SHAPEFILE
  
  # Fetch user's folder structure for file browser:
  volumes <- getVolumes()()
  
  # Add server link to file chooser:
  shinyFileChoose(input,
                  id = "addshapefile",
                  roots = volumes,
                  session = session)
  
  # Get file path from user input:
  file_selected <- eventReactive(input$addshapefile, {

    req(input$addshapefile)

    parseFilePaths(volumes, input$addshapefile)

  })

  # Use file path to read in shape file as a reactive:
  user_shp <- eventReactive(input$addshapefile, {
    
    if(nrow(file_selected()) > 0 & file.exists(file_selected()$datapath)) {
      
      sf::st_read(dsn = file_selected()$datapath)
      
    } 
    
  })
  
  # Get column names from shapefile:
  observeEvent(input$addshapefile, {
    
    if(!is.null(user_shp())) {
      
      # Extract list of column names from shapefile:
      sfcolnames <- colnames(user_shp())
      
      # Provide list of shapefile column names to choose region name col from:
      updateSelectInput(session,
                        inputId = "shapefile_names",
                        label = "Nom des régions (sélectionner) :",
                        choices  = sfcolnames,
                        selected = "")
      
      # Provide list of shapefile column names to choose region pcode col from:
      updateSelectInput(session,
                        inputId = "shapefile_pcodes",
                        label = "Codes des régions (sélectionner) :",
                        choices  = sfcolnames,
                        selected = "")

    } 
      
  })
  
  # Make column names reactive:
  sf_names <- reactive(input$shapefile_names)
  sf_pcodes <- reactive(input$shapefile_pcodes)
  


  #########################################
  # SET REACTIVE VALUES

  
  # Make table of results reactive:
  tab <- reactiveVal(tab)
  
  # Create a reactive holder for placename:
  location <- reactive({
    
    return(input$placename)
    
  })
  

  #########################################
  # CREATE MAP AT CORRECT LOCATION

  # If submit placename action button is pressed:
  observeEvent(input$submit_placename, {
    
    # Query location to find coordinates:
    maplocation <- tmaptools::geocode_OSM(q = location(), 
                                          as.data.frame = TRUE)
    
    if(!is.null(maplocation)) {
      
      # Confirm that location was found on map:
      output$placefinder_result <- renderText({ 
        "\n\n Emplacement trouvé ; voir la carte" 
        })
      
      # Make the map:
      map <- leaflet() %>% 
        
        # Set view to user-defined place:
        setView(lng = maplocation$lon, lat = maplocation$lat, zoom = 14) %>% 
        
        # Add open street map (default base layer)
        addTiles(options = tileOptions(maxZoom = 18))
      
      # Render the map:
      output$map <- renderLeaflet(map)
      
    } else {
      
      # Print message if location could not be found on map:
      output$placefinder_result <- renderText({ 
        "\n\n Emplacement non trouvé; veuillez réessayer" 
        })
      
    }
    
  })
  
  
  observeEvent(input$addshapefile, {
    
    # Add polygons if available:
    if(!is.null(user_shp())) {
      
      # Update the map to include shapefile overlay:
      leafletProxy('map') %>%
        
        # Add transformed shapefile of regions
        addPolygons(data = user_shp(),
                    weight = 1,
                    color = "black",
                    fillColor = "lightgrey",
                    popup = user_shp()$sf_names())
      
    }
    
  })
  

  #########################################
  # ADD INPUT BOX TO LABEL MAP POINTS

  
  # Add label for GPS point on map click:
  observeEvent(input$map_click, {
    
    shinyalert(
      
      # Allow HTML tags:
      html = TRUE,
      
      # Set title of GPS label:
      title = "Etiquette de point GPS", 
      
      text = as.character(tagList(HTML(paste0("Identifiant :<br>")),
                                  
                                  # Add the freetext box: 
                                  textInput(inputId = "prop", 
                                            label = ""),
                                  
                                  # Add the submit button:
                                  actionButton(inputId = "enter", 
                                               label = "Valider"))), 
      
      showConfirmButton = FALSE
      
    )
    
  })
  
  
  #########################################
  # ADD MARKERS AND LABELS TO OUTPUT TABLE

  
  # Add marker for GPS points on map click and link to label:
  observeEvent(input$enter, {
    
    leafletProxy('map') %>% 
      
      # Add marker based on icon url including the label:
      addAwesomeMarkers(lng = input$map_click$lng, 
                        lat = input$map_click$lat, 
                        icon = makeAwesomeIcon(icon_url),
                        label = input$prop)
    
    # Add GPS points and their labels to the results table:
    tab(
      rbind(
        tab(), 
        data.frame(ID = input$prop, 
                   Latitude = input$map_click$lat,
                   Longitude = input$map_click$lng)
      )
    )
  }) 
  
  
  observeEvent(input$addshapefile, {
    
    if(!is.null(user_shp())) {
      
      tab <- tab %>% 
        
        # Convert to sf:
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
        
        # Use same projection as for quartiers:
        st_transform(st_crs(user_shp())) %>%
        
        # Join the two files together using the intersection:
        st_join(user_shp(), join = st_intersects) %>% 
        
        # Drop geometry:
        sf::st_drop_geometry() %>% 
        
        # Select columns:
        select(ID, 
               Latitude, 
               Longitude, 
               all_of(sf_names()), 
               all_of(sf_pcodes()))
      
    }
    
  })
  
  
  # Render the results table
  output$tab <- renderDT({
    datatable(tab(), 
              # Change help language to French:
              options = list(language = list(
                url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')))
  })
  
  
  
  #########################################
  # ADD RESULTS DOWNLOAD BUTTON

  
  # Add the download button:
  output$download_resultstab <- downloadHandler(
    
    # Define file name:
    filename = "Points_GPS.csv",
    
    # Write file to csv:
    content = function(file) {
      
      write.csv(as.data.frame(tab()), 
                file, 
                row.names = FALSE)
    }
  )
  
  
  #########################################
  # STOP THE APP WHEN THE SESSION ENDS:
  
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }

}



# Run the app -------------------------------------------------------------

shinyApp(ui = ui, server = server)
