##################################################
# Test updating table with region names and codes:
##################################################


# Import data -------------------------------------------------------------

# Import points:
mypoints <- rio::import("Points_GPS.csv")

# Import shapefile:
haiti <- sf::st_read(here("inst", "extdata", "HTI_ADMIN4.shp"))

# Name cols of interest:
sf_names <- "adm4_name"
sf_pcodes <- "pcode_msf"

# Assign values to test function:
lat <- mypoints$Latitude
long <- mypoints$Longitude
shapefile <- haiti
col2return <- sf_names



# Function to get regions -------------------------------------------------

# Function to get region names:
get_region <- function(lat, 
                       long, 
                       shapefile, 
                       col2return){
  
  # Switch off spherical geometry:
  sf_use_s2(use_s2 = FALSE)
  
  # Create point object from coordinates:
  mypoint = st_point(c(long, lat)) %>% 
    st_zm()
  
  # Get region name or code:
  region = shapefile %>% 
    
    filter(st_contains(x = geometry,
                       y = mypoint,
                       sparse = FALSE, 
                       model = "closed") == 1) %>% 
    
    st_drop_geometry() %>% 
    
    select(all_of(col2return)) %>% 
    
    pull()
  
  # Return region name or code:
  return(region)

}



# Test function on table --------------------------------------------------

# Try function on table:
mypoints <- mypoints %>% 
  
  rowwise() %>%
  
  # Add region name:
  mutate(Region = get_region(lat = Latitude, 
                             long = Longitude, 
                             shapefile = haiti, 
                             col2return = sf_names)) %>% 
  
  # Add region code:
  mutate(Code = get_region(lat = Latitude, 
                           long = Longitude, 
                           shapefile = haiti, 
                           col2return = sf_pcodes))



