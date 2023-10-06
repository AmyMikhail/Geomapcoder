# RINNO packaging script for lab2godata


# Install required packages to create the installer -----------------------

# Install RInno package:
install.packages("RInno")

# Install installr package:
install.packages('installr')

# Now use installr to install inno:
installr::install.inno(quick_start_pack = TRUE)



# Create settings for the installer with RINNO ----------------------------

# Load libraries:
library(installr)
library(RInno)


# Create settings for lab2godata app:
RInno::create_app(

  # Give the name of the app:
  app_name = "lab2godata",

  # Give the file path to the app (inside package directory):
  app_dir = "inst/app",

  # Change the icon of the app:
  app_icon = "www/godataR_logo.ico",

  # Include the user licence in the installer:
  license_file = "LICENSE",

  # Create a name for the installer:
  dir_out = "lab2godata_installer",

  # Specify CRAN repository:
  repo = "https://cran.ma.imperial.ac.uk/",

  # # CRAN package dependencies:
  # pkgs = c("rio",
  #          "data.table",
  #          "dplyr",
  #          "purrr",
  #          "lubridate",
  #          "stringdist",
  #          "shiny",
  #          "shinyjs",
  #          "shinyvalidate"),
  #
  # # Github package dependencies:
  # remotes = c("EmilBode/EmilMisc", "WorldHealthOrganization/godataR"),

  # # Download R and install it with the app if not already present:
  # include_R = TRUE,

  # # Specify the R version:
  # R_version = "4.2.2",

  # # Download and install Rtools with the package if needed:
  # include_Rtools = TRUE,
  #
  # # Specify RTools version:
  # Rtools_version = "4.2",

  # Gihub lab2godata repository address for automated updates:
  app_repo_url = "https://github.com/WorldHealthOrganization/lab2godata",

  # Specify default install directory:
  default_dir = "userdesktop",

  # Specify install level:
  privilege = "lowest"

  )



# Compile the installer with RINNO ----------------------------------------

# Now compile the installer:
RInno::compile_iss()


c(list.dirs("C:/Program Files", TRUE, FALSE),
  list.dirs("C:/Program Files (x86)", TRUE, FALSE))
