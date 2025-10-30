# MycoLasso - Interactive Spatial Data Mapping & Lasso Selection Tool
# Created by: Lucien PIAT, Bioinformatician
# License: MIT - Free of use and distribution

library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)
library(dplyr)
library(plotly)
library(sp)
library(shinycssloaders)
library(readxl)

# Check if required packages are installed
required_packages <- c("shiny", "leaflet", "leaflet.extras", "DT", 
                       "dplyr", "plotly", "sp", "shinycssloaders", "readxl")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  stop(paste("Missing required packages:", paste(missing_packages, collapse = ", "), 
             "\nPlease install with: install.packages(c('", 
             paste(missing_packages, collapse = "', '"), "'))"))
}

# Source UI and Server files
tryCatch({
  source("ui.R")
  source("server.R")
}, error = function(e) {
  stop("Error loading ui.R or server.R: ", e$message)
})

# Run the application
shinyApp(ui = ui, server = server)