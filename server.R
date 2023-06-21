library(leaflet)
library(RColorBrewer)
library(shinyalert)
library(here)
library(tidyverse)
library(sf)
library(raster)

function(input, output, session) {
  
  # sample_labels <- sprintf(
  #   "<div id = 'sample-label'><strong>%s</strong><br/>(Click to expand detailed view)</div>",
  #   sampling$project_module) %>%
  #   lapply(htmltools::HTML)
  
  ## Interactive Map ###########################################
  
  pda_day1 = raster::raster(here("data", "test.tif"))
  stranding_regions <- read_sf(here("data", "stranding_regions", "stranding_regions.shp"))
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lng = -120.82, lat = 34.60, zoom = 8) %>%
      addRasterImage(x = pda_day1, colors = "Spectral", opacity = 1)
      # %>% addPolygons(
      #   data = stranding_regions,
      #   fillColor = "#D0D0D0",
      #   fillOpacity  = 0.75,
      #   color = "#000000",
      #   opacity = 1,
      #   weight = 2
      # )
  }) 
  
  observe({
    selectedTiles = c("OpenStreetMap.Mapnik")
    
    leafletProxy("map", session)%>%
      clearTiles() %>%
      addProviderTiles(selectedTiles, providerTileOptions(zIndex=-10, continuousWorld=FALSE), group="base")%>%
      clearImages() %>%
      addRasterImage(pda_day1, colors="Spectral", project=FALSE, opacity = 0.7)
  })
  
  # This observer is responsible for onMouseclick popups
  observe({
    leafletProxy("map") %>% clearPopups() %>% clearGroup(group = "detailed")
    event <- input$map_marker_click
    if (is.null(event) || is.null(event$id))
      return()
    
    isolate({
      #showSampleData(event$id, event$lat, event$lng)
    })
  })
  
  showSampleData <- function(marker_id, lat, lng) {
    if (is.null(id)) {
      return()
    }
    
    selectedSample <- sampling %>% filter(id == marker_id)
    
    content <- as.character(tagList(
      tags$h5("PIs:", selectedSample$p_is),
      tags$strong(HTML(sprintf("Institutions: %s", selectedSample$institution))), tags$hr(),
      
      tags$strong(HTML("Overview:")), tags$br(),
      sprintf("%s", selectedSample$overview), tags$br(), tags$br(),
      sprintf("Parameters: %s", selectedSample$parameters_measured), tags$hr(),
      
      tags$strong(HTML(sprintf("Sampling Dates: %s - %s", selectedSample$sampling_date_start, selectedSample$sampling_date_end))), tags$br(),
      sprintf("Collection Status: %s", selectedSample$status_collection), tags$br(),
      sprintf("Analysis Status: %s", selectedSample$status_analysis)
    ))
    
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id, options = popupOptions(closeOnClick = TRUE, keepInView = TRUE))
  }
  
  
  ## Welcome message ##############################
  shinyalert(
    html = T,
    title = "June 2023 California Domoic Acid Stranding Event",
    text = "This tool is intended to display the latest forecast of Domoic Acid, showcase information for regional stranding centers, and where available, provide an up to date look at the marine mammal stranding counts across the California coast.<hr>Website in development. For more information, go to <a href='https://www.fisheries.noaa.gov/s3/2023-04/stranding-network-california-2023.pdf'>https://www.fisheries.noaa.gov/s3/2023-04/stranding-network-california-2023.pdf</a>.",
    type = "",
    size = "m",
    imageUrl = "https://sccoos.org/wp-content/uploads/2022/05/SCCOOS_logo-01.png", #https://s2020.s3.amazonaws.com/media/logo-scripps-ucsd-dark.png", #
    imageWidth = 500,
    imageHeight = 60
  )
  
  ## Learn more page #############################
  # iFrame ui element
  output$frame <- renderUI({
    tags$iframe(src="https://www.fisheries.noaa.gov/feature-story/toxic-algal-bloom-suspected-dolphin-and-sea-lion-deaths-southern-california", frameborder = "no", style="height: 100vh;", allowfullscreen = "TRUE", allow="geolocation")
  })
  
}