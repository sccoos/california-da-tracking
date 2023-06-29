library(leaflet)
library(RColorBrewer)
library(shinyalert)
library(here)
library(tidyverse)
library(sf)
library(raster)

source("addLegend_decreasing.R")

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  pda_day = list()
  pda_day["0"] = raster::raster(here("data", "current_forecast", "forecast_day_0.tif"))
  pda_day["1"] = raster::raster(here("data", "current_forecast", "forecast_day_1.tif"))
  pda_day["2"] = raster::raster(here("data", "current_forecast", "forecast_day_2.tif"))
  
  center_reports = read_csv(here("data", "Stranding_Center_Reports_2023-06.csv"))
  counties = read_csv(here("data", "stranding_regions", "zone_counties.csv"))
  
  total_cases_by_zone = center_reports %>% group_by(zone) %>% summarize(total_cases = sum(stranding_cases, na.rm=T))
  
  stranding_regions <- read_sf(here("data", "stranding_regions", "stranding_regions.shp")) %>%
    left_join(total_cases_by_zone, c("OBJECTID"="zone")) %>%
    left_join(counties, c("OBJECTID"="zone"))
  
  region_labels <- sprintf(
    "<div id = 'sample-label'><h3>%s</h3><h5>Total DA Cases Reported: %s</h5>(Click for more information.)</div>",
    stranding_regions$county_names, stranding_regions$total_cases) %>%
    lapply(htmltools::HTML)
  
  rainbow = c("purple", "blue", "cyan", "green", "yellow", "orange", "red", "darkred")
  cols = colorNumeric(
    rainbow,
    0:1,
    na.color = NA,
    alpha = T,
    reverse = F
  )
  
  cols_hack = colorNumeric(
    rainbow,
    0:1,
    na.color = NA,
    alpha = T,
    reverse = T
  )
  
  region_color = function(cases) {
    if(cases == 0) {
      return("#D0D0D0")
    } else if(cases < 5) {
      return("#FDE992")
    } else if(cases < 20) {
      return("#FF9D5C")
    } else if(cases >= 20) {
      return("#CA3433")
    }
  }
  
  stranding_regions = stranding_regions %>% rowwise() %>% mutate(fillcol = region_color(total_cases))
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(crs = leafletCRS('L.CRS.EPSG3857'))) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lng = -120.78, lat = 34.56, zoom = 8) %>%
      addPolygons(
        data = stranding_regions,
        fillColor = stranding_regions$fillcol,
        fillOpacity  = 1,
        color = "#000000",
        opacity = 1,
        weight = 2,
        layerId = ~OBJECTID,
        label = ~region_labels,
        popup = buildStrandingPopups(stranding_regions$OBJECTID),
        highlightOptions = highlightOptions(color = "white", weight = 4,bringToFront = TRUE)
      ) %>% 
      addLegend_decreasing(position = "bottomleft", title = "pDA", pal = cols_hack, values = seq(0,1,0.1), decreasing = F)
  }) 
  
  observe({
    forecast_day = input$dateSelect
    raster_img = pda_day[[forecast_day]]

    selectedTiles = c("OpenStreetMap.Mapnik")
    leafletProxy("map", session)%>%
      clearTiles() %>%
      addProviderTiles(selectedTiles, providerTileOptions(zIndex=-10, continuousWorld=FALSE), group="base")%>%
      clearImages() %>%
      addRasterImage(x = raster_img, colors = cols, opacity = 1)
  })
  
  # popup alterntive code
  # observeEvent(input$map_shape_click, {
  #   leafletProxy("map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   print(event)
  #   
  #   isolate({
  #     showStrandingPopup(event$id, event$lat, event$lng)
  #   })
  # })
  # 
  buildStrandingPopups <- function(zone) {
    if (is.null(zone)) {
      return()
    }
    
    content = c()
    for(i in 1:length(zone)) {
      #print(i)
      region_info = center_reports %>% filter(zone == i)
      
      content_rows = c()
      for(j in 1:nrow(region_info)) {
        row = region_info[j,]
        content_rows = c(content_rows,
            as.character(tagList(
              tags$h3(row$center),
              HTML(row$coverage), tags$br(),
              sprintf("Hotline: %s", row$contact), tags$br(),
              tags$strong(HTML(sprintf("Suspected Marine Mammal DA Cases during event:\n    %s (as of %s)", ifelse(is.na(row$stranding_cases), "No data reported", row$stranding_cases), row$last_report_date))), tags$br(),
            ))
        )
      }
      
      con = paste(content_rows, collapse="<hr>")
      
      content = c(content, con)
    }
    
    return(content)
  }
  
  output$downloadData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      day = as.numeric(input$dateSelect)
      paste0("pDA_cHARMv3_", lubridate::today()+day)
    },
    content = function(file) {
      day = as.numeric(input$dateSelect)
      date = today()+day
      downloadURL = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/wvcharmV3_3day.png?particulate_domoic%5B(",date,"T12:00:00Z)%5D%5B(31.3):(42.01)%5D%5B(234.99):(243.0)%5D&.draw=surface&.vars=longitude%7Clatitude%7Cparticulate_domoic&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff")
      download.file(downloadURL, file)
    },
    contentType = 'image/png'
  )
  
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
    text = "This tool is intended to display domoic acid event risk, and allow users to click around the coastal stranding regions for detailed information on local stranding centers and their outlook of marine mammal strandings throughout this event.<hr>To learn more about domoic acid toxicosis, go to <a href='https://www.cimwi.org/domoic-acid' target='_blank'>https://www.cimwi.org/domoic-acid</a>.<hr><a href='https://www.fisheries.noaa.gov/s3/2023-04/stranding-network-california-2023.pdf' target='_blank'>The full California stranding network map is available here.</a>",
    type = "",
    size = "m",
    imageUrl = "https://sccoos.org/wp-content/uploads/2022/05/SCCOOS_logo-01.png", #https://s2020.s3.amazonaws.com/media/logo-scripps-ucsd-dark.png", #
    imageWidth = 500,
    imageHeight = 60
  )
  
}
