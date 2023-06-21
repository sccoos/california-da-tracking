library(leaflet)
library(shinyWidgets)

navbarPage("Domoic Acid Marine Mammal Stranding Tracker", id="nav",
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Data filters:"),
                                      checkboxInput("cluster", "Toggle clustering", value = FALSE),
                                      p(id = "attribution", em("(Source: USC Sea Grant; approximated from 1973 SCCWRP Report')")),
                        ),
                        
                        tags$div(id="cite",
                                 'Sources: Esri, GEBCO, NOAA, National Geographic, DeLorme, HERE, Geonames.org, and other contributors'
                        )
                    )
           ),
           
           tabPanel("Latest News",
                    htmlOutput("frame")
           ),
)