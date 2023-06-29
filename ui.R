library(leaflet)
library(shinyWidgets)
library(shinydashboard)
library(lubridate)

todaysDate = lubridate::now(tzone = "EST") - lubridate::hours(11)

ui = dashboardPage( skin = "blue",
  dashboardHeader(title = "California Domoic Acid Forecasting and Marine Mammal Stranding Tool", titleWidth = "700px"),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      column( width = 8,
        div(class="outer",
            
            tags$head(
              # Include our custom CSS
              includeCSS("styles.css")
            ),
            
            # If not using custom CSS, set height of leafletOutput to a number instead of percent
            leafletOutput("map", height = "100%"),
            
            # Shiny versions prior to 0.11 should use class = "modal" instead.
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                          width = 330, height = "auto",
                          
                          h3("C-HARM v3, 3-day Forecast:\nParticulate Domoic Acid"),
                          em("Particulate domoic acid (pDA) is the measurement of total domoic acid toxin that is potentially extant in a given area. This forecast provides a probability for where that concentration of toxin is predicted to exceed the threshold that classifies a Harmful Algal Bloom (greater than 500 nanogramsper liter)."),
                          
                          hr(),
                          
                          radioGroupButtons(
                            inputId = "dateSelect",
                            label = "Forecast Date",
                            choiceNames = c(
                              paste(month(today(), label = TRUE, abbr = FALSE), mday(todaysDate)), 
                              paste(month(todaysDate+days(1), label = TRUE, abbr = FALSE), mday(todaysDate+days(1))),
                              paste(month(todaysDate+days(2), label = TRUE, abbr = FALSE), mday(todaysDate+days(2)))
                            ), choiceValues = c(0, 1, 2),
                            selected = 0,
                            justified = TRUE
                          ),
                          
                          hr(),
                          
                          downloadButton("downloadData", "Download Forecast Image"),
                          p(id = "attribution", br(),em("(Source: https://coastwatch.pfeg.noaa.gov/erddap/griddap/wvcharmV3_3day.graph?particulate_domoic')")),
            ),
            
            tags$div(id="cite",
                     'Sources: NOAA, US Census Bureau 2016 MAF/TIGER database, California Coastal Commission, and other contributors.'
            )
        )
      ),
      column(width = 4,
             box(id = "news", title = "Recent News", collapsible = T, collapsed = T, width = "100%",
                 box(
                   title = a(
                     "Mammals getting sick due to toxic algae along California coast",
                     href= "https://www.foxweather.com/watch/play-67b1e62aa000a01",
                     target="_blank"
                   ),
                   "June 27 - Fox Weather",
                   width = NULL
                 ),
                 box(
                   title = a(
                     "Sea lions suffering seizures and dying on Orange County beaches due to neurotoxin-producing algae bloom",
                     href= "https://www.latimes.com/socal/daily-pilot/news/story/2023-06-22/sea-lions-suffering-seizures-and-dying-on-orange-county-beaches-due-to-neurotoxin-producing-algae-bloom",
                     target="_blank"
                   ),
                   "June 22 - LA Times",
                   width = NULL
                 ),
                 box(
                   title = a(
                     "Hundreds of dolphins and sea lions have washed up dead or sick in California amid toxic algae outbreak ",
                     href= "https://www.cnn.com/2023/06/22/us/toxic-algae-bloom-california-dolphins-climate-scn/index.html",
                     target="_blank"
                    ),
                   "June 22 - CNN",
                   width = NULL
                 ),
                 box(
                   title = a(
                     "Over 1,000 sea lions, dolphins are getting sick and dying as toxic bloom off coast expands",
                     href= "https://www.ocregister.com/2023/06/21/sea-lions-dolphins-are-getting-sick-and-dying-as-toxic-bloom-off-coast-expands/",
                     target="_blank"
                   ),
                   "June 21 - Orange County Register",
                   width = NULL
                 ),
                 box(
                   title = a(
                     "Hundreds of Sea Lions Are Dying. Is an Algal Bloom to Blame?",
                      href= "https://www.nytimes.com/2023/06/21/us/algae-bloom-california-sea-lion-dolphin.html",
                     target="_blank"
                    ),
                   "June 21 - The New York Times",
                   width = NULL
                 ),
                 box(
                   title = a(
                     "Toxic Algal Bloom Suspected in Dolphin and Sea Lion Deaths in Southern California",
                     href= "https://www.fisheries.noaa.gov/feature-story/toxic-algal-bloom-suspected-dolphin-and-sea-lion-deaths-southern-california",
                     target="_blank"
                    ),
                   "June 16 - NOAA Fisheries",
                   width = NULL
                 )
             )
      ),
    )
  )
)