library(shiny)
library(leaflet)
library(maps)

ui <- fluidPage(
  
  leafletOutput("CountryMap", width = 1000, height = 500)
)

server <- function(input, output){
  
  Country = map("world", fill = TRUE, plot = FALSE, regions="Argentina", exact=F)
  output$CountryMap <- renderLeaflet({
    leaflet(Country) %>% addTiles() %>%
      fitBounds(Country$range[1], Country$range[3], Country$range[2], Country$range[4])%>%
      addPolygons(fillOpacity = 0.6,  smoothFactor = 0.5, stroke = TRUE, weight = 1)
  })
}


shinyApp(ui =ui, server = server)
