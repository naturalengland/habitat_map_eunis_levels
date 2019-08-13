#plot the map of EUNIs levels

library(leaflet)
library(ggplot2)
leaflet(english_waters) %>%
        addTiles() %>%
        addPolygons(color = "#444444", weight = 1, fillOpacity = 0.5,
                    fillColor = ~colorQuantile("YlOrRd", level)(level),
                    popup = paste(english_waters$level))


plot(english_waters["level"], axes = TRUE)


ggplot() + 
        geom_sf(data = english_waters aes(fill = levels))

#example
input <- list(gridSpacing = 0.2,
              gridPoints = 10,
              latCenter = 0,
              longCenter = 0)

side_length <- input$gridSpacing * (input$gridPoints - 1)

points <- expand.grid(lat = seq(input$latCenter - side_length / 2,
                                input$latCenter + side_length / 2,
                                length.out = input$gridPoints),
                      long = seq(input$longCenter - side_length / 2,
                                 input$longCenter + side_length / 2,
                                 length.out = input$gridPoints)) %>% 
        as.matrix %>% st_multipoint(dim = "XY") %>% st_sfc %>% 
        st_set_crs("+init=epsg:4326") %>% st_cast("POINT") %>% as("Spatial")

leaflet(data = points) %>% 
        addProviderTiles("Stamen.Watercolor") %>% 
        setView(lng = input$longCenter, lat = input$latCenter, zoom = 7) %>% 
        addMarkers()
