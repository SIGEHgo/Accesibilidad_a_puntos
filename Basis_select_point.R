# app.R
library(shiny)
library(leaflet)
library(bslib)
library(sf)

mun = sf::read_sf("Accesibilidad/municipiosjair.shp")

ui <- page_sidebar(
  
  tags$style(
    HTML(
      "
    html, body,
    .main.bslib-gap-spacing.html-fill-container {
      height: 100%;
      margin: 0 !important;
      padding: 0 !important;
    }
    "
    )
  ),
  
  
  sidebar = sidebar(
    width = 500,
    tags$img(
      src = "https://raw.githubusercontent.com/Eduardo-Alanis-Garcia/Js/main/Planeacion_dorado.png",     
      height = "50px",      
      width = "auto",       
      style = "display: block; margin: 0 auto;" # Centrar en el sidebar
    ),
    
    
    h4("Sitio seleccionado"),
    dataTableOutput("puntos") # Mostrar los puntos guardados
    # Pensar en usar dataTableOutput
    ),
    leafletOutput("mapa", height = "100vh"),
)

server <- function(input, output, session) {
  
  # Importante reactiveVal
  puntos = reactiveVal(data.frame(id = numeric(0), latitud = numeric(0), longitud = numeric(0)))
  
  output$mapa <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addPolygons(data = mun, color = "black", fillColor = "black", fillOpacity = 0.1, weight = 1,
                  label = paste0("Municipio: ", mun$NOM_MUN)
      )
  })
  
  # Click en el mapa
  observeEvent(input$mapa_click, {
    click = input$mapa_click
    nuevo = data.frame(id = nrow(puntos()) + 1, latitud = click$lat, longitud = click$lng)
    
    # Añadir pulsados
    puntos(rbind(puntos(), nuevo))
    
    # Añadir al mapa
    leafletProxy("mapa") |>
      addMarkers(lng = click$lng, lat = click$lat)
  })
  
  output$puntos <- renderDataTable({
    mostrar = puntos() |> 
      dplyr::select(latitud, longitud) |> 
      dplyr::mutate(latitud = round(x = latitud, digits = 4),
                    longitud = round(x = longitud, digits = 4))
    mostrar
  })
}

shinyApp(ui, server)
