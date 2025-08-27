# app.R
library(shiny)
library(leaflet)
library(bslib)
library(sf)
library(DT)
library(dplyr)

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
      style = "display: block; margin: 0 auto;"
    ),
    
    h4("Sitio seleccionado"),
    DTOutput("puntos")
  ),
  leafletOutput("mapa", height = "100vh"),
)

server <- function(input, output, session) {
  
  puntos = reactiveVal(data.frame(id = numeric(0), latitud = numeric(0), longitud = numeric(0)))
  
  output$mapa <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addPolygons(data = mun, color = "black", fillColor = "black", fillOpacity = 0.1, weight = 1,
                  label = paste0("Municipio: ", mun$NOM_MUN)
      )
  })
  
  # Click en el mapa: agregar punto
  observeEvent(input$mapa_click, {
    click = input$mapa_click
    nuevo_id <- ifelse(nrow(puntos()) == 0, 1, max(puntos()$id) + 1)  # id único y estable
    nuevo = data.frame(id = nuevo_id, latitud = click$lat, longitud = click$lng)
    puntos(rbind(puntos(), nuevo))
    
    leafletProxy("mapa") |>
      addMarkers(lng = click$lng, lat = click$lat, layerId = nuevo_id)  # usar id único como layerId
  })
  
  # Mostrar tabla con botón de eliminar
  output$puntos <- renderDT({
    df = puntos() |> 
      mutate(
        latitud = round(latitud, 4),
        longitud = round(longitud, 4),
        borrar = sprintf('<button class="delete_btn" id="del_%s">Eliminar</button>', id)
      )
    
    datatable(df[, c("latitud", "longitud", "borrar")], escape = FALSE, rownames = FALSE)
  }, server = FALSE)
  
  # Escuchar clicks en botones de borrar
  observeEvent(input$puntos_cell_clicked, {
    info <- input$puntos_cell_clicked
    
    if (!is.null(info$value) && grepl("^<button", info$value)) {
      # Extraer id del botón
      id_borrar <- as.numeric(sub("del_", "", gsub(".*id=\"(.*)\".*", "\\1", info$value)))
      
      # Filtrar el punto eliminado de la tabla
      puntos(puntos() |> filter(id != id_borrar))
      
      # Eliminar marcador del mapa
      leafletProxy("mapa") |> removeMarker(layerId = id_borrar)
    }
  })
}

shinyApp(ui, server)
