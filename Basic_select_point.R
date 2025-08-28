library(shiny)
library(leaflet)
library(bslib)
library(sf)
library(shinyjs)

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
    DTOutput("puntos"), # Mostrar los puntos guardados
    actionButton(inputId = "borrar_puntos", label = "Eliminar puntos", class = "btn-danger")
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
  
  ######################################
  ### Click en el mapa añadir puntos ###
  ######################################
  
  observeEvent(input$mapa_click, {
    click = input$mapa_click
    id_unico = dplyr::if_else(condition = nrow(puntos()) == 0, true = 1, false = max(puntos()$id) + 1)
    nuevo = data.frame(id = id_unico, latitud = click$lat, longitud = click$lng)
    cat("Se ha hecho click en:", click$lat, click$lng, "\n")
     
    # Añadir pulsados
    puntos(dplyr::bind_rows(puntos(), nuevo))
    
    # Añadir al mapa
    leafletProxy("mapa") |>
      addMarkers(lng = nuevo$longitud, lat = nuevo$latitud, layerId = as.character(nuevo$id), 
                 label = paste0(
                   "ID: ", "<b>", nuevo$id, "</b>", "<br>", 
                   "Latitud: ", "<b>", nuevo$latitud |>  round(digits = 4), "</b>", "<br>", 
                   "Longitud: ", "<b>", nuevo$longitud |>  round(digits = 4), "</b>"
                   ) |> 
                   lapply(FUN = function(x) { htmltools::HTML(x)}),
                 popup = paste0(
                   "ID: ", "<b>", nuevo$id, "</b>", "<br>", 
                   "Latitud: ", "<b>", nuevo$latitud |>  round(digits = 4), "</b>", "<br>", 
                   "Longitud: ", "<b>", nuevo$longitud |>  round(digits = 4), "</b>"
                 ) |> 
                   lapply(FUN = function(x) { htmltools::HTML(x)})
                 )
  })
  
  
  ########################
  ### Mostrar la tabla ###
  ########################
  
  output$puntos <-  DT::renderDT({
    mostrar = puntos() |> 
      dplyr::mutate(latitud = round(x = latitud, digits = 4),
                    longitud = round(x = longitud, digits = 4))
    datatable(mostrar, selection = "single", rownames = FALSE, options = list(pageLength = 5))
  })
  
  ##############################################################
  ### Seleccionado mapa es seleccionado en tabla y viseversa ###
  ##############################################################
  
  ### Seleccionas marcador entonces se selecciona fila
  observeEvent(input$mapa_marker_click, {
    marcador_seleccionado = input$mapa_marker_click$id
    cat("Marcador seleccionado: ", marcador_seleccionado, " que tiene la clase de ", marcador_seleccionado |> class(), "\n")
    
    # Seleccionar esa fila en la tabla
    selectRows(dataTableProxy("puntos"), as.integer(marcador_seleccionado))
  })
  
  ### Seleccionas fila entonces se selecciona marcador
  observeEvent(input$mapa_marker_click, {
    fila_seleccionada = input$puntos_rows_selected
    cat("Marcador seleccionado: ", fila_seleccionada, " que tiene la clase de ", fila_seleccionada |> class(), "\n")
    
    tabla = puntos()
    tabla = tabla |> 
      dplyr::filter(id == is.integer(fila_seleccionada))
    
    # Pendiente como realizarlo
  })
  
  
  
  ##########################################
  ### Eliminar seleccionados en la tabla ###
  ##########################################
  observeEvent(input$borrar_puntos, {
    
    seleccionado = input$puntos_rows_selected
    cat("Imprimiendo fila seleccionada id: ", seleccionado, " donde su clase es", seleccionado |>  class(), "\n")
    
    if (is.null(seleccionado) || length(seleccionado) == 0) {
      showNotification("Selecciona una fila en la tabla antes de borrar.", type = "warning")
      return()
    }
    
    tabla = puntos()
    tabla = tabla[-seleccionado, ] 
    puntos(tabla)
    
    if (nrow(tabla) > 0) {
      leafletProxy("mapa") |>  clearMarkers() |> 
        addMarkers(data = tabla, lng = tabla$longitud, lat = tabla$latitud, layerId = as.character(tabla$id), 
                   label = paste0(
                     "ID: ", "<b>", tabla$id, "</b>", "<br>", 
                     "Latitud: ", "<b>", tabla$latitud |>  round(digits = 4), "</b>", "<br>", 
                     "Longitud: ", "<b>", tabla$longitud |>  round(digits = 4), "</b>"
                   ) |> 
                     lapply(FUN = function(x) { htmltools::HTML(x)}),
                   popup = paste0(
                     "ID: ", "<b>", tabla$id, "</b>", "<br>", 
                     "Latitud: ", "<b>", tabla$latitud |>  round(digits = 4), "</b>", "<br>", 
                     "Longitud: ", "<b>", tabla$longitud |>  round(digits = 4), "</b>"
                   ) |> 
                     lapply(FUN = function(x) { htmltools::HTML(x)})
        )
    } else{
      leafletProxy("mapa") |>  clearMarkers()
    }
  })
  
  
}

shinyApp(ui, server)
