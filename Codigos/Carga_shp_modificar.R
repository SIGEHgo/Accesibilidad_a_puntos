library(shiny)
library(leaflet)
library(bslib)
library(sf)
library(shinyjs)
library(DT)
library(archive)

### Carga de previos
source("Previos.R")


####################################
### Diseño del arrastrar archivo ###
####################################

fileInputArea <- function(inputId, label, multiple = FALSE, accept = NULL,
                          width = NULL, buttonLabel = "Browse...", placeholder = "No file selected") {
  restoredValue <- restoreInput(id = inputId, default = NULL)
  
  # Catch potential edge case - ensure that it's either NULL or a data frame.
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  
  inputTag <- tags$input(
    id = inputId,
    name = inputId,
    type = "file",
    # Don't use "display: none;" style, which causes keyboard accessibility issue; instead use the following workaround: https://css-tricks.com/places-its-tempting-to-use-display-none-but-dont/
    style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
    `data-restore` = restoredValue
  )
  
  if (multiple) {
    inputTag$attribs$multiple <- "multiple"
  }
  if (length(accept) > 0) {
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  }
  
  div(
    class = "form-group shiny-input-container",
    style = htmltools::css(width = htmltools::validateCssUnit(width)),
    shiny:::shinyInputLabel(inputId, ""),
    div(
      class = "input-group",
      # input-group-prepend is for bootstrap 4 compat
      tags$label(
        class = "input-group-btn input-group-prepend",
        span(
          class = "btn btn-area", inputTag,
          div(tags$image(src = icon_encoded, width = "80px;"), style = "margin-top: 0rem;"),
          div(p(label), style = "font-size: 1.2rem; font-weight: 700; padding-top: 0rem;"),
          div(p(buttonLabel), style = "font-size: 1rem; font-weight: 400; margin-bottom: 0rem;")
        )
      )
    ),
    tags$div(
      id = paste(inputId, "_progress", sep = ""),
      class = "progress active shiny-file-input-progress",
      tags$div(class = "progress-bar")
    )
  )
}

# Use Bootstrap 5 colors $gray-700 and $gray-600
css_btn_area <- textConnection("
.btn-area {
  color: #495057;
  border-color: #495057;
  border-style: dashed;
  border-width: 2px;
  border-radius: 20px;
  background-color: transparent;
}

.btn-area:hover {
  color: #6c757d;
}

.progress {
  height: 32px;
}

.progress .progress-bar {
  font-size: 16px;
  line-height: 28px;
}")

# Icon from <https://icons.getbootstrap.com/icons/upload/>
icon_file <- tempfile(fileext = ".svg")
writeLines('
<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="#495057" class="bi bi-upload" viewBox="0 0 16 16">
  <path d="M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5z"/>
  <path d="M7.646 1.146a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1-.708.708L8.5 2.707V11.5a.5.5 0 0 1-1 0V2.707L5.354 4.854a.5.5 0 1 1-.708-.708l3-3z"/>
</svg>',
           con = icon_file
)
icon_encoded <- xfun::base64_uri(icon_file)



########################
### Inicio de la app ###
########################

ui <- page_sidebar(
  useShinyjs(),
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
  tags$script(HTML("
     Shiny.addCustomMessageHandler('simulateMarkerClick', function(data) {
      console.log('simulateMarkerClick recibido con:', data);
      var w = HTMLWidgets.find('#mapa');
      if(!w) return;
      var map = w.getMap();
      map.eachLayer(function(layer){
        if(layer.options && layer.options.layerId == data.id){
          console.log('Layer detectado:', layer.options.layerId);
          try { layer.fire('click'); }
          catch(e) { console.warn('no se pudo disparar click en layer', e); }
        }
      });
    });
  ")),
  
  
  
  sidebar = sidebar(
    width = 700,
    tags$img(
      src = "https://raw.githubusercontent.com/Eduardo-Alanis-Garcia/Js/main/Planeacion_dorado.png",     
      height = "50px",      
      width = "auto",       
      style = "display: block; margin: 0 auto;" # Centrar en el sidebar
    ),
    
    h2("Cálculo de Accesibilidad"),
    HTML(
      "<p>
             La accesibilidad se calcula como el costo de traslado a un lugar de destino predefinido. Para obtenerlo, se considera:
             <ul>
               <li><strong>Vialidades carreteras</strong> en el estado, así como sus velocidades promedio.</li>
               <li>Tipo de <strong>uso de suelo</strong>.</li>
               <li>Modelo digital de <strong>elevación</strong>.</li>
             </ul>
             Un modelo de movilidad sobre grafos determina el costo mínimo de traslado (en minutos) desde cada punto del estado hacia el más cercano de los lugares destino.
           </p>"
    ),
    h4("Agrega las ubicaciones, al subir un archivo", id = "titulo_mensaje_subido"),
      ### Añadir el archivo
    card(
      id = "subir_archivo",
      # title = "Agrega las ubicaciones. Puedes seleccionar varios archivos o subir un archivo .rar",
      # Centering content and adding scroll if content overflows
      div(id = "upload_area", style = "display: flex; flex-direction: column; justify-content: center; align-items: center; padding: 0px; overflow-y: auto;", # Added padding and overflow-y
          fileInputArea(
            inputId = "filemap",
            label = "Arrastra o selecciona tus archivos .shp, .dbf, .shx, .prj, etc. aquí:",
            buttonLabel = "Click para seleccionar archivos",
            multiple = TRUE,
            accept = c('.shp',".kml",".GeoJSON",".kmz", ".rar", ".zip")
          ),
          shiny::tableOutput("files")
      ),
    ),
    
    h4("Datos"),
    DTOutput("puntos"), # Mostrar los puntos guardados
    actionButton(inputId = "borrar_puntos", label = "Eliminar punto", class = "btn btn-outline-danger"),
    actionButton(inputId = "accesibilidad", label = "Iniciar Accesibilidad", class = "btn btn-outline-success"),
    downloadButton(outputId = "downloadTiff", label = "Descargar TIFF", class = "btn btn-info"),
  ),
  leafletOutput("mapa", height = "100vh"),
)

server <- function(input, output, session) {
  
  #######################
  ### Carga de datos ####
  #######################
  
  datos_carga = reactive({
    req(input$filemap)
    temp_dir <- rutina_crear_copias_temporales(input$filemap)
    shapes=list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    kmls=list.files(temp_dir, pattern = "\\.kml$", full.names = TRUE)
    geojsons=list.files(temp_dir, pattern = "\\.geojson$", full.names = TRUE)
    para_leer=list(shapes,kmls,geojsons)[which.max(list(shapes,kmls,geojsons) |> lapply(length))]
    if(which.max(list(shapes,kmls,geojsons) |> lapply(length))==1){
      read_sf(para_leer) |> st_zm()
    }
    else{
      st_read(para_leer)|> st_zm()
    }
  })
  
  datos = reactive({
    req(datos_carga())
    if(is.na(st_crs(datos_carga()))){
      datos = st_set_crs(datos_carga(),value = sf::st_crs(mun) )
    }else{
      datos = datos_carga()
    }
    datos = datos |> st_transform(sf::st_crs(mun))
    datos = datos |> 
      dplyr::mutate(id = 1:nrow(datos))
    
    
    coordenadas = sf::st_coordinates(datos) |>  as.data.frame()
    coordenadas = coordenadas |> 
      dplyr::rename(longitud = X,
                    latitud = Y)
    
    datos = dplyr::bind_cols(datos, coordenadas)
    datos
  })
  
  ######################################
  ### Eliminar donde añades archivos ###
  ######################################
  
  observeEvent(input$filemap, {
    req(input$filemap) 
    insertUI(
      selector = "#subir_archivo",
      where = "afterEnd",
      ui = tags$div(
        id = "mensaje_subido",
        tags$h4("✅ Archivo(s) subido(s) correctamente")
      )
    )
    removeUI(selector = "#subir_archivo")
    removeUI(selector = "#titulo_mensaje_subido")
  })
  
  
  
  
  
  

  
  # Mostrar el mapa sin nada
  output$mapa <- renderLeaflet({
    leaflet(options = leafletOptions(doubleClickZoom = FALSE)) |>
      addTiles() |>
      addPolygons(data = mun, color = "black", fillColor = "black", fillOpacity = 0.1, weight = 1,
                  label = paste0("Municipio: ", mun$NOM_MUN)
      ) 
  })
  
  # Inicializa siempre
  puntos <- reactiveVal(data.frame(id = numeric(0), latitud = numeric(0), longitud = numeric(0)))
  
  observe({
    req(datos())  # asegura que no sea NULL
    
    if (nrow(datos()) == 0) {
      puntos(data.frame(id = numeric(0), latitud = numeric(0), longitud = numeric(0)))
    } else {
      puntos(datos())
    }
  })
  
  observe({
    req(puntos()) 
    req(nrow(puntos()) > 0)
    
    leafletProxy("mapa") |>
      clearMarkers() |>  
      addMarkers(
        lng = puntos()$longitud,
        lat = puntos()$latitud,
        layerId = as.character(puntos()$id),
        label = lapply(
          paste0(
            "ID: ", "<b>", puntos()$id, "</b>", "<br>", 
            "Latitud: ", "<b>", round(puntos()$latitud, 4), "</b>", "<br>", 
            "Longitud: ", "<b>", round(puntos()$longitud, 4), "</b>"
          ),
          htmltools::HTML
        )
      )
  })
  
  ######################################
  ### Click en el mapa añadir puntos ###
  ######################################
  
  observeEvent(input$mapa_click, {
    click = input$mapa_click
    cat("Tenemos que nrow(puntos()) es: ", nrow(puntos()), "\n")
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
    
    cat("El que nos interesa: ", which(puntos()$id == as.integer(marcador_seleccionado)), "donde su clase es: ", which(puntos()$id == as.integer(marcador_seleccionado)) |>  class(), "\n")
    fila_coincide = which(puntos()$id == as.integer(marcador_seleccionado))
    
    # Seleccionar esa fila en la tabla
    selectRows(dataTableProxy("puntos"), as.integer(fila_coincide))
  })
  
  ### Seleccionas fila entonces se selecciona marcador
  ### Todavia tiene errores que debo de verificar
  observeEvent(input$puntos_rows_selected, {
    fila_seleccionada = input$puntos_rows_selected
    cat("Fila seleccionada: ", fila_seleccionada, " que tiene la clase de ", class(fila_seleccionada), "\n")
    
    datos_fila = puntos()[fila_seleccionada, ]
    fila_id = datos_fila$id
    cat("Tenemos que Fila id es: ", fila_id)
    
    if (!is.null(fila_id) && length(fila_id) > 0) {
      session$sendCustomMessage("simulateMarkerClick", list(id = as.character(fila_id)))
      leafletProxy("mapa") |>
        setView(lng = datos_fila$longitud, lat = datos_fila$latitud, zoom = 11)
    }
  })
  
  
  
  ##########################################
  ### Eliminar seleccionados en la tabla ###
  ##########################################
  observeEvent(input$borrar_puntos, {
    
    seleccionado = input$puntos_rows_selected
    cat("Imprimiendo fila seleccionada id: ", seleccionado, " donde su clase es", seleccionado |>  class(), "\n")
    
    if (is.null(seleccionado) || length(seleccionado) == 0) {
      showNotification("Selecciona una fila en la tabla o un marcador en el mapa antes de borrar.", type = "warning")
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
                     lapply(FUN = function(x) { htmltools::HTML(x)})
        )
    } else{
      leafletProxy("mapa") |>  clearMarkers()
    }
  })
}

shinyApp(ui, server)
