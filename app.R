###########################
# Parche de actualizacion #
###########################

# Puntos solo se pueden colocar dentro de Hidalgo 
# Boton al mapa para centrar                     
# Marcadores solo colocar en Hidalgo               
# Boton para subir otro archivo                   
# Deslizador para tiempo maximo                   
# Boton para limpiar el mapa                      
# Boton para descargar tabla                     
# Realizar delineado de contorno         


library(shiny)
library(leaflet)
library(bslib)
library(sf)
library(shinyjs)
library(DT)
library(archive)
library(shinycssloaders)
library(waiter)
library(leaflet.extras)
library(leaflet.extras2)
library(shinyWidgets)

### Carga de previos
#source("Previos.R")
source("Previos_caminando.R")


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
  useWaiter(),
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
  
  tags$style(HTML("
    .sk-fading-circle .sk-circle:before {
      background-color: #9c2141 !important; 
    }
  ")),
  
  # Buscador de capas en el mapa
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
  
  # Boton para eliminar un marcador
  tags$script(HTML("
    document.addEventListener('keydown', function(e) {
      if ((e.key === 'Delete' || e.key === 'Backspace') && 
          e.target.tagName !== 'INPUT' && e.target.tagName !== 'TEXTAREA') {
        Shiny.setInputValue('delete_key', new Date().getTime());
      }
    });
  ")),
  
  ### Borrar toda la tabla
  tags$script(HTML("
    document.addEventListener('keydown', function(e) {
      if ((e.key === 'Delete' || e.key === 'Backspace') && e.shiftKey &&
          e.target.tagName !== 'INPUT' && e.target.tagName !== 'TEXTAREA') {
        Shiny.setInputValue('shift_delete_key', new Date().getTime());
      }
    });
  ")),
  
  tags$style(HTML("
      .btn-outline-gob1 {
        color: #9c2141;
        border-color: #9c2141;
        background-color: white;
      }
      .btn-outline-gob1:hover {
        background-color: #9c2141;
        color: white;
      }
    ")),
  
  tags$style(HTML("
      .btn-outline-gob2 {
        color: #b38e5d;
        border-color: #b38e5d;
        background-color: white;
      }
      .btn-outline-gob2:hover {
        background-color: #b38e5d;
        color: white;
      }
    ")),
  
  tags$style(
    HTML("
    #shiny-notification-panel#shiny-notification-panel{
      position: fixed ;
      bottom: calc(var(--bslib-spacer, 1rem) / 2);
      left: calc(var(--bslib-spacer, 1rem) / 2);
      right: auto !important;  
    }
  ")
  ),
  
  tags$style(HTML("
      /* Estilo personalizado para botones default */
      .btn-outline-default, 
      .btn-default:not(.btn-primary,.btn-secondary,.btn-info,
                       .btn-success,.btn-danger,.btn-warning,
                       .btn-light,.btn-dark,.btn-link,
                       [class*='btn-outline-']) {
        --bs-btn-color: #b38e5d;
        --bs-btn-border-color: #b38e5d;
        --bs-btn-hover-color: #fff;
        --bs-btn-hover-bg: #b38e5d;
        --bs-btn-hover-border-color: #b38e5d;
        --bs-btn-focus-shadow-rgb: 64, 64, 64;
        --bs-btn-active-color: #fff;
        --bs-btn-active-bg: #b38e5d;
        --bs-btn-active-border-color: #b38e5d;
        --bs-btn-active-shadow: inset 0 3px 5px rgba(0, 0, 0, 0.125);
        --bs-btn-disabled-color: #404040;
        --bs-btn-disabled-bg: transparent;
        --bs-btn-disabled-border-color: #404040;
        --bs-btn-bg: transparent;
        --bs-gradient: none;
      }
    ")),
  
  
  
  sidebar = sidebar(
    position = "right",
    width = 700,
    tags$img(
      src = "https://raw.githubusercontent.com/Eduardo-Alanis-Garcia/Js/main/Planeacion_dorado.png",     
      height = "50px",      
      width = "auto",       
      style = "display: block; margin: 0 auto;" # Centrar en el sidebar
    ),
    
    h2("Cálculo de Accesibilidad caminando"),
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
    
    div(id = "upload_placeholder",
        tags$h4("Agrega las ubicaciones, al subir un archivo", id = "titulo_mensaje_subido"),
        card(
          id = "subir_archivo",
          div(id = "upload_area", style = "display: flex; flex-direction: column; justify-content: center; align-items: center; padding: 0px; overflow-y: auto;", # Added padding and overflow-y
              fileInputArea(
                inputId = "filemap",
                label = "Arrastra o selecciona tus archivos .shp, .dbf, .shx, .prj, .rar, .zip, .geojson, .kmz, .kml, etc. aquí:",
                buttonLabel = "Click para seleccionar archivos",
                multiple = TRUE,
                accept = c('.shp',".kml",".GeoJSON",".kmz", ".rar", ".zip")
              ),
              shiny::tableOutput("files")
          ),
        ),
        # mensaje que aparece AL SUBIR, inicialmente oculto
        hidden(
          div(id = "mensaje_subido",
              tags$h4("✅ Archivo(s) subido(s) correctamente")
          )
        )
    ),
    
    h4("Datos"),
    DTOutput("puntos"), # Mostrar los puntos guardados
    fluidRow(
      column(width = 4, actionButton("borrar_puntos", "Eliminar punto", class = "btn-outline-gob1")),
      column(width = 4, actionButton("borrar_todo", "Limpiar tabla", class = "btn btn-outline-danger")),
      column(width = 4, dropdownButton(
        label = "Descargar Tabla",
        circle = FALSE,
        status = "default",
        icon = icon("download"),
        tooltip = tooltipOptions(title = "Elige formato"),
        downloadButton(outputId = "downloadTabla", label = "CSV"),
        downloadButton(outputId = "downloadExcel", label = "Excel")
      ))
    ),
    fluidRow(
      column(width = 4, numericInput(inputId = "slider", label = "Tiempo máximo", min = 30, max = 500, value = 300)),
      column(width = 4, actionButton(inputId = "accesibilidad", label = HTML("Calcular <br> Accesibilidad"), class = "btn btn-outline-success")),
      column(width = 4, downloadButton(outputId = "downloadTiff", label = HTML("Descargar <br> TIFF"), class = "btn btn-outline-primary"))
    )
  ),
  withSpinner(leafletOutput("mapa", height = "100vh"), type = 4, color = "#9c2141")
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
    datos = sf::st_drop_geometry(datos)
    cat("Vamos imprimir datos cuando se cargan: ", "\n")
    print(datos[, c((ncol(datos) -2), ncol(datos))])
    datos
  })
  
  ######################################
  ### Eliminar donde añades archivos ###
  ######################################
  
  observeEvent(input$filemap, {
    req(input$filemap) 
    show("mensaje_subido")
    hide("subir_archivo")
    hide("titulo_mensaje_subido")
  })
  
  
  
  
  
  
  
  
  # Mostrar el mapa sin nada
  output$mapa <- renderLeaflet({
    leaflet(options = leafletOptions(doubleClickZoom = FALSE)) |>
      addTiles() |>
      addPolygons(data = mun, color = "black", fillColor = "black", fillOpacity = 0.1, weight = 1,
                  label = paste0("Municipio: ", mun$NOM_MUN), group = "Hidalgo"
      ) |> 
      addResetMapButton()
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
        ),
        group = "Marcadores"
      )
  })
  
  ######################################
  ### Click en el mapa añadir puntos ###
  ######################################
  
  observeEvent(input$mapa_click, {
    click = input$mapa_click
    
    punto_click = sf::st_as_sf(
      data.frame(lon = click$lng, lat = click$lat),
      coords = c("lon", "lat"),
      crs = sf::st_crs(mun) 
    )
    
    # Checar si esta dentro de Hidalgo
    dentro = sf::st_intersects(x = punto_click, mun)
    cat("Tenemoms que dentro es: ", "\n")
    print(dentro)
    
    if (lengths(dentro) > 0) {
      cat("Tenemos que nrow(puntos()) es: ", nrow(puntos()), "\n")
      id_unico = dplyr::if_else(condition = nrow(puntos()) == 0, true = 1, false = max(puntos()$id) + 1)
      nuevo = data.frame(id = id_unico, latitud = click$lat, longitud = click$lng)
      cat("Se ha hecho click en:", click$lat, click$lng, "\n")
      cat("Vamos imprimir puntos sin hacer la union: ", "\n")
      print(puntos())
      
      # Añadir pulsados
      puntos(dplyr::bind_rows(puntos(), nuevo))
      cat("Vamos  a imprimir puntos cuando hace la union: ", "\n")
      print(puntos())
      
      # Añadir al mapa
      leafletProxy("mapa") |>
        addMarkers(lng = nuevo$longitud, lat = nuevo$latitud, layerId = as.character(nuevo$id), 
                   label = paste0(
                     "ID: ", "<b>", nuevo$id, "</b>", "<br>", 
                     "Latitud: ", "<b>", nuevo$latitud |>  round(digits = 4), "</b>", "<br>", 
                     "Longitud: ", "<b>", nuevo$longitud |>  round(digits = 4), "</b>"
                   ) |> 
                     lapply(FUN = function(x) { htmltools::HTML(x)}),
                  group = "Marcadores" 
        )
    }else{
      showNotification("El punto está fuera de Hidalgo", type = "error")
    }
    
    
    
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
  observeEvent(input$puntos_rows_selected, {
    fila_seleccionada = input$puntos_rows_selected
    cat("Fila seleccionada: ", fila_seleccionada, " que tiene la clase de ", class(fila_seleccionada), "\n")
    
    datos_fila = puntos()[fila_seleccionada, ]
    fila_id = datos_fila$id
    cat("Tenemos que Fila id es: ", fila_id)
    cat("Zoom: ")
    print(input$mapa_zoom)
    
    if (!is.null(fila_id) && length(fila_id) > 0) {
      session$sendCustomMessage("simulateMarkerClick", list(id = as.character(fila_id)))
      leafletProxy("mapa") |>
        setView(lng = datos_fila$longitud, lat = datos_fila$latitud, zoom = dplyr::if_else(condition = input$mapa_zoom >= 11, true = input$mapa_zoom, false = 11))
    }
    
    
    
  })
  
  
  
  ##########################################
  ### Eliminar seleccionados en la tabla ###
  ##########################################
  borrar_puntos_fun = function() {
    seleccionado = input$puntos_rows_selected
    cat("Imprimiendo fila seleccionada id: ", seleccionado, " donde su clase es", seleccionado |> class(), "\n")
    
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
                     lapply(FUN = function(x) { htmltools::HTML(x)}),
                   group = "Marcadores"
        )
    } else{
      
      leafletProxy("mapa") |> 
        clearMarkers() |> clearImages() |>  clearControls() |> clearShapes() |> removeLayersControl() |>  
        addPolygons(data = mun, color = "black", fillColor = "black", fillOpacity = 0.1, weight = 1,
                    label = paste0("Municipio: ", mun$NOM_MUN), group = "Hidalgo"
        ) |> 
        setView(lng = -98.88704, lat = 20.47901, zoom = 9)
      
      show("subir_archivo")
      show("titulo_mensaje_subido")
      hide("mensaje_subido")
      
      try(shinyjs::reset("subir_archivo"), silent = TRUE)
    }
  }
  
  # Boton solo lo mando a llamar
  observeEvent(input$borrar_puntos, {
    borrar_puntos_fun()
  })
  
  # Mando a llamar la funcion cuando pulso la tecla
  observeEvent(input$delete_key, {
    borrar_puntos_fun()
  })
  
  
  #####################
  ### Limpiar tabla ###
  #####################
  
  # Queda pendiente que cuando pulsas las teclas no te saque el mensaje que cuando no seleccionas nada
  
  borrar_todo = function(){
    if (is.null(puntos()) || length(puntos()) == 0 || nrow(puntos()) == 0) {
      showNotification("No tienes ningun elemento", type = "warning")
      return()
    }
    
    puntos(data.frame(id = numeric(0), latitud = numeric(0), longitud = numeric(0)))
    
    leafletProxy("mapa") |> 
      clearMarkers() |> clearImages() |>  clearControls() |> clearShapes() |> removeLayersControl() |>
      addPolygons(data = mun, color = "black", fillColor = "black", fillOpacity = 0.1, weight = 1,
                  label = paste0("Municipio: ", mun$NOM_MUN), group = "Hidalgo"
      ) |> 
      setView(lng = -98.88704, lat = 20.47901, zoom = 9)
    
    
    show("subir_archivo")
    show("titulo_mensaje_subido")
    hide("mensaje_subido")
    
    try(shinyjs::reset("subir_archivo"), silent = TRUE)
  }
  
  observeEvent(input$borrar_todo,{
    borrar_todo()
  })
  
  observeEvent(input$shift_delete_key, {
    borrar_todo()
  })
  
  
  
  
  
  ##############################
  ### Realizar accesibilidad ###
  ##############################
  
  tiempo_zona_p = reactiveVal(NULL)
  
  # Hace accesibilidad
  observeEvent(input$accesibilidad, {
    req(puntos())
    
    if (is.null(puntos()) || length(puntos()) == 0 || nrow(puntos()) == 0) {
      leafletProxy("mapa") |>  
        clearImages() |> 
        clearControls()          # Quitar la 
      
      showNotification("Selecciona un punto en el mapa o sube un archivo.", type = "warning")
      tiempo_zona_p(NULL)
      return()  
    }
    
    if (input$slider < 30 || input$slider >500) {
      showNotification("Selecciona un valor entre 30 y 500.", type = "warning")
      return() 
    }
    
    
    
    waiter_show(html = spin_fading_circles(), color = "rgba(255,255,255,0.8)")
    
    
    df = puntos()
    print("Estamos imprimiendo el df como llega: ")
    print(df)
    df = sf::st_as_sf(x = df, coords = c("longitud", "latitud"),crs = sf::st_crs(mun))
    cat("Estamos imprimiendo el df luego de pasarlo a sf: ")
    print(df)
    df = df |> st_transform(st_crs(hidalgo))
    coordenadas = sf::st_coordinates(df)
    print(coordenadas)
    tiempo_zona = accCost(T.GC, coordenadas)
    print(tiempo_zona)
    raster::crs(tiempo_zona) = crs(hidalgo)
    tiempo_zona_p(tiempo_zona)  # Lo guardo en tiempo_zona_p
    
    
    tiempo_zona[is.infinite(tiempo_zona)] = NA
    tiempo_zona[tiempo_zona >= input$slider] = input$slider
    min_valor = raster::cellStats(tiempo_zona, stat = 'min')
    max_valor = raster::cellStats(tiempo_zona, stat = 'max')
    
    print(tiempo_zona)
    print(min_valor)
    print(max_valor)
    
    paleta = colorNumeric(
      palette = viridisLite::turbo(n = length(min_valor:max_valor), direction = -1, alpha = 0.5),
      domain = values(tiempo_zona),  
      na.color = "transparent"
    )
    contorno_interes = cortes(paleta = paleta, valores = c(min_valor:max_valor))
    contornos = raster::rasterToContour(tiempo_zona, levels = seq(min_valor, max_valor, length.out = contorno_interes$n))
    cat("Imprimir colores de contorno: ")
    print(contorno_interes$colors)
    crs(contornos) = crs(hidalgo)
    
    contornos = sf::st_as_sf(x = contornos)
    contornos = sf::st_transform(x = contornos, crs = sf::st_crs(mun))
    cat("Vamos a imprimir los colores: ")
    print(contorno_interes$colors[-1])
    
    
    leafletProxy("mapa") |>  
      clearImages() |> 
      clearControls() |> 
      clearShapes() |> 
      removeLayersControl() |>
      addPolygons(data = mun, color = "black", fillColor = "black", fillOpacity = 0.1, weight = 1,
                  label = paste0("Municipio: ", mun$NOM_MUN), group = "Hidalgo"
      ) |> 
      addRasterImage(x = tiempo_zona, 
                     colors = viridis::turbo(n = length(min_valor:max_valor),
                      direction = -1, alpha = 0.5), 
                     group = "Raster",
                     layerId = "Raster") |> 
      addLegend(values = c(min_valor:max_valor) , pal = paleta, title = paste0("Tiempo", "<br>","Aproximado"), position = "bottomright",
                labFormat = labelFormat(
                  between = " – ",
                  suffix = " min",
                  transform = function(x) {x}
                )) |> 
      setView(lng = -98.88704, lat = 20.47901, zoom = 9) |> 
      addPolylines(data = contornos, color = contorno_interes$colors[-1], 
                   weight = 6, dashArray = "7,9", 
                   label = paste(contornos$level, "minutos"),
                   group = "Contorno") |>  # Primero longitud del segmento dibujado, segundo longitud del espacio en blanco entre segmentos.
      addLayersControl(overlayGroups = c("Marcadores", "Raster", "Contorno"),
                       position = "topright",
                       options = layersControlOptions(collapsed = F))
    waiter_hide()
  }) 
  
  
  
  #################
  ### Descargas ###
  #################
  
  # Descargar TIFF
  output$downloadTiff <- downloadHandler(
    filename = function() {
      paste0("mi_raster_shiny_", Sys.Date(), ".tif")
    },
    content = function(file) {
      if (is.null(tiempo_zona_p())) {
        showNotification("No se ha generado ningun TIFF.", type = "warning")
        return()
      }
      writeRaster(tiempo_zona_p(), file, overwrite = TRUE)
      showNotification("TIFF generado con éxito ✅", type = "message", duration = 5)
    }
  )
  
  
  # Descargar Tabla
  output$downloadTabla <- downloadHandler(
    filename = function() {
      paste0("mi_tabla_shiny_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (is.null(puntos()) || length(puntos()) == 0 || nrow(puntos()) == 0) {
        showNotification("No se ha generado ningun CSV.", type = "warning")
        return()
      }
      write.csv(puntos(), file, row.names = F, fileEncoding = "latin1")
      showNotification("CSV generado con éxito ✅", type = "message", duration = 5)
    }
  )
  
  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste0("mi_tabla_shiny_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if (is.null(puntos()) || length(puntos()) == 0 || nrow(puntos()) == 0) {
        showNotification("No se ha generado ningun Excel.", type = "warning")
        return()
      }
      openxlsx::write.xlsx(puntos(), file, overwrite = TRUE)
      showNotification("Excel generado con éxito ✅", type = "message", duration = 5)
    }
  )
  
  
  
}

shinyApp(ui, server)
