FROM rocker/geospatial:4.5.0

ENV DEBIAN_FRONTEND=noninteractive

# Instalar dependencias de sistema adicionales
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libarchive-dev \
  && rm -rf /var/lib/apt/lists/*

# Instalar paquetes R (repos Posit para estabilidad)
RUN R -e "options(repos = c(CRAN='https://packagemanager.posit.co/cran/latest')); \
          install.packages(c('shiny','leaflet','bslib','sf','shinyjs','DT','archive','shinycssloaders','waiter','leaflet.extras','leaflet.extras2','shinyWidgets','leafem','viridis','raster','gdistance','openxlsx'))"

# Copiar la app al contenedor
WORKDIR /home/shiny-app
COPY . /home/shiny-app

# Exponer puerto
EXPOSE 3838

# Ejecutar con shiny directamente (si no usas shiny-server)
CMD ["R", "-e", "shiny::runApp('/home/shiny-app/app.R', port=3838, host='0.0.0.0')"]
