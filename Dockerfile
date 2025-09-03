# Dockerfile (recomendado)
FROM rocker/geospatial:latest

ENV DEBIAN_FRONTEND=noninteractive

# paquetes de sistema extra que sf y otros paquetes R suelen necesitar
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libarchive-dev \
  && rm -rf /var/lib/apt/lists/*

# instalar paquetes R (usa repositorio HTTPS)
RUN R -e "install.packages(c('shiny','leaflet','bslib','sf','shinyjs','DT','archive','shinycssloaders','waiter','leaflet.extras','leaflet.extras2','shinyWidgets','leafem','viridis','raster','gdistance','openxlsx'), repos='https://cran.rstudio.com')"

# copiar la app al directorio típico de shiny-server
COPY . /home/shiny-app 

EXPOSE 8180

CMD ["R", "-e", "shiny::runApp('/home/shiny-app/app.R', port=8180, host='0.0.0.0')"]
