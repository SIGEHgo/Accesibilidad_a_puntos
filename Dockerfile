#Usa una imagen base de R
#FROM r-base:latest 
FROM rocker/shiny
#rocker/shiny
#FROM r-base:4.3.1

# Instala las dependencias del sistema necesarias para R y sus paquetes
RUN apt-get -y update && apt-get install -y  libudunits2-dev libgdal-dev libgeos-dev libproj-dev libarchive13

# Actualiza las librerías de R  # Yo creo que estas ni se instalan.
RUN R -e "update.packages(ask = FALSE, repos = 'https://cran.rstudio.com/')"
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('raster', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gdistance', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet.extras', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflegend', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leafem', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sf', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('viridis', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('archive', repos='http://cran.rstudio.com/',dependencies=TRUE)"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('archive', repos='http://cran.rstudio.com/',dependencies=TRUE)"
# Copy our R script to the container
#Copiamos todo
COPY . /home/shiny-app 

# COPY /hidalgo /home/rstudio/hidalgo
# COPY app_new2.R /home/rstudio/app_new2.R
# COPY carreteras.tif /home/rstudio/carreteras.tif
# COPY pendiente.tif /home/rstudio/pendiente.tif
# COPY uso_de_suelo_friccion.tif /home/rstudio/uso_de_suelo_friccion.tif
#RUN R -e "source('Accesibilidad/libraries.R')"

#Lo vamos a ver en el localhost:8180
EXPOSE 8180

#Las rutas dentro de app.R deben ser relativas a /home/shiny-app. Es decir, asumir que estamos en shiny-app/
CMD ["R", "-e", "shiny::runApp('/home/shiny-app/app.R', port=8180, host='0.0.0.0')"]
#El host= 0.0.0.0 es para que sea accesible desde fuera del contenedor (en localhost:8180)



#Para construir la imagen, en la terminal:
# docker build . -t accessibility_app

# docker run -p 8180:8180 accessibility_app

#Checar localhost:8180