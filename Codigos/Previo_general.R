library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflegend)
library(leafem)
library(archive)
library(DT)
library(sf)
library(viridis)
library(raster)
library(gdistance)


#################
### Funciones ###
#################

# Archivos Temporales
rutina_crear_copias_temporales <- function(inputFiles) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  if (!grepl("\\.(rar|zip|kmz)$", inputFiles$datapath[1], ignore.case = TRUE)) {
    for (i in seq_along(inputFiles$name)) {
      file.copy(inputFiles$datapath[i], file.path(temp_dir, inputFiles$name[i]))
    }
  } else {
    file.copy(inputFiles$datapath[1], file.path(temp_dir, inputFiles$name[1]))
    archive_extract(file.path(temp_dir, inputFiles$name[1]), dir = temp_dir)
  }
  return(temp_dir)
}

cortes <- function(paleta, valores) {
  if (attr(paleta, "colorType") == "numeric") {
    bins <- 7
    cuts <- if (length(bins) == 1) pretty(valores, n = bins) else bins
    r <- range(valores, na.rm = TRUE)
    cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
    n <- length(cuts)
    p <- (cuts - r[1]) / (r[2] - r[1])
    print(p)
    colors <- paleta(c(cuts))
    return(list(n = n, colors = colors))
  } else {
    stop("La paleta no es numérica")
  }
}

mun = sf::read_sf("Accesibilidad/municipiosjair.shp")
hidalgo= sf::st_read("Accesibilidad/hidalgo/LIM_MUNICIPALES.shp")

###############################
##### Accesibilidad Previa ####
###############################

municipios = sf::read_sf("Accesibilidad//municipiosjair.shp")
#setwd("Accesibilidad/Accesibilidad/")
uso_de_suelo=raster("Accesibilidad/uso_de_suelo_friccion.tif")
pendiente=raster("Accesibilidad/pendiente.tif")
carreteras=raster("Accesibilidad/carreteras.tif")
extent(carreteras)==extent(pendiente) & extent(uso_de_suelo)==extent(pendiente)

#Sí me voy a tomar la libertad de actualizar los valores del raster que estén cerca de 90 grados
pendiente[pendiente<95.9 & pendiente>=90]=95.9
pendiente[pendiente<=90 & pendiente>84.9]=84.9



