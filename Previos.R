#################
### Funciones ###
#################

### Archivos Temporales
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



mun = sf::read_sf("Accesibilidad/municipiosjair.shp")
hidalgo= sf::st_read("Accesibilidad/hidalgo/LIM_MUNICIPALES.shp")

###############################
##### Accesibilidad Previa ####
###############################

# municipios = sf::read_sf("Accesibilidad//municipiosjair.shp")
# #setwd("Accesibilidad/Accesibilidad/")
# uso_de_suelo=raster("Accesibilidad/uso_de_suelo_friccion.tif")
# pendiente=raster("Accesibilidad/pendiente.tif")
# carreteras=raster("Accesibilidad/carreteras.tif")
# extent(carreteras)==extent(pendiente) &
#   extent(uso_de_suelo)==extent(pendiente)
# 
# #Sí me voy a tomar la libertad de actualizar los valores del raster que estén cerca de 90 grados
# pendiente[pendiente<95.9 & pendiente>=90]=95.9
# pendiente[pendiente<=90 & pendiente>84.9]=84.9
# 
# ####Accesibilidad a pie
# slp_walk = 6 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.05))  # Calcula la velocidad de caminata ajustada por la pendiente.
# terrain_walk_spd = uso_de_suelo * slp_walk       #Le quité el /5.0. Quiero pensar que es la velocidad de caminata según uso de suelo. El promedio es de 5.5 km/h         # Calcula la velocidad sobre el terreno ajustada por la pendiente y el uso de suelo.
# 
# ##Accesibilidad por carreteras
# slp_car = 50 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.12))  # Calcula la velocidad sobre carreteras ajustada por la pendiente.
# sloped_road_spd = carreteras * slp_car / 50.0 # Calcula la velocidad ajustada por pendiente para carreteras y la convierte en un raster.
# merged_spd = merge(sloped_road_spd, terrain_walk_spd)     # Combina los rasters de velocidad de carreteras y terreno.
# friction = 1.0 / (merged_spd * 1000 / 60.0 ) 
# 
# library(gdistance)
# Trans = transition(friction, function(x) 1 / mean(x), 8)  # Crea una matriz de transición basada en la fricción.
# T.GC = geoCorrection(Trans, type="c") 
# 
# hidalgo= sf::st_read("Accesibilidad/hidalgo/LIM_MUNICIPALES.shp")
