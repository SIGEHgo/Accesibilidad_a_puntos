library(raster)
library(terra)
uso_de_suelo=raster("Accesibilidad/uso_de_suelo_friccion.tif")
pendiente=raster("Accesibilidad/pendiente.tif")
carreteras=raster("Accesibilidad/carreteras.tif")


slp_walk = 6 * exp(-0.5 * abs(tan(pendiente * pi / 180) + 0.05))  # Calcula la velocidad de caminata ajustada por la pendiente.
terrain_walk_spd = uso_de_suelo * slp_walk# Calcula la velocidad sobre el terreno ajustada por la pendiente y el uso de suelo.
slp_car = 6 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.05))  #  Calcula la velocidad sobre carreteras ajustada por la pendiente.
sloped_road_spd = (carreteras*0+1) * slp_car  # Calcula la velocidad ajustada por pendiente para carreteras y la convierte en un raster

merged_spd = merge(sloped_road_spd, terrain_walk_spd)
friction = 1.0 / (merged_spd * 1000 / 60.0 )

library(gdistance)
Trans = transition(friction, function(x) 1 / mean(x), 8)  # Crea una matriz de transición basada en la fricción.
T.GC = geoCorrection(Trans, type="c")
hidalgo=st_read("Accesibilidad/hidalgo/LIM_MUNICIPALES.shp")






lugares_destino_ficticios = st_sample(hidalgo$geometry,7)
coordenadas = sf::st_coordinates(lugares_destino_ficticios)
tiempo = accCost(T.GC, coordenadas) 
plot(tiempo, main='Tiempo de traslado al destino más cercano',ylab=' ',xaxt='n',yaxt='n',xlab=' ')
plot(lugares_destino_ficticios,add=T)
legend("topleft", legend=c("Lugares destino"), pch = 'o', cex=0.5)





library(gdistance)
library(sf)

mun = sf::read_sf("Accesibilidad/municipiosjair.shp")
punto_A = sf::st_sfc(st_point(c(-98.92643, 19.87149)), crs = sf::st_crs(mun))  
punto_B = sf::st_sfc(st_point(c(-98.92005, 19.86936)), crs = sf::st_crs(mun))  

punto_A = sf::st_sfc(st_point(c(-98.73737784166791, 20.122591208691354)), crs = sf::st_crs(mun))  
punto_B = sf::st_sfc(st_point(c(-98.74678117565801, 20.116019827515164)), crs = sf::st_crs(mun)) 

#20.480043117214482, -98.24321844316829
#20.4607047557211, -98.25371736820487

punto_A = sf::st_sfc(st_point(c(-98.24321844316829, 20.480043117214482)), crs = sf::st_crs(mun))  
punto_B = sf::st_sfc(st_point(c(-98.25371736820487, 20.4607047557211)), crs = sf::st_crs(mun)) 


punto_A = sf::st_transform(punto_A, crs(friction))
punto_B = sf::st_transform(punto_B, crs(friction))

coordenadas_A = sf::st_coordinates(punto_A)
coordenadas_B = sf::st_coordinates(punto_B)


tiempo_AB = gdistance::costDistance(T.GC, fromCoords = coordenadas_A, toCoords = coordenadas_B)
camino = gdistance::shortestPath(T.GC, origin = coordenadas_A, goal = coordenadas_B, output="SpatialLines")



corte_friction = raster::crop(friction, camino@bbox + 100)
plot(corte_friction, main="Ruta más rápida de A a B")
plot(camino, add=TRUE, col="red", lwd=2)
plot(punto_A, col="blue", pch=16, add=TRUE)
plot(punto_B, col="blue", pch=16, add=TRUE)

legend("topright", legend=c("A","B","Ruta más rápida"), 
       col=c("blue","green","red"), pch=c(16,16,NA), lty=c(NA,NA,1))

print(tiempo_AB)
