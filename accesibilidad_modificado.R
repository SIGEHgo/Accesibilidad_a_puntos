library(raster)
library(gdistance)
library(sf)

# Cargar capas
uso_de_suelo = raster("Accesibilidad/uso_de_suelo_friccion.tif")
pendiente    = raster("Accesibilidad/pendiente.tif")             # Esta en porcentaje donde Pendiente(%)=tan(θ)×100
hidalgo      = st_read("Accesibilidad/hidalgo/LIM_MUNICIPALES.shp")
pendiente[pendiente>45]=NA
pendiente|> plot()

# Convertir % pendiente a rise/run
#s=pendiente/100
s = tan(pendiente)
plot(s)
# Tobler para caminata (km/h)
tobler_kmh = 6 * exp(-0.4 * abs(s + 0.05))
#tobler_kmh |> plot()
# Velocidad ajustada por uso de suelo
terrain_walk_kmh = tobler_kmh * uso_de_suelo
terrain_walk_kmh[terrain_walk_kmh < 0.1] = 0.1  # evitar infinitos
friction = 1 / (terrain_walk_kmh * 1000 / 60)

# Crear matriz de transición
Tr = transition(friction, function(x) 1/mean(x), directions=16)
Tr = geoCorrection(Tr, type="c")



mun = sf::read_sf("Accesibilidad/municipiosjair.shp")
punto_A = sf::st_sfc(st_point(c(-98.92643, 19.87149)), crs = sf::st_crs(mun))  
punto_B = sf::st_sfc(st_point(c(-98.92005, 19.86936)), crs = sf::st_crs(mun)) 

punto_A = sf::st_sfc(st_point(c(-98.73737784166791, 20.122591208691354)), crs = sf::st_crs(mun))  
punto_B = sf::st_sfc(st_point(c(-98.74678117565801, 20.116019827515164)), crs = sf::st_crs(mun)) 

punto_A = sf::st_sfc(st_point(c(-98.24321844316829, 20.480043117214482)), crs = sf::st_crs(mun))  
punto_B = sf::st_sfc(st_point(c(-98.25371736820487, 20.4607047557211)), crs = sf::st_crs(mun)) 


# Otro pnto

punto_A = sf::st_transform(punto_A, crs(friction))
punto_B = sf::st_transform(punto_B, crs(friction))

coordenadas_A = sf::st_coordinates(punto_A)
coordenadas_B = sf::st_coordinates(punto_B)


tiempo_AB = gdistance::costDistance(Tr, fromCoords = coordenadas_A, toCoords = coordenadas_B)
camino = gdistance::shortestPath(Tr, origin = coordenadas_A, goal = coordenadas_B, output="SpatialLines")

corte_friction = raster::crop(friction, camino@bbox + 100)
plot(corte_friction, main="Ruta más rápida de A a B")
plot(camino, add=TRUE, col="red", lwd=2)
plot(punto_A, col="blue", pch=16, add=TRUE)
plot(punto_B, col="green", pch=16, add=TRUE)
legend("topright", legend=c("A","B","Ruta más rápida"), 
       col=c("blue","green","red"), pch=c(16,16,NA), lty=c(NA,NA,1))

print(tiempo_AB)


