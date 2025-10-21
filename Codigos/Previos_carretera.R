####Accesibilidad a pie
slp_walk = 6 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.05))  # Calcula la velocidad de caminata ajustada por la pendiente.
terrain_walk_spd = uso_de_suelo * slp_walk       #Le quité el /5.0. Quiero pensar que es la velocidad de caminata según uso de suelo. El promedio es de 5.5 km/h         # Calcula la velocidad sobre el terreno ajustada por la pendiente y el uso de suelo.

##Accesibilidad por carreteras
slp_car = 50 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.12))  # Calcula la velocidad sobre carreteras ajustada por la pendiente.
sloped_road_spd = carreteras * slp_car / 50.0 # Calcula la velocidad ajustada por pendiente para carreteras y la convierte en un raster.
merged_spd = merge(sloped_road_spd, terrain_walk_spd)     # Combina los rasters de velocidad de carreteras y terreno.
friction = 1.0 / (merged_spd * 1000 / 60.0 ) 

# library(gdistance)
# Trans = transition(friction, function(x) 1 / mean(x), 8)  # Crea una matriz de transición basada en la fricción.
# T.GC = geoCorrection(Trans, type="c") 
# 
# 
# saveRDS(object = Trans, file = "Output/Trans_carretera.rds")
#saveRDS(object = T.GC, file = "Output/T.GC_carretera.rds")

