## code to prepare `shape_eu` dataset goes here

map_shape <- sf::st_read(dsn = "./dev/App/Data/world_map", 
                    layer = "world_map_simplified")

usethis::use_data(map_shape, overwrite = TRUE)
