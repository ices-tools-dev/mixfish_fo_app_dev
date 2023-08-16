## code to prepare `eco_shape` dataset goes here

eco_shape <- sf::st_read(dsn = "./dev/App/Data/shape_eco_simplified", 
        layer = "shape_eco_simplified")

# Change one Ecoregion name (this comes handy when we filter the stock list table)

eco_shape <- dplyr::filter(eco_shape, !Ecoregion %in% c("Western Mediterranean Sea", "Ionian Sea and the Central Mediterranean Sea", "Adriatic Sea", "Black Sea", "Aegean-Levantine Sea"))
# Add an id to each ecoregion (this potentially can be eliminated because the ecoregions in the shape file have already an id)
eco_shape$uid <- paste0("P", 1:12)

usethis::use_data(eco_shape, overwrite = TRUE)
