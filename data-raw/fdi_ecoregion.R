## code to prepare `fdi_ecoregion` dataset goes here

fdi_data <- data.table::fread("./dev/App/Data/fdi_ecoregion/all_ecoregions.csv")

usethis::use_data(fdi_data, overwrite = TRUE)

