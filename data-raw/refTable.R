## code to prepare `refTable` dataset goes here
refTable <- read.table("./dev/App/Data/refTable.csv")

usethis::use_data(refTable, overwrite = TRUE)
