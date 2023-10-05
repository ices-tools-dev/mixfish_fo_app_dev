## code to prepare `catchScenarioStock` dataset goes here

catchScenarioStk <- read.table("./dev/App/Data/catchScenStk.csv")

usethis::use_data(catchScenStk, overwrite = TRUE)
