## code to prepare `stfFltSum` dataset goes here

stfFltSum <- read.table("./dev/App/Data/stfFltSum.csv")

usethis::use_data(stfFltSum, overwrite = TRUE)
