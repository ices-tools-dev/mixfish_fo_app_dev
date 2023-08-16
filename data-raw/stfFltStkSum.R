## code to prepare `stfFltStkSum` dataset goes here

stfFltStkSum <- read.table("./dev/App/Data/stfFltStkSum.csv")

usethis::use_data(stfFltStkSum, overwrite = TRUE)
