## code to prepare `DATASET` dataset goes here

# LaBr
LaBr <- rxylib::read_xyData("data-raw/LaBr.CNF")
LaBr <- spc[["dataset"]][["InSpector 1000 spectrum"]][["data_block"]]
LaBr <- as.data.frame(LaBr)
colnames(LaBr) <- c("energy", "count")
usethis::use_data(LaBr, overwrite = FALSE)

# BEGe
BEGe <- rxylib::read_xyData("data-raw/BEGe.CNF")
BEGe <- BEGe[["dataset"]][["Sample title."]][["data_block"]]
BEGe <- as.data.frame(BEGe)
colnames(BEGe) <- c("energy", "count")
usethis::use_data(BEGe, overwrite = FALSE)
