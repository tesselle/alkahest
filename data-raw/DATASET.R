## code to prepare `DATASET` dataset goes here

# Powder XRD (BDX15664)
XRD <- rxylib::read_xyData("data-raw/XRD.raw")
XRD <- XRD[["dataset"]][[1]][["data_block"]]
XRD <- as.data.frame(XRD)
colnames(XRD) <- c("theta", "count")
XRD <- XRD[XRD$theta >= 10, ]
usethis::use_data(XRD, overwrite = FALSE)

# gamma spectrometry (LaBr)
LaBr <- rxylib::read_xyData("data-raw/LaBr.CNF")
LaBr <- LaBr[["dataset"]][["InSpector 1000 spectrum"]][["data_block"]]
LaBr <- as.data.frame(LaBr)
colnames(LaBr) <- c("energy", "count")
usethis::use_data(LaBr, overwrite = FALSE)

# gamma spectrometry (BEGe)
BEGe <- rxylib::read_xyData("data-raw/BEGe.CNF")
BEGe <- BEGe[["dataset"]][["Sample title."]][["data_block"]]
BEGe <- as.data.frame(BEGe)
colnames(BEGe) <- c("energy", "count")
usethis::use_data(BEGe, overwrite = FALSE)

# Raman (BDX14722)
Raman <- read.delim("data-raw/Raman.PRN", header = FALSE)
Raman <- Raman[-1183, ]
Raman[] <- lapply(X = Raman, FUN = as.numeric)
colnames(Raman) <- c("shift", "intensity")
usethis::use_data(Raman, overwrite = FALSE)
