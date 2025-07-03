# Persiapan - Mengatur Working Directory
dir.path <- dirname(file.choose())
setwd(dir.path)

# Persiapan - Load Library
check_and_load_ForecastPDRB64 <- function() {
  if (!require("ForecastPDRB64", character.only = TRUE)) {
    if (!require("devtools", character.only = TRUE)) {
      install.packages("devtools")
      library(devtools)
    }
    devtools::install_github("idabdulmaj/ForecastPDRB64")
    library(ForecastPDRB64, character.only = TRUE)
    cek.package.nya()
  }
}
check_and_load_ForecastPDRB64()

# Import Data PDRB
data_pdrb <- load.data.pdrb()

# Forecasting PDRB
suppressWarnings(forecast.pdrb.64(data_pdrb))
buka.hasil()