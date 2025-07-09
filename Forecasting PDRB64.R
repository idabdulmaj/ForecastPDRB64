# Mengatur Working Directory
setwd(dirname(file.choose()))

# Persiapan - Load Library
if (!requireNamespace("ForecastPDRB64", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::install_github("idabdulmaj/ForecastPDRB64")
}

library(ForecastPDRB64)
cek.package.nya()

# Import Data & Forecasting PDRB
data_pdrb <- load.data.pdrb()
suppressWarnings(forecast.pdrb.64(data_pdrb))
buka.hasil()