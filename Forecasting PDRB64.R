# Persiapan - Mengatur Working Directory
dir.path <- dirname(file.choose())
setwd(dir.path)

# Persiapan - Load Library
install.packages("devtools")
devtools::install_github("idabdulmaj/ForecastPDRB64") 

library(ForecastPDRB64)
cek.package.nya()

# Import Data PDRB
data_pdrb <- load.data.pdrb()

# Forecasting PDRB
suppressWarnings(forecast.pdrb.64(data_pdrb))
buka.hasil()