# Penjelasan
browseURL("https://github.com/idabdulmaj/ForecastPDRB64/blob/Utama/README.md")

# Persiapan - Install Package yang Dibutuhkan
# Install Package Forecasting PDRB (HANYA UNTUK PERTAMA)
install.packages("devtools")
devtools::install_github("idabdulmaj/ForecastPDRB64")

# ==========================================================================

# Persiapan - Mengatur Working Directory
dir.path <- dirname(file.choose())
setwd(dir.path)

# Library yang Dibutuhkan
library(ForecastPDRB64)
cek.package.nya()

# Import Data PDRB
data_pdrb <- load.data.pdrb()

# Forecasting PDRB
suppressWarnings(forecast.pdrb.64(data_pdrb))
buka.hasil()