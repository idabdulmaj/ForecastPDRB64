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
# cek working directory
getwd()

# Library yang Dibutuhkan
library(ForecastPDRB64)
cek.package.nya()

# Import Data PDRB dan Status Seasonal
pdrb_df <- read.xlsx("1. Data Input/pdrb.xlsx")
data.pdrb <- pdrb_df[, 3:ncol(pdrb_df)]
data.seasonal <- read.xlsx("1. Data Input/seasonal.xlsx")

# Forecasting PDRB
suppressWarnings(forecast.pdrb.64(data.pdrb, data.seasonal))
buka.hasil()