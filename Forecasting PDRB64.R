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

# Import Data dan Penyiapan dataframe
pdrb_df <- read.xlsx("1. Data Input/pdrb.xlsx", "pdrb")
pdrb_df <- t(pdrb_df)
colnames(pdrb_df) <- pdrb_df[1, ]
data.pdrb <- pdrb_df[2:nrow(pdrb_df), ]
data.seasonal <- read.xlsx("1. Data Input/pdrb.xlsx", "seasonal")

# Forecasting PDRB
suppressWarnings(forecast.pdrb.64(data.pdrb, data.seasonal))
buka.hasil()