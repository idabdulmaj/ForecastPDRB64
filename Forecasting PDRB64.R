# Penjelasan
browseURL("https://github.com/idabdulmaj/ForecastPDRB64/blob/Utama/README.md")

# ==========================================================================

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

# Data Loading
pdrb_df <- read.xlsx("1. Data Input/pdrb.xlsx")
data.pdrb <- pdrb_df[, 3:ncol(pdrb_df)]

# Forecasting PDRB
suppressWarnings(forecast.pdrb.64(data.pdrb))
