# Persiapan - Install Package ForecastPDRB64
# Jika sudah terinstall, silakan langsung ke bagian "Mengatur Working Directory"
install.packages("devtools")
devtools::install_github("idabdulmaj/ForecastPDRB64")

# Mengatur Working Directory
setwd(dirname(file.choose()))

library(ForecastPDRB64)
cek.package.nya()

# Import Data & Forecasting PDRB
data_pdrb <- load.data.pdrb()
suppressWarnings(forecast.pdrb.64(data_pdrb))
buka.hasil()