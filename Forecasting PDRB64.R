# Installing ForecastPDRB64 Package ============================================
# If the package is already installed, you can skip this step ==================
install.packages("devtools")
devtools::install_github("idabdulmaj/ForecastPDRB64")

# Working Directory Setting ====================================================
setwd(dirname(file.choose()))

# Load the package and check if it's working ===================================
library(ForecastPDRB64)
package.check()

# Import Data & Forecasting PDRB ===============================================
data_pdrb <- load.data.pdrb()
suppressWarnings(forecast.pdrb.64(data_pdrb))
buka.hasil()

# Last but not the least, copy the output to compilation file ==================