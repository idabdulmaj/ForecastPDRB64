package.check <- function() {
  packages <- c("openxlsx", "forecast", "tibble", "ggplot2")
  packages1 <- c("mixOmics")
  packages2 <- c("ggcats")
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  missing_packages1 <- packages1[!(packages1 %in% installed.packages()[,"Package"])]
  missing_packages2 <- packages2[!(packages2 %in% installed.packages()[,"Package"])]
  
  if (length(missing_packages1) > 0) {
    cat("Package mixOmics belum terinstall. Package akan diinstall. \n")
    install.packages("BiocManager")
    BiocManager::install("mixOmics")
    cat("Package mixOmics telah terinstall. \n")
  } else {
    cat("Package mixOmics telah tersedia. \n")
  }
  
  if (length(missing_packages2) > 0) {
    cat("Package ggcats belum terinstall. Package akan diinstall. \n")
    devtools::install_github("R-CoderDotCom/ggcats@main")
    cat("Package ggcats telah terinstall. \n")
  } else {
    cat("Package ggcats telah tersedia. \n")
  }
  if (length(missing_packages) > 0) {
    cat("Package ini belum terinstall:", paste(missing_packages, collapse = ", "), ". Package akan diinstall. \n")
    install.packages(paste0(missing_packages))
    cat("Package ", paste(packages, collapse = ", "), " telah diinstall. \n")
  } else {
    cat("Package ", paste(packages, collapse = ", "), " telah tersedia. \n")
  }
  
  cat("\n===== Loading semua package yang dibutuhkan =====\n")
  
  library(openxlsx)
  library(forecast)
  library(mixOmics)
  library(tibble)
  library(ggplot2)
  library(ggcats)
  
  cat("\n===== Seluruh package yang dibutuhkan telah di-Load =====\n")
}

load.data.pdrb <- function() {
  # Load Data PDRB
  pdrb_df <- read.xlsx("1. Data Input/pdrb.xlsx", "pdrb")
  data.pdrb <- pdrb_df[, 3:ncol(pdrb_df)]
  
  # Load Data Seasonal
  data.seasonal <- read.xlsx("1. Data Input/pdrb.xlsx", "seasonal")

  return(list(pdrb_df = pdrb_df, data.pdrb = data.pdrb, data.seasonal = data.seasonal))
}

pdrb.forecast.arima <- function(pdrb_df, data_df, seasonal_df) {
  # INISIASI
  forecasted_df <- data.frame()
  fitted_df <- data.frame()
  fitted_val <- data.frame()
  plot_list <- list()

  # Melakukan forecasting untuk setiap variabel
  metrics_arima <- data.frame()

  for (i in 1:ncol(data_df)) {
    # Melakukan arima dengan parameter yang ditentukan
    if (seasonal_df[i, 2] != 1) {
      prediksi <- auto.arima(data_df[, i])
    } else {
      ts_data <- ts(data_df[, i], start = 1, frequency = 4)
      prediksi <- auto.arima(ts_data)
    }

    # Melakukan forecasting
    forecasted_values <- forecast(prediksi, h = 1)

    # Menambahkan hasil forecasting ke dalam dataframe
    forecasted_df[1, i] <- forecasted_values$mean
    forecasted_df[2, i] <- forecasted_values$upper
    forecasted_df[3, i] <- forecasted_values$lower

    # Menambahkan hasil fitted ke dalam dataframe
    fitted_val <- ts(forecasted_values$fitted, frequency = 1)
    for (j in 1:nrow(data_df)) {
      fitted_df[j, i] <- fitted_val[j]
    }


    # Menyimpan hasil forecast dalam plot
    mypath <- file.path("2. ARIMA Plot dan Model", paste0("ARIMA - ", i, ". ", colnames(data_df[i]), ".png"))
    png(mypath)
    plot_list[i] <- plot(forecasted_values, col = "red", main = paste0(colnames(data_df[i])), ylab = "Nilai", xlab = "Triwulan ke")
    plot_list[i] <- lines(forecasted_values$fitted, pch = 20, col = "green")
    plot_list[i] <- legend("topleft", c("Smoothed data", "Actual data"), lty = 8, col = c("green", "red"), cex = 0.8)
    dev.off()

    # Mencetak hasil forecast dalam list
    cat("-----------------------------------------------------------------------", "\n")
    cat(colnames(data_df)[i], "\n")
    cat("-----------------------------------------------------------------------", "\n")
    print(summary(prediksi))
    cat("\n", "\n")

    # Collect mean error (ME) metric
    me_val <- tryCatch({
      acc <- accuracy(prediksi)
      acc[1, "ME"]
    }, error = function(e) NA)

    metrics_arima[i, "Kategori_Subkategori"] <- colnames(data_df)[i]
    metrics_arima[i, "Model"] <- "auto.arima"
    metrics_arima[i, "ME"] <- me_val

  }

  # Save forecast
  names(forecasted_df) <- names(data_df)
  # Transpose
  forecasted_df <- t(forecasted_df)
  colnames(forecasted_df) <- c("Mean", "Upper", "Lower")
  forecasted_df <- as.data.frame(forecasted_df)
  # Tambah Kolom
  forecasted_df <- tibble::rownames_to_column(forecasted_df, "Kategori_Subkategori")

  # Save fitted
  names(fitted_df) <- names(data_df)
  fitted_df <- data.frame(pdrb_df[, 1:2], fitted_df)


  # Save Output
  return(list(forecastedval = forecasted_df, fittedval = fitted_df, metrics = metrics_arima))
}

pdrb.forecast.es <- function(pdrb_df, data_df, seasonal_df) {
  # INISIASI
  forecasted_df <- data.frame()
  fitted_df <- data.frame()
  fitted_val <- data.frame()
  plot_list <- list()

  # Melakukan forecasting untuk setiap variabel
  metrics_es <- data.frame()
  for (i in 1:ncol(data_df)) {
    # Mengambil data time series
    ts_data <- ts(data_df[, i], start = 1, frequency = 4)

    # Melakukan exponential smoothing dengan parameter yang ditentukan
    if (seasonal_df[i, 2] != 1) {
      prediksi <- holt(ts_data, damped = TRUE, alpha = NULL, beta = NULL)
    } else {
      prediksi <- ets(ts_data)
    }

    # Melakukan forecasting
    forecasted_values <- forecast(prediksi, h = 1)

    # Menambahkan hasil forecasting ke dalam dataframe
    forecasted_df[1, i] <- forecasted_values$mean
    forecasted_df[2, i] <- forecasted_values$upper
    forecasted_df[3, i] <- forecasted_values$lower

    # Menambahkan hasil fitted ke dalam dataframe
    fitted_val <- ts(forecasted_values$fitted, frequency = 1)
    for (j in 1:nrow(data_df)) {
      fitted_df[j, i] <- fitted_val[j]
    }

    # Menyimpan hasil forecast dalam plot
    mypath <- file.path("3. Exp Smoothing Plot dan Model", paste0("Exp Smoothing - ", i, ". ", colnames(data_df[i]), ".png"))
    png(mypath)
    plot_list[i] <- plot(forecasted_values, col = "red", main = paste0(colnames(data_df[i])), ylab = "Nilai", xlab = "Triwulan ke")
    plot_list[i] <- lines(forecasted_values$fitted, pch = 20, col = "green")
    plot_list[i] <- legend("topleft", c("Smoothed data", "Actual data"), lty = 8, col = c("green", "red"), cex = 0.8)
    dev.off()

    # Mencetak hasil forecast dalam list
    cat(colnames(data_df)[i], "\n")
    cat("-----------------------------------------------------------------------", "\n")
    print(summary(prediksi))
    cat("\n", "\n")

    # Collect mean error (ME) metric
    me_val <- tryCatch({
      acc <- accuracy(prediksi)
      acc[1, "ME"]
    }, error = function(e) NA)

    metrics_es[i, "Kategori_Subkategori"] <- colnames(data_df)[i]
    metrics_es[i, "Model"] <- ifelse(seasonal_df[i, 2] != 1, "holt(damped)", "ets")
    metrics_es[i, "ME"] <- me_val
  }

  # Save forecast
  names(forecasted_df) <- names(data_df)
  # Transpose
  forecasted_df <- t(forecasted_df)
  colnames(forecasted_df) <- c("Mean", "Upper", "Lower")
  forecasted_df <- as.data.frame(forecasted_df)
  # Tambah Kolom
  forecasted_df <- tibble::rownames_to_column(forecasted_df, "Kategori_Subkategori")

  # Save fitted
  names(fitted_df) <- names(data_df)
  fitted_df <- data.frame(pdrb_df[, 1:2], fitted_df)


  # Save Output
  return(list(forecastedval = forecasted_df, fittedval = fitted_df, metrics = metrics_es))
}


export.hasil <- function(arima.forecastedval, arima.fittedval, es.forecastedval, es.fittedval, metrics_arima = NULL, metrics_es = NULL) {
  savetoexcel <- list("Forecast ARIMA" = arima.forecastedval, "Forecast Exp Smoothing" = es.forecastedval,
                      "Fitted ARIMA" = arima.fittedval, "Fitted Exp Smoothing" = es.fittedval)

  if (!is.null(metrics_arima)) {
    savetoexcel[["Metrics ARIMA"]] <- metrics_arima
  }
  if (!is.null(metrics_es)) {
    savetoexcel[["Metrics Exp Smoothing"]] <- metrics_es
  }

  file_path <- file.path("4. Output R/Hasil Forecasting ARIMA dan EXPONENTIAL SMOOTHING.xlsx")
  write.xlsx(savetoexcel, file = file_path)

  cat("File Excel Forcasted Value, Fitted Value, dan Metrics telah disimpan di Folder 4. Output R \n")
  cat("-------------------------------------------------------------------------------------- \n")

}

forecast.pdrb.64 <- function(data_list) {
  cat("-------------------------------------------------------------------------------------- \n")
  cat("Forecasting sedang berlangsung \n")
  cat("-------------------------------------------------------------------------------------- \n")
  data.pdrb <- data_list$data.pdrb
  seasonal_df <- data_list$data.seasonal
  pdrb_df <- data_list$pdrb_df
  mypath <- file.path("2. ARIMA Plot dan Model/ModelARIMA.txt")
  sink(mypath)
  arima <- pdrb.forecast.arima(pdrb_df, data.pdrb, seasonal_df)
  sink()

  mypath <- file.path("3. Exp Smoothing Plot dan Model/ModelExponentialSmoothing.txt")
  sink(mypath)
  es <- pdrb.forecast.es(pdrb_df, data.pdrb, seasonal_df)
  sink()

  cat("\n-------------------------------------------------------------------------------------- \n")
  cat("Model dan Plot ARIMA disimpan pada folder 2. ARIMA Plot dan Model \n")
  cat("-------------------------------------------------------------------------------------- \n")
  cat("Model dan Plot Exponential Smoothing disimpan pada 3. Exp Smoothing Plot dan Model \n")
  cat("-------------------------------------------------------------------------------------- \n")

  export.hasil(arima$forecastedval, arima$fittedval, es$forecastedval, es$fittedval, metrics_arima = arima$metrics, metrics_es = es$metrics)
}

buka.hasil <- function(){
  shell.exec(paste0(getwd(), "/4. Output R/Hasil Forecasting ARIMA dan EXPONENTIAL SMOOTHING.xlsx"))
  shell.exec(paste0(getwd(), "/5. Hasil Forecast/64 Kaltim Data Forecast COMPILE.xlsx"))

  cat("Hasil Forecasting akan terbuka secara otomatis di Excel. \n")
  cat("Jika tidak terbuka, silakan buka file tersebut secara manual pada folder '4. Output R' dan '5. Hasil Forecast'. \n")
  cat("-------------------------------------------------------------------------------------- \n")
  cat("Silakan masukkan file Hasil Forecasting ke file '5. Hasil Forecast/64 Kaltim Data Forecast COMPILE.xlsx' untuk melihat hasil forecast secara lengkap. \n")
}