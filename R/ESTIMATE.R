cek.package.nya <- function() {
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
  
  cat("\nSeluruh package yang dibutuhkan telah di-Load.\n")
}

pdrb.forecast.arima <- function(data_df, seasonal_df) {
  # INISIASI
  forecasted_df <- data.frame()
  fitted_df <- data.frame()
  fitted_val <- data.frame()
  plot_list <- list()

  # Melakukan forecasting untuk setiap variabel
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
    plot_list[i] <- plot(forecasted_values, col = "red", main = paste0(colnames(data_df[i])), ylab = "PDRB", xlab = "Triwulan ke")
    plot_list[i] <- lines(forecasted_values$fitted, pch = 20, col = "green")
    plot_list[i] <- legend("topleft", c("Actual data", "Smoothed data"), lty = 8, col = c("green", "red"), cex = 0.8)
    dev.off()

    # Mencetak hasil forecast dalam list
    cat("-----------------------------------------------------------------------", "\n")
    cat(colnames(data_df)[i], "\n")
    cat("-----------------------------------------------------------------------", "\n")
    print(summary(prediksi))
    cat("\n", "\n")

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
  return(list(forecastedval = forecasted_df, fittedval = fitted_df))
}

pdrb.forecast.es <- function(data_df, seasonal_df) {
  # INISIASI
  forecasted_df <- data.frame()
  fitted_df <- data.frame()
  fitted_val <- data.frame()
  plot_list <- list()

  # Melakukan forecasting untuk setiap variabel
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
    plot_list[i] <- plot(forecasted_values, col = "red", main = paste0(colnames(data_df[i])), ylab = "PDRB", xlab = "Triwulan ke")
    plot_list[i] <- lines(forecasted_values$fitted, pch = 20, col = "green")
    plot_list[i] <- legend("topleft", c("Actual data", "Smoothed data"), lty = 8, col = c("green", "red"), cex = 0.8)
    dev.off()

    # Mencetak hasil forecast dalam list
    cat(colnames(data_df)[i], "\n")
    cat("-----------------------------------------------------------------------", "\n")
    print(summary(prediksi))
    cat("\n", "\n")
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
  return(list(forecastedval = forecasted_df, fittedval = fitted_df))
}


cat.final <- function() {
  catlist <- c("bongo", "colonel", "grumpy", "hipster", "lil_bub", "maru", "mouth", "pop", "pop_close",
                "pusheen", "pusheen_pc", "toast", "venus","shironeko")
  grid <- expand.grid(3, 2)
  df <- data.frame(x = grid[, 1],
                   y = grid[, 2],
                   image = sample(catlist, 1))
  ggplot(df) +
  geom_cat(aes(x, y, cat = image), size = 10) +
            xlim(c(0.25, 5.5)) +
            ylim(c(0.25, 3.5)) +
            annotate("text", x = 3, y = 0.8, size = 12,
                      label = "Terima Kasih", fontface = "bold") +
            annotate("text", x = 3, y = 3.2, size = 12,
                      label = "Forecasting telah selesai.", fontface = "bold")
}

export.hasil <- function(arima.forecastedval, arima.fittedval, es.forecastedval, es.fittedval) {
  savetoexcel <- list("Forecast ARIMA" = arima.forecastedval, "Fitted ARIMA" = arima.fittedval,
                      "Forecast Exp Smoothing" = es.forecastedval, "Fitted Exp Smoothing" = es.fittedval)

  file_path <- file.path("4. Output R/Hasil Forecasting ARIMA dan EXPONENTIAL SMOOTHING.xlsx")
  write.xlsx(savetoexcel, file = file_path)

  cat("File Excel Forcasted Value dan Fitted Value telah disimpan di Folder 4. Output R \n")
  cat("-------------------------------------------------------------------------------------- \n")
  
}

forecast.pdrb.64 <- function(data_df, seasonal_df) {
  cat("-------------------------------------------------------------------------------------- \n")
  cat("Forecasting sedang berlangsung \n")
  cat("-------------------------------------------------------------------------------------- \n")
  mypath <- file.path("2. ARIMA Plot dan Model/ModelARIMA.txt")
  sink(mypath)
  arima <- pdrb.forecast.arima(data.pdrb, seasonal_df)
  sink()

  mypath <- file.path("3. Exp Smoothing Plot dan Model/ModelExponentialSmoothing.txt")
  sink(mypath)
  es <- pdrb.forecast.es(data.pdrb, seasonal_df)
  sink()

  cat("\n-------------------------------------------------------------------------------------- \n")
  cat("Model dan Plot ARIMA disimpan pada folder 2. ARIMA Plot dan Model \n")
  cat("-------------------------------------------------------------------------------------- \n")
  cat("Model dan Plot Exponential Smoothing disimpan pada 3. Exp Smoothing Plot dan Model \n")
  cat("-------------------------------------------------------------------------------------- \n")

  export.hasil(arima$forecastedval, arima$fittedval,
               es$forecastedval, es$fittedval)
  
  cat.final()
}

buka.hasil <- function(){
  shell.exec(paste0(getwd(), "/4. Output R/Hasil Forecasting ARIMA dan EXPONENTIAL SMOOTHING.xlsx"))
  shell.exec(paste0(getwd(), "/5. Hasil Forecast/64 Kaltim Data Forecast COMPILE.xlsx"))

  cat("Hasil Forecasting akan terbuka secara otomatis di Excel. \n")
  cat("Jika tidak terbuka, silakan buka file tersebut secara manual. \n")
  cat("-------------------------------------------------------------------------------------- \n")
  cat("Silakan masukan file Hasil Forecasting ke file 64 Kaltim Data Forecast COMPILE.xlsx untuk melihat hasil forecast secara lengkap. \n")
}
