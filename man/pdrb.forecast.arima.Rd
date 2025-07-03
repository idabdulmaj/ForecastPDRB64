\name{pdrb.forecast.arima}
\alias{pdrb.forecast.arima}
\title{
Forecast ARIMA
}
\description{
Melakukan forecasting data menggunakan fungsi auto.arima() yang berasal dari
package forecast. Di dalam fungsi ini juga dilakukan penyimpanan model dalam
file berformat *.txt dan gambar dalam format *.png. Output yang tersimpan dari
fungsi ini adalah forecasted value dan fitted value dalam bentuk data frame.
}
\usage{
pdrb.forecast.arima(data_df, seasonal_df)
}

\arguments{
  \item{data_df}{
data yang sudah disesuaikan melalui code di file *.R
  }
  \item{seasonal_df}{
data yang sudah disesuaikan melalui code di file *.R
  }
}

\note{
Untuk Subkategori Tanaman Pangan, saat ini mempertimbangkan faktor musiman.
}

\author{
Muhammad Abdul Majid
}
