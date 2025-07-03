\name{pdrb.forecast.es}
\alias{pdrb.forecast.es}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecasting dengan Exponential Smoothing
}
\description{
Melakukan forecasting data menggunakan fungsi holt() dan ets() yang berasal dari
package forecast. Di dalam fungsi ini juga dilakukan penyimpanan model dalam
file berformat *.txt dan gambar dalam format *.png. Output yang tersimpan dari
fungsi ini adalah forecasted value dan fitted value dalam bentuk data frame.
}
\usage{
pdrb.forecast.es(data_df, seasonal_df)
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
