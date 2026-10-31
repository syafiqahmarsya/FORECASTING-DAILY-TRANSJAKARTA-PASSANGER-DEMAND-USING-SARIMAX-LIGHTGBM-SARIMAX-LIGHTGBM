## LOAD LIBRARY
library(TSA)
library(forecast)
library(tseries)
library(MASS)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(lmtest)

## DATA
# LOAD DATA
#data <- read_excel("brt_transjakarta.xlsx")
data <- read_excel("brt_baru.xlsx")

# Cek struktur data
head(data)
tail(data)
str(data)
summary(data)

# Atur format tanggal
# Jika read_excel() sudah membaca tanggal sebagai Date/POSIXt, cukup ubah ke Date.
# Jika masih berupa teks, coba beberapa format tanggal yang mungkin digunakan.
if (inherits(data$tanggal, "Date")) {
  data$tanggal <- as.Date(data$tanggal)
} else if (inherits(data$tanggal, "POSIXt")) {
  data$tanggal <- as.Date(data$tanggal)
} else {
  tanggal_parsed <- suppressWarnings(
    parse_date_time(
      as.character(data$tanggal),
      orders = c("dmy", "Ymd", "A, d B Y", "A, d b Y"),
      locale = "C"
    )
  )
  data$tanggal <- as.Date(tanggal_parsed)
}

if (anyNA(data$tanggal)) {
  stop("Terdapat nilai tanggal yang gagal dikonversi. Periksa format kolom tanggal.")
}

data <- data[order(data$tanggal), ]

# Plot data pengguna
options(scipen = 999)
plot(
  data$tanggal,
  data$pengguna,
  type = "l",
  xlab = "Tahun",
  ylab = "Jumlah Pengguna Harian BRT Trans Jakarta"
)

# outlier menggunakan boxplot
boxplot(data$pengguna)
which(data$pengguna %in% boxplot.stats(data$pengguna)$out)

# outlier menggunakan ts.outlier
data_ts <- ts(data$pengguna, frequency = 7) # musiman mingguan
tsoutliers(data_ts)

# plot STL dekomposisi
decomp <- stl(data_ts, s.window = "periodic")
plot(decomp)

# plot dekomposisi
f <- decompose(data_ts)
f
plot(f)

## SPLIT DATA
train_df <- subset(data, tanggal <= as.Date("2025-05-31"))
test_df  <- subset(data, tanggal > as.Date("2025-05-31"))

# Buat time series training
y_train <- ts(train_df$pengguna, frequency = 7)
y_test  <- ts(test_df$pengguna, 
              start=end(y_train)+c(0,1),
              frequency = 7)
# Matrix exogen 
xreg_train <- as.matrix(train_df[, c("libur_nasional",
                                     "lebaran",
                                     'promo_tarif')]) 

xreg_test  <- as.matrix(test_df[, c("libur_nasional",
                                    "lebaran",
                                    "promo_tarif")])

# Pastikan matrix exogen bertipe numerik dan nama kolom train-test sama
storage.mode(xreg_train) <- "double"
storage.mode(xreg_test) <- "double"
stopifnot(identical(colnames(xreg_train), colnames(xreg_test)))

# Cek panjang pembagian datanya
length(y_train)  
length(y_test)   
nrow(xreg_train)
nrow(xreg_test)
plot(y_train)


## CEK STASIONERITAS TERHADAP VARIANS (DENGAN TRANSFORMASI BOXCOX)
# lambda < 1 → perlu transformasi
# lambda = 1 → tidak perlu transformasi
# boxcox.ar
# Dijalankan pada data training asli (belum ditransformasi)
bc <- BoxCox.ar(y_train)
bc
lambda <- bc$mle

# boxcox.lambda berdasarkan buku Hyndman
x <- BoxCox.lambda(y_train)
x
# karena lambda < 1 maka dilakukan transformasi Box-Cox
y_train_trans <- BoxCox(y_train, x)

# plot data setelah ditransformasi
plot(y_train_trans)
# PP test sebelum differencing
pp.test(y_train_trans)

## CEK STASIONERITAS TERHADAP MEAN (SEBELUM DIFF)
# ADF
# H0: tidak stasioner → p < 0.05 berarti stasioner
adf.test(y_train_trans)
## HASIL P-VALUE <0.05 [DATA STASIONER]
## KPSS
# H0: stasioner → p > 0.05 berarti stasioner
# KPSS TREND
kpss.test(y_train_trans,null=c('Trend'))
# KPSS LEVEL
kpss.test(y_train_trans,null=c('Level'))
# KPSS 
kpss.test(y_train_trans)
## HASIL P-VALUE < 0.05 [TIDAK STASIONER]
### KESIMPULAN: Karena hasil kpss dan adf berbeda maka tetap dilakukan differencing


# CEK ACF DAN PACF [DATA TRAIN TRANSFORMASI SEBELUM DIFFERENCING]
par(mfrow = c(1,2))
Acf(y_train_trans)
Pacf(y_train_trans)


## DIFFERENCING DATA TRAIN [KARENA BELUM STASIONER]
# menggunakan fungsi ndiff() dan nsdiffs() untuk melihat orde differencing 
# Cek orde diff non musiman
ndiffs(y_train_trans)
# cek orde diff musiman
nsdiffs(y_train_trans)
# dari hasil ndiffs() dan nsdiffs() pada data train diperoleh d=1 dan D=1
data_diff <- diff(y_train_trans, differences = 1)
data_diff1 <- diff(data_diff,lag = 7)
data_diff2 <- diff(y_train_trans, lag=7)


## CEK STASIONERITAS SETELAH DIFFERENCING (d=1 dan D=1)  
# ADF
# H0: tidak stasioner → p < 0.05 berarti stasioner
adf.test(data_diff1)
## Jika p-value < 0.05, H0 unit root ditolak sehingga data dianggap stasioner.

# Phillips-Perron setelah differencing
pp.test(data_diff1)

## KPSS
# H0: stasioner → p > 0.05 berarti stasioner
# KPSS TREND
kpss.test(data_diff1,null=c('Trend'))
# KPSS LEVEL
kpss.test(data_diff1,null=c('Level'))
# KPSS 
kpss.test(data_diff1)
## Interpretasi KPSS:
## p-value > 0.05  -> gagal menolak H0, data dapat dianggap stasioner.
## p-value <= 0.05 -> menolak H0, data belum stasioner.
### KESIMPULAN ditentukan berdasarkan output aktual ADF, PP, dan KPSS.


# IDENTIFIKASI ORDE SARIMA

## CEK ACF DAN PACF DATA DIFF
par(mfrow = c(1,2))
Acf(data_diff1)
Pacf(data_diff1, lag.max = 63)
# KESIMPULAN: Kandidat orde (p=0,1,2, persimoni), (q=0,1,2), (P = 0), (Q=0,1)


# ESTIMASI MODEL

# 1. SARIMAX (0,1,0)(0,1,0)[7]
## Fit model
fit1 <- Arima( y_train_trans,
               order = c(0, 1, 0),
               seasonal = list(order=c(0, 1, 0),period=7),
               xreg = xreg_train)
summary(fit1)
## Signifikansi Parameter
coeftest(fit1)
## Cek residuals
checkresiduals(fit1)

# 2. SARIMAX (0,1,1)(0,1,0)[7]
## Fit model
fit2 <- Arima( y_train_trans,
               order = c(0, 1, 1),
               seasonal = list(order=c(0, 1, 0),period=7),
               xreg = xreg_train)
summary(fit2)
## Signifikansi Parameter
coeftest(fit2)
## Cek residuals
checkresiduals(fit2)

# 3. SARIMAX (0,1,2)(0,1,0)[7]
## Fit model
fit3 <- Arima( y_train_trans,
               order = c(0, 1, 2),
               seasonal = list(order=c(0, 1, 0),period=7),
               xreg = xreg_train)
summary(fit3)
## Signifikansi Parameter
coeftest(fit3)
## Cek residuals
checkresiduals(fit3)

# 4. SARIMAX (0,1,0)(0,1,1)[7]
## Fit model
fit4 <- Arima( y_train_trans,
               order = c(0, 1, 0),
               seasonal = list(order=c(0, 1, 1),period=7),
               xreg = xreg_train)
summary(fit4)
## Signifikansi Parameter
coeftest(fit4)
## Cek residuals
checkresiduals(fit4)

# 5. SARIMAX (0,1,1)(0,1,1)[7]
## Fit model
fit5 <- Arima( y_train_trans,
               order = c(0, 1, 1),
               seasonal = list(order=c(0, 1, 1),period=7),
               xreg = xreg_train)
summary(fit5)
## Signifikansi Parameter
coeftest(fit5)
## Cek residuals
checkresiduals(fit5)

# 6. SARIMAX (0,1,2)(0,1,1)[7]
## Fit model
fit6 <- Arima( y_train_trans,
               order = c(0, 1, 2),
               seasonal = list(order=c(0, 1, 1),period=7),
               xreg = xreg_train)
summary(fit6)
## Signifikansi Parameter
coeftest(fit6)
## Cek residuals
checkresiduals(fit6)

# 7. SARIMAX (1,1,0)(0,1,0)[7]
## Fit model
fit7 <- Arima( y_train_trans,
               order = c(1, 1, 0),
               seasonal = list(order=c(0, 1, 0),period=7),
               xreg = xreg_train)
summary(fit7)
## Signifikansi Parameter
coeftest(fit7)
## Cek residuals
checkresiduals(fit7)

# SARIMAX (1,1,1)(0,1,0)[7]
## Fit model
fit8 <- Arima( y_train_trans,
               order = c(1, 1, 1),
               seasonal = list(order=c(0, 1, 0),period=7),
               xreg = xreg_train)
summary(fit8)
## Signifikansi Parameter
coeftest(fit8)
## Cek residuals
checkresiduals(fit8)

# SARIMAX (1,1,2)(0,1,0)[7]
## Fit model
fit9 <- Arima( y_train_trans,
               order = c(1, 1, 2),
               seasonal = list(order=c(0, 1, 0),period=7),
               xreg = xreg_train)
summary(fit9)
## Signifikansi Parameter
coeftest(fit9)
## Cek residuals
checkresiduals(fit9)

# SARIMAX (1,1,0)(0,1,1)[7]
## Fit model
fit10 <- Arima( y_train_trans,
                order = c(1, 1, 0),
                seasonal = list(order=c(0, 1, 1),period=7),
                xreg = xreg_train)
summary(fit10)
## Signifikansi Parameter
coeftest(fit10)
## Cek residuals
checkresiduals(fit10)

# SARIMAX (1,1,1)(0,1,1)[7]
## Fit model
fit11 <- Arima( y_train_trans,
                order = c(1, 1, 1),
                seasonal = list(order=c(0, 1, 1),period=7),
                xreg = xreg_train)
summary(fit11)
## Signifikansi Parameter
coeftest(fit11)
## Cek residuals
checkresiduals(fit11)

# SARIMAX (1,1,2)(0,1,1)[7]
## Fit model
fit12 <- Arima( y_train_trans,
                order = c(1, 1, 2),
                seasonal = list(order=c(0, 1, 1),period=7),
                xreg = xreg_train)
summary(fit12)
## Signifikansi Parameter
coeftest(fit12)
## Cek residuals
checkresiduals(fit12)

# SARIMAX (2,1,0)(0,1,0)[7]
## Fit model
fit13 <- Arima( y_train_trans,
                order = c(2, 1, 0),
                seasonal = list(order=c(0, 1, 0),period=7),
                xreg = xreg_train)
summary(fit13)
## Signifikansi Parameter
coeftest(fit13)
## Cek residuals
checkresiduals(fit13)

# SARIMAX (2,1,1)(0,1,0)[7]
## Fit model
fit14 <- Arima( y_train_trans,
                order = c(2, 1, 1),
                seasonal = list(order=c(0, 1, 0),period=7),
                xreg = xreg_train)
summary(fit14)
## Signifikansi Parameter
coeftest(fit14)
## Cek residuals
checkresiduals(fit14)

# SARIMAX (2,1,2)(0,1,0)[7]
## Fit model
fit15 <- Arima( y_train_trans,
                order = c(2, 1, 2),
                seasonal = list(order=c(0, 1, 0),period=7),
                xreg = xreg_train)
summary(fit15)
## Signifikansi Parameter
coeftest(fit15)
## Cek residuals
checkresiduals(fit15)

# SARIMAX (2,1,0)(0,1,1)[7]
## Fit model
fit16 <- Arima( y_train_trans,
                order = c(2, 1, 0),
                seasonal = list(order=c(0, 1, 1),period=7),
                xreg = xreg_train)
summary(fit16)
## Signifikansi Parameter
coeftest(fit16)
## Cek residuals
checkresiduals(fit16)

# SARIMAX (2,1,1)(0,1,1)[7]
## Fit model
fit17 <- Arima( y_train_trans,
                order = c(2, 1, 1),
                seasonal = list(order=c(0, 1, 1),period=7),
                xreg = xreg_train)
summary(fit17)
## Signifikansi Parameter
coeftest(fit17)
## Cek residuals
checkresiduals(fit17)

# SARIMAX (2,1,2)(0,1,1)[7]
## Fit model
fit18 <- Arima( y_train_trans,
                order = c(2, 1, 2),
                seasonal = list(order=c(0, 1, 1),period=7),
                xreg = xreg_train)
summary(fit18)
## Signifikansi Parameter
coeftest(fit18)
## Cek residuals
checkresiduals(fit18)
## KESIMPULAN: MODEL YANG DIGUNAKAN MODEL FIT6 DAN FIT11

# DIAGNOSIS MODEL 

# FIT 6 
## UJI LJUNG BOX
res6 <- as.numeric(na.omit(residuals(fit6)))
plot(res6)
Box.test(
  res6,
  lag = 7, fitdf=3,          # boleh 7 atau 14 untuk data harian
  type = "Ljung-Box"
)
Box.test(
  res6,
  lag = 14,fitdf=3,          # boleh 7 atau 14 untuk data harian
  type = "Ljung-Box"
)
Box.test(
  res6,
  lag = 21,fitdf=3,          # boleh 7 atau 14 untuk data harian
  type = "Ljung-Box"
)

Box.test(
  res6,
  lag = 28,fitdf=3,          # boleh 7 atau 14 untuk data harian
  type = "Ljung-Box"
)
# Uji portmanteau hingga lag 28 (bukan lag default = 1)
Box.test(
  res6,
  lag = 28,
  fitdf = 3,
  type = "Ljung-Box"
)
## Cek normalitas dengan histogram dan QQ plot
par(mfrow=c(1,2))
hist(res6)
qqnorm(res6)
qqline(res6)

## Uji normalitas
shapiro.test(res6)
jarque.bera.test(res6)
# Interpretasi berdasarkan output: p-value > 0.05 berarti gagal menolak H0 normalitas.


## Cek autokorelasi: individu
par(mfrow=c(1,1))
acf(res6)


# FIT 11
## UJI LJUNG BOX
res11 <- as.numeric(na.omit(residuals(fit10)))
plot(res6)
Box.test(
  res11,
  lag = 7, fitdf=2,          # boleh 7 atau 14 untuk data harian
  type = "Ljung-Box"
)
Box.test(
  res11,
  lag = 14,fitdf=3,          # boleh 7 atau 14 untuk data harian
  type = "Ljung-Box"
)
Box.test(
  res11,
  lag = 21,fitdf=3,          # boleh 7 atau 14 untuk data harian
  type = "Ljung-Box"
)

Box.test(
  res11,
  lag = 28,fitdf=3,          # boleh 7 atau 14 untuk data harian
  type = "Ljung-Box"
)
# Uji portmanteau hingga lag 28 (bukan lag default = 1)
Box.test(
  res11,
  lag = 28,
  fitdf = 3,
  type = "Ljung-Box"
)
## Cek normalitas dengan histogram dan QQ plot
par(mfrow=c(1,2))
hist(res11)
qqnorm(res11)
qqline(res11)

## Uji normalitas
shapiro.test(res11)
jarque.bera.test(res11)
# Interpretasi berdasarkan output: p-value > 0.05 berarti gagal menolak H0 normalitas.

## Cek autokorelasi: individu
par(mfrow=c(1,1))
acf(res11)



# FORECAST n=61
fc6  <- forecast(fit6, xreg = xreg_test, h = length(y_test))
pred_asli <- InvBoxCox(fc6$mean, x)
accuracy(pred_asli, y_test)

# Metrik pada skala asli
mape_sarimax <- mean(abs((as.numeric(y_test) - as.numeric(pred_asli)) /
                           as.numeric(y_test))) * 100
mae_sarimax <- mean(abs(as.numeric(y_test) - as.numeric(pred_asli)))
rmse_sarimax <- sqrt(mean((as.numeric(y_test) - as.numeric(pred_asli))^2))
cat("MAPE SARIMAX:", mape_sarimax, "%\n")
cat("MAE SARIMAX :", mae_sarimax, "\n")
cat("RMSE SARIMAX:", rmse_sarimax, "\n")


# FIT 16
## UJI LJUNG BOX
res16 <- as.numeric(na.omit(residuals(fit16)))
plot(res16)
Box.test(
  res16,
  lag = 7, fitdf = 3,
  type = "Ljung-Box"
)
Box.test(
  res16,
  lag = 14, fitdf = 3,
  type = "Ljung-Box"
)
Box.test(
  res16,
  lag = 21, fitdf = 3,
  type = "Ljung-Box"
)
Box.test(
  res16,
  lag = 28, fitdf = 3,
  type = "Ljung-Box"
)
## Cek normalitas dengan histogram dan QQ plot
par(mfrow = c(1,2))
hist(res16)
qqnorm(res16)
qqline(res16)

## Uji normalitas
shapiro.test(res16)
jarque.bera.test(res16)
# Interpretasi berdasarkan output: p-value > 0.05 berarti gagal menolak H0 normalitas.

## Cek autokorelasi: individu
par(mfrow = c(1,1))
acf(res16)


# FIT 10
## UJI LJUNG BOX
res10 <- as.numeric(na.omit(residuals(fit5)))
plot(res10)
Box.test(
  res10,
  lag = 20, fitdf = 2,
  type = "Ljung-Box"
)
Box.test(
  res10,
  lag = 14, fitdf = 2,
  type = "Ljung-Box"
)
Box.test(
  res10,
  lag = 21, fitdf = 2,
  type = "Ljung-Box"
)
Box.test(
  res10,
  lag = 28, fitdf = 2,
  type = "Ljung-Box"
)
## Cek normalitas dengan histogram dan QQ plot
par(mfrow = c(1,2))
hist(res10)
qqnorm(res10)
qqline(res10)

## Uji normalitas
shapiro.test(res10)
jarque.bera.test(res10)
# Interpretasi berdasarkan output: p-value > 0.05 berarti gagal menolak H0 normalitas.

## Cek autokorelasi: individu
par(mfrow = c(1,1))
acf(res10)

# LJUNG-BOX PADA BEBERAPA LAG UNTUK SETIAP MODEL
# Catatan: satu Box.test(lag = 28) menguji autokorelasi secara bersama-sama
# hingga lag 28. Tabel berikut menampilkan hasil pada lag 7, 14, 21, dan 28.
uji_ljung_multi <- function(res, fitdf, nama_model) {
  lags_uji <- c(7, 14, 21, 28)
  
  do.call(
    rbind,
    lapply(lags_uji, function(L) {
      hasil <- Box.test(
        res,
        lag = L,
        fitdf = fitdf,
        type = "Ljung-Box"
      )
      
      data.frame(
        model = nama_model,
        lag = L,
        statistik_Q = unname(hasil$statistic),
        df = unname(hasil$parameter),
        p_value = hasil$p.value,
        keputusan = ifelse(
          hasil$p.value > 0.05,
          "Gagal menolak H0 (white noise)",
          "Tolak H0 (belum white noise)"
        )
      )
    })
  )
}

hasil_ljungbox_semua <- rbind(
  uji_ljung_multi(res5,  fitdf = 2, nama_model = "fit5"),
  uji_ljung_multi(res6,  fitdf = 3, nama_model = "fit6"),
  uji_ljung_multi(res10, fitdf = 2, nama_model = "fit10"),
  uji_ljung_multi(res11, fitdf = 3, nama_model = "fit11")
)
print(hasil_ljungbox_semua)

# Residual model final yang digunakan
residual_model_final <- residuals(fit6)

# Buat Q-Q plot
qqnorm(res6)
qqline(res6, col = "red")
# Buat Q-Q plot
qqnorm(res11)
qqline(res11, col = "blue")

summary(fit5)
summary(fit14)
x
# FORECAST n=61
fc6  <- forecast(fit6, xreg = xreg_test, h = length(y_test))
pred_asli <- InvBoxCox(fc6$mean, x)
accuracy(pred_asli, y_test)

# Gabung jadi satu file
resid_train <- residuals(fit6)
fc6 <- forecast(fit6, xreg = xreg_test, h = length(y_test))
pred_sarimax_boxcox <- fc6$mean

# Objek di atas digunakan kembali untuk ekspor residual train dan prediksi test.
write.csv(data.frame(
  tanggal = c(train_df$tanggal, test_df$tanggal),
  pengguna = c(train_df$pengguna, test_df$pengguna),
  sarimax_output = c(as.numeric(resid_train), 
                     as.numeric(pred_sarimax_boxcox))
), "sarimax_output6i.csv", row.names=FALSE)

getwd()
summary(y_train_trans)
summary(as.numeric(pred_sarimax_boxcox))
# File 1: residual train untuk LightGBM fit
write.csv(data.frame(
  tanggal = train_df$tanggal,
  residual_sarimax = as.numeric(resid_train)
), "resid_train6i.csv", row.names=FALSE)

# File 2: prediksi SARIMAX test untuk kombinasi hybrid
write.csv(data.frame(
  tanggal = test_df$tanggal,
  pred_sarimax = as.numeric(pred_sarimax_boxcox)
), "pred_sarimax_test6i.csv", row.names=FALSE)

summary(fit5)
summary(fit6)
summary(fit11)
summary(fit10)
summary(fit14)
summary(fit17)
summary(fit12)

coeftest(fit5)
coeftest(fit6)
coeftest(fit10)
coeftest(fit11)
coeftest(fit12)
coeftest(fit14)
coeftest(fit17)


