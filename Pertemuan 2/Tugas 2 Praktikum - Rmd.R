---
  title: "Tugas 2 Praktikum MPDW"
author: "Fachry Wirayuda Alindri"
date: "2025-09-02"
output: html_document
---
  # Import Library & Load Data
  

library(readxl)
library(dplyr)
library(forecast)
library(graphics)
library(TTR)
library(TSA)
library(rio)
library(ggplot2)
```

data <- read_excel("C:/Users/LENOVO/Downloads/Fachry.xlsx")
head(data)
```

.
df <- data %>%
  mutate(
    Tanggal = as.Date(Tanggal, format = "%b %d, %Y"),
    Harga = as.numeric(gsub(",", "", Harga))
  )

print(df)
```

.
df <- df[order(df$Tanggal), ]
df
```

.
View(df)
str(df)
dim(df)
```

.
df.ts <- ts(df$Harga)
df.ts
```

.
summary(df.ts)
```

.
ts.plot(df.ts, xlab="Time Period ", ylab="harga", 
        main = "Time Series Plot")
points(df.ts)

```

# Single Moving Average

.
n <- nrow(df)

n_train <- 95

training_ma <- df[1:n_train, ]
testing_ma  <- df[(n_train+1):n, ]

train_ma.ts <- ts(training_ma$Harga)
test_ma.ts  <- ts(testing_ma$Harga)
```

.
plot(df.ts, col="red",main="Plot semua data")
points(df.ts)
```

.
plot(train_ma.ts, col="blue",main="Plot data latih")
points(train_ma.ts)
```

.
plot(test_ma.ts, col="blue",main="Plot data uji")
points(test_ma.ts)
```

.
ggplot() + 
  geom_line(data = training_ma, aes(x = Tanggal, y = Harga, col = "Data Latih")) +
  geom_line(data = testing_ma, aes(x = Tanggal, y = Harga, col = "Data Uji")) +
  labs(x = "Periode Waktu", y = "harga", color = "Legend") +
  scale_colour_manual(name="Keterangan:", breaks = c("Data Latih", "Data Uji"),
                      values = c("blue", "red")) + 
  theme_bw() + theme(legend.position = "bottom",
                     plot.caption = element_text(hjust=0.5, size=12))
```

.
data.sma<-SMA(train_ma.ts, n=4)
data.sma
```

.
data.ramal<-c(NA,data.sma)
data.ramal
```

.
n <- length(df.ts)   

data.gab <- cbind(
  aktual    = c(df.ts),
  pemulusan = c(data.sma, rep(NA, n - length(data.sma))),
  ramalan   = c(data.ramal, rep(tail(data.ramal,1), n - length(data.ramal)))
)

data.gab
```

.
ts.plot(df.ts, xlab="Time Period ", ylab="Sales", main= "SMA N=4 Data Saham ICBP")
points(df.ts)
lines(data.gab[,2],col="green",lwd=2)
lines(data.gab[,3],col="red",lwd=2)
legend("topleft",c("data aktual","data pemulusan","data peramalan"), lty=8, col=c("black","green","red"), cex=0.5)
```

.
error_train.sma = train_ma.ts-data.ramal[1:length(train_ma.ts)]

SSE_train.sma = sum(error_train.sma[5:length(train_ma.ts)]^2)
MSE_train.sma = mean(error_train.sma[5:length(train_ma.ts)]^2)
MAPE_train.sma = mean(abs((error_train.sma[5:length(train_ma.ts)]/train_ma.ts[5:length(train_ma.ts)])*100))

akurasi_train.sma <- matrix(c(SSE_train.sma, MSE_train.sma, MAPE_train.sma))
row.names(akurasi_train.sma)<- c("SSE", "MSE", "MAPE")
colnames(akurasi_train.sma) <- c("Akurasi m = 4")
akurasi_train.sma
```

.
pred_test <- tail(data.ramal, length(test_ma.ts))  
error_test.sma <- test_ma.ts - pred_test

SSE_test.sma  <- sum(error_test.sma^2)
MSE_test.sma  <- mean(error_test.sma^2)
MAPE_test.sma <- mean(abs(error_test.sma / test_ma.ts * 100))

akurasi_test.sma <- matrix(c(SSE_test.sma, MSE_test.sma, MAPE_test.sma))
row.names(akurasi_test.sma) <- c("SSE","MSE","MAPE")
colnames(akurasi_test.sma) <- c("Akurasi m = 4")
akurasi_test.sma
```

# Double Moving Average

.
h <- length(test_ma.ts)
h
```

.
dma <- SMA(data.sma, n = 4)
At <- 2*data.sma - dma
Bt <- 2/(4-1)*(data.sma - dma)
data.dma<- At+Bt
data.ramal2<- c(NA, data.dma)

t = 1:30
f = c()

for (i in t) {
  f[i] = At[length(At)] + Bt[length(Bt)]*(i)
}

data.gab2 <- cbind(aktual = c(df.ts), 
                   pemulusan1 = c(data.sma,rep(NA,30)),
                   pemulusan2 = c(dma, rep(NA,30)),
                   At = c(At, rep(NA,30)), 
                   Bt = c(Bt,rep(NA,30)),
                   ramalan = c(data.ramal2, f[-1]))
data.gab2
```

.
ts.plot(df.ts, xlab="Time Period ", ylab="Harga", main= "DMA N=4 Data Saham ICBP")
points(df.ts)
lines(data.gab2[,3],col="green",lwd=2)
lines(data.gab2[,6],col="red",lwd=2)
legend("topleft",c("data aktual","data pemulusan","data peramalan"), lty=8, col=c("black","green","red"), cex=0.8)
```

.
error_train.dma = train_ma.ts-data.ramal2[1:length(train_ma.ts)]

SSE_train.dma = sum(error_train.dma[8:length(train_ma.ts)]^2)
MSE_train.dma = mean(error_train.dma[8:length(train_ma.ts)]^2)
MAPE_train.dma = mean(abs((error_train.dma[8:length(train_ma.ts)]/train_ma.ts[8:length(train_ma.ts)])*100))

akurasi_train.dma <- matrix(c(SSE_train.dma, MSE_train.dma, MAPE_train.dma))
row.names(akurasi_train.dma)<- c("SSE", "MSE", "MAPE")
colnames(akurasi_train.dma) <- c("Akurasi m = 4")
akurasi_train.dma
```

.
error_test.dma = test_ma.ts - data.gab2[(n_train+1):(n_train+length(test_ma.ts)), 6]

SSE_test.dma = sum(error_test.dma[5:length(test_ma.ts)]^2)
MSE_test.dma = mean(error_test.dma[5:length(test_ma.ts)]^2)
MAPE_test.dma = mean(abs((error_test.dma[5:length(test_ma.ts)] / test_ma.ts[5:length(test_ma.ts)]) * 100))

akurasi_test.dma <- matrix(c(SSE_test.dma, MSE_test.dma, MAPE_test.dma))
row.names(akurasi_test.dma) <- c("SSE", "MSE", "MAPE")
colnames(akurasi_test.dma) <- c("Akurasi m = 4")
akurasi_test.dma
```

# Single Exponential Smoothing

.
n <- nrow(df)

n_train <- 95

training <- df[1:n_train, ]
testing  <- df[(n_train+1):n, ]

train.ts <- ts(training$Harga)
test.ts  <- ts(testing$Harga)
```

.
plot(df.ts, col="black",main="Plot semua data")
points(df.ts)
```

.
plot(train.ts, col="red",main="Plot data latih")
points(train.ts)
```

.
plot(test.ts, col="blue",main="Plot data uji")
points(test.ts)
```

.
ggplot() + 
  geom_line(data = training, aes(x = Tanggal, y = Harga, col = "Data Latih")) +
  geom_line(data = testing, aes(x = Tanggal, y = Harga, col = "Data Uji")) +
  labs(x = "Periode Waktu", y = "Harga Saham", color = "Legend") +
  scale_colour_manual(name="Keterangan:", breaks = c("Data Latih", "Data Uji"),
                      values = c("blue", "red")) + 
  theme_bw() + theme(legend.position = "bottom",
                     plot.caption = element_text(hjust=0.5, size=12))
```

.
ses.1 <- ses(train.ts, h = 10, alpha = 0.2)
plot(ses.1)
```

.
ses.1
```

.
ses.2<- ses(train.ts, h = 10, alpha = 0.7)
plot(ses.2)
```

.
ses.2
```

.
autoplot(ses.1) +
  autolayer(fitted(ses.1), series="Fitted") +
  ylab("Harga Saham") + xlab("Periode")
```

.
ses1<- HoltWinters(train.ts, gamma = FALSE, beta = FALSE, alpha = 0.2)
plot(ses1)
```

.
ramalan1<- forecast(ses1, h=30)
ramalan1
```

.
ses2<- HoltWinters(train.ts, gamma = FALSE, beta = FALSE, alpha = 0.7)
plot(ses2)
```

.
ramalan2<- forecast(ses2, h=30)
ramalan2
```

.
ses.opt <- ses(train.ts, h = 30, alpha = NULL)
plot(ses.opt)
```

.
ses.opt
```

.
HWopt<- HoltWinters(train.ts, gamma = FALSE, beta = FALSE,alpha = NULL)
HWopt
```

.
plot(HWopt)
```

.
ramalanopt<- forecast(HWopt, h=30)
ramalanopt
```

.
SSE1<-ses1$SSE
MSE1<-ses1$SSE/length(train.ts)
RMSE1<-sqrt(MSE1)

akurasi1 <- matrix(c(SSE1,MSE1,RMSE1))
row.names(akurasi1)<- c("SSE", "MSE", "RMSE")
colnames(akurasi1) <- c("Akurasi lamda=0.2")
akurasi1
```

.
SSE2<-ses2$SSE
MSE2<-ses2$SSE/length(train.ts)
RMSE2<-sqrt(MSE2)

akurasi2 <- matrix(c(SSE2,MSE2,RMSE2))
row.names(akurasi2)<- c("SSE", "MSE", "RMSE")
colnames(akurasi2) <- c("Akurasi lamda=0.7")
akurasi2
```

.
fitted1<-ramalan1$fitted
sisaan1<-ramalan1$residuals
head(sisaan1)
```

.
resid1<-training$Harga-ramalan1$fitted
head(resid1)
```

.
SSE.1=sum(sisaan1[2:length(train.ts)]^2)
SSE.1
```

.
MSE.1 = SSE.1/length(train.ts)
MSE.1
```

.
MAPE.1 = sum(abs(sisaan1[2:length(train.ts)]/train.ts[2:length(train.ts)])*
               100)/length(train.ts)
MAPE.1
```

.
akurasi.1 <- matrix(c(SSE.1,MSE.1,MAPE.1))
row.names(akurasi.1)<- c("SSE", "MSE", "MAPE")
colnames(akurasi.1) <- c("Akurasi lamda=0.2")
akurasi.1
```

.
fitted2<-ramalan2$fitted
sisaan2<-ramalan2$residuals
head(sisaan2)
```

.
resid2<-training$Harga-ramalan2$fitted
head(resid2)
```

.
SSE.2=sum(sisaan2[2:length(train.ts)]^2)
SSE.2
```

.
MSE.2 = SSE.2/length(train.ts)
MSE.2
```

.
MAPE.2 = sum(abs(sisaan2[2:length(train.ts)]/train.ts[2:length(train.ts)])*
               100)/length(train.ts)
MAPE.2
```

.
akurasi.2 <- matrix(c(SSE.2,MSE.2,MAPE.2))
row.names(akurasi.2)<- c("SSE", "MSE", "MAPE")
colnames(akurasi.2) <- c("Akurasi lamda=0.7")
akurasi.2
```

.
n_test <- nrow(testing)

e1   <- as.numeric(ramalan1$mean)[1:n_test] - as.numeric(testing$Harga)
e2   <- as.numeric(ramalan2$mean)[1:n_test] - as.numeric(testing$Harga)
eopt <- as.numeric(ramalanopt$mean)[1:n_test] - as.numeric(testing$Harga)

SSEtesting1  <- sum(e1^2,  na.rm = TRUE)
MSEtesting1  <- mean(e1^2, na.rm = TRUE)
RMSEtesting1 <- sqrt(MSEtesting1)

SSEtesting2  <- sum(e2^2,  na.rm = TRUE)
MSEtesting2  <- mean(e2^2, na.rm = TRUE)
RMSEtesting2 <- sqrt(MSEtesting2)

SSEtestingopt  <- sum(eopt^2,  na.rm = TRUE)
MSEtestingopt  <- mean(eopt^2, na.rm = TRUE)
RMSEtestingopt <- sqrt(MSEtestingopt)

akurasitesting_SSE <- matrix(c(SSEtesting1, SSEtesting2, SSEtestingopt),
                             nrow = 3,
                             dimnames = list(c("SSE1","SSE2","SSEopt"), "Nilai"))
akurasitesting_MSE <- matrix(c(MSEtesting1, MSEtesting2, MSEtestingopt),
                             nrow = 3,
                             dimnames = list(c("MSE1","MSE2","MSEopt"), "Nilai"))
akurasitesting_RMSE <- matrix(c(RMSEtesting1, RMSEtesting2, RMSEtestingopt),
                              nrow = 3,
                              dimnames = list(c("RMSE1","RMSE2","RMSEopt"), "Nilai"))

akurasitesting_SSE
```

.
akurasitesting_MSE
```

.
akurasitesting_RMSE
```

.
accuracy(ramalan1, testing$Harga)
```

.
accuracy(ramalan2, testing$Harga)
```

.
accuracy(ramalanopt, testing$Harga)
```

dari beberapa metrik evaluasi diatas, terlihat bahwa ramalanopt merupakan yang terbaik untuk data test.

# Double Exponential Smoothing

.
des.1<- HoltWinters(train.ts, gamma = FALSE, beta = 0.2, alpha = 0.2)
plot(des.1)
```

.
ramalandes1<- forecast(des.1, h=30)
ramalandes1
```

.
des.2<- HoltWinters(train.ts, gamma = FALSE, beta = 0.3, alpha = 0.6)
plot(des.2)
```

.
ramalandes2<- forecast(des.2, h=30)
ramalandes2
```

.
plot(df.ts)
lines(des.2$fitted[,1], lty=2, col="blue")
lines(ramalandes1$mean, col="red")
```

.
des.opt<- HoltWinters(train.ts, gamma = FALSE)
des.opt
```

.
plot(des.opt)
```

.
ramalandesopt<- forecast(des.opt, h=30)
ramalandesopt
```

.
ssedes.train1<-des.1$SSE
msedes.train1<-ssedes.train1/length(train.ts)
sisaandes1<-ramalandes1$residuals
head(sisaandes1)
```

.
mapedes.train1 <- sum(abs(sisaandes1[3:length(train.ts)]/train.ts[3:length(train.ts)])
                      *100)/length(train.ts)

akurasides.1 <- matrix(c(ssedes.train1,msedes.train1,mapedes.train1))
row.names(akurasides.1)<- c("SSE", "MSE", "MAPE")
colnames(akurasides.1) <- c("Akurasi lamda=0.2 dan gamma=0.2")
akurasides.1
```

.
ssedes.train2<-des.2$SSE
msedes.train2<-ssedes.train2/length(train.ts)
sisaandes2<-ramalandes2$residuals
head(sisaandes2)
```

.
mapedes.train2 <- sum(abs(sisaandes2[3:length(train.ts)]/train.ts[3:length(train.ts)])
                      *100)/length(train.ts)

akurasides.2 <- matrix(c(ssedes.train2,msedes.train2,mapedes.train2))
row.names(akurasides.2)<- c("SSE", "MSE", "MAPE")
colnames(akurasides.2) <- c("Akurasi lamda=0.6 dan gamma=0.3")
akurasides.2
```

.
selisihdes1<-ramalandes1$mean-testing$Harga
selisihdes1
```

.
SSEtestingdes1<-sum(selisihdes1^2)
MSEtestingdes1<-SSEtestingdes1/length(testing$Harga)
MAPEtestingdes1<-sum(abs(selisihdes1/testing$Harga)*100)/length(testing$Harga)

selisihdes2<-ramalandes2$mean-testing$Harga
selisihdes2
```

.
SSEtestingdes2<-sum(selisihdes2^2)
MSEtestingdes2<-SSEtestingdes2/length(testing$Harga)
MAPEtestingdes2<-sum(abs(selisihdes2/testing$Harga)*100)/length(testing$Harga)

selisihdesopt<-ramalandesopt$mean-testing$Harga
selisihdesopt
```

.
SSEtestingdesopt<-sum(selisihdesopt^2)
MSEtestingdesopt<-SSEtestingdesopt/length(testing$Harga)
MAPEtestingdesopt<-sum(abs(selisihdesopt/testing$Harga)*100)/length(testing$Harga)

akurasitestingdes <-
  matrix(c(SSEtestingdes1,MSEtestingdes1,MAPEtestingdes1,SSEtestingdes2,MSEtestingdes2,
           MAPEtestingdes2,SSEtestingdesopt,MSEtestingdesopt,MAPEtestingdesopt),
         nrow=3,ncol=3)
row.names(akurasitestingdes)<- c("SSE", "MSE", "MAPE")
colnames(akurasitestingdes) <- c("des ske1","des ske2","des opt")
akurasitestingdes
```

.
MSEfull <-
  matrix(c(MSEtesting1,MSEtesting2,MSEtestingopt,MSEtestingdes1,MSEtestingdes2,
           MSEtestingdesopt),nrow=3,ncol=2)
row.names(MSEfull)<- c("ske 1", "ske 2", "ske opt")
colnames(MSEfull) <- c("ses","des")
MSEfull
```

# Kesimpulan

## Evaluasi SMA

.
akurasi_test.sma
```

## Evaluasi DMA

.
akurasi_test.dma
```

## Evaluasi SES

.
accuracy(ramalanopt, testing$Harga)
```

## Evaluasi DES

.
akurasitestingdes
```

Dari perbandingan evaluasi diatas, didapatkan bahwa Double Moving Average adalah yang terbaik, dengan MAPE dan MSE terkecil.
