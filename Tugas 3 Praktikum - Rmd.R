---
  title: "Tugas Pertemuan-3"
author: "Fachry Wirayuda Alindri"
date: "2025-09-16"
output: html_document
---
  ##Import library dan Baca Data
  ```{r}
library(readr)
library(dplyr)
library(forecast)   
library(caret)    
library(dLagM)
library(nardl)
library(dynlm)
library(zoo)
library(MLmetrics)
library(Metrics)

data <- read_csv("C:/Users/LENOVO/Downloads/NewDelhi_Air_quality.csv")
data
```

##Pilih variabel (misalnya X = pm25, Y = AQI) dan Pisahkan data train-test ( 80% train, 20% test)
```{r}
df <- data.frame(
  Yt = data$AQI,
  Xt = data$pm25
)

#split data
set.seed(123)
n <- nrow(df)
train_size <- round(0.8 * n)


train <- df[1:train_size, ]
test  <- df[(train_size+1):n, ]
#MODEL KOYCK
model.koyck <- koyckDlm(x = train$Xt, y = train$Yt)
summary(model.koyck)

AIC(model.koyck)
BIC(model.koyck)
```
Dari hasil tersebut, didapat bahwa peubah $$x_t$$ dan $$y_{t−1}$$ memiliki nilai P−Value < 0.05 Hal ini menunjukkan bahwa peubah $$x_t$$ dan $$y_{t−1}$$ berpengaruh signifikan terhadap y Adapun model keseluruhannya adalah sebagai berikut:
  
  $$
  \hat{Y_t}=4.0771 - 0.572X_t + 0.8897 Y_{t-1}
$$
  
  ###Peramalan dan Akurasi
  ```{r}
fore.koyck <- dLagM::forecast(model = model.koyck, x = test$Xt, h = 5)
fore.koyck
mape.koyck <- MAPE(y_pred = fore.koyck$forecasts, 
                   y_true = head(test$Yt, 5))
mape.koyck

#akurasi data training
GoF(model.koyck)
```
##Regresi dengan Distributed Lag
###Pemodelan (Lag=2)
```{r}
#Pemodelan (Lag=2)
model.dlm <- dlm(x = train$Xt,y = train$Yt , q = 2)
summary(model.dlm)
AIC(model.dlm)
BIC(model.dlm)
```
Dari hasil tersebut, didapat bahwa peubah $$x_{t-1}$$ dan P−Value dari intercept < 0.05 Hal ini menunjukkan bahwa $$x_{t-1}$$ dan intercept berpengaruh terhadap y Adapun model keseluruhannya adalah sebagai berikut:
  
  $$
  \hat{Y_t}=35.453 - 8.438X_t + 6.515 x_{t-1} - 2.178x_{t-2}
$$
  ### Peramalan dan Akurasi
  Berikut merupakan hasil peramalan $$y$$ untuk 5 periode kedepan
```{r}
fore.dlm <- dLagM::forecast(model = model.dlm, x = test$Xt, h = 5)

mape.dlm <- MAPE(y_pred = fore.dlm$forecasts,
                 y_true = test$Yt[1:5])

#akurasi data training
GoF(model.dlm)
```
###Lag Optimum
```{r}
#penentuan lag optimum 
finiteDLMauto(formula = Yt ~ Xt,
              data = data.frame(train), q.min = 1, q.max = 10,
              model.type = "dlm", error.type = "AIC", trace = TRUE)

#model dlm dengan lag optimum
model.dlm2 <- dlm(x = train$Xt,y = train$Yt , q = 6)
summary(model.dlm2)

AIC(model.dlm2)
BIC(model.dlm2)
```
Dari hasil tersebut terdapat beberapa peubah yang berpengaruh signifikan terhadap taraf nyata 5% yaitu $$x_t$$, $$x_{t−2}$$, $$x_{t−4}$$, $$x_{t−6}$$ Adapun keseluruhan model yang terbentuk adalah
$$
  \hat{Y_t}=37.87 - -12.0761X_t + 10.8089x_{t-1} +.... - 1.4458x_{t-6}
$$
  
  ```{r}
fore.dlm2 <- dLagM::forecast(model = model.dlm2, x = test$Xt, h = 5)

mape.dlm2 <- MAPE(y_pred = fore.dlm2$forecasts,
                  y_true = test$Yt[1:5])

#akurasi data training
GoF(model.dlm2)
```
Model tersebut merupakan model yang sangat baik dengan nilai MAPE yang kurang dari 10%.

##Model Autoregressive
###Pemodelan
```{r}
model.ardl <- ardlDlm(x = train$Xt, y = train$Yt, p = 1 , q = 1)
summary(model.ardl)

AIC(model.ardl)
BIC(model.ardl)
```
Hasil di atas menunjukkan bahwa selain peubah $$y_{t−1}$$, hasil uji t menunjukkan nilai-p pada peubah ≥ 0.05 Hal ini menunjukkan bahwa peubah $$y_{t−1}$$ berpengaruh signifikan terhadap $$y_t$$,sementara $$x_t$$ dan $$x_{t−1}$$ berpengaruh signifikan terhadap $$y_t$$. Model keseluruhannya adalah sebagai berikut:
  $$
  \hat{Y_t}=4.00535 - 0.52588X_t -0.3046X_{t-1} + 0.89112Y_{t-1}
$$
  
  ### Peramalan dan Akurasi
  Berikut merupakan hasil peramalan $$y$$ untuk 5 periode kedepan
```{r}
fore.ardl <- dLagM::forecast(model = model.ardl, x = test$Xt, h = 5)
fore.ardl
mape.ardl <- MAPE(y_pred = fore.ardl$forecasts, 
                  y_true = head(test$Yt, 5))
mape.ardl

#akurasi data training
GoF(model.ardl)
```
Berdasarkan akurasi di atas, terlihat bahwa nilai MAPE keduanya tidak jauh berbeda. Artinya, model regresi dengan distribusi lag ini tidak overfitted atau underfitted

### Lag Optimum
```{r}
str(train)
head(train)


model.ardl.opt <- ardlBoundOrders(
  data    = train,
  formula = Yt ~ Xt,   # ganti sesuai nama variabel di data kamu
  ic      = "AIC"
)


summary(model.ardl.opt)

min_p <- c()
for (i in 1:6) {
  min_p[i] <- min(model.ardl.opt$Stat.table[[i]], na.rm = TRUE)
}

q_opt <- which(min_p == min(min_p, na.rm = TRUE))

p_opt <- which(model.ardl.opt$Stat.table[[q_opt]] == 
                 min(model.ardl.opt$Stat.table[[q_opt]], na.rm = TRUE))

lag_opt <- data.frame(
  q_optimum = q_opt,
  p_optimum = p_opt,
  AIC       = model.ardl.opt$min.Stat
)

lag_opt
```
Dari tabel di atas, dapat terlihat bahwa nilai AIC terendah didapat ketika p=5 dan q=10, yaitu sebesar 61.973934. Artinya, model autoregressive optimum didapat ketika p=5 dan q=10.

##Perbandingan Model
```{r}
akurasi <- matrix(c(mape.koyck, mape.dlm, mape.dlm2, mape.ardl))
row.names(akurasi)<- c("Koyck","DLM 1","DLM 2","Autoregressive")
colnames(akurasi) <- c("MAPE")
akurasi
```
Berdasarkan nilai MAPE, model paling optimum didapat pada Model DLM 2 (Lag =2) karena memiliki nilai MAPE yang terkecil.

###Plot
```{r}
library(ggplot2)

df.gg <- data.frame(
  Xt = rep(test$Xt[1:5], 5),
  Yt = c(test$Yt[1:5],
         fore.koyck$forecasts,
         fore.dlm$forecasts,
         fore.dlm2$forecasts,
         fore.ardl$forecasts),
  Model = rep(c("Aktual","Koyck","DLM1","DLM2","ARDL"), each=5)
)

ggplot(df.gg, aes(x=Xt, y=Yt, color=Model)) +
  geom_line() + geom_point() +
  theme_minimal() +
  labs(title="Perbandingan Forecast Model")
```


