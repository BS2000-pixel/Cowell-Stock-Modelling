library(Hmisc)
library(pastecs)
library(TSA)
library(tseries)
library(forecast)
library(lmtest)
library("readxl")
library(MLmetrics)
library(FinTS)
library(AnalyzeTS)
library(rugarch)
library(moments)
library(fGarch)
library(fable)

data = read.csv("D:/Brian Stefano/Semester 4/ADW/COWL.JK.csv")
tsdata = ts(data$Open)
#data2 = read.csv("E:/Tugas Junaidy/Tingkat II/Semester 4/Soal dan Bahan/Analisis Deret Waktu/Proyek/BMRI.JK (1).csv")
#tsdata2 = ts(data2$Open)
#data3 = read.csv("E:/Tugas Junaidy/Tingkat II/Semester 4/Soal dan Bahan/Analisis Deret Waktu/Proyek/BMRI.JK (2).csv")
#tsdata3 = ts(data$Open, start = c(2003, 07), end = c(2020, 04), frequency = 12)
#data("airmiles")
#tsdata = airmiles

#Plot data deret waktu
plot(tsdata, main = 'Saham PT Cowell Developmert Tbk', ylab ='Nilai Saham')
abline(h=mean(tsdata), col = 'blue')
#Plot data secara seasonal
#ggseasonplot(tsdata)
#Plot per subseries, misalnya perbulan
#ggsubseriesplot(tsdata)
summary(tsdata)
#describe(tsdata)
stat.desc(tsdata)
(skew = skewness(tsdata, na.rm = TRUE))
(kurt = kurtosis(tsdata, na.rm = TRUE))
boxplot(tsdata, data = tsdata, main = "Boxplot Saham PT Cowell Developmert Tbk")

#Uji Kestasioneran
adf.test(tsdata)

# Breusch-Pagan test
#var.func <- lm(data = data)
#summary(var.func)

Acf(tsdata, main = 'Plot ACF Saham PT Cowell Developmert Tbk')
Pacf(tsdata, main = 'Plot PACF Saham PT Cowell Developmert Tbk')

tsdatadiff = diff(tsdata, differences = 1)
plot(tsdatadiff, main='Plot hasil diferensi Saham PT Cowell Developmert Tbk',col='brown')
adf.test(tsdatadiff)

acf(tsdatadiff, main = 'Plot ACF Saham PT Cowell Developmert Tbk Diferensi Sekali')
pacf(tsdatadiff, main = 'Plot PACF Saham PT Cowell Developmert Tbk Diferensi Sekali')

#3. Estimasi parameter model ARIMA
auto.arima(tsdata)
(modelmanual1 = Arima(tsdata, order = c(0,1,2)))
(modelmanual2 = arima(tsdata, order = c(1,1,0))) #Pilih modelmanual2
(modelmanual3 = Arima(tsdata, order = c(1,1,1)))
(modelmanual4 = Arima(tsdata, order = c(1,1,2)))
(modelmanual5 = Arima(tsdata, order = c(0,1,1)))

#4. Uji diagnostik residual
#cek residual model manual
checkresiduals(modelmanual2)
Box.test(residuals(modelmanual2), type = "Ljung-Box")

#5. Uji homoskedastis residual model ARIMA
ArchTest(residuals(modelmanual2))

#6. Definisikan barisan residual rt dan rt^2
rt <- residuals(modelmanual2)
rt2 <- rt^2
adf.test(rt2)

#7. Idertifikasi orde model deret waktu dari residual model ARIMA
Acf(rt2, main = 'Plot ACF rt2')
Pacf(rt2, main = 'Plot PACF rt2')

#8. Estimasi parameter model deret waktu heteroskedastis
(modelrt2_1 <- Arima(rt2, order = c(0,0,3)))
(modelrt2_2 <- Arima(rt2, order = c(1,0,3))) #tidak memenuhi syarat ketaknegatifan
(modelrt2_3 <- Arima(rt2, order = c(1,0,1))) #tidak memenuhi syarat ketaknegatifan
(modelrt2_4 <- Arima(rt2, order = c(2,0,0))) #Pilih modelrt2_4
(modelrt2_5 <- Arima(rt2, order = c(2,0,1))) #tidak memenuhi syarat ketaknegatifan
(modelrt2_6 <- Arima(rt2, order = c(2,0,2))) #tidak memenuhi syarat ketaknegatifan
(modelrt2_7 <- Arima(rt2, order = c(2,0,3))) #tidak memenuhi syarat ketaknegatifan
(modelautort2 = auto.arima(rt2)) #tidak memenuhi syarat ketaknegatifan

#9. Uji efek heteroskedastis residual model yang diperoleh
ArchTest(residuals(modelrt2_4))

#10. Hitung h-step variansi ke depan
omega = modelrt2_4$coef[3]
alpha1 = modelrt2_4$coef[1]
alpha2 = modelrt2_4$coef[2]
Nsim = length(rt2)
Var<-data.frame(k=c(1:Nsim),variansi=NA)
Var[1,2]= 0
Var[2,2]= 0
residu = data.frame(modelrt2_4$fitted)
for(k in 1:(Nsim-2)) {
  Var[k+2,2] = omega + alpha1*residu[k+1,1] + alpha2*residu[k,1]
}

#Menghitung variansi h = 5 step ke depan
Var[Nsim+1,2] <- omega + alpha1*Var[Nsim,2] + alpha2*Var[Nsim-1,2]
Var[Nsim+2,2] <- omega + alpha1*Var[Nsim+1,2] + alpha2*Var[Nsim,2]
for(k in 1:3) {
  Var[Nsim+k+2,2] <- omega + alpha1*Var[Nsim+k+1,2] + alpha2*Var[Nsim+k,2]
}

#=================================================================================================================
#menggunakan penaksir tak bias r^2t
residu = data.frame(modelrt2_4$fitted)
residu = data.frame(Fitted.arima(modelrt2))
n = nrow(residu)
a = residu[n, 1]
var1 = 0
(var1[1] <- omega + alpha1*residu[n, 1] + alpha2*residu[n-1, 1])
(var1[2] <- omega + alpha1*var1[1] + alpha2*residu[n, 1])
(var1[3] <- omega + alpha1*var1[2] + alpha1*var1[1])
(var1[4] <- omega + alpha1*var1[3] + alpha1*var1[2])
(var1[5] <- omega + alpha1*var1[4] + alpha1*var1[3])

#=================================================================================================================

#Forecast
#model ARIMA
(fit1 = arima(tsdata, order = c(1, 1, 0)))

#model GARCH
res1 = resid(fit1)
(fit2 = garch(rt, order = c(0, 2)))

forecastGARCH(fit1, fit2, r = 6, trace = TRUE) #r = banyak angka di belakang koma
forecastGARCH(fit1, fit2, r = 6) #hanya bisa forecast next-day
#hasilnya beda karena beda metode
#urtuk model rt2 yang dari fungsi arima pakai CSS
#urtuk model rt2 yang dari fungsi garch pakai LM

tsdata2 = append(tsdata, 179.198672, after = length(tsdata))
fit1 = arima(tsdata2, order = c(1, 1, 0))
(modelmanual2 = Arima(tsdata2, order = c(1,1,0)))
rt = residuals(modelmanual2)
fit2 = garch(rt, order = c(0, 2))
forecastGARCH(fit1, fit2, r = 6)

tsdata3 = append(tsdata2, 179.03814, after = length(tsdata))
fit1 = arima(tsdata3, order = c(1, 1, 0))
modelmanual2 = Arima(tsdata3, order = c(1,1,0))
rt = residuals(modelmanual2)
fit2 = garch(rt, order = c(0, 2))
forecastGARCH(fit1, fit2, r = 6)

tsdata4 = append(tsdata3, 179.230831, after = length(tsdata))
fit1 = arima(tsdata4, order = c(1, 1, 0))
modelmanual2 = Arima(tsdata4, order = c(1,1,0))
rt = residuals(modelmanual2)
fit2 = garch(rt, order = c(0, 2))
forecastGARCH(fit1, fit2, r = 6)

tsdata5 = append(tsdata4, 179.230831, after = length(tsdata))
fit1 = arima(tsdata5, order = c(1, 1, 0))
modelmanual2 = Arima(tsdata5, order = c(1,1,0))
rt = residuals(modelmanual2)
fit2 = garch(rt, order = c(0, 2))
forecastGARCH(fit1, fit2, r = 6)
#================================================================================================================
#Fitting model
#spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(0, 2), submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), 
#mean.model = list(armaOrder = c(1, 1), external.regressors = NULL, distribution.model = "norm", start.pars = list(), fixed.pars = list()))

#garch <- ugarchfit(spec = spec, data = tsdata, solver.cortrol = list(trace=0))

#garch@fit$coef

#plot(tsdata)
#lines(garch@robust.val@fitted.values)

#ugarchforecast(garch, data = tsdata, n.ahead = 10)

#garch@fit$sigma
#garch@fit$z
#str(garch)

set.seed(15000); 
white_noise <- arima.sim(model = list(order = c(0, 0, 0)), n = 5)
forecast = 0
(forecast[1] = 1.2003*tsdata[1721] - 0.2003*tsdata[1720]+(var1[1])^(0.5)*white_noise[1])
(forecast[2] = 1.2003*forecast[1] - 0.2003*tsdata[1721]+(var1[2])^(0.5)*white_noise[2])
for(k in 3:5) {
  (forecast[k] = 1.2003*forecast[k-1] - 0.2003*forecast[k-2]+(var1[k])^(0.5)*white_noise[k])
}

#Plot Forecast
plot(tsdata, type="l", col="black")
lines(1722:1726, forecast,type = "l", col="blue")

forecast(tsdata,model = modelmanual2, h=5)
