# Metode Peramalan
#install packages
install.packages("TSA")
install.packages("forecast")
install.packages('tseries')
install.packages("normtest")

#load packages
library(TSA)
library(forecast)
library(tseries)
library(normtest)

TSLA1 = data.frame(TSLA$Close)
View(TSLA1)  #ini merupakan data yang akan dipakai

# PART 1
# jadikan data kita jadi data time series
TESLAts = ts(TSLA1, frequency = 1, start = c(1,1))  
head(TESLAts)
tail(TESLAts)
plot(TESLAts) #untuk melihat apakah sudah stasioner

#ADF test, uji hipotesis untuk kestasioneran
adf.test(TESLAts) 
#karena p-value > alpha, maka H0 diterima (tidak stasioner)

#karena data tidak stasioner, kita lakukan differencing
difftsla = diff(TESLAts, differences = 1)
plot(difftsla)
adf.test(difftsla) # karena p-value < 0.05, maka H0 ditolak = Stasioner


#PART 2
#Cari Order ARIMA (x,x,x)
tsdisplay(difftsla) #liat Plot ACF, PACF
#kita lihat orde AR dan MA dari acf, pacf
eacf(difftsla)
#maka terlihat bahwa dapat digunakan model dimana ar = 0, dan ma = 0

#PART 3
#dari hasil plot acf, pacf, dan eacf, dicoba beberapa model lain (Overfitting)
model1 = Arima(TESLAts, order = c(0,1,1))
model2 = Arima(TESLAts, order = c(1,1,0))
model3 = Arima(TESLAts, order = c(1,1,1))
model1
model2
model3
cbind(model1,model2,model3) # untuk merangkum semuanya
# karena menunjukkan hasil yang mirip, maka akan digunakan model yang paling sederhana yaitu arima(0,1,0)

#PART 4
#ARIMA(0,1,0)
#Penaksiran Parameter
fit = Arima(TESLAts, order = c(0,1,0))
fit 
#Metode momen = MME, default = ML (maximum likelihood)

#PART 5
#diagnostic checking / cek asumsi
checkresiduals(fit)  
# 2. cek normality residual
# Hipotesis = H0: residual data berdist. normal, H1: residual data TIDAK berdist. normal
qqnorm(TESLAts, nrepl = 2000) #p-value>alpha = H0 diterima
# 3. overfitting data
model1 = Arima(TESLAts, order = c(0,1,1))
model2 = Arima(TESLAts, order = c(1,1,0))
model3 = Arima(TESLAts, order = c(1,1,1))
model1
model2
model3
cbind(model1,model2,model3) # untuk merangkum semuanya
# karena menunjukkan hasil yang mirip, maka akan digunakan model yang paling sederhana yaitu arima(0,1,0)

#PART 6
#Cross-Validation
actual = window(TESLAts, start=c(248))  
TESLAts2 = window(TESLAts, end = c(247)) 
fit2 = Arima(TESLAts2, order=c(0,1,0)) #TRAINING
#lakukan forecasting u/ data testing
forecast2 = forecast(fit2, h=5)
plot(forecast2)
cbind(actual,forecast2)
# selanjutnya, lakukan forecasting untuk data keseluruhan
forecast = forecast(fit, h=3)
plot(forecast)
forecast
