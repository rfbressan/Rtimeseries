list.of.packages <- c("urca","stargazer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(quantmod)
library(tseries)
library(ggplot2)
library(highfrequency)
library(vars)
library(forecast)
library(httpuv)
library(devtools)
library(ggplot2)
library(xts) 
library(urca) # teste de raiz unitaria
library(stargazer)


### Simula??o de 100 realiza??es de um processo de passeio aleat?rio (random walk)

x <- w <- rnorm(100)
for (t in 2:100) x[t] <- x[t - 1] + w[t]
plot(x, type = "l", main = "Passeio aleatorio", xlab = '', ylab = '', bty='l')

## Simula 100 realiza??es um passeio aleat?rio com tend?ncia

dates <- seq(1:100)
set.seed(1)
Zt <- rnorm(length(dates), mean=40, sd=20)
TD <- (1:length(dates))^(1.9)/20
Yt <- TD + Zt
data <- data.frame(dates, Yt)
plot(data, xlab='', ylab='', type= 'l', main='Passeio aleatorio com tendencia', bty='l')


## Realiza o teste ADF para a s?rie de pre?os da Petrobr?s
petr4 <- read.table('https://www.dropbox.com/s/rwk1xlfw72364m4/petr4.csv?dl=1',
                    header = TRUE, sep = ';', dec = ',')
petr4$data <- as.Date(petr4$data, format='%Y-%m-%d')
petr4 <- xts(petr4$fechamento, order.by = petr4$data)

# Teste ADF com intercepto e tend?ncia
adf.t <- ur.df(petr4, type='trend', lags=3)
table <- cbind(t(adf.t@teststat), adf.t@cval)
stargazer(table, title='Teste ADF com intercepto e tendência', type = "text")

# Teste ADF com intercepto e drift
adf.d <- ur.df(petr4, type='drift', lags=3)
table <- cbind(t(adf.d@teststat), adf.d@cval)
stargazer(table, title='Teste ADF com intercepto', type = "text")

# Teste ADF sem intercepto e sem tend?ncia
adf.n <- ur.df(petr4, type='none', lags=3)
table <- cbind(t(adf.n@teststat), adf.n@cval)
stargazer(table, title='Teste ADF sem intercepto e sem tend?ncia', type = "text")

### Ordernar novamente os dados 
logretornos <- makeReturns(petr4)

# Refazemos o teste
adf.n2 <- ur.df(logretornos, type='none', lags=3)
table <- cbind(t(adf.n2@teststat), adf.n2@cval)
stargazer(table, title='Teste ADF sem intercepto e sem tend?ncia', type = "text")

### Usa fun??o ndiffs para testar o n?mero de diferen?as para tornar a s?rie estacion?ria
ndiffs(petr4)

### Modelagem univariada das s?ries macroecon?micas do arquivo "dados.Rdata"

# dolar
fit.dolar <- auto.arima(dados[,"dolar"], max.order=12, max.d=1)
summary(fit.dolar)
# previs?es
future = forecast(fit.dolar, h = 12)
future
# Gr?fico das previs?es
plot(future)
# Gr?fico dos fitted values
plot(fit.dolar$x,col="red")
lines(fitted(fit.dolar),col="blue")

# ibcbr
fit.ibcbr <- auto.arima(dados[,"ibcbr"], max.order=12, max.d=1)
summary(fit.ibcbr)
# previs?es
future = forecast(fit.ibcbr, h = 12)
future
# Gr?fico das previs?es
plot(future)
# Gr?fico dos fitted values
plot(fit.ibcbr$x,col="red")
lines(fitted(fit.ibcbr),col="blue")

# desocupa??o
fit.desocup <- auto.arima(dados[,"desocup"], max.order=12, max.d=1)
summary(fit.desocup)
# previs?es
future = forecast(fit.desocup, h = 12)
future
# Gr?fico das previs?es
plot(future)
# Gr?fico dos fitted values
plot(fit.desocup$x,col="red")
lines(fitted(fit.desocup),col="blue")


