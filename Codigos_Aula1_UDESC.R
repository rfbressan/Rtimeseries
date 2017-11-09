### Simula??o da Lei dos Grandes N?meros
die <- 1:6
roll <- function(n) {
  mean(sample(die, size = n, replace = TRUE))
}
plot(sapply(1:1000, roll), type = "l", xlab = 
       "Lancamentos", ylab = "Media")
abline(h = 3.5, col = "red")

### Estat?stica descritiva para a s?rie de retornos das a??es da Petrobr?s
library(forecast)
library(ggplot2)
library(xts)
library(highfrequency)
library(moments)

# Carregamos nosso database diretamente da web
petr4 <- read.table('https://www.dropbox.com/s/rwk1xlfw72364m4/petr4.csv?dl=1',
                    header = TRUE, sep = ';', dec = ',')

# Ordernar os dados de acordo com um vetor de datas
petr4$data <- as.Date(petr4$data, format='%Y-%m-%d')
petr4 <- xts(petr4, order.by = petr4$data)

# calculamos os log-retornos de nossa s?rie de pre?os
logretornos <- makeReturns(petr4$fechamento)*100 #highfrequency

# Gr?ficos
par(mar=c(2,2,2,2))
par(mfrow=c(2,1))
ts.plot(petr4$fechamento, xlab='', main='Precos de fechamento')
ts.plot(logretornos, xlab='', main='Logretornos do preco de fechamento')

# Estat?stica descritiva
summary(logretornos)
skewness(logretornos)
kurtosis(logretornos)

# Gr?fico da densidade
par(mfrow=c(1,1))
d <- density(logretornos)
plot(d,ylim=c(0,0.4),main = "Densidade dos log-retornos")
xfit<-seq(min(logretornos),max(logretornos),length=length(logretornos)) 
yfit<-dnorm(xfit,mean=mean(logretornos),sd=sd(logretornos)) 
lines(yfit)
lines(xfit, yfit, col="blue", lwd=2)

### Estimadores de m?xima verossimilhan?a
fn <- function(theta) {
  sum ( 0.5*(logretornos - theta[1])^2/theta[2] + 0.5* log(theta[2]) )
}
nlm(fn, theta <- c(0,1), hessian=TRUE)



### Simula??o de Equa??es a Diferen?a

## Caso 1: Ra?zes reais e distintas
# Exemplo 1: y(t)=0.2*y(t-1)+0.35*y(t-2)
lambda1 <- 0.7
lambda2 <- 0.5
c1 <- 1
c2 <- 1
t <- 1:20
yt <- c1*(lambda1^t)+(c2*lambda2^t)
plot(yt, xlab='', ylab='', bty='l', col='black', type = 'l',
     main='Caso 1: ra?zes reais e distintas, Exemplo 1')


# Exemplo 2: y(t)=0.7*y(t-1)+0.35*y(t-2) 
lambda3 <- 1.037;
lambda4 <- -0.337;
c1 <- 1
c2 <- 1
yt2 <- c1*(lambda3^t)+c2*(lambda4^t);
plot(yt2, xlab='', ylab='', bty='l', col='black',
     main='Caso 1: ra?zes reais e distintas, Exemplo 2',type = 'l')

# Observando as duas juntas #

par(mfrow=c(1,2))
plot(yt, xlab='', ylab='', bty='l', col='black', type = 'l',
     main='Caso 1: ra?zes reais e distintas, Exemplo 2')
plot(yt2, xlab='', ylab='', bty='l', col='black', type = 'l',
     main='Caso 1: ra?zes reais e distintas, Exemplo 1')

## Caso 2: ra?zes reais e iguais
# Exemplo 1 #
t2 <- 1:200
c1 <- 1
c2 <- 1
phi <- 0.9
lambda5 <- phi/2
yt3 <- c1*(lambda5^t2)+c2*(t2*lambda5^t2)
plot(yt3, xlab='', ylab='', bty='l', col='black',
     main='Caso 2: ra?zes reais e iguais, Exemplo 1',type = 'l')

# Exemplo2 #
c1 <- 1
c2 <- 1
phi2 <- -1.9
lambda6 <- phi2/2
yt4 <- c1*(lambda6^t2)+c2*t2*(lambda6^t2)
plot(yt4, xlab='', ylab='', bty='l', col='black',
     main='Caso 2: ra?zes reais e iguais, Exemplo 2', type = 'l')

# Observando as duas juntas #

par(mfrow=c(1,2))
plot(yt3, xlab='', ylab='', bty='l', col='black',
     main='Caso 2: ra?zes reais e iguais, Exemplo 1', type = 'l')
plot(yt4, xlab='', ylab='', bty='l', col='black',
     main='Caso 2: ra?zes reais e iguais, Exemplo 2', type = 'l')

## Caso 3: ra?zes imagin?rias
# Exemplo1: y(t)=1.6*y(t-1)-0.9*y(t-2)
t3 <- 1:100
beta1 <- 1
beta2 <- 0
r <- 1.05
theta <- 0.8
yt5 <- beta1*(r^t3)*cos(theta*t3+beta2);
# yt6=(sqrt(1.6)^t3)*(cos(theta*t3) + sin(theta*t3))

plot(yt5, xlab='', ylab='', bty='l', col='black',
     main='Caso 3: ra?zes imagin?rias', type = 'l')
