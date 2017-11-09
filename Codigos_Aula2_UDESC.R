# Simula??o de processo estacion?rio
plot.ts(rnorm(1000,0,1),ylab="dados",main="1000 observacoes de um processo estacionario")

# Simula??o de processo n?o-estacion?rio
plot.ts(cumprod(1+rnorm(1000,0,1)/100)-1,ylab="dados",
        main="1000 observacoes de um processo nao-estacionario")



# Autocorrela??o dos log-retornos da petrobras
library(forecast)
library(ggplot2)
library(xts)
library(highfrequency)

petr4 <- read.table('https://www.dropbox.com/s/rwk1xlfw72364m4/petr4.csv?dl=1',
                    header = TRUE, sep = ';', dec = ',')
petr4$data <- as.Date(petr4$data, format='%Y-%m-%d')
petr4 <- xts(petr4, order.by = petr4$data)
logretornos <- makeReturns(petr4$fechamento)
autoplot(logretornos, xlab='', main='Logretorno dos precos de fechamento')
ggAcf(logretornos)
logsq <- logretornos^2
ggAcf(logsq)
logabs <- abs(logretornos)
ggAcf(logabs)

# Autocorrela??o parcial dos log-retornos da petrobras
ggPacf(logretornos)







