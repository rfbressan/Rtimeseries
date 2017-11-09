list.of.packages <- c("forecast","ggplot2","xts","highfrequency","fGarch","tseries","BatchGetSymbols",
                      "gridExtra","DescTools","reshape2","vars","quantmod","ustyc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(forecast)
library(ggplot2)
library(xts)
library(highfrequency)
library(fGarch)
library(tseries)
library(BatchGetSymbols)
library(DescTools)
library(reshape2)
library(gridExtra)
library(vars)
library(quantmod)
library(ustyc)
options(scipen=999)

## Simula um processo ARCH(1) para T observa??es
spec <- garchSpec(model = list(alpha = c(0.2), beta = 0))
arch1 <- garchSim(spec, n = 10000)
summary(arch1)
ggtsdisplay(arch1)

## Simula um processo ARCH(2) e depois estima com fun??o "garch"
n <- 1100
a <- c(0.1, 0.5, 0.2)  # ARCH(2) coefficients
e <- rnorm(n)  
x <- double(n)
x[1:2] <- rnorm(2, sd = sqrt(a[1]/(1.0-a[2]-a[3]))) 
for(i in 3:n)  # Generate ARCH(2) process
{
  x[i] <- e[i]*sqrt(a[1]+a[2]*x[i-1]^2+a[3]*x[i-2]^2)
}
x <- ts(x[101:1100])
x.arch <- garch(x, order = c(0,2), trace = FALSE)  # Fit ARCH(2) 
summary(x.arch)
plot(x.arch$fitted.values[,1],main="Volatilidades do modelo ARCH(2)")


## Simula um processo GARCH(1,1)
spec <- garchSpec(model = list())
garch11 <- garchSim(spec, n = 10000)
plot.ts(garch11, type="l", main="Simula??o de um GARCH(1,1)", ylab='')


## Modelo GARCH(1,1) para a s?rie de retornos da Petrobr?s
petr4 <- read.table('https://www.dropbox.com/s/rwk1xlfw72364m4/petr4.csv?dl=1',
                    header = TRUE, sep = ';', dec = ',')
petr4$data <- as.Date(petr4$data, format='%Y-%m-%d')
petr4 <- xts(petr4$fechamento, order.by = petr4$data)
logretornos <- makeReturns(petr4)*100
garch <- garchFit(formula = ~garch(1,1), data = logretornos, title = "Garch(1,1)", trace = FALSE)
summary(garch)
plot(garch, which = 2)


# Baixa ?ltimos 1500 dados mais recentes das a??es da AAPL
my.ticker <- c('AAPL')
first.date <- Sys.Date()-1500
last.date <- Sys.Date()
l.out <- BatchGetSymbols(tickers = my.ticker,first.date = first.date,last.date = last.date)
returns <- data.frame(prices=l.out$df.tickers$price.adjusted[2:l.out$df.control$total.obs],retornos=diff(log(l.out$df.tickers$price.adjusted))*100,datas=l.out$df.tickers$ref.date[2:l.out$df.control$total.obs])
# Estima modelo
resultados.finais <- garchFit(formula = ~ arma(1,0) + garch(1, 1), 
                               data = returns[,1], cond.dist = c("QMLE"), include.mean = T)
summary(resultados.finais)
sigma.t <- resultados.finais@sigma.t
returns <- data.frame(returns,sigma.t=sigma.t)
# Faz gr?fico 
p1 <- ggplot(data = returns, aes(x = datas, y = prices))
p1 <- p1 + geom_line()
p1 <- p1 + labs(x = 'Dates', y = 'Pre?os')

p2 <- ggplot(data = returns, aes(x = datas, y = retornos))
p2 <- p2 + geom_line()
p2 <- p2 + labs(x = 'Dates', y = 'Retornos')

p3 <- ggplot(data = returns, aes(x = datas, y = sigma.t))
p3 <- p3 + geom_line()
p3 <- p3 + labs(x = 'Dates', y = 'Desvio padr?o condicional')

grid.arrange(p1, p2, p3, ncol=1)


### Modelo VAR para infla??o e desemprego nos EUA

#Capturamos a s?rie do CPI americano no database do Quantmod
getSymbols("CPIAUCSL", src = "FRED")
autoplot(CPIAUCSL)

#Calculamos a primeira diferen?a dos logs
CPI <- log(CPIAUCSL)
CPID <- diff(diff(CPI))
autoplot(CPID)
ndiffs(CPID,alpha = 0.1)

#Capturamos a s?rie da taxa de desemprego do Federal Economic Reserve Data (FRED)
getSymbols('UNRATE',src='FRED') 
autoplot(UNRATE)

#Calculamos a primeira diferen?a dos logs
unrate <- log(UNRATE)
unrated <- diff(unrate)
ndiffs(unrated)

autoplot(unrated)

#Unimos os dados
nossovar <- na.exclude(merge(CPID, unrated))

#Atribuimos nomes a nossas colunas
colnames(nossovar) <- c('CPI', 'Taxa de desemprego')

#Escolhemos nosso modelo
VARselect(nossovar, lag.max=24)

#Estimamos nosso VAR
var <- VAR(nossovar, 4, type='both') 
summary(var)


#Gr?ficos
par(mar=c(1,1,1,1))
plot(var, names = "CPI")
plot(predict(var,12))

#Diagn?stico do ajuste do modelo baseado nos res?duos:
serial.test(var, lags.pt = 16, type = "PT.asymptotic")
normality.test(var)


#Fun??o impulso resposta para a vari?vel "CPID"
VAR.IRF <- irf(var, response = "CPI", n.ahead = 12, boot = TRUE)
plot(VAR.IRF)

#Causalidade de Granger
causality(var, cause = "CPI")
causality(var, cause = "Taxa.de.desemprego")

### Testando cointegra??o entre maturidades da estrutura a termo das taxas de juros dos EUA

# Baixa curva de juros dos EUA entre 2014 e 2017
juros = do.call(rbind,lapply(2014:2017,function(x) getYieldCurve(x)$df))
juros <- juros[juros$BC_1MONTH!=0,] # Remove valores 0 da base
datas <- as.Date(rownames(juros),format='%Y-%m-%d')
juros.xts <- xts(juros, order.by = datas)
plot(juros.xts)

# Modelo VEC entre maturidades  1 m?s e 3 meses
dados <- data.frame(MAT1 = juros$BC_1MONTH, MAT2 = juros$BC_3MONTH)
# testa cointegra??o
summary(ca.jo(dados, type = "trace", ecdet = "none",K = 3, spec = "transitory"))
# Estima modelo VEC
vecm <- ca.jo(dados, type = "trace", ecdet = "none", K = 3, spec = "transitory")
vecm <- cajorls(vecm, r = 1)
summary(vecm$rlm)






