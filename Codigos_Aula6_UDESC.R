list.of.packages <- c("xts","vars","quantmod","ustyc","rmgarch")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(ggplot2)
library(xts)
library(vars)
library(quantmod)
library(ustyc)
library(BETS)
library(tseries)
library(rmgarch)
options(scipen=999)

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

# Modelo VEC entre maturidades  1 mês, 3 meses e 1 ano
dados <- data.frame(MAT1 = juros$BC_1MONTH, MAT2 = juros$BC_3MONTH, MAT5 = juros$BC_1YEAR)
# testa cointegração
summary(ca.jo(dados, type = "trace", ecdet = "none",K = 3, spec = "transitory"))
# Estima modelo VEC
vecm <- ca.jo(dados, type = "trace", ecdet = "none", K = 3, spec = "transitory")
vecm <- cajorls(vecm, r = 1)
summary(vecm$rlm)

### Modelo de correla??es condicionais din?micas entre infla??o e desemprego nos EUA

#Capturamos a s?rie do CPI americano do Federal Economic Reserve Data (FRED)
getSymbols("CPIAUCSL", src = "FRED")
autoplot(CPIAUCSL)
#Calculamos a segunda diferen?a dos logs
CPI <- log(CPIAUCSL)
CPID <- diff(diff(CPI))
autoplot(CPID)
#Capturamos a s?rie da taxa de desemprego do Federal Economic Reserve Data (FRED)
getSymbols('UNRATE',src='FRED') 
autoplot(UNRATE)
#Calculamos a primeira diferen?a dos logs
unrate <- log(UNRATE)
unrated <- diff(unrate)
autoplot(unrated)
#Unimos os dados
dados <- na.exclude(merge(CPID, unrated))
#Atribuimos nomes a nossas colunas
colnames(dados) <- c('CPI', 'Desemprego')

# Estima modelo DCC
spec1 <- ugarchspec(distribution = "norm")
mspec <- multispec(c(spec1,spec1))
dcc.spec<-dccspec(mspec,VAR=TRUE,lag=1,dccOrder=c(1,1),model="DCC",distribution="mvnorm")
d.fit.2<-dccfit(dcc.spec,dados)
print(d.fit.2)
cov.dcc <- rcov(d.fit.2)
# plota vari?ncia conditional da s?rie taxa de infla??o
ts.plot(cov.dcc[1,1,])
# plota correla??o conditional entre infla??o e desemprego
ts.plot(cov.dcc[1,2,]/(sqrt(cov.dcc[1,1,])*sqrt(cov.dcc[2,2,])))

