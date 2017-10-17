# Instala pacotes
list.of.packages <- c("devtools","httpuv","quantmod","tseries","xts","reshape",
                      "highfrequency","vars","forecast","readxl","ggplot2","psych",
                      "car","MASS","stargazer","ggfortify","Rtools","moments")
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

# Instala BETS
devtools::install_github("pedrocostaferreira/BETS")
library(BETS)

# Remove formata??o cient?fica
options(scipen=999)

# L? arquivo externo de dados
dados <- read.table("http://www.dropbox.com/s/8wuobh332sbcg18/Arrecadacao.dat?dl=1", header=TRUE)

# Vamos visualizar a parte inicial dos dados e tamb?m tabular o n?mero de munic?pios por Estado:
head(dados)
table(dados$Estado,dados$Ano)

# Gr?fico de barras do PIB dos munic?pios de SC:
ggplot(data=subset(dados,Estado=="SC"),aes(x=Municipio,y = PIB)) + 
  geom_bar(stat = "identity") 

# Mesmo gr?fico anterior, s? que ano a ano
ggplot(data=subset(dados,Estado=="SC"),
       aes(x=Municipio,y = PIB)) + 
  geom_bar(stat = "identity") +
  facet_wrap(facets = ~Ano)		

# Histograma da popula??o de todos os estados:
ggplot(data=dados,aes(POPULACAO)) + 
  geom_histogram(bins = 50) + 
  xlim(0, 2e+06)

# Vers?o ``fashion'' do Histograma da popula??o de todos os estados:
ggplot(data=dados,aes(POPULACAO,fill = Estado))+ 
  geom_histogram(bins = 50) + 
  xlim(0, 2e+06)

# Boxplot da popula??o dos 3 estados do Sul:
ggplot(data=subset(dados,Estado==c("SC","PR","RS")), 
       aes(x=Estado,y=POPULACAO)) + 
  geom_boxplot()


# Gr?fico de dispers?o entre PIB e popula??o, com linha de tend?ncia e intervalo de confian?a:
ggplot(data=dados, aes(x=POPULACAO, y=PIB)) + 
  geom_point(shape=1) + 
  xlim(0, 2e+06) + 
  ylim(0,1e+11) +  
  geom_smooth()


