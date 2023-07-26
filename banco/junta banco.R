
############ Analisa1 #############
setwd('D:/Downloads/ESTAT - Geani')

# Carrega os pacotes requeridos
library(survey)
library(tidyverse)
library(srvyr)

# Ler o arquivo de dados com a amostra da pesquisa
plano <- readRDS("plano.rds")
names(plano)

#Ler banco sangue
# setwd("/Users/ludmillavianajacobson/Library/CloudStorage/Dropbox/Projetos de Pesquisa/Projeto ZDC/Geani")

dados<- readRDS("DB_ZDC_FASE2.rds")
names(dados)

#Junta variáveis
#Selecionando variaveis
dados2<-dados[,c(1:6,8,277:281)]
names(dados2)
str(dados2)
dados2$X01_UF<-as.integer(dados2$X01_UF)
dados2$X02_MUNIC<-as.integer(dados2$X02_MUNIC)
dados2$X03_DISTRITO <-as.integer(dados2$X03_DISTRITO) 
dados2$X04_SUBDISTRITO<-as.integer(dados2$X04_SUBDISTRITO)
dados2$X05_SETOR<-as.integer(dados2$X05_SETOR)
dados2$X06_DOMICILIO <-as.integer(dados2$X06_DOMICILIO)  
dados2$B00_NUMERO<-as.integer(dados2$B00_NUMERO)

table(dados2$ZIK_PRNT20)                           

# Traz variaveis
sangue<- left_join(plano$variables, dados2,
                    by = join_by("x01_uf"=="X01_UF" , "x02_munic"=="X02_MUNIC" , 
                           "x03_distrito"=="X03_DISTRITO", 
                           "x04_subdistrito"=="X04_SUBDISTRITO", 
                           "x05_setor"=="X05_SETOR" , "x06_domicilio"=="X06_DOMICILIO" ,
                           "b00_numero"=="B00_NUMERO") ) 

### Cria objeto com dados, desenho e peso corrigido para pessoa.
#Aqui o peso não está corrigido para a amostra de sangue.
plano <- svydesign(ids = ~p06_upa+x06_domicilio, strata = ~p05_estrato, data = sangue,
                   weights = ~peso_teste, nest = TRUE)

