
############ Analisa1 #############
# Define pasta de trabalho onde encontrar arquivos de dados
# setwd("/Users/ludmillavianajacobson/Library/CloudStorage/Dropbox/Projetos de Pesquisa/Projeto ZDC/Geani")
# Limpa area de trabalho
rm(list = ls())

# Carrega os pacotes requeridos
library(survey)
library(tidyverse)
library(srvyr)

# Ler o arquivo de dados com a amostra da pesquisa
plano <- readRDS("plano.rds")

summary(plano)

###############################################
##### Cria Variável do Artigo PlosOne

# Cria classes de idade dos entrevistados
summary(plano$variables$b05_idade)

table(plano$variables$b05_idade)
plano <- mutate(plano,
                c_idade = cut(b05_idade, c(0,14,29,59,120), include.lowest = TRUE))

svytable(~b05_idade+c_idade, plano)


#################################
# Cria Sexo
table(plano$variables$b04_sexo)

plano$variables$c_sexo <-"Male"
plano$variables$c_sexo[plano$variables$b04_sexo=="Feminino"]<-"Female"

table(plano$variables$b04_sexo,plano$variables$c_sexo)

# Cria raça
table(plano$variables$b06_cor)

plano$variables$raca <-"Non-black"
plano$variables$raca[plano$variables$b06_cor=="Preta"| 
                       plano$variables$b06_cor=="Parda (mulata, cabocla, cafuza, mameluca ou mestiça)"]<-"Black"

table(plano$variables$b06_cor,plano$variables$raca)

# Cria escolaridade
table(plano$variables$b09_curso,plano$variables$Escol)

plano$variables$esc <-" 0 - 04 anos"
plano$variables$esc[plano$variables$Escol==3|
                      plano$variables$Escol==4|plano$variables$Escol==5|plano$variables$Escol==6]<-"05 - 09 anos"
plano$variables$esc[plano$variables$Escol==7|plano$variables$Escol==8|plano$variables$Escol==9]<-"10 - 12 anos"
plano$variables$esc[plano$variables$Escol==10]<-"13 ou + anos"
plano$variables$esc[plano$variables$Escol==11]<-NA

table(plano$variables$b07_ler,plano$variables$esc)
table(plano$variables$b05_idade,plano$variables$esc)
table(plano$variables$b05_idade,plano$variables$Escol)

# Estado civil
table(plano$variables$b11_civil)

plano$variables$TP_uniao <-"Sem união estavel"
plano$variables$TP_uniao[plano$variables$b11_civil == "Casado(a) ou união estável"]<-"Casado(a) ou união estável"
plano$variables$TP_uniao[plano$variables$b11_civil =="Não sabe"] <-NA

table(plano$variables$b11_civil,plano$variables$TP_uniao)                                                 


#febre amarela
table(plano$variables$d13_febre_vacina)

plano$variables$yellow <-"No"
plano$variables$yellow[plano$variables$d13_febre_vacina == "Sim"]<-"Yes"
plano$variables$yellow[plano$variables$d13_febre_vacina =="Não sabe"] <-NA

table(plano$variables$d13_febre_vacina , plano$variables$yellow)

# Cria Dengue
#DG
table(plano$variables$f04_d_g_resultado)

plano$variables$DG <-"2-Negativo"
plano$variables$DG[plano$variables$f04_d_g_resultado=="Positivo/Reativo"]<-"1-Positivo"

table(plano$variables$f04_d_g_resultado,plano$variables$DG)

#DM
table(plano$variables$f06_d_m_resultado)

plano$variables$DM <-"2-Negativo"
plano$variables$DM[plano$variables$f06_d_m_resultado=="Positivo/Reativo"]<-"1-Positivo"

table(plano$variables$f06_d_m_resultado,plano$variables$DM)

# Cria zika
#ZG
table(plano$variables$f08_z_g_resultado)

plano$variables$ZG <-"2-Negativo"
plano$variables$ZG[plano$variables$f08_z_g_resultado=="Positivo/Reativo"]<-"1-Positivo"

  table(plano$variables$f08_z_g_resultado,plano$variables$ZG)

#ZM
table(plano$variables$f10_z_m_resultado)

plano$variables$ZM <-"2-Negativo"
plano$variables$ZM[plano$variables$f10_z_m_resultado=="Positivo/Reativo"]<-"1-Positivo"

table(plano$variables$f10_z_m_resultado,plano$variables$ZM)

#ZICA_IgG x dengue_IgG 
table(plano$variables$DG,plano$variables$ZG)
table(plano$variables$DG)
table(plano$variables$ZG)

plano$variables$ZD_IgG <-"1-F_IgG"
plano$variables$ZD_IgG[plano$variables$DG=="1-Positivo"& plano$variables$ZG=="2-Negativo"]<-"2-D_IgG"
plano$variables$ZD_IgG[plano$variables$DG=="2-Negativo"& plano$variables$ZG=="1-Positivo"]<-"3-Z_IgG"
plano$variables$ZD_IgG[plano$variables$DG=="2-Negativo"& plano$variables$ZG=="2-Negativo"]<-"4-SemF_IgG"

table(plano$variables$DG,plano$variables$ZG,plano$variables$ZD_IgG)
table(plano$variables$ZD_IgG)

#ZICA_IgM x dengue_IgM 
table(plano$variables$DM,plano$variables$ZM)
table(plano$variables$DM)
table(plano$variables$ZM)

plano$variables$ZD_IgM <-"1-F_IgM"
plano$variables$ZD_IgM[plano$variables$DM=="1-Positivo"& plano$variables$ZM=="2-Negativo"]<-"2-D_IgM"
plano$variables$ZD_IgM[plano$variables$DM=="2-Negativo"& plano$variables$ZM=="1-Positivo"]<-"3-Z_IgM"
plano$variables$ZD_IgM[plano$variables$DM=="2-Negativo"& plano$variables$ZM=="2-Negativo"]<-"4-SemF_IgM"

table(plano$variables$DM,plano$variables$ZM,plano$variables$ZD_IgM)
table(plano$variables$ZD_IgM)

#ZD_IgG x ZD_IgM 
table(plano$variables$ZD_IgG,plano$variables$ZD_IgM)


plano$variables$ZD <-"1-flavivirus"
plano$variables$ZD[plano$variables$ZD_IgG=="4-SemF_IgG"& plano$variables$ZD_IgM=="3-Z_IgM"]<-"2-Zika"
plano$variables$ZD[plano$variables$ZD_IgG=="2-D_IgG"& plano$variables$ZD_IgM=="2-D_IgM"]<-"3-Dengue"
plano$variables$ZD[plano$variables$ZD_IgG=="4-SemF_IgG"& plano$variables$ZD_IgM=="2-D_IgM"]<-"3-Dengue"
plano$variables$ZD[plano$variables$ZD_IgG=="2-D_IgG"& plano$variables$ZD_IgM=="4-SemF_IgM"]<-"3-Dengue"
plano$variables$ZD[plano$variables$ZD_IgG=="3-Z_IgG"& plano$variables$ZD_IgM=="4-SemF_IgM"]<-"2-Zika"
plano$variables$ZD[plano$variables$ZD_IgG=="4-SemF_IgG"& plano$variables$ZD_IgM=="4-SemF_IgM"]<-"4-S/Flavivirus"


table(plano$variables$ZD_IgG,plano$variables$ZD_IgM,plano$variables$ZD)

table(plano$variables$ZD)

svytable(~f08_z_g_resultado+f10_z_m_resultado+zika, plano, Ntotal=100)

# Cria chikungunya
#CG
table(plano$variables$f12_c_g_resultado)

plano$variables$CG <-"2-Negativo"
plano$variables$CG[plano$variables$f12_c_g_resultado=="Positivo/Reativo"]<-"1-Positivo"

table(plano$variables$f12_c_g_resultado,plano$variables$CG)

#CM
table(plano$variables$f14_c_m_resultado)

plano$variables$CM <-"2-Negativo"
plano$variables$CM[plano$variables$f14_c_m_resultado=="Positivo/Reativo"]<-"1-Positivo"

table(plano$variables$f14_c_m_resultado,plano$variables$CM)

#C_IgGxC_IgM

table(plano$variables$CM,plano$variables$CG)

plano$variables$ChiKu <-"2-Negativo"
plano$variables$ChiKu[plano$variables$CG=="1-Positivo"|plano$variables$CM=="1-Positivo"]<-"1-Positivo"

table(plano$variables$CG,plano$variables$CM,plano$variables$ChiKu)

# Cria Dengue
table(plano$variables$ZD)
plano$variables$dengue <-"2-Negativo"
plano$variables$dengue[plano$variables$ZD=="3-Dengue"]<-"1-Positivo"
table(plano$variables$dengue,plano$variables$ZD)

# Cria Zika
plano$variables$zika <-"2-Negativo"
plano$variables$zika[plano$variables$ZD=="2-Zika"]<-"1-Positivo"
table(plano$variables$zika,plano$variables$ZD)

# Cria Flavivirus
plano$variables$flavivirus <-"2-Negativo"
plano$variables$flavivirus[plano$variables$ZD=="1-flavivirus"]<-"1-Positivo"
table(plano$variables$flavivirus,plano$variables$ZD)

# Cria Arbovirus
table(plano$variables$ZD,plano$variables$ChiKu)
plano$variables$arbovirus <-"1-Positivo"
plano$variables$arbovirus[plano$variables$ZD=="4-S/Flavivirus"& plano$variables$ChiKu=="2-Negativo"]<-"2-Negativo"
table(plano$variables$arbovirus,plano$variables$ZD)



#Resultados

###### Tabela 1

##Dengue
table(plano$variables$dengue)
svytable(~dengue, plano, round=TRUE)
svytable(~dengue, plano, Ntotal=100)
D1=svyciprop(~I(dengue=="1-Positivo"), plano, method="me")
D7=rbind((attr(D1, "ci") *100));D7


##Zika
table(plano$variables$zika)
svytable(~zika, plano, round=TRUE)
svytable(~zika, plano, Ntotal=100)
Z1=svyciprop(~I(zika=="1-Positivo"), plano, method="me")
Z7=rbind((attr(Z1, "ci") *100));Z7

#Chikungunya
table(plano$variables$ChiKu)
svytable(~ChiKu, plano, round=TRUE)
svytable(~ChiKu, plano, Ntotal=100)
C1=svyciprop(~I(ChiKu=="1-Positivo"), plano, method="me")
C7=rbind((attr(C1, "ci") *100));C7

#fLAVIVIRUS
table(plano$variables$flavivirus)
svytable(~flavivirus, plano, round=TRUE)
svytable(~flavivirus, plano, Ntotal=100)
F1=svyciprop(~I(flavivirus=="1-Positivo"), plano, method="me")
F7=rbind((attr(F1, "ci") *100));F7

#Arbovirus
table(plano$variables$arbovirus)
svytable(~arbovirus, plano, round=TRUE)
svytable(~arbovirus, plano, Ntotal=100)
A1=svyciprop(~I(arbovirus=="S/arbov?rus"), plano, method="me")
A7=rbind((attr(A1, "ci") *100));A7


###### Intervalos de Confianca
##Dengue
table(plano$variables$dengue)
svytable(~dengue, plano, round=TRUE)
svytable(~dengue, plano, Ntotal=100)
D1=svyciprop(~I(dengue=="1-Positivo"), plano, method="me")
D2=svyciprop(~I(dengue=="2-Negativo"), plano, method="me")
D7=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));D7




## Tabela 2

plano$variables$coluna <-"coluna"

#Idade
table(plano$variables$c_idade,plano$variables$coluna)
svytable(~c_idade + coluna, plano, round=TRUE)
svytable(~c_idade + coluna, plano, Ntotal=100)
D1=svyciprop(~I(c_idade=="[0,14]"), plano, method="me")
D2=svyciprop(~I(c_idade=="(14,29]"), plano, method="me")
D3=svyciprop(~I(c_idade=="(29,59]"), plano, method="me")
D4=svyciprop(~I(c_idade=="(59,120]"), plano, method="me")
X=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100), (attr(D3, "ci") *100),(attr(D4, "ci") *100));X

D1
#sexo
table(plano$variables$c_sexo,plano$variables$coluna)
svytable(~c_sexo + coluna, plano, round=TRUE)
svytable(~c_sexo + coluna, plano, Ntotal=100)
D1=svyciprop(~I(c_sexo=="Female"), plano, method="me")
D2=svyciprop(~I(c_sexo=="Male"), plano, method="me")
X=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X

#raca
table(plano$variables$raca,plano$variables$coluna)
svytable(~raca + coluna, plano, round=TRUE)
svytable(~raca + coluna, plano, Ntotal=100)
D1=svyciprop(~I(raca=="Black"), plano, method="me")
D2=svyciprop(~I(raca=="Non-black"), plano, method="me")
X=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X

#escolaridade
table(plano$variables$esc,plano$variables$coluna)
svytable(~esc + coluna, plano, round=TRUE)
svytable(~esc + coluna, plano, Ntotal=100)
D1=svyciprop(~I(esc==" 0 - 04 anos"), plano, method="li")
D2=svyciprop(~I(esc=="05 - 09 anos"), plano, method="li")
D3=svyciprop(~I(esc=="10 - 12 anos"), plano, method="li")
D4=svyciprop(~I(esc=="13 ou + anos"), plano, method="li")
X=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100), (attr(D3, "ci") *100), (attr(D4, "ci") *100));X

#Estado civil
table(plano$variables$TP_uniao,plano$variables$coluna)
svytable(~TP_uniao + coluna, plano, round=TRUE)
svytable(~TP_uniao + coluna, plano, Ntotal=100)
D1=svyciprop(~I(TP_uniao=="Casado(a) ou uni?o est?vel"), plano, method="li")
D2=svyciprop(~I(TP_uniao=="Sem uni?o estavel"), plano, method="li")
X=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X

#Tomou vacina de febre amarela
table(plano$variables$yellow,plano$variables$coluna)
svytable(~yellow + coluna, plano, round=TRUE)
svytable(~yellow + coluna, plano, Ntotal=100)
D1=svyciprop(~I(yellow=="No"), plano, method="li")
D2=svyciprop(~I(yellow=="Yes"), plano, method="li")
X=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X

#Alguma vez "voc?/nome" j? foi diagnosticado(a) com dengue
table(plano$variables$d01_dengue,plano$variables$coluna)
svytable(~d01_dengue + coluna, plano, round=TRUE)
svytable(~d01_dengue + coluna, plano, Ntotal=100)
D1=svyciprop(~I(d01_dengue=="Sim"), plano, method="li")
D2=svyciprop(~I(d01_dengue=="N?o"), plano, method="li")
X=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X

#Alguma vez "voc?/nome" j? foi diagnosticado(a) com Zika
table(plano$variables$d05_zika,plano$variables$coluna)
svytable(~d05_zika + coluna, plano, round=TRUE)
svytable(~d05_zika + coluna, plano, Ntotal=100)
D1=svyciprop(~I(d05_zika=="Sim"), plano, method="li")
D2=svyciprop(~I(d05_zika=="N?o"), plano, method="li")
X=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X

#. Alguma vez "voc?/nome" j? foi diagnosticado(a) com Chikungunya
table(plano$variables$d09_chiku,plano$variables$coluna)
svytable(~d09_chiku + coluna, plano, round=TRUE)
svytable(~d09_chiku + coluna, plano, Ntotal=100)
D1=svyciprop(~I(d09_chiku=="Sim"), plano, method="li")
D2=svyciprop(~I(d09_chiku=="N?o"), plano, method="li")
X=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X


####################################################
########## TABELA 3


#############################DENGUE#################################
#SEXO###############################################################
####################intervalo no resultado###############################
table(plano$variables$c_sexo,plano$variables$dengue)

plano$variables$linha_DSF<-NA
plano$variables$linha_DSF[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$c_sexo== "Female"]<-"DF_1pos"
plano$variables$linha_DSF[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$c_sexo== "Female"]<-"DF_2neg"
plano$variables$linha_DSM<-NA
plano$variables$linha_DSM[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$c_sexo== "Male"] <-"DM_1pos"
plano$variables$linha_DSM[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$c_sexo== "Male"] <-"DM_2neg"
table(plano$variables$linha_DSF)
table(plano$variables$linha_DSM)
#sexo
svytable(~dengue + c_sexo, plano, round=TRUE)
svytable(~linha_DSF+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_DSF=="DF_1pos"), plano, method="li")
D2=svyciprop(~I(linha_DSF=="DF_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1
svytable(~linha_DSM+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_DSM=="DM_1pos"), plano, method="li")
D4=svyciprop(~I(linha_DSM=="DM_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#ra?a##############################################################
table(plano$variables$raca,plano$variables$dengue)

plano$variables$linha_DRB<-NA
plano$variables$linha_DRB[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$raca== "Black"]<-"DB_1pos"
plano$variables$linha_DRB[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$raca== "Black"]<-"DB_2neg"
plano$variables$linha_DRN<-NA
plano$variables$linha_DRN[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$raca== "Non-black"] <-"DN_1pos"
plano$variables$linha_DRN[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$raca== "Non-black"] <-"DN_2neg"
table(plano$variables$raca,plano$variables$dengue)
table(plano$variables$linha_DRB)
table(plano$variables$linha_DRN)
#RACA
svytable(~raca + dengue, plano, round=TRUE)
svytable(~linha_DRB+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_DRB=="DB_1pos"), plano, method="li")
D2=svyciprop(~I(linha_DRB=="DB_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1
svytable(~linha_DRN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_DRN=="DN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_DRN=="DN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#Idade##############################################################
table(plano$variables$c_idade,plano$variables$dengue)

plano$variables$linha_DI0<-NA
plano$variables$linha_DI0[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$c_idade== "[0,14]"]<-"D0_1pos"
plano$variables$linha_DI0[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$c_idade== "[0,14]"]<-"D0_2neg"
plano$variables$linha_DI14<-NA
plano$variables$linha_DI14[plano$variables$dengue=="1-Positivo"& 
                             plano$variables$c_idade== "(14,29]"]<-"D14_1pos"
plano$variables$linha_DI14[plano$variables$dengue=="2-Negativo"& 
                             plano$variables$c_idade== "(14,29]"]<-"D14_2neg"
plano$variables$linha_DI29<-NA
plano$variables$linha_DI29[plano$variables$dengue=="1-Positivo"& 
                             plano$variables$c_idade== "(29,59]"]<-"D29_1pos"
plano$variables$linha_DI29[plano$variables$dengue=="2-Negativo"& 
                             plano$variables$c_idade== "(29,59]"]<-"D29_2neg"
plano$variables$linha_DI59<-NA
plano$variables$linha_DI59[plano$variables$dengue=="1-Positivo"& 
                             plano$variables$c_idade== "(59,120]"]<-"D59_1pos"
plano$variables$linha_DI59[plano$variables$dengue=="2-Negativo"& 
                             plano$variables$c_idade== "(59,120]"]<-"D59_2neg"
table(plano$variables$c_idade,plano$variables$dengue)
table(plano$variables$linha_DI0)
table(plano$variables$linha_DI14)
table(plano$variables$linha_DI29)
table(plano$variables$linha_DI59)
#IDADE
svytable(~c_idade + dengue, plano, round=TRUE)
svytable(~linha_DI0+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_DI0=="D0_1pos"), plano, method="li")
D2=svyciprop(~I(linha_DI0=="D0_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_DI14+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_DI14=="D14_1pos"), plano, method="li")
D4=svyciprop(~I(linha_DI14=="D14_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

svytable(~linha_DI29+coluna, plano, Ntotal=100)
D5=svyciprop(~I(linha_DI29=="D29_1pos"), plano, method="li")
D6=svyciprop(~I(linha_DI29=="D29_2neg"), plano, method="li")
X3=rbind((attr(D5, "ci") *100),(attr(D6, "ci") *100));X3
svytable(~linha_DI59+coluna, plano, Ntotal=100)
D7=svyciprop(~I(linha_DI59=="D59_1pos"), plano, method="li")
D8=svyciprop(~I(linha_DI59=="D59_2neg"), plano, method="li")
X4=rbind((attr(D7, "ci") *100),(attr(D8, "ci") *100));X4

#Anos de estudo##############################################################
table(plano$variables$esc,plano$variables$dengue)

plano$variables$linha_DE0<-NA
plano$variables$linha_DE0[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$esc== " 0 - 04 anos"]<-"DE0_1pos"
plano$variables$linha_DE0[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$esc== " 0 - 04 anos"]<-"DE0_2neg"
plano$variables$linha_DE5<-NA
plano$variables$linha_DE5[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$esc== "05 - 09 anos"]<-"DE5_1pos"
plano$variables$linha_DE5[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$esc== "05 - 09 anos"]<-"DE5_2neg"
plano$variables$linha_DE10<-NA
plano$variables$linha_DE10[plano$variables$dengue=="1-Positivo"& 
                             plano$variables$esc== "10 - 12 anos"]<-"DE10_1pos"
plano$variables$linha_DE10[plano$variables$dengue=="2-Negativo"& 
                             plano$variables$esc== "10 - 12 anos"]<-"DE10_2neg"
plano$variables$linha_DE13<-NA
plano$variables$linha_DE13[plano$variables$dengue=="1-Positivo"& 
                             plano$variables$esc== "13 ou + anos"]<-"DE13_1pos"
plano$variables$linha_DE13[plano$variables$dengue=="2-Negativo"& 
                             plano$variables$esc== "13 ou + anos"]<-"DE13_2neg"

table(plano$variables$esc,plano$variables$dengue)
table(plano$variables$linha_DE0)
table(plano$variables$linha_DE5)
table(plano$variables$linha_DE10)
table(plano$variables$linha_DE13)
#esc
svytable(~esc + dengue, plano, round=TRUE)
svytable(~linha_DE0+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_DE0=="DE0_1pos"), plano, method="li")
D2=svyciprop(~I(linha_DE0=="DE0_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_DE5+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_DE5=="DE5_1pos"), plano, method="li")
D4=svyciprop(~I(linha_DE5=="DE5_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

svytable(~linha_DE10+coluna, plano, Ntotal=100)
D5=svyciprop(~I(linha_DE10=="DE10_1pos"), plano, method="li")
D6=svyciprop(~I(linha_DE10=="DE10_2neg"), plano, method="li")
X3=rbind((attr(D5, "ci") *100),(attr(D6, "ci") *100));X3

svytable(~linha_DE13+coluna, plano, Ntotal=100)
D7=svyciprop(~I(linha_DE13=="DE13_1pos"), plano, method="li")
D8=svyciprop(~I(linha_DE13=="DE13_2neg"), plano, method="li")
X4=rbind((attr(D7, "ci") *100),(attr(D8, "ci") *100));X4

#Tipo de uni?o##############################################################
table(plano$variables$TP_uniao,plano$variables$dengue)

plano$variables$linha_DUc<-NA
plano$variables$linha_DUC[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$TP_uniao== "Casado(a) ou uni?o est?vel"]<-"DC_1pos"
plano$variables$linha_DUC[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$TP_uniao== "Casado(a) ou uni?o est?vel"]<-"DC_2neg"
plano$variables$linha_DUS<-NA
plano$variables$linha_DUS[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$TP_uniao== "Sem uni?o estavel"] <-"DS_1pos"
plano$variables$linha_DUS[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$TP_uniao== "Sem uni?o estavel"] <-"DS_2neg"
table(plano$variables$TP_uniao,plano$variables$dengue)
table(plano$variables$linha_DUC)
table(plano$variables$linha_DUS)
#TP_uniao
svytable(~dengue + TP_uniao, plano, round=TRUE)
svytable(~linha_DUC+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_DUC=="DC_1pos"), plano, method="li")
D2=svyciprop(~I(linha_DUC=="DC_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_DUS+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_DUS=="DS_1pos"), plano, method="li")
D4=svyciprop(~I(linha_DUS=="DS_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#Previous yellow fever##############################################################
table(plano$variables$dengue,plano$variables$yellow)

plano$variables$linha_DYS<-NA
plano$variables$linha_DYS[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$yellow== "Yes"]<-"DS_1pos"
plano$variables$linha_DYS[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$yellow== "Yes"]<-"DS_2neg"
plano$variables$linha_DYN<-NA
plano$variables$linha_DYN[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$yellow== "No"] <-"DN_1pos"
plano$variables$linha_DYN[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$yellow== "No"] <-"DN_2neg"
table(plano$variables$yellow,plano$variables$dengue)
table(plano$variables$linha_DYS)
table(plano$variables$linha_DYN)


#yellow
svytable(~yellow + dengue, plano, round=TRUE)
svytable(~linha_DYS+coluna, plano, round=TRUE)
svytable(~linha_DYS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_DYS=="DS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_DYS=="DS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_DYN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_DYN=="DN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_DYN=="DN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2
#diagnostico de dengue##############################################################
table(plano$variables$d01_dengue,plano$variables$dengue)

plano$variables$linha_DDS<-NA
plano$variables$linha_DDS[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$d01_dengue== "Sim"]<-"DS_1pos"
plano$variables$linha_DDS[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$d01_dengue== "Sim"]<-"DS_2neg"
plano$variables$linha_DDN<-NA
plano$variables$linha_DDN[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$d01_dengue== "N?o"] <-"DN_1pos"
plano$variables$linha_DDN[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$d01_dengue== "N?o"] <-"DN_2neg"
table(plano$variables$d01_dengue,plano$variables$dengue)
table(plano$variables$linha_DDS)
table(plano$variables$linha_DDN)

#d01_dengue
svytable(~d01_dengue + dengue, plano, round=TRUE)
svytable(~linha_DDS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_DDS=="DS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_DDS=="DS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_DDN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_DDN=="DN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_DDN=="DN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#diagnostico de zika##############################################################
table(plano$variables$d05_zika,plano$variables$dengue)

plano$variables$linha_DZS<-NA
plano$variables$linha_DZS[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$d05_zika== "Sim"]<-"DS_1pos"
plano$variables$linha_DZS[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$d05_zika== "Sim"]<-"DS_2neg"
plano$variables$linha_DZN<-NA
plano$variables$linha_DZN[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$d05_zika== "N?o"] <-"DN_1pos"
plano$variables$linha_DZN[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$d05_zika== "N?o"] <-"DN_2neg"
table(plano$variables$d05_zika,plano$variables$dengue)
table(plano$variables$linha_DZS)
table(plano$variables$linha_DZN)

#d05_zika
svytable(~d05_zika + dengue, plano, round=TRUE)
svytable(~linha_DZS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_DZS=="DS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_DZS=="DS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_DZN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_DZN=="DN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_DZN=="DN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#diagnostico de chikungunya##################################################
table(plano$variables$d09_chiku,plano$variables$dengue)

plano$variables$linha_DCS<-NA
plano$variables$linha_DCS[plano$variables$dengue=="1-Positivo"& 
                            plano$variables$d09_chiku== "Sim"]<-"DS_1pos"
plano$variables$linha_DCS[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$d09_chiku== "Sim"]<-"DS_2neg"
plano$variables$linha_DCN<-NA
plano$variables$linha_DCN[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$d09_chiku== "Sim"] <-"DN_1pos"
plano$variables$linha_DCN[plano$variables$dengue=="2-Negativo"& 
                            plano$variables$d09_chiku== "N?o"] <-"DN_2neg"
table(plano$variables$d09_chiku,plano$variables$dengue)
table(plano$variables$linha_DCS)
table(plano$variables$linha_DCN)

#d09_chiku
svytable(~d09_chiku + dengue, plano, round=TRUE)
svytable(~linha_DCS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_DCS=="DS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_DCS=="DS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_DCN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_DCN=="DN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_DCN=="DN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#############################ZIKA#################################
#SEXO###############################################################
####################intervalo no resultado###############################
table(plano$variables$c_sexo,plano$variables$zika)

plano$variables$linha_ZSF<-NA
plano$variables$linha_ZSF[plano$variables$zika=="1-Positivo"& 
                            plano$variables$c_sexo== "Female"]<-"ZF_1pos"
plano$variables$linha_ZSF[plano$variables$zika=="2-Negativo"& 
                            plano$variables$c_sexo== "Female"]<-"ZF_2neg"
plano$variables$linha_ZSM<-NA
plano$variables$linha_ZSM[plano$variables$zika=="1-Positivo"& 
                            plano$variables$c_sexo== "Male"] <-"ZM_1pos"
plano$variables$linha_ZSM[plano$variables$zika=="2-Negativo"& 
                            plano$variables$c_sexo== "Male"] <-"ZM_2neg"
table(plano$variables$linha_ZSF)
table(plano$variables$linha_ZSM)
#sexo
svytable(~zika + c_sexo, plano, round=TRUE)
svytable(~linha_ZSF+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_ZSF=="ZF_1pos"), plano, method="li")
D2=svyciprop(~I(linha_ZSF=="ZF_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1
svytable(~linha_ZSM+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_ZSM=="ZM_1pos"), plano, method="li")
D4=svyciprop(~I(linha_ZSM=="ZM_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#ra?a##############################################################
table(plano$variables$raca,plano$variables$zika)

plano$variables$linha_ZRB<-NA
plano$variables$linha_ZRB[plano$variables$zika=="1-Positivo"& 
                            plano$variables$raca== "Black"]<-"ZB_1pos"
plano$variables$linha_ZRB[plano$variables$zika=="2-Negativo"& 
                            plano$variables$raca== "Black"]<-"ZB_2neg"
plano$variables$linha_ZRN<-NA
plano$variables$linha_ZRN[plano$variables$zika=="1-Positivo"& 
                            plano$variables$raca== "Non-black"] <-"ZN_1pos"
plano$variables$linha_ZRN[plano$variables$zika=="2-Negativo"& 
                            plano$variables$raca== "Non-black"] <-"ZN_2neg"
table(plano$variables$raca,plano$variables$zika)
table(plano$variables$linha_ZRB)
table(plano$variables$linha_ZRN)
#RACA
svytable(~raca + zika, plano, round=TRUE)
svytable(~linha_ZRB+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_ZRB=="ZB_1pos"), plano, method="li")
D2=svyciprop(~I(linha_ZRB=="ZB_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1
svytable(~linha_ZRN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_ZRN=="ZN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_ZRN=="ZN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#Idade##############################################################
table(plano$variables$c_idade,plano$variables$zika)

plano$variables$linha_ZI0<-NA
plano$variables$linha_ZI0[plano$variables$zika=="1-Positivo"& 
                            plano$variables$c_idade== "[0,14]"]<-"Z0_1pos"
plano$variables$linha_ZI0[plano$variables$zika=="2-Negativo"& 
                            plano$variables$c_idade== "[0,14]"]<-"Z0_2neg"
plano$variables$linha_ZI14<-NA
plano$variables$linha_ZI14[plano$variables$zika=="1-Positivo"& 
                             plano$variables$c_idade== "(14,29]"]<-"Z14_1pos"
plano$variables$linha_ZI14[plano$variables$zika=="2-Negativo"& 
                             plano$variables$c_idade== "(14,29]"]<-"Z14_2neg"
plano$variables$linha_ZI29<-NA
plano$variables$linha_ZI29[plano$variables$zika=="1-Positivo"& 
                             plano$variables$c_idade== "(29,59]"]<-"Z29_1pos"
plano$variables$linha_ZI29[plano$variables$zika=="2-Negativo"& 
                             plano$variables$c_idade== "(29,59]"]<-"Z29_2neg"
plano$variables$linha_ZI59<-NA
plano$variables$linha_ZI59[plano$variables$zika=="1-Positivo"& 
                             plano$variables$c_idade== "(59,120]"]<-"Z59_1pos"
plano$variables$linha_ZI59[plano$variables$zika=="2-Negativo"& 
                             plano$variables$c_idade== "(59,120]"]<-"Z59_2neg"
table(plano$variables$c_idade,plano$variables$zika)
table(plano$variables$linha_ZI0)
table(plano$variables$linha_ZI14)
table(plano$variables$linha_ZI29)
table(plano$variables$linha_ZI59)
#IDADE
svytable(~c_idade + zika, plano, round=TRUE)
svytable(~linha_ZI0+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_ZI0=="Z0_1pos"), plano, method="li")
D2=svyciprop(~I(linha_ZI0=="Z0_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_ZI14+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_ZI14=="Z14_1pos"), plano, method="li")
D4=svyciprop(~I(linha_ZI14=="Z14_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

svytable(~linha_ZI29+coluna, plano, Ntotal=100)
D5=svyciprop(~I(linha_ZI29=="Z29_1pos"), plano, method="li")
D6=svyciprop(~I(linha_ZI29=="Z29_2neg"), plano, method="li")
X3=rbind((attr(D5, "ci") *100),(attr(D6, "ci") *100));X3
svytable(~linha_ZI59+coluna, plano, Ntotal=100)
D7=svyciprop(~I(linha_ZI59=="Z59_1pos"), plano, method="li")
D8=svyciprop(~I(linha_ZI59=="Z59_2neg"), plano, method="li")
X4=rbind((attr(D7, "ci") *100),(attr(D8, "ci") *100));X4

#Anos de estudo##############################################################
table(plano$variables$esc,plano$variables$zika)

plano$variables$linha_ZE0<-NA
plano$variables$linha_ZE0[plano$variables$zika=="1-Positivo"& 
                            plano$variables$esc== " 0 - 04 anos"]<-"ZE0_1pos"
plano$variables$linha_ZE0[plano$variables$zika=="2-Negativo"& 
                            plano$variables$esc== " 0 - 04 anos"]<-"ZE0_2neg"
plano$variables$linha_ZE5<-NA
plano$variables$linha_ZE5[plano$variables$zika=="1-Positivo"& 
                            plano$variables$esc== "05 - 09 anos"]<-"ZE5_1pos"
plano$variables$linha_ZE5[plano$variables$zika=="2-Negativo"& 
                            plano$variables$esc== "05 - 09 anos"]<-"ZE5_2neg"
plano$variables$linha_ZE10<-NA
plano$variables$linha_ZE10[plano$variables$zika=="1-Positivo"& 
                             plano$variables$esc== "10 - 12 anos"]<-"ZE10_1pos"
plano$variables$linha_ZE10[plano$variables$zika=="2-Negativo"& 
                             plano$variables$esc== "10 - 12 anos"]<-"ZE10_2neg"
plano$variables$linha_ZE13<-NA
plano$variables$linha_ZE13[plano$variables$zika=="1-Positivo"& 
                             plano$variables$esc== "13 ou + anos"]<-"ZE13_1pos"
plano$variables$linha_ZE13[plano$variables$zika=="2-Negativo"& 
                             plano$variables$esc== "13 ou + anos"]<-"ZE13_2neg"

table(plano$variables$esc,plano$variables$zika)
table(plano$variables$linha_ZE0)
table(plano$variables$linha_ZE5)
table(plano$variables$linha_ZE10)
table(plano$variables$linha_ZE13)
#esc
svytable(~esc + zika, plano, round=TRUE)
svytable(~linha_ZE0+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_ZE0=="ZE0_1pos"), plano, method="li")
D2=svyciprop(~I(linha_ZE0=="ZE0_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_ZE5+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_ZE5=="ZE5_1pos"), plano, method="li")
D4=svyciprop(~I(linha_ZE5=="ZE5_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

svytable(~linha_ZE10+coluna, plano, Ntotal=100)
D5=svyciprop(~I(linha_ZE10=="ZE10_1pos"), plano, method="li")
D6=svyciprop(~I(linha_ZE10=="ZE10_2neg"), plano, method="li")
X3=rbind((attr(D5, "ci") *100),(attr(D6, "ci") *100));X3

svytable(~linha_ZE13+coluna, plano, Ntotal=100)
D7=svyciprop(~I(linha_ZE13=="ZE13_1pos"), plano, method="li")
D8=svyciprop(~I(linha_ZE13=="ZE13_2neg"), plano, method="li")
X4=rbind((attr(D7, "ci") *100),(attr(D8, "ci") *100));X4

#Tipo de uni?o##############################################################
table(plano$variables$TP_uniao,plano$variables$zika)

plano$variables$linha_ZUc<-NA
plano$variables$linha_ZUC[plano$variables$zika=="1-Positivo"& 
                            plano$variables$TP_uniao== "Casado(a) ou uni?o est?vel"]<-"ZC_1pos"
plano$variables$linha_ZUC[plano$variables$zika=="2-Negativo"& 
                            plano$variables$TP_uniao== "Casado(a) ou uni?o est?vel"]<-"ZC_2neg"
plano$variables$linha_ZUS<-NA
plano$variables$linha_ZUS[plano$variables$zika=="1-Positivo"& 
                            plano$variables$TP_uniao== "Sem uni?o estavel"] <-"ZS_1pos"
plano$variables$linha_ZUS[plano$variables$zika=="2-Negativo"& 
                            plano$variables$TP_uniao== "Sem uni?o estavel"] <-"ZS_2neg"
table(plano$variables$TP_uniao,plano$variables$zika)
table(plano$variables$linha_ZUC)
table(plano$variables$linha_ZUS)
#TP_uniao
svytable(~zika + TP_uniao, plano, round=TRUE)
svytable(~linha_ZUC+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_ZUC=="ZC_1pos"), plano, method="li")
D2=svyciprop(~I(linha_ZUC=="ZC_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_ZUS+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_ZUS=="ZS_1pos"), plano, method="li")
D4=svyciprop(~I(linha_ZUS=="ZS_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#Previous yellow fever##############################################################
table(plano$variables$zika,plano$variables$yellow)

plano$variables$linha_ZYS<-NA
plano$variables$linha_ZYS[plano$variables$zika=="1-Positivo"& 
                            plano$variables$yellow== "Yes"]<-"ZS_1pos"
plano$variables$linha_ZYS[plano$variables$zika=="2-Negativo"& 
                            plano$variables$yellow== "Yes"]<-"ZS_2neg"
plano$variables$linha_ZYN<-NA
plano$variables$linha_ZYN[plano$variables$zika=="1-Positivo"& 
                            plano$variables$yellow== "No"] <-"ZN_1pos"
plano$variables$linha_ZYN[plano$variables$zika=="2-Negativo"& 
                            plano$variables$yellow== "No"] <-"ZN_2neg"
table(plano$variables$yellow,plano$variables$zika)
table(plano$variables$linha_ZYS)
table(plano$variables$linha_ZYN)


#yellow
svytable(~zika + yellow, plano, round=TRUE)
svytable(~linha_ZYS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_ZYS=="ZS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_ZYS=="ZS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_ZYN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_ZYN=="ZN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_ZYN=="ZN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2
#diagnostico de dengue##############################################################
table(plano$variables$d01_dengue,plano$variables$zika)

plano$variables$linha_ZDS<-NA
plano$variables$linha_ZDS[plano$variables$zika=="1-Positivo"& 
                            plano$variables$d01_dengue== "Sim"]<-"ZS_1pos"
plano$variables$linha_ZDS[plano$variables$zika=="2-Negativo"& 
                            plano$variables$d01_dengue== "Sim"]<-"ZS_2neg"
plano$variables$linha_ZDN<-NA
plano$variables$linha_ZDN[plano$variables$zika=="1-Positivo"& 
                            plano$variables$d01_dengue== "N?o"] <-"ZN_1pos"
plano$variables$linha_ZDN[plano$variables$zika=="2-Negativo"& 
                            plano$variables$d01_dengue== "N?o"] <-"ZN_2neg"
table(plano$variables$d01_dengue,plano$variables$zika)
table(plano$variables$linha_ZDS)
table(plano$variables$linha_ZDN)

#d05_zika
svytable(~d01_dengue + zika, plano, round=TRUE)
svytable(~linha_ZDS+coluna, plano, round=TRUE)
svytable(~linha_ZDS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_ZDS=="ZS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_ZDS=="ZS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_ZDN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_ZDN=="ZN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_ZDN=="ZN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#diagnostico de zika##############################################################
table(plano$variables$d05_zika,plano$variables$zika)

plano$variables$linha_ZZS<-NA
plano$variables$linha_ZZS[plano$variables$zika=="1-Positivo"& 
                            plano$variables$d05_zika== "Sim"]<-"ZS_1pos"
plano$variables$linha_ZZS[plano$variables$zika=="2-Negativo"& 
                            plano$variables$d05_zika== "Sim"]<-"ZS_2neg"
plano$variables$linha_ZZN<-NA
plano$variables$linha_ZZN[plano$variables$zika=="1-Positivo"& 
                            plano$variables$d05_zika== "N?o"] <-"ZN_1pos"
plano$variables$linha_ZZN[plano$variables$zika=="2-Negativo"& 
                            plano$variables$d05_zika== "N?o"] <-"ZN_2neg"
table(plano$variables$d05_zika,plano$variables$zika)
table(plano$variables$linha_ZZS)
table(plano$variables$linha_ZZN)

#d05_zika
svytable(~d05_zika + zika, plano, round=TRUE)
svytable(~linha_ZZS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_ZZS=="ZS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_ZZS=="ZS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_ZZN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_ZZN=="ZN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_ZZN=="ZN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#diagnostico de chikungunya##################################################
table(plano$variables$d09_chiku,plano$variables$zika)

plano$variables$linha_ZCS<-NA
plano$variables$linha_ZCS[plano$variables$zika=="1-Positivo"& 
                            plano$variables$d09_chiku== "Sim"]<-"ZS_1pos"
plano$variables$linha_ZCS[plano$variables$zika=="2-Negativo"& 
                            plano$variables$d09_chiku== "Sim"]<-"ZS_2neg"
plano$variables$linha_ZCN<-NA
plano$variables$linha_ZCN[plano$variables$zika=="2-Negativo"& 
                            plano$variables$d09_chiku== "Sim"] <-"ZN_1pos"
plano$variables$linha_ZCN[plano$variables$zika=="2-Negativo"& 
                            plano$variables$d09_chiku== "N?o"] <-"ZN_2neg"
table(plano$variables$d09_chiku,plano$variables$zika)
table(plano$variables$linha_ZCS)
table(plano$variables$linha_ZCN)

#d09_chiku
svytable(~d09_chiku + zika, plano, round=TRUE)
svytable(~linha_ZCS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_ZCS=="ZS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_ZCS=="ZS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_ZCN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_ZCN=="ZN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_ZCN=="ZN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#############################chikungunya#################################
#SEXO###############################################################
####################intervalo no resultado###############################
table(plano$variables$c_sexo,plano$variables$ChiKu)

plano$variables$linha_CSF<-NA
plano$variables$linha_CSF[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$c_sexo== "Female"]<-"CF_1pos"
plano$variables$linha_CSF[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$c_sexo== "Female"]<-"CF_2neg"
plano$variables$linha_CSM<-NA
plano$variables$linha_CSM[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$c_sexo== "Male"] <-"CM_1pos"
plano$variables$linha_CSM[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$c_sexo== "Male"] <-"CM_2neg"
table(plano$variables$linha_CSF)
table(plano$variables$linha_CSM)
#sexo
svytable(~ChiKu + c_sexo, plano, round=TRUE)
svytable(~linha_CSF+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_CSF=="CF_1pos"), plano, method="li")
D2=svyciprop(~I(linha_CSF=="CF_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1
svytable(~linha_CSM+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_CSM=="CM_1pos"), plano, method="li")
D4=svyciprop(~I(linha_CSM=="CM_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#ra?a##############################################################
table(plano$variables$raca,plano$variables$ChiKu)

plano$variables$linha_CRB<-NA
plano$variables$linha_CRB[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$raca== "Black"]<-"CB_1pos"
plano$variables$linha_CRB[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$raca== "Black"]<-"CB_2neg"
plano$variables$linha_CRN<-NA
plano$variables$linha_CRN[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$raca== "Non-black"] <-"CN_1pos"
plano$variables$linha_CRN[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$raca== "Non-black"] <-"CN_2neg"
table(plano$variables$raca,plano$variables$ChiKu)
table(plano$variables$linha_CRB)
table(plano$variables$linha_CRN)
#RACA
svytable(~raca + ChiKu, plano, round=TRUE)
svytable(~linha_CRB+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_CRB=="CB_1pos"), plano, method="li")
D2=svyciprop(~I(linha_CRB=="CB_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1
svytable(~linha_CRN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_CRN=="CN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_CRN=="CN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#Idade##############################################################
table(plano$variables$c_idade,plano$variables$ChiKu)

plano$variables$linha_CI0<-NA
plano$variables$linha_CI0[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$c_idade== "[0,14]"]<-"C0_1pos"
plano$variables$linha_CI0[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$c_idade== "[0,14]"]<-"C0_2neg"
plano$variables$linha_CI14<-NA
plano$variables$linha_CI14[plano$variables$ChiKu=="1-Positivo"& 
                             plano$variables$c_idade== "(14,29]"]<-"C14_1pos"
plano$variables$linha_CI14[plano$variables$ChiKu=="2-Negativo"& 
                             plano$variables$c_idade== "(14,29]"]<-"C14_2neg"
plano$variables$linha_CI29<-NA
plano$variables$linha_CI29[plano$variables$ChiKu=="1-Positivo"& 
                             plano$variables$c_idade== "(29,59]"]<-"C29_1pos"
plano$variables$linha_CI29[plano$variables$ChiKu=="2-Negativo"& 
                             plano$variables$c_idade== "(29,59]"]<-"C29_2neg"
plano$variables$linha_CI59<-NA
plano$variables$linha_CI59[plano$variables$ChiKu=="1-Positivo"& 
                             plano$variables$c_idade== "(59,120]"]<-"C59_1pos"
plano$variables$linha_CI59[plano$variables$ChiKu=="2-Negativo"& 
                             plano$variables$c_idade== "(59,120]"]<-"C59_2neg"
table(plano$variables$c_idade,plano$variables$ChiKu)
table(plano$variables$linha_CI0)
table(plano$variables$linha_CI14)
table(plano$variables$linha_CI29)
table(plano$variables$linha_CI59)
#IDADE
svytable(~c_idade + ChiKu, plano, round=TRUE)
svytable(~linha_CI0+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_CI0=="C0_1pos"), plano, method="li")
D2=svyciprop(~I(linha_CI0=="C0_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_CI14+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_CI14=="C14_1pos"), plano, method="li")
D4=svyciprop(~I(linha_CI14=="C14_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

svytable(~linha_CI29+coluna, plano, Ntotal=100)
D5=svyciprop(~I(linha_CI29=="C29_1pos"), plano, method="li")
D6=svyciprop(~I(linha_CI29=="C29_2neg"), plano, method="li")
X3=rbind((attr(D5, "ci") *100),(attr(D6, "ci") *100));X3
svytable(~linha_CI59+coluna, plano, Ntotal=100)
D7=svyciprop(~I(linha_CI59=="C59_1pos"), plano, method="li")
D8=svyciprop(~I(linha_CI59=="C59_2neg"), plano, method="li")
X4=rbind((attr(D7, "ci") *100),(attr(D8, "ci") *100));X4

#Anos de estudo##############################################################
table(plano$variables$esc,plano$variables$ChiKu)

plano$variables$linha_CE0<-NA
plano$variables$linha_CE0[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$esc== " 0 - 04 anos"]<-"CE0_1pos"
plano$variables$linha_CE0[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$esc== " 0 - 04 anos"]<-"CE0_2neg"
plano$variables$linha_CE5<-NA
plano$variables$linha_CE5[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$esc== "05 - 09 anos"]<-"CE5_1pos"
plano$variables$linha_CE5[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$esc== "05 - 09 anos"]<-"CE5_2neg"
plano$variables$linha_CE10<-NA
plano$variables$linha_CE10[plano$variables$ChiKu=="1-Positivo"& 
                             plano$variables$esc== "10 - 12 anos"]<-"CE10_1pos"
plano$variables$linha_CE10[plano$variables$ChiKu=="2-Negativo"& 
                             plano$variables$esc== "10 - 12 anos"]<-"CE10_2neg"
plano$variables$linha_CE13<-NA
plano$variables$linha_CE13[plano$variables$ChiKu=="1-Positivo"& 
                             plano$variables$esc== "13 ou + anos"]<-"CE13_1pos"
plano$variables$linha_CE13[plano$variables$ChiKu=="2-Negativo"& 
                             plano$variables$esc== "13 ou + anos"]<-"CE13_2neg"

table(plano$variables$esc,plano$variables$ChiKu)
table(plano$variables$linha_CE0)
table(plano$variables$linha_CE5)
table(plano$variables$linha_CE10)
table(plano$variables$linha_CE13)
#esc
svytable(~esc + ChiKu, plano, round=TRUE)
svytable(~linha_CE0+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_CE0=="CE0_1pos"), plano, method="li")
D2=svyciprop(~I(linha_CE0=="CE0_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_CE5+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_CE5=="CE5_1pos"), plano, method="li")
D4=svyciprop(~I(linha_CE5=="CE5_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

svytable(~linha_CE10+coluna, plano, Ntotal=100)
D5=svyciprop(~I(linha_CE10=="CE10_1pos"), plano, method="li")
D6=svyciprop(~I(linha_CE10=="CE10_2neg"), plano, method="li")
X3=rbind((attr(D5, "ci") *100),(attr(D6, "ci") *100));X3

svytable(~linha_CE13+coluna, plano, Ntotal=100)
D7=svyciprop(~I(linha_CE13=="CE13_1pos"), plano, method="li")
D8=svyciprop(~I(linha_CE13=="CE13_2neg"), plano, method="li")
X4=rbind((attr(D7, "ci") *100),(attr(D8, "ci") *100));X4

#Tipo de uni?o##############################################################
table(plano$variables$TP_uniao,plano$variables$ChiKu)

plano$variables$linha_CUc<-NA
plano$variables$linha_CUC[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$TP_uniao== "Casado(a) ou uni?o est?vel"]<-"CC_1pos"
plano$variables$linha_CUC[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$TP_uniao== "Casado(a) ou uni?o est?vel"]<-"CC_2neg"
plano$variables$linha_CUS<-NA
plano$variables$linha_CUS[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$TP_uniao== "Sem uni?o estavel"] <-"CS_1pos"
plano$variables$linha_CUS[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$TP_uniao== "Sem uni?o estavel"] <-"CS_2neg"
table(plano$variables$TP_uniao,plano$variables$ChiKu)
table(plano$variables$linha_CUC)
table(plano$variables$linha_CUS)
#TP_uniao
svytable(~ChiKu + TP_uniao, plano, round=TRUE)
svytable(~linha_CUC+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_CUC=="CC_1pos"), plano, method="li")
D2=svyciprop(~I(linha_CUC=="CC_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_CUS+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_CUS=="CS_1pos"), plano, method="li")
D4=svyciprop(~I(linha_CUS=="CS_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#Previous yellow fever##############################################################
table(plano$variables$ChiKu,plano$variables$yellow)

plano$variables$linha_CYS<-NA
plano$variables$linha_CYS[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$yellow== "Yes"]<-"CS_1pos"
plano$variables$linha_CYS[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$yellow== "Yes"]<-"CS_2neg"
plano$variables$linha_CYN<-NA
plano$variables$linha_CYN[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$yellow== "No"] <-"CN_1pos"
plano$variables$linha_CYN[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$yellow== "No"] <-"CN_2neg"
table(plano$variables$yellow,plano$variables$ChiKu)
table(plano$variables$linha_CYS)
table(plano$variables$linha_CYN)


#yellow
svytable(~ChiKu + yellow, plano, round=TRUE)
svytable(~linha_CYS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_CYS=="CS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_CYS=="CS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_CYN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_CYN=="CN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_CYN=="CN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2
#diagnostico de dengue##############################################################
table(plano$variables$d01_dengue,plano$variables$ChiKu)

plano$variables$linha_CDS<-NA
plano$variables$linha_CDS[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$d01_dengue== "Sim"]<-"CS_1pos"
plano$variables$linha_CDS[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$d01_dengue== "Sim"]<-"CS_2neg"
plano$variables$linha_CDN<-NA
plano$variables$linha_CDN[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$d01_dengue== "N?o"] <-"CN_1pos"
plano$variables$linha_CDN[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$d01_dengue== "N?o"] <-"CN_2neg"
table(plano$variables$d01_dengue,plano$variables$ChiKu)
table(plano$variables$linha_CDS)
table(plano$variables$linha_CDN)

#d01_dengue
svytable(~d01_dengue + ChiKu, plano, round=TRUE)
svytable(~linha_CDS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_CDS=="CS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_CDS=="CS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_CDN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_CDN=="CN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_CDN=="CN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#diagnostico de zika##############################################################
table(plano$variables$d05_zika,plano$variables$ChiKu)

plano$variables$linha_CZS<-NA
plano$variables$linha_CZS[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$d05_zika== "Sim"]<-"CS_1pos"
plano$variables$linha_CZS[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$d05_zika== "Sim"]<-"CS_2neg"
plano$variables$linha_CZN<-NA
plano$variables$linha_CZN[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$d05_zika== "N?o"] <-"CN_1pos"
plano$variables$linha_CZN[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$d05_zika== "N?o"] <-"CN_2neg"
table(plano$variables$d05_zika,plano$variables$ChiKu)
table(plano$variables$linha_CZS)
table(plano$variables$linha_CZN)

#d05_zika
svytable(~d05_zika + ChiKu, plano, round=TRUE)
svytable(~linha_CZS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_CZS=="CS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_CZS=="CS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_CZN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_CZN=="CN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_CZN=="CN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#diagnostico de chikungunya##################################################
table(plano$variables$d09_chiku,plano$variables$ChiKu)

plano$variables$linha_CCS<-NA
plano$variables$linha_CCS[plano$variables$ChiKu=="1-Positivo"& 
                            plano$variables$d09_chiku== "Sim"]<-"CS_1pos"
plano$variables$linha_CCS[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$d09_chiku== "Sim"]<-"CS_2neg"
plano$variables$linha_CCN<-NA
plano$variables$linha_CCN[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$d09_chiku== "Sim"] <-"CN_1pos"
plano$variables$linha_CCN[plano$variables$ChiKu=="2-Negativo"& 
                            plano$variables$d09_chiku== "N?o"] <-"CN_2neg"
table(plano$variables$d09_chiku,plano$variables$ChiKu)
table(plano$variables$linha_CCS)
table(plano$variables$linha_CCN)

#d09_chiku
svytable(~d09_chiku + ChiKu, plano, round=TRUE)
svytable(~linha_CCS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_CCS=="CS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_CCS=="CS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_CCN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_CCN=="CN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_CCN=="CN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2
#############################Flavivirus#################################
#SEXO###############################################################
####################intervalo no resultado###############################
table(plano$variables$c_sexo,plano$variables$flavivirus)

plano$variables$linha_FSF<-NA
plano$variables$linha_FSF[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$c_sexo== "Female"]<-"FF_1pos"
plano$variables$linha_FSF[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$c_sexo== "Female"]<-"FF_2neg"
plano$variables$linha_FSM<-NA
plano$variables$linha_FSM[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$c_sexo== "Male"] <-"FM_1pos"
plano$variables$linha_FSM[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$c_sexo== "Male"] <-"FM_2neg"
table(plano$variables$linha_FSF)
table(plano$variables$linha_FSM)
#sexo
svytable(~flavivirus + c_sexo, plano, round=TRUE)
svytable(~linha_FSF+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_FSF=="FF_1pos"), plano, method="li")
D2=svyciprop(~I(linha_FSF=="FF_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1
svytable(~linha_FSM+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_FSM=="FM_1pos"), plano, method="li")
D4=svyciprop(~I(linha_FSM=="FM_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#ra?a##############################################################
table(plano$variables$raca,plano$variables$flavivirus)

plano$variables$linha_FRB<-NA
plano$variables$linha_FRB[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$raca== "Black"]<-"FB_1pos"
plano$variables$linha_FRB[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$raca== "Black"]<-"FB_2neg"
plano$variables$linha_FRN<-NA
plano$variables$linha_FRN[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$raca== "Non-black"] <-"FN_1pos"
plano$variables$linha_FRN[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$raca== "Non-black"] <-"FN_2neg"
table(plano$variables$raca,plano$variables$flavivirus)
table(plano$variables$linha_FRB)
table(plano$variables$linha_FRN)
#RACA
svytable(~raca + flavivirus, plano, round=TRUE)
svytable(~linha_FRB+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_FRB=="FB_1pos"), plano, method="li")
D2=svyciprop(~I(linha_FRB=="FB_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1
svytable(~linha_FRN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_FRN=="FN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_FRN=="FN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#Idade##############################################################
table(plano$variables$c_idade,plano$variables$flavivirus)

plano$variables$linha_FI0<-NA
plano$variables$linha_FI0[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$c_idade== "[0,14]"]<-"F0_1pos"
plano$variables$linha_FI0[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$c_idade== "[0,14]"]<-"F0_2neg"
plano$variables$linha_FI14<-NA
plano$variables$linha_FI14[plano$variables$flavivirus=="1-Positivo"& 
                             plano$variables$c_idade== "(14,29]"]<-"F14_1pos"
plano$variables$linha_FI14[plano$variables$flavivirus=="2-Negativo"& 
                             plano$variables$c_idade== "(14,29]"]<-"F14_2neg"
plano$variables$linha_FI29<-NA
plano$variables$linha_FI29[plano$variables$flavivirus=="1-Positivo"& 
                             plano$variables$c_idade== "(29,59]"]<-"F29_1pos"
plano$variables$linha_FI29[plano$variables$flavivirus=="2-Negativo"& 
                             plano$variables$c_idade== "(29,59]"]<-"F29_2neg"
plano$variables$linha_FI59<-NA
plano$variables$linha_FI59[plano$variables$flavivirus=="1-Positivo"& 
                             plano$variables$c_idade== "(59,120]"]<-"F59_1pos"
plano$variables$linha_FI59[plano$variables$flavivirus=="2-Negativo"& 
                             plano$variables$c_idade== "(59,120]"]<-"F59_2neg"
table(plano$variables$c_idade,plano$variables$flavivirus)
table(plano$variables$linha_FI0)
table(plano$variables$linha_FI14)
table(plano$variables$linha_FI29)
table(plano$variables$linha_FI59)
#IDADE
svytable(~c_idade + flavivirus, plano, round=TRUE)
svytable(~linha_FI0+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_FI0=="F0_1pos"), plano, method="li")
D2=svyciprop(~I(linha_FI0=="F0_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_FI14+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_FI14=="F14_1pos"), plano, method="li")
D4=svyciprop(~I(linha_FI14=="F14_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

svytable(~linha_FI29+coluna, plano, Ntotal=100)
D5=svyciprop(~I(linha_FI29=="F29_1pos"), plano, method="li")
D6=svyciprop(~I(linha_FI29=="F29_2neg"), plano, method="li")
X3=rbind((attr(D5, "ci") *100),(attr(D6, "ci") *100));X3
svytable(~linha_FI59+coluna, plano, Ntotal=100)
D7=svyciprop(~I(linha_FI59=="F59_1pos"), plano, method="li")
D8=svyciprop(~I(linha_FI59=="F59_2neg"), plano, method="li")
X4=rbind((attr(D7, "ci") *100),(attr(D8, "ci") *100));X4

#Anos de estudo##############################################################
table(plano$variables$esc,plano$variables$flavivirus)

plano$variables$linha_FE0<-NA
plano$variables$linha_FE0[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$esc== " 0 - 04 anos"]<-"FE0_1pos"
plano$variables$linha_FE0[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$esc== " 0 - 04 anos"]<-"FE0_2neg"
plano$variables$linha_FE5<-NA
plano$variables$linha_FE5[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$esc== "05 - 09 anos"]<-"FE5_1pos"
plano$variables$linha_FE5[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$esc== "05 - 09 anos"]<-"FE5_2neg"
plano$variables$linha_FE10<-NA
plano$variables$linha_FE10[plano$variables$flavivirus=="1-Positivo"& 
                             plano$variables$esc== "10 - 12 anos"]<-"FE10_1pos"
plano$variables$linha_FE10[plano$variables$flavivirus=="2-Negativo"& 
                             plano$variables$esc== "10 - 12 anos"]<-"FE10_2neg"
plano$variables$linha_FE13<-NA
plano$variables$linha_FE13[plano$variables$flavivirus=="1-Positivo"& 
                             plano$variables$esc== "13 ou + anos"]<-"FE13_1pos"
plano$variables$linha_FE13[plano$variables$flavivirus=="2-Negativo"& 
                             plano$variables$esc== "13 ou + anos"]<-"FE13_2neg"

table(plano$variables$esc,plano$variables$flavivirus)
table(plano$variables$linha_FE0)
table(plano$variables$linha_FE5)
table(plano$variables$linha_FE10)
table(plano$variables$linha_FE13)
#esc
svytable(~esc + flavivirus, plano, round=TRUE)
svytable(~linha_FE0+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_FE0=="FE0_1pos"), plano, method="li")
D2=svyciprop(~I(linha_FE0=="FE0_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_FE5+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_FE5=="FE5_1pos"), plano, method="li")
D4=svyciprop(~I(linha_FE5=="FE5_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

svytable(~linha_FE10+coluna, plano, Ntotal=100)
D5=svyciprop(~I(linha_FE10=="FE10_1pos"), plano, method="li")
D6=svyciprop(~I(linha_FE10=="FE10_2neg"), plano, method="li")
X3=rbind((attr(D5, "ci") *100),(attr(D6, "ci") *100));X3

svytable(~linha_FE13+coluna, plano, Ntotal=100)
D7=svyciprop(~I(linha_FE13=="FE13_1pos"), plano, method="li")
D8=svyciprop(~I(linha_FE13=="FE13_2neg"), plano, method="li")
X4=rbind((attr(D7, "ci") *100),(attr(D8, "ci") *100));X4

#Tipo de uni?o##############################################################
table(plano$variables$TP_uniao,plano$variables$flavivirus)

plano$variables$linha_FUc<-NA
plano$variables$linha_FUC[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$TP_uniao== "Casado(a) ou uni?o est?vel"]<-"FC_1pos"
plano$variables$linha_FUC[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$TP_uniao== "Casado(a) ou uni?o est?vel"]<-"FC_2neg"
plano$variables$linha_FUS<-NA
plano$variables$linha_FUS[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$TP_uniao== "Sem uni?o estavel"] <-"FS_1pos"
plano$variables$linha_FUS[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$TP_uniao== "Sem uni?o estavel"] <-"FS_2neg"
table(plano$variables$TP_uniao,plano$variables$flavivirus)
table(plano$variables$linha_FUC)
table(plano$variables$linha_FUS)
#TP_uniao
svytable(~flavivirus + TP_uniao, plano, round=TRUE)
svytable(~linha_FUC+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_FUC=="FC_1pos"), plano, method="li")
D2=svyciprop(~I(linha_FUC=="FC_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_FUS+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_FUS=="FS_1pos"), plano, method="li")
D4=svyciprop(~I(linha_FUS=="FS_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#Previous yellow fever##############################################################
table(plano$variables$flavivirus,plano$variables$yellow)

plano$variables$linha_FYS<-NA
plano$variables$linha_FYS[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$yellow== "Yes"]<-"FS_1pos"
plano$variables$linha_FYS[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$yellow== "Yes"]<-"FS_2neg"
plano$variables$linha_FYN<-NA
plano$variables$linha_FYN[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$yellow== "No"] <-"FN_1pos"
plano$variables$linha_FYN[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$yellow== "No"] <-"FN_2neg"
table(plano$variables$yellow,plano$variables$flavivirus)
table(plano$variables$linha_FYS)
table(plano$variables$linha_FYN)


#yellow
svytable(~flavivirus + yellow, plano, round=TRUE)
svytable(~linha_FYS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_FYS=="FS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_FYS=="FS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_FYN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_FYN=="FN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_FYN=="FN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2
#diagnostico de dengue##############################################################
table(plano$variables$d01_dengue,plano$variables$flavivirus)

plano$variables$linha_FDS<-NA
plano$variables$linha_FDS[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$d01_dengue== "Sim"]<-"FS_1pos"
plano$variables$linha_FDS[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$d01_dengue== "Sim"]<-"FS_2neg"
plano$variables$linha_FDN<-NA
plano$variables$linha_FDN[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$d01_dengue== "N?o"] <-"FN_1pos"
plano$variables$linha_FDN[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$d01_dengue== "N?o"] <-"FN_2neg"
table(plano$variables$d01_flavivirus,plano$variables$flavivirus)
table(plano$variables$linha_FDS)
table(plano$variables$linha_FDN)

#d01_dengue
svytable(~d01_dengue + flavivirus, plano, round=TRUE)
svytable(~linha_FDS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_FDS=="FS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_FDS=="FS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_FDN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_FDN=="FN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_FDN=="FN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#diagnostico de zika##############################################################
table(plano$variables$d05_zika,plano$variables$flavivirus)

plano$variables$linha_FZS<-NA
plano$variables$linha_FZS[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$d05_zika== "Sim"]<-"FS_1pos"
plano$variables$linha_FZS[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$d05_zika== "Sim"]<-"FS_2neg"
plano$variables$linha_FZN<-NA
plano$variables$linha_FZN[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$d05_zika== "N?o"] <-"FN_1pos"
plano$variables$linha_FZN[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$d05_zika== "N?o"] <-"FN_2neg"
table(plano$variables$d05_zika,plano$variables$flavivirus)
table(plano$variables$linha_FZS)
table(plano$variables$linha_FZN)

#d05_zika
svytable(~d05_zika + flavivirus, plano, round=TRUE)
svytable(~linha_FZS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_FZS=="FS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_FZS=="FS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_FZN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_FZN=="FN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_FZN=="FN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#diagnostico de chikungunya##################################################
table(plano$variables$d09_chiku,plano$variables$flavivirus)

plano$variables$linha_FCS<-NA
plano$variables$linha_FCS[plano$variables$flavivirus=="1-Positivo"& 
                            plano$variables$d09_chiku== "Sim"]<-"FS_1pos"
plano$variables$linha_FCS[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$d09_chiku== "Sim"]<-"FS_2neg"
plano$variables$linha_FCN<-NA
plano$variables$linha_FCN[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$d09_chiku== "Sim"] <-"FN_1pos"
plano$variables$linha_FCN[plano$variables$flavivirus=="2-Negativo"& 
                            plano$variables$d09_chiku== "N?o"] <-"FN_2neg"
table(plano$variables$d09_flavivirus,plano$variables$flavivirus)
table(plano$variables$linha_FCS)
table(plano$variables$linha_FCN)

#d09_chiku
svytable(~d09_chiku + flavivirus, plano, round=TRUE)
svytable(~linha_FCS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_FCS=="FS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_FCS=="FS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_FCN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_FCN=="FN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_FCN=="FN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2



#############################No arbovirus#################################
#SEXO###############################################################
####################intervalo no resultado###############################
table(plano$variables$c_sexo,plano$variables$arbovirus)

plano$variables$linha_ASF<-NA
plano$variables$linha_ASF[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$c_sexo== "Female"]<-"AF_1pos"
plano$variables$linha_ASF[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$c_sexo== "Female"]<-"AF_2neg"
plano$variables$linha_ASM<-NA
plano$variables$linha_ASM[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$c_sexo== "Male"] <-"AM_1pos"
plano$variables$linha_ASM[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$c_sexo== "Male"] <-"AM_2neg"
table(plano$variables$linha_ASF)
table(plano$variables$linha_ASM)
#sexo
svytable(~arbovirus + c_sexo, plano, round=TRUE)
svytable(~linha_ASF+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_ASF=="AF_1pos"), plano, method="li")
D2=svyciprop(~I(linha_ASF=="AF_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1
svytable(~linha_ASM+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_ASM=="AM_1pos"), plano, method="li")
D4=svyciprop(~I(linha_ASM=="AM_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#ra?a##############################################################
table(plano$variables$raca,plano$variables$arbovirus)

plano$variables$linha_ARB<-NA
plano$variables$linha_ARB[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$raca== "Black"]<-"AB_1pos"
plano$variables$linha_ARB[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$raca== "Black"]<-"AB_2neg"
plano$variables$linha_ARN<-NA
plano$variables$linha_ARN[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$raca== "Non-black"] <-"AN_1pos"
plano$variables$linha_ARN[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$raca== "Non-black"] <-"AN_2neg"
table(plano$variables$raca,plano$variables$arbovirus)
table(plano$variables$linha_ARB)
table(plano$variables$linha_ARN)
#RACA
svytable(~raca + arbovirus, plano, round=TRUE)
svytable(~linha_ARB+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_ARB=="AB_1pos"), plano, method="li")
D2=svyciprop(~I(linha_ARB=="AB_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1
svytable(~linha_ARN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_ARN=="AN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_ARN=="AN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#Idade##############################################################
table(plano$variables$c_idade,plano$variables$arbovirus)

plano$variables$linha_AI0<-NA
plano$variables$linha_AI0[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$c_idade== "[0,14]"]<-"A0_1pos"
plano$variables$linha_AI0[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$c_idade== "[0,14]"]<-"A0_2neg"
plano$variables$linha_AI14<-NA
plano$variables$linha_AI14[plano$variables$arbovirus=="C/arbov?rus"& 
                             plano$variables$c_idade== "(14,29]"]<-"A14_1pos"
plano$variables$linha_AI14[plano$variables$arbovirus=="S/arbov?rus"& 
                             plano$variables$c_idade== "(14,29]"]<-"A14_2neg"
plano$variables$linha_AI29<-NA
plano$variables$linha_AI29[plano$variables$arbovirus=="C/arbov?rus"& 
                             plano$variables$c_idade== "(29,59]"]<-"A29_1pos"
plano$variables$linha_AI29[plano$variables$arbovirus=="S/arbov?rus"& 
                             plano$variables$c_idade== "(29,59]"]<-"A29_2neg"
plano$variables$linha_AI59<-NA
plano$variables$linha_AI59[plano$variables$arbovirus=="C/arbov?rus"& 
                             plano$variables$c_idade== "(59,120]"]<-"A59_1pos"
plano$variables$linha_AI59[plano$variables$arbovirus=="S/arbov?rus"& 
                             plano$variables$c_idade== "(59,120]"]<-"A59_2neg"
table(plano$variables$c_idade,plano$variables$arbovirus)
table(plano$variables$linha_AI0)
table(plano$variables$linha_AI14)
table(plano$variables$linha_AI29)
table(plano$variables$linha_AI59)
#IDADE
svytable(~c_idade + arbovirus, plano, round=TRUE)
svytable(~linha_AI0+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_AI0=="A0_1pos"), plano, method="li")
D2=svyciprop(~I(linha_AI0=="A0_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_AI14+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_AI14=="A14_1pos"), plano, method="li")
D4=svyciprop(~I(linha_AI14=="A14_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

svytable(~linha_AI29+coluna, plano, Ntotal=100)
D5=svyciprop(~I(linha_AI29=="A29_1pos"), plano, method="li")
D6=svyciprop(~I(linha_AI29=="A29_2neg"), plano, method="li")
X3=rbind((attr(D5, "ci") *100),(attr(D6, "ci") *100));X3
svytable(~linha_AI59+coluna, plano, Ntotal=100)
D7=svyciprop(~I(linha_AI59=="A59_1pos"), plano, method="li")
D8=svyciprop(~I(linha_AI59=="A59_2neg"), plano, method="li")
X4=rbind((attr(D7, "ci") *100),(attr(D8, "ci") *100));X4

#Anos de estudo##############################################################
table(plano$variables$esc,plano$variables$arbovirus)

plano$variables$linha_AE0<-NA
plano$variables$linha_AE0[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$esc== " 0 - 04 anos"]<-"AE0_1pos"
plano$variables$linha_AE0[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$esc== " 0 - 04 anos"]<-"AE0_2neg"
plano$variables$linha_AE5<-NA
plano$variables$linha_AE5[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$esc== "05 - 09 anos"]<-"AE5_1pos"
plano$variables$linha_AE5[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$esc== "05 - 09 anos"]<-"AE5_2neg"
plano$variables$linha_AE10<-NA
plano$variables$linha_AE10[plano$variables$arbovirus=="C/arbov?rus"& 
                             plano$variables$esc== "10 - 12 anos"]<-"AE10_1pos"
plano$variables$linha_AE10[plano$variables$arbovirus=="S/arbov?rus"& 
                             plano$variables$esc== "10 - 12 anos"]<-"AE10_2neg"
plano$variables$linha_AE13<-NA
plano$variables$linha_AE13[plano$variables$arbovirus=="C/arbov?rus"& 
                             plano$variables$esc== "13 ou + anos"]<-"AE13_1pos"
plano$variables$linha_AE13[plano$variables$arbovirus=="S/arbov?rus"& 
                             plano$variables$esc== "13 ou + anos"]<-"AE13_2neg"

table(plano$variables$esc,plano$variables$arbovirus)
table(plano$variables$linha_AE0)
table(plano$variables$linha_AE5)
table(plano$variables$linha_AE10)
table(plano$variables$linha_AE13)
#esc
svytable(~esc + arbovirus, plano, round=TRUE)
svytable(~linha_AE0+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_AE0=="AE0_1pos"), plano, method="li")
D2=svyciprop(~I(linha_AE0=="AE0_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_AE5+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_AE5=="AE5_1pos"), plano, method="li")
D4=svyciprop(~I(linha_AE5=="AE5_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

svytable(~linha_AE10+coluna, plano, Ntotal=100)
D5=svyciprop(~I(linha_AE10=="AE10_1pos"), plano, method="li")
D6=svyciprop(~I(linha_AE10=="AE10_2neg"), plano, method="li")
X3=rbind((attr(D5, "ci") *100),(attr(D6, "ci") *100));X3

svytable(~linha_AE13+coluna, plano, Ntotal=100)
D7=svyciprop(~I(linha_AE13=="AE13_1pos"), plano, method="li")
D8=svyciprop(~I(linha_AE13=="AE13_2neg"), plano, method="li")
X4=rbind((attr(D7, "ci") *100),(attr(D8, "ci") *100));X4

#Tipo de uni?o##############################################################
table(plano$variables$TP_uniao,plano$variables$arbovirus)

plano$variables$linha_AUc<-NA
plano$variables$linha_AUC[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$TP_uniao== "Casado(a) ou uni?o est?vel"]<-"AC_1pos"
plano$variables$linha_AUC[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$TP_uniao== "Casado(a) ou uni?o est?vel"]<-"AC_2neg"
plano$variables$linha_AUS<-NA
plano$variables$linha_AUS[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$TP_uniao== "Sem uni?o estavel"] <-"AS_1pos"
plano$variables$linha_AUS[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$TP_uniao== "Sem uni?o estavel"] <-"AS_2neg"
table(plano$variables$TP_uniao,plano$variables$arbovirus)
table(plano$variables$linha_AUC)
table(plano$variables$linha_AUS)
#TP_uniao
svytable(~arbovirus + TP_uniao, plano, round=TRUE)
svytable(~linha_AUC+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_AUC=="AC_1pos"), plano, method="li")
D2=svyciprop(~I(linha_AUC=="AC_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_AUS+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_AUS=="AS_1pos"), plano, method="li")
D4=svyciprop(~I(linha_AUS=="AS_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#Previous yellow fever##############################################################
table(plano$variables$arbovirus,plano$variables$yellow)

plano$variables$linha_AYS<-NA
plano$variables$linha_AYS[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$yellow== "Yes"]<-"AS_1pos"
plano$variables$linha_AYS[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$yellow== "Yes"]<-"AS_2neg"
plano$variables$linha_AYN<-NA
plano$variables$linha_AYN[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$yellow== "No"] <-"AN_1pos"
plano$variables$linha_AYN[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$yellow== "No"] <-"AN_2neg"
table(plano$variables$yellow,plano$variables$arbovirus)
table(plano$variables$linha_AYS)
table(plano$variables$linha_AYN)


#yellow
svytable(~arbovirus + yellow, plano, round=TRUE)
svytable(~linha_AYS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_AYS=="AS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_AYS=="AS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_AYN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_AYN=="AN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_AYN=="AN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#diagnostico de dengue##############################################################
table(plano$variables$d01_dengue,plano$variables$arbovirus)

plano$variables$linha_ADS<-NA
plano$variables$linha_ADS[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$d01_dengue== "Sim"]<-"AS_1pos"
plano$variables$linha_ADS[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$d01_dengue== "Sim"]<-"AS_2neg"
plano$variables$linha_ADN<-NA
plano$variables$linha_ADN[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$d01_dengue== "N?o"] <-"AN_1pos"
plano$variables$linha_ADN[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$d01_dengue== "N?o"] <-"AN_2neg"
table(plano$variables$d01_arbovirus,plano$variables$arbovirus)
table(plano$variables$linha_ADS)
table(plano$variables$linha_ADN)

#d01_dengue
svytable(~d01_dengue + arbovirus, plano, round=TRUE)
svytable(~linha_ADS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_ADS=="AS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_ADS=="AS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_ADN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_ADN=="AN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_ADN=="AN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#diagnostico de zika##############################################################
table(plano$variables$d05_zika,plano$variables$arbovirus)

plano$variables$linha_AZS<-NA
plano$variables$linha_AZS[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$d05_zika== "Sim"]<-"AS_1pos"
plano$variables$linha_AZS[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$d05_zika== "Sim"]<-"AS_2neg"
plano$variables$linha_AZN<-NA
plano$variables$linha_AZN[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$d05_zika== "N?o"] <-"AN_1pos"
plano$variables$linha_AZN[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$d05_zika== "N?o"] <-"AN_2neg"
table(plano$variables$d05_zika,plano$variables$arbovirus)
table(plano$variables$linha_AZS)
table(plano$variables$linha_AZN)

#d05_zika
svytable(~d05_zika + arbovirus, plano, round=TRUE)
svytable(~linha_AZS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_AZS=="AS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_AZS=="AS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_AZN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_AZN=="AN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_AZN=="AN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2

#diagnostico de chikungunya##################################################
table(plano$variables$d09_chiku,plano$variables$arbovirus)

plano$variables$linha_ACS<-NA
plano$variables$linha_ACS[plano$variables$arbovirus=="C/arbov?rus"& 
                            plano$variables$d09_chiku== "Sim"]<-"AS_1pos"
plano$variables$linha_ACS[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$d09_chiku== "Sim"]<-"AS_2neg"
plano$variables$linha_ACN<-NA
plano$variables$linha_ACN[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$d09_chiku== "Sim"] <-"AN_1pos"
plano$variables$linha_ACN[plano$variables$arbovirus=="S/arbov?rus"& 
                            plano$variables$d09_chiku== "N?o"] <-"AN_2neg"
table(plano$variables$d09_chiku,plano$variables$arbovirus)
table(plano$variables$linha_ACS)
table(plano$variables$linha_ACN)

#d09_chiku
svytable(~d09_chiku + arbovirus, plano, round=TRUE)
svytable(~linha_ACS+coluna, plano, Ntotal=100)
D1=svyciprop(~I(linha_ACS=="AS_1pos"), plano, method="li")
D2=svyciprop(~I(linha_ACS=="AS_2neg"), plano, method="li")
X1=rbind((attr(D1, "ci") *100),(attr(D2, "ci") *100));X1

svytable(~linha_ACN+coluna, plano, Ntotal=100)
D3=svyciprop(~I(linha_ACN=="AN_1pos"), plano, method="li")
D4=svyciprop(~I(linha_ACN=="AN_2neg"), plano, method="li")
X2=rbind((attr(D3, "ci") *100),(attr(D4, "ci") *100));X2
#######################################################################################
#######################################################################################





