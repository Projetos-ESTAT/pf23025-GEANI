source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

#SEMPRE FAZER ANTES DE COMEÇAR JUNTO CO AS CORES
library(tidyverse)
library(scales)
library(gridExtra)

#Fazer esse processo de setar o diterorio quando começar
# getwd()
# [1] "C:/Users/Cliente/Desktop/Projetos - ESTAT/PF23025 - GEANI/pf23025-GEANI"
# > setwd("C:/Users/Cliente/Desktop/Projetos - ESTAT/PF23025 - GEANI/pf23025-GEANI/rdocs")

#----------------------------Cores ESTAT-------------------------------
cores_estat <-
  c('#A11D21','#003366','#CC9900','#663333','#FF6600','#CC9966','#999966','#006606','#008091','#041835','#666666')

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}



#---------------Criando objeto com as variáveis que vamos trabalhar---------

#Pegar cor raça do Plano rds(pq no banco dela está branco e não branco e ela quer negro e não negro)

cor<-data.frame(plano[["variables"]][["b06_cor"]], plano[["variables"]][["CHIK_PRNT20"]])

colnames(cor)[1] <- "Cor/Raça"
colnames(cor)[2] <- "Chikungunya"

#Mundando o campo cor para Não negros: brancos e amarelos ; Negros: pretos, pardos, indigenas

cor$`Cor/Raça` <- ifelse(
  cor$`Cor/Raça` %in% c("Branca", "Amarela"),
  "Não negros",
  "Negros"
)

#Mundando os nomes dos campos NEG E POS

cor$Chikungunya <- factor(cor$Chikungunya,
                                 levels = c('NEG', 'POS'),
                                 labels = c('Negativo', 'Positivo')
)   

#Criando um objeto com as variaveis idade, sexo, cor, vacina, teste rapido dengue 
banco_dengue_tr <- sangue %>% 
  dplyr:: select(B05_IDADE, 
                 B04_SEXO, 
                 D13_FEBRE_VACINA, 
                 F03_D_G_VALOR_C, 
                 F07_Z_G_VALOR_C, 
                 DENGUE_PRNT20,
                 ZIK_PRNT20_2
  )

#Junta os bandos um do lado do outro
banco_dengue_tr <- bind_cols(banco_dengue_tr, cor)

#----------------------------PAINEL-------------------------------------

#Organizando as Idades de acordo com os grupos da tabela

#Definindo os limites das classes
limites_classes_idades <- c(0,11,18,29,59,Inf)

#agrupar as idades nas classes
banco_dengue_tr$B05_IDADE <- cut(banco_dengue_tr$B05_IDADE, breaks = limites_classes_idades, labels = c("0-11", "12-18", "19-29", "30-59", ">=60"))

#Gráficos

caminho_painel <- "C:/Users/Cliente/Desktop/Projetos - ESTAT/pf23025-GEANI/resultados/Lucas/Painel"

#Idade

idade <- banco_dengue_tr %>%
  filter(!is.na(B05_IDADE)) %>%
  count(B05_IDADE) %>%
  mutate(
    freq = round((n / sum(n)),4)*100 ,
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

grafico_idade <- ggplot(idade) +
  aes(x = B05_IDADE, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 2
  ) + 
  scale_y_continuous(breaks = seq(from = 0, to = 1250, by = 250), limits=c(0, 1250))+
  labs(x = "Grupo Etário (Anos Completos)", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_painel, "colunas-uni-freq-idade.pdf"), width = 158, height = 93, units = "mm")

# Sexo

sexo <- banco_dengue_tr %>%
  filter(!is.na(B04_SEXO)) %>%
  count(B04_SEXO) %>%
  mutate(
    freq = round((n / sum(n)),4)*100 ,
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

grafico_sexo <- ggplot(sexo) +
  aes(x = fct_reorder(B04_SEXO, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 2.5
  ) + 
  scale_y_continuous(breaks = seq(from = 0, to = 1250, by = 250), limits=c(0, 1270))+
  labs(x = "Sexo", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_painel,"colunas-uni-freq-sexo.pdf"), width = 158, height = 93, units = "mm")

#Cor 
cor_raca <- banco_dengue_tr %>%
  filter(!is.na(`Cor/Raça`)) %>%
  count(`Cor/Raça`) %>%
  mutate(
    freq = round((n / sum(n)),4)*100 ,
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

grafico_cor <- ggplot(cor_raca) +
  aes(x = fct_reorder(`Cor/Raça`, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 2.5
  ) + 
  scale_y_continuous(breaks = seq(from = 0, to = 1250, by = 250), limits=c(0, 1250))+
  labs(x = "Cor/Raça", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_painel,"colunas-uni-freq-cor.pdf"), width = 158, height = 93, units = "mm")


#Vacina
vacina <- banco_dengue_tr %>%
  filter(!is.na(D13_FEBRE_VACINA)) %>%
  count(D13_FEBRE_VACINA) %>%
  mutate(
    freq = round((n / sum(n)),4)*100 ,
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

grafico_vacina <- ggplot(vacina) +
  aes(x = fct_reorder(D13_FEBRE_VACINA, n, .desc=T), y = n, label = label )+
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 2.5
  ) + 
  scale_y_continuous(breaks = seq(from = 0, to = 1250, by = 250), limits=c(0, 1250))+
  labs(x = "Vacinação Prévia Febre Amarela", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_painel,"colunas-uni-freq-vacina.pdf"), width = 158, height = 93, units = "mm")


#Criando o Painel

ggarrange(grafico_sexo,
          grafico_idade,
          grafico_cor,
          grafico_vacina,
          ncol = 2, nrow = 2)
ggsave(filename = file.path(caminho_painel,"painel-sexo-idade-cor-vacina.pdf"), width = 158, height = 158, units = "mm")




#---------------------Explotarória CHIK_PRNT20--------------------------------

#Contabilizando todas as linhas que possuem NA na coluna CHIK_PRNT20 = 1384
sum(is.na(banco_dengue_tr$CHIK_PRNT20))

chik_idade <- banco_dengue_tr %>% 
  dplyr::select(B05_IDADE, CHIK_PRNT20) %>% 
  filter(!is.na(CHIK_PRNT20)) %>% 
  group_by(B05_IDADE,CHIK_PRNT20) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round((freq / sum(freq)),4)*100
  )

sum(chik_idade$freq_relativa)

porcentagens <- str_c(chik_idade$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(chik_idade$freq, " (", porcentagens, ")"))

                    
caminho_pct_chik <- "C:/Users/Cliente/Desktop/Projetos - ESTAT/pf23025-GEANI/resultados/Lucas/Pacote Chikungunya"

ggplot(chik_idade) +
  aes(
    x = B05_IDADE, y = freq,
    fill =`Chikungunya`, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.4,
    size = 2.5
  ) +
  labs(x = "Grupo Etário (Anos Completos)", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_pct_chik,"colunas-bi-freq-idadechik.pdf"), width = 158, height = 93, units = "mm")







#-----------------------Exploratória Dengue----------------------

#Organizando as Idades de acordo com os grupos da tabela

#Definindo os limites das classes
limites_classes_idades <- c(0,11,18,29,59,Inf)

#agrupar as idades nas classes
banco_dengue_tr$B05_IDADE <- cut(banco_dengue_tr$B05_IDADE, breaks = limites_classes_idades, labels = c("0-11", "12-18", "19-29", "30-59", ">=60"))


#Definindo banco com a variável que vamos utilizar 

banco_dengue <- banco_dengue_tr %>% 
  select(B05_IDADE,B04_SEXO,D13_FEBRE_VACINA,`Cor/Raça`,F03_D_G_VALOR_C) 


banco_dengue$F03_D_G_VALOR_C <- as.factor(banco_dengue$F03_D_G_VALOR_C)
banco_dengue$B05_IDADE <- as.factor(banco_dengue$B05_IDADE)
banco_dengue$B04_SEXO <- as.factor(banco_dengue$B04_SEXO)
banco_dengue$D13_FEBRE_VACINA <- as.factor(banco_dengue$D13_FEBRE_VACINA)
banco_dengue$`Cor/Raça` <- as.factor(banco_dengue$`Cor/Raça`)

colnames(banco_dengue)  <- c("idade","sexo","Vacina","Cor", "Teste Rápido Dengue")


#Gráficos

caminho_dengue <- "C:/Users/Cliente/Desktop/Projetos - ESTAT/pf23025-GEANI/resultados/Lucas/Pacote Dengue"

#Idade

bancoteste <- banco_dengue %>% 
  select(Vacina,`Teste Rápido Dengue`)

sum(is.na(banco_dengue$`Teste Rápido Dengue`))
sum(is.na(banco_dengue$Cor))


idade_dengue <- banco_dengue %>%
  select(idade, `Teste Rápido Dengue`) %>% 
  filter(!is.na(idade), !is.na(`Teste Rápido Dengue`)) %>% 
  group_by(idade, `Teste Rápido Dengue`) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

porcentagens <- str_c(idade_dengue$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(idade_dengue$freq, " (", porcentagens, ")"))

ggplot(idade_dengue) +
  aes(
    x = idade,
    y = freq,
    fill = `Teste Rápido Dengue` ,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 2
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 200), limits=c(0, 1000)) +
  labs(x = "Grupo Etário (Anos Completos)", y = "Frequência") +
  theme_estat()


ggsave(filename = file.path(caminho_dengue,"colunas-bi-freq-idade-dengue.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_dengue,"colunas-bi-freq-idade-dengue.png"), width = 158, height = 93, units = "mm")

#Sexo

sexo_dengue <- banco_dengue %>%
  select(sexo, `Teste Rápido Dengue`) %>% 
  filter(!is.na(sexo), !is.na(`Teste Rápido Dengue`)) %>% 
  group_by(sexo, `Teste Rápido Dengue`) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

porcentagens <- str_c(sexo_dengue$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(sexo_dengue$freq, " (", porcentagens, ")"))

ggplot(sexo_dengue) +
  aes(
    x = fct_reorder(sexo, freq, .desc = T),
    y = freq,
    fill = `Teste Rápido Dengue`,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 200), limits=c(0, 1000)) +
  labs(x = "Sexo", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_dengue,"colunas-bi-freq-sexo-dengue.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_dengue,"colunas-bi-freq-sexo-dengue.png"), width = 158, height = 93, units = "mm")



#Vacina 
vacina_dengue <- banco_dengue %>%
  select(Vacina, `Teste Rápido Dengue`) %>% 
  filter(!is.na(Vacina), !is.na(`Teste Rápido Dengue`)) %>% 
  group_by(Vacina, `Teste Rápido Dengue`) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

porcentagens <- str_c(vacina_dengue$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(vacina_dengue$freq, " (", porcentagens, ")"))

ggplot(vacina_dengue) +
  aes(
    x = fct_reorder(Vacina, freq, .desc = T),
    y = freq,
    fill = `Teste Rápido Dengue`,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 200), limits=c(0, 1000)) +
  labs(x = "Vacina Prévia Contra Febre Amarela", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_dengue,"colunas-bi-freq-vacina-dengue.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_dengue,"colunas-bi-freq-vacina-dengue.png"), width = 158, height = 93, units = "mm")


#Cor

cor_dengue <- banco_dengue %>%
  select(Cor, `Teste Rápido Dengue`) %>% 
  filter(!is.na(Cor), !is.na(`Teste Rápido Dengue`)) %>% 
  group_by(Cor, `Teste Rápido Dengue`) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

porcentagens <- str_c(cor_dengue$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(cor_dengue$freq, " (", porcentagens, ")"))

ggplot(cor_dengue) +
  aes(
    x = fct_reorder(Cor, freq, .desc = T),
    y = freq,
    fill = `Teste Rápido Dengue`,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 200), limits=c(0, 1000)) +
  labs(x = "Raça/Cor", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_dengue,"colunas-bi-freq-cor-dengue.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_dengue,"colunas-bi-freq-cor-dengue.png"), width = 158, height = 93, units = "mm")

