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



library(summarytools)

###Análise exploratória
##Banco
banco_chik <- sangue %>% 
  dplyr:: select(B05_IDADE, B04_SEXO, B06_COR, D13_FEBRE_VACINA, C01_TIPO,CHIK_PRNT20,)
colnames(banco_chik)  <- c("idade","sexo","Cor","vacina", "tipo de residencia", "Chikungunya")

banco <- na.omit(banco_chik)
banco$Chikungunya[banco$Chikungunya == "NEG"] <- "Negativo"
banco$Chikungunya[banco$Chikungunya == "POS"] <- "Positivo"

banco$`tipo de residencia`[banco$`tipo de residencia` == "Habitação em casa de cômodos, cortiço ou cabeça de porco"] <- "Habitação em casa de cômodos,\ cortiço ou cabeça de porco"
view(banco)

###Tema estat
cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")
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


###Salvando graficos
caminho_chik <- "C:/Users/izade/OneDrive/Área de Trabalho/ESTAT/pf23025-GEANI/resultados/iza/Pacote chikungunya"

##analise 1 Sexo
###banco nomeando colunas
sexo<-data.frame(plano[["variables"]][["b04_sexo"]], plano[["variables"]][["CHIK_PRNT20"]])
sexo <- na.omit(sexo)
colnames(sexo)  <- c("sexo","Chikungunya")
sexo$Chikungunya[sexo$Chikungunya == "NEG"] <- "Negativo"
sexo$Chikungunya[sexo$Chikungunya == "POS"] <- "Positivo"

###gráfico
chic <- sexo %>%
  mutate(sexo = case_when(
    sexo %>% str_detect("Feminino") ~ "Feminino",
    sexo %>% str_detect("Masculino") ~ "Masculino"
  )) %>%
  group_by(sexo, Chikungunya) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq/sum(freq)*100,2)
  )
porcentagens <- str_c(chic$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(chic$freq, " (", porcentagens, ")"))

ggplot(chic) +
  aes(
    x = fct_reorder(sexo, freq, .desc = T), y = freq,
    fill = Chikungunya, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  scale_y_continuous(breaks = seq(0,350, by = 50), limits = c(0,350))+
  labs(x = "Sexo", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_chik,"colunas-bi-freq-sexchik.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_chik,"colunas-bi-freq-sexchik.png"), width = 158, height = 93, units = "mm")

####analise 2 cor
##Puxando o banco e tirando as NAs
cor<-data.frame(plano[["variables"]][["b06_cor"]], plano[["variables"]][["CHIK_PRNT20"]])
cor <- na.omit(cor)
##mudando o nome das colunas
colnames(cor)  <- c("A", "Chikungunya")
##mudando nome das observações 
cor$A <- ifelse(
  cor$A %in% c("Branca", "Amarela"),
  "Não negros",
  "Negros"
)
cor$Chikungunya <- factor(cor$Chikungunya,
                          levels = c('NEG', 'POS'),
                          labels = c('Negativo', 'Positivo')
)   

#Gráfico
raça_cor <- cor %>%
  group_by(A, Chikungunya) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq/sum(freq)*100,2)
  )
porcentagens <- str_c(raça_cor$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(raça_cor$freq, " (", porcentagens, ")"))

ggplot(raça_cor) +
  aes(
    x = fct_reorder(A, freq, .desc = T), y = freq,
    fill = Chikungunya, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  scale_y_continuous(breaks = seq(0,350, by = 50), limits = c(0,350))+
  labs(x = "Raça/cor", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_chik,"colunas-bi-freq-corchik.pdf"), width = 158, height = 93, units = "mm")


####Analise 3 casa
###banco e nomeando colunas
casa<-data.frame(plano[["variables"]][["c01_tipo"]], plano[["variables"]][["CHIK_PRNT20"]])
casa <- na.omit(casa)
colnames(casa)  <- c("tipo de residencia","Chikungunya")
casa$Chikungunya[casa$Chikungunya == "NEG"] <- "Negativo"
casa$Chikungunya[casa$Chikungunya == "POS"] <- "Positivo"

###gráfico
tipo_casa <- casa %>%
  mutate(`tipo de residencia` = case_when(
    `tipo de residencia` %>% str_detect("Casa de vila ou em condomínio") ~ "Casa de vila ou \nem condomínio",
    `tipo de residencia` %>% str_detect("Casa") ~ "Casa",
    `tipo de residencia` %>% str_detect("Apartamento") ~ "Apartamento"
  ))%>%
  filter(`tipo de residencia` != "Habitação em casa de cômodos, cortiço ou cabeça de porco")%>%
  group_by(`tipo de residencia`, Chikungunya) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq/sum(freq)*100,2)
  )
porcentagens <- str_c(tipo_casa$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(tipo_casa$freq, "(", porcentagens, ")"))


ggplot(tipo_casa) +
  aes(
    x = fct_reorder(`tipo de residencia`, freq, .desc = T), y = freq,
    fill = Chikungunya, label = legendas
    ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(aes(label = paste(freq, "(", freq_relativa, "%)")),
            position = position_dodge(width = .9),
            vjust = -0.5, hjust = 0.5,
            size = 3
  ) +
  scale_y_continuous(breaks = seq(0,400, by = 50), limits = c(0,400))+
  labs(x = "Tipos de residência", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_chik,"colunas-bi-freq-reschik.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_chik,"colunas-bi-freq-reschik.png"), width = 158, height = 93, units = "mm")


####Analise 4 vacina
###banco e nomeando colunas
vacina<-data.frame(plano[["variables"]][["d13_febre_vacina"]], plano[["variables"]][["CHIK_PRNT20"]])
vacina <- na.omit(vacina) 
colnames(vacina)  <- c("vacina","Chikungunya")
vacina$Chikungunya[vacina$Chikungunya == "NEG"] <- "Negativo"
vacina$Chikungunya[vacina$Chikungunya == "POS"] <- "Positivo"

###gráfico
vacina_chik <- vacina %>%
  filter(vacina != "Não sabe")%>%
  group_by(vacina, Chikungunya) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq/sum(freq)*100,2)
  )
porcentagens <- str_c(vacina_chik$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(vacina_chik$freq, " (", porcentagens, ")"))

ggplot(vacina_chik) +
  aes(
    x = fct_reorder(vacina, freq, .desc = T), y = freq,
    fill = Chikungunya, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  scale_y_continuous(breaks = seq(0,350, by = 50), limits = c(0,350))+
  labs(x = "Vacinação", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_chik,"colunas-bi-freq-vacchik.pdf"), width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_chik,"colunas-bi-freq-vacchik.png"), width = 158, height = 93, units = "mm")


 
##__________________Analise exploratória parte2_________________________##
##Caminho 
caminho_zika <- "C:/Users/izade/OneDrive/Área de Trabalho/ESTAT/pf23025-GEANI/resultados/iza/Zika PRNT"

 ###criando banco da zika e banco cor
cor<-data.frame(plano[["variables"]][["b06_cor"]], plano[["variables"]][["CHIK_PRNT20"]])
colnames(cor)[1] <- "Cor/Raça"
colnames(cor)[2] <- "Chikungunya"
cor$`Cor/Raça` <- ifelse(
  cor$`Cor/Raça` %in% c("Branca", "Amarela"),
  "Não negros",
  "Negros"
)

banco_zika <- sangue %>% 
  dplyr:: select(B05_IDADE, 
                 B04_SEXO, 
                 D13_FEBRE_VACINA, 
                 F07_Z_G_VALOR_C,
  )
colnames(banco_zika)  <- c("idade","sexo","vacina", "Zika")

#Junta os bandos um do lado do outro
banco_z <- bind_cols(banco_zika, cor)


banco_z <- banco_z %>% 
   select(idade,sexo,vacina,Zika,`Cor/Raça`) %>% 
   filter(Zika == 'Reagente')

##Juntando com o cor
bancoz <- bind_cols(banco_zika, cor)
 
##Sexo
 sex_zik <- banco_z %>%
   filter(!is.na(sexo)) %>%
   count(sexo) %>%
   mutate(
     freq = n %>% percent(),
   ) %>%
   mutate(
     freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
     label = str_c(n, " (", freq, ")") %>% str_squish()
   )
 
 ggplot(sex_zik) +
   aes(
     x = fct_reorder(sexo, n, .desc = T),
     y = n,
     label = label
   ) +
   geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
   geom_text(
     position = position_dodge(width = .9),
     vjust = -0.5,  hjust = 0.5,
     size = 3
   ) +
   scale_y_continuous(breaks = seq(0,650, by = 100), limits = c(0,625))+
   labs(x = "Sexo", y = "Frequência") +
   theme_estat()
 
 ggsave(filename = file.path(caminho_zika,"colunas-uni-freq-sexzik.pdf"), width = 158, height = 93, units = "mm")
 ggsave(filename = file.path(caminho_zika,"colunas-uni-freq-sexzik.png"), width = 158, height = 93, units = "mm")
 
 
 ##Vacina
 ##Quantidade de NA =15
 vac_zik <- banco_z %>%
   filter(!is.na(vacina)) %>%
   count(vacina) %>%
   mutate(
     freq = n %>% percent(),
   ) %>%
   mutate(
     freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
     label = str_c(n, " (", freq, ")") %>% str_squish()
   )
 
 ggplot(vac_zik) +
   aes(
     x = fct_reorder(vacina, n, .desc = T),
     y = n,
     label = label
   ) +
   geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
   geom_text(
     position = position_dodge(width = .9),
     vjust = -0.5,  hjust = .5,
     size = 3
   ) +
   scale_y_continuous(breaks = seq(0,600, by = 100), limits = c(0,600))+
   labs(x = "Vacinação", y = "Frequência") +
   theme_estat()
 
 ggsave(filename = file.path(caminho_zika,"colunas-uni-freq-vaczik.pdf"), width = 158, height = 93, units = "mm")
 ggsave(filename = file.path(caminho_zika,"colunas-uni-freq-vaczik.png"), width = 158, height = 93, units = "mm")
 
 
 ###Cor
cor_zik <- banco_z %>%
   filter(!is.na(`Cor/Raça`)) %>%
   count(`Cor/Raça`) %>%
   mutate(
     freq = n %>% percent(),
   ) %>%
   mutate(
     freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
     label = str_c(n, " (", freq, ")") %>% str_squish()
   )
 
 ggplot(cor_zik) +
   aes(
     x = fct_reorder(`Cor/Raça`, n, .desc = T),
     y = n,
     label = label
   ) +
   geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
   geom_text(
     position = position_dodge(width = .9),
     vjust = -0.5,  hjust = .5,
     size = 3
   ) +
   scale_y_continuous(breaks = seq(0,660, by = 100), limits = c(0,660))+
   labs(x = "Raça/Cor", y = "Frequência") +
   theme_estat()
 
 ggsave(filename = file.path(caminho_zika,"colunas-uni-freq-corzik.pdf"), width = 158, height = 93, units = "mm")
 ggsave(filename = file.path(caminho_zika,"colunas-uni-freq-corzik.png"), width = 158, height = 93, units = "mm")
 
 
 ##Idade
 #Definindo os limites das classes
 limites_classes_idades <- c(0,11,18,29,59,Inf)
 
 #agrupar as idades nas classes
 banco_z$idade <- cut(banco_z$idade, breaks = limites_classes_idades, labels = c("0-11", "12-18", "19-29", "30-59", ">=60"))
 
 #Gráfico
 idade_zik <- banco_z %>%
   filter(!is.na(idade)) %>%
   count(idade) %>%
   mutate(
     freq = n %>% percent(),
   ) %>%
   mutate(
     freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
     label = str_c(n, " (", freq, ")") %>% str_squish()
   )
 
 ggplot(idade_zik) +
   aes(
     x = idade,
     y = n,
     label = label
   ) +
   geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
   geom_text(
     position = position_dodge(width = .9),
     vjust = -0.5, hjust = .5,
     size = 3
   ) +
   scale_y_continuous(breaks = seq(0,550, by = 100), limits = c(0,520))+
   labs(x = "Grupo Etário (Anos completos)", y = "Frequência") +
   theme_estat()
 
 ggsave(filename = file.path(caminho_zika,"colunas-uni-freq-idadezik.pdf"), width = 158, height = 93, units = "mm")
 ggsave(filename = file.path(caminho_zika,"colunas-uni-freq-idadezik.png"), width = 158, height = 93, units = "mm")
 
 