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
chic <- banco %>%
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
tipo_casa <- banco %>%
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
    fill = Chikungunya) +
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


####Analise 4 vacina
vacina_chik <- banco %>%
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


