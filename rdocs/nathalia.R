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

pacman::p_load(dplyr, DescTools, survey)

#### importando os bancos de dados ####

dados<-readRDS('banco/DB_ZDC_FASE2.rds')

plano<-readRDS('banco/plano.rds')

#### tratamento dos dados ####

plano2 <- plano$variables %>% 
  select(x01_uf, x02_munic, x03_distrito, x04_subdistrito, x05_setor,
         x06_domicilio, b00_numero, p06_upa, x06_domicilio, p05_estrato,
         peso_teste)

dados2 <- dados %>% 
  mutate(across(c(X01_UF, X02_MUNIC, X03_DISTRITO, X04_SUBDISTRITO, X05_SETOR, 
                  X06_DOMICILIO, B00_NUMERO), \(x) as.integer(x)), 
         DENGUE_PRNT20 = case_when(
           DENGUE1_PRNT20 == "POS" | 
             DENGUE2_PRNT20 == "POS" | 
             DENGUE3_PRNT20 == "POS" ~ "POS", 
           DENGUE1_PRNT20 %in% c("NEG", NA) & 
             DENGUE2_PRNT20 %in% c("NEG", NA) & 
             DENGUE3_PRNT20 %in% c("NEG", NA, "/") ~ "NEG") %>% 
           as.factor(),
         DENGUE_PRNT20_2 = case_when(
           DENGUE1_PRNT20 == "POS" | 
             DENGUE2_PRNT20 == "POS" | 
             DENGUE3_PRNT20 == "POS" ~ "POS", 
           DENGUE1_PRNT20 == "NEG" & 
             DENGUE2_PRNT20 == "NEG" & 
             DENGUE3_PRNT20 == "NEG" ~ "NEG",
           TRUE ~ "IND")%>% 
           as.factor(),
         ZIK_PRNT20_2 = case_when(
           ZIK_PRNT20 == "POS" ~ "POS", 
           ZIK_PRNT20 %in% c("NEG", NA) ~ "NEG"),
         ZIK_PRNT20_3 = case_when(
           ZIK_PRNT20 == "POS" ~ "POS", 
           ZIK_PRNT20 == "NEG" ~ "NEG",
           is.na(ZIK_PRNT20) ~ "IND"),
         F03_D_G_VALOR_C = case_when(
           between(F03_D_G_VALOR, -Inf,18)~"Não reagente",
           between(F03_D_G_VALOR, 22, Inf)~"Reagente"),
         F07_Z_G_VALOR_C = case_when(
           between(F07_Z_G_VALOR, -Inf, 22) ~ "Não reagente", 
           between(F07_Z_G_VALOR, 28, Inf) ~ "Reagente"),
         F11_C_G_VALOR_C = case_when(
           between(F11_C_G_VALOR, -Inf,16)~"Não reagente",
           between(F11_C_G_VALOR, 20, Inf)~"Reagente"))


#### juntando os bancos ####

sangue <- 
  left_join(
    plano2, dados2, by = join_by("x01_uf" == "X01_UF", "x02_munic" == "X02_MUNIC", 
                                 "x03_distrito" == "X03_DISTRITO",
                                 "x04_subdistrito" == "X04_SUBDISTRITO", 
                                 "x05_setor" == "X05_SETOR", 
                                 "x06_domicilio" == "X06_DOMICILIO",
                                 "b00_numero" == "B00_NUMERO")
    ) 

################################  AMOSTRAGEM  ################################

# -------------------------------- Parte 1 ----------------------------------- #

#### tabela Begg e Grenes, adaptado por Alonzo - sem pesos amostrais ####

### para Dengue ###

table(sangue$DENGUE_PRNT20_2,sangue$F03_D_G_VALOR_C)
addmargins(table(sangue$DENGUE_PRNT20_2,sangue$F03_D_G_VALOR_C))
# difícil vizualização

# alterando a ordem
DENGUE_PRNT20_2 <- factor(sangue$DENGUE_PRNT20_2, 
                          levels = c("POS", "NEG", "IND"))
F03_D_G_VALOR_C <- factor(sangue$F03_D_G_VALOR_C, 
                          levels = c(c("Reagente", "Não reagente")))

# bemm melhor
table(DENGUE_PRNT20_2,F03_D_G_VALOR_C)
tabela_dengue <- addmargins(table(DENGUE_PRNT20_2,F03_D_G_VALOR_C),1)
tabela_dengue

### para Zika ###

table(sangue$ZIK_PRNT20_3,sangue$F07_Z_G_VALOR_C)
addmargins(table(sangue$ZIK_PRNT20_3,sangue$F07_Z_G_VALOR_C))
# difícil vizualização

# alterando a ordem
ZIK_PRNT20_3 <- factor(sangue$ZIK_PRNT20_3, 
                       levels = c("POS", "NEG", "IND"))
F07_Z_G_VALOR_C <- factor(sangue$F07_Z_G_VALOR_C, 
                          levels = c(c("Reagente", "Não reagente")))


# bemm melhor
table(ZIK_PRNT20_3,F07_Z_G_VALOR_C)
tabela_zika <- addmargins(table(ZIK_PRNT20_3,F07_Z_G_VALOR_C),1)
tabela_zika

#### adicionando os pesos amostrais ####

bancoAdj <- srvyr::as_survey_design(sangue, ids = c(ids = p06_upa, x06_domicilio), 
                                    strata = p05_estrato, 
                                    data = sangue,
                                    weights = peso_teste, 
                                    nest = TRUE)


### função Begg e Grenes, adaptado por Alonzo, com RVP e RVN ###
# RV - razão de verossimilhança

svy_bgadjust <- function(bancoAdj, d, d1, d0, t, t1, t0) {
  tab <- 
    bancoAdj |> 
    srvyr::survey_count({{d}}, {{t}})
  
  total <- 
    bancoAdj |> 
    srvyr::survey_count({{t}})
  
  s1 <- tab |> dplyr::filter({{d}} == d1, {{t}} == t1) |> dplyr::pull(n)
  s0 <- tab |> dplyr::filter({{d}} == d1, {{t}} == t0) |> dplyr::pull(n)
  r1 <- tab |> dplyr::filter({{d}} == d0, {{t}} == t1) |> dplyr::pull(n)
  r0 <- tab |> dplyr::filter({{d}} == d0, {{t}} == t0) |> dplyr::pull(n)
  
  n1 <- total |> dplyr::filter({{t}} == t1) |> dplyr::pull(n)
  n0 <- total |> dplyr::filter({{t}} == t0) |> dplyr::pull(n)
  
  sens_adjusted <- (s1 * n1 / (s1 + r1))/((s1 * n1 / (s1 + r1)) + (s0 * n0 / (s0 + r0)))
  spec_adjusted <- (r0 * n0 / (s0 + r0))/((r0 * n0 / (s0 + r0)) + (r1 * n1 / (s1 + r1)))
  
  res <- dplyr::tibble(
    V = c(1, 1, 0),
    D = c(1, 0, NA),
    `T=1` = c(s1, r1, n1 - s1 - r1),
    `T=0` = c(s0, r0, n0 - s0 - r0)
  )
  
  return(list(
    table = res,
    n1 = n1,
    n0 = n0,
    sens_adjusted = sens_adjusted, 
    spec_adjusted = spec_adjusted,
    RVP_adjusted = sens_adjusted/(1-spec_adjusted),
    RVN_adjusted = (1-sens_adjusted)/spec_adjusted
  )
  )
}

#### sensibilidade, especificidade, RVP e RVN - DENGUE ####

#obs: NEG e NA separados
svy_bgadjust(
  bancoAdj, 
  DENGUE_PRNT20_2, 
  "POS",
  "NEG",
  F03_D_G_VALOR_C,
  "Reagente",
  "Não reagente"
)
# sensibilidade = 80,99%
# especificidade = 66,67%
# RVP = 2,43
# RVN = 0,28

#### sensibilidade e especificidade, RVP e RVN - ZIKA ####

#obs: NEG e NA separados
svy_bgadjust(
  bancoAdj, 
  ZIK_PRNT20_3, 
  "POS",
  "NEG",
  F07_Z_G_VALOR_C,
  "Reagente",
  "Não reagente"
)
# sensibilidade = 91,53%
# especificidade = 77,1%
# RVP = 4
# RVN = 0,11

# -------------------------------- Parte 2 ----------------------------------- #

#### função para prevalêcia / proporção ####
test <- function(n,total,doença) {
  freq <- n/total
  if (doença %in% c("dengue", "d", "Dengue", "D")) {
    sens <- 0.8099269 
    espec <- 0.6666953
  } else {
    sens <- 0.9152719 
    espec <- 0.7709884
  }
  prev_ajust <- (freq+espec-1)/(sens+espec-1)
  z <- 1.96
  erro_prev <- z*sqrt(prev_ajust*(1-prev_ajust)/total)
  inf_prev <- prev_ajust - erro_prev
  sup_prev <- prev_ajust + erro_prev
  erro_prop <- z*sqrt(freq*(1-freq)/total)
  inf_prop <- freq - erro_prop
  sup_prop <- freq + erro_prop
  return(list(
    prev_ajust = prev_ajust,
    prev_intervalo = paste("intervalo de conf da prevalência:", inf_prev, ";", sup_prev)#,
    #propoção = freq,
    #prop_intervalo = paste("intervalo de conf da proporção:", inf_prop, ";", sup_prop)
  ))
}

## Manipulação necessária para raça/cor
plano <- plano %>% 
  mutate(plano$variables,
         DENGUE_PRNT20 = case_when(
           DENGUE1_PRNT20 == "POS" | 
             DENGUE2_PRNT20 == "POS" | 
             DENGUE3_PRNT20 == "POS" ~ "POS", 
           DENGUE1_PRNT20 %in% c("NEG", NA) & 
             DENGUE2_PRNT20 %in% c("NEG", NA) & 
             DENGUE3_PRNT20 %in% c("NEG", NA, "/") ~ "NEG") %>% 
           as.factor(),
         DENGUE_PRNT20_2 = case_when(
           DENGUE1_PRNT20 == "POS" | 
             DENGUE2_PRNT20 == "POS" | 
             DENGUE3_PRNT20 == "POS" ~ "POS", 
           DENGUE1_PRNT20 == "NEG" & 
             DENGUE2_PRNT20 == "NEG" & 
             DENGUE3_PRNT20 == "NEG" ~ "NEG",
           TRUE ~ "IND")%>% 
           as.factor(),
         ZIK_PRNT20_2 = case_when(
           ZIK_PRNT20 == "POS" ~ "POS", 
           ZIK_PRNT20 %in% c("NEG", NA) ~ "NEG"),
         ZIK_PRNT20_3 = case_when(
           ZIK_PRNT20 == "POS" ~ "POS", 
           ZIK_PRNT20 == "NEG" ~ "NEG",
           is.na(ZIK_PRNT20) ~ "IND"),
         F03_D_G_VALOR_C = case_when(
           between(f03_d_g_valor, -Inf,18)~"Não reagente",
           between(f03_d_g_valor, 22, Inf)~"Reagente"),
         F07_Z_G_VALOR_C = case_when(
           between(f07_z_g_valor, -Inf, 22) ~ "Não reagente", 
           between(f07_z_g_valor, 28, Inf) ~ "Reagente"),
         F11_C_G_VALOR_C = case_when(
           between(f11_c_g_valor, -Inf,16)~"Não reagente",
           between(f11_c_g_valor, 20, Inf)~"Reagente"))


###############################     DENGUE     ###############################

#### geral ####

# n
table(bancoAdj$variables$F03_D_G_VALOR_C)

# N
svytable(~F03_D_G_VALOR_C, bancoAdj, round=TRUE)
svytotal(~F03_D_G_VALOR_C, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
test(1587,2108,"d")


#### sexos

# n
table(bancoAdj$variables$F03_D_G_VALOR_C,bancoAdj$variables$B04_SEXO)

# N 
svytable(~F03_D_G_VALOR_C+B04_SEXO, bancoAdj, round=TRUE)
svytotal(~interaction(F03_D_G_VALOR_C, B04_SEXO),design=bancoAdj,na.rm=T)

## criando variaveis
bancoAdj$variables$Dengue_fem<-NA
bancoAdj$variables$Dengue_fem[bancoAdj$variables$F03_D_G_VALOR_C=="Reagente"& 
                                bancoAdj$variables$B04_SEXO== "Feminino"]<-"positivo_f"
bancoAdj$variables$Dengue_fem[bancoAdj$variables$F03_D_G_VALOR_C=="Não reagente"& 
                                bancoAdj$variables$B04_SEXO== "Feminino"]<-"negativo_f"
bancoAdj$variables$Dengue_masc<-NA
bancoAdj$variables$Dengue_masc[bancoAdj$variables$F03_D_G_VALOR_C=="Reagente"& 
                                 bancoAdj$variables$B04_SEXO== "Masculino"] <-"positivo_m"
bancoAdj$variables$Dengue_masc[bancoAdj$variables$F03_D_G_VALOR_C=="Não reagente"& 
                                 bancoAdj$variables$B04_SEXO== "Masculino"] <-"negativo_m"
table(bancoAdj$variables$Dengue_fem)
table(bancoAdj$variables$Dengue_masc)


#### feminino ####

# n
table(bancoAdj$variables$Dengue_fem)

# N
svytable(~Dengue_fem, bancoAdj, round=TRUE)
svytotal(~Dengue_fem, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(934,2108,"d")
#test(934,1587,"d")
test(934,1256,"d")

#### masculino ####

# n
table(bancoAdj$variables$Dengue_masc)

# N
svytable(~Dengue_masc, bancoAdj, round=TRUE)
svytotal(~Dengue_masc, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(653,2108,"d")
#test(653,1587,"d")
test(653,852,"d")

#### faixas etárias

## criando faixas 
bancoAdj <- mutate(bancoAdj,
                   c_idade = cut(B05_IDADE, c(0,11,18,29,59,120), include.lowest = TRUE))


# n
table(bancoAdj$variables$c_idade,bancoAdj$variables$F03_D_G_VALOR_C)

# N 
svytable(~F03_D_G_VALOR_C+c_idade, bancoAdj, round=TRUE)
svytotal(~interaction(F03_D_G_VALOR_C,c_idade), design=bancoAdj, na.rm=T)


## criando variaveis

table(bancoAdj$variables$c_idade,bancoAdj$variables$F03_D_G_VALOR_C)

bancoAdj$variables$D_idade0a11<-NA
bancoAdj$variables$D_idade0a11[bancoAdj$variables$F03_D_G_VALOR_C=="Reagente"& 
                                 bancoAdj$variables$c_idade== "[0,11]"]<-"positivo_0a11"
bancoAdj$variables$D_idade0a11[bancoAdj$variables$F03_D_G_VALOR_C=="Não reagente"& 
                                 bancoAdj$variables$c_idade== "[0,11]"]<-"negativo_0a11"
bancoAdj$variables$D_idade12a18<-NA
bancoAdj$variables$D_idade12a18[bancoAdj$variables$F03_D_G_VALOR_C=="Reagente"& 
                                  bancoAdj$variables$c_idade== "(11,18]"]<-"positivo_12a18"
bancoAdj$variables$D_idade12a18[bancoAdj$variables$F03_D_G_VALOR_C=="Não reagente"& 
                                  bancoAdj$variables$c_idade== "(11,18]"]<-"negativo_12a18"
bancoAdj$variables$D_idade19a29<-NA
bancoAdj$variables$D_idade19a29[bancoAdj$variables$F03_D_G_VALOR_C=="Reagente"& 
                                  bancoAdj$variables$c_idade== "(18,29]"]<-"positivo_19a29"
bancoAdj$variables$D_idade19a29[bancoAdj$variables$F03_D_G_VALOR_C=="Não reagente"& 
                                  bancoAdj$variables$c_idade== "(18,29]"]<-"negativo_19a29"
bancoAdj$variables$D_idade30a59<-NA
bancoAdj$variables$D_idade30a59[bancoAdj$variables$F03_D_G_VALOR_C=="Reagente"& 
                                  bancoAdj$variables$c_idade== "(29,59]"]<-"positivo_30a59"
bancoAdj$variables$D_idade30a59[bancoAdj$variables$F03_D_G_VALOR_C=="Não reagente"& 
                                  bancoAdj$variables$c_idade== "(29,59]"]<-"negativo_30a59"
bancoAdj$variables$D_idade60<-NA
bancoAdj$variables$D_idade60[bancoAdj$variables$F03_D_G_VALOR_C=="Reagente"& 
                               bancoAdj$variables$c_idade== "(59,120]"]<-"positivo_60"
bancoAdj$variables$D_idade60[bancoAdj$variables$F03_D_G_VALOR_C=="Não reagente"& 
                               bancoAdj$variables$c_idade== "(59,120]"]<-"negativo_60"


## verificando
table(bancoAdj$variables$c_idade,bancoAdj$variables$F03_D_G_VALOR_C)
table(bancoAdj$variables$D_idade0a11)
table(bancoAdj$variables$D_idade12a18)
table(bancoAdj$variables$D_idade19a29)
table(bancoAdj$variables$D_idade30a59)
table(bancoAdj$variables$D_idade60)

####  0 a 11  ####

# n
table(bancoAdj$variables$D_idade0a11)

# N
svytable(~D_idade0a11, bancoAdj, round=TRUE)
svytotal(~D_idade0a11, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(57,2108,"d")
#test(57,1587,"d")
test(57,127,"d")


####  12 a 18  ####

# n
table(bancoAdj$variables$D_idade12a18)

# N
svytable(~D_idade12a18, bancoAdj, round=TRUE)
svytotal(~D_idade12a18, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(97,2108,"d")
#test(97,1587,"d")
test(97,161,"d")

####  19 a 29  ####

# n
table(bancoAdj$variables$D_idade19a29)

# N
svytable(~D_idade19a29, bancoAdj, round=TRUE)
svytotal(~D_idade19a29, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(201,2108,"d")
#test(201,1587,"d")
test(201,258,"d")

####  30 a 59  ####

# n
table(bancoAdj$variables$D_idade30a59)

# N
svytable(~D_idade30a59, bancoAdj, round=TRUE)
svytotal(~D_idade30a59, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(765,2108,"d")
#test(765,1587,"d")
test(765,960,"d")

####  60+  ####

# n
table(bancoAdj$variables$D_idade60)

# N
svytable(~D_idade60, bancoAdj, round=TRUE)
svytotal(~D_idade60, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(467,2108,"d")
#test(467,1587,"d")
test(467,602,"d")


### Raça/cor

# criando raça/cor

table(plano$variables$b06_cor)

plano$variables$raca_cor <-"Não negro"
plano$variables$raca_cor[plano$variables$b06_cor=="Preta"| plano$variables$b06_cor=="Indígena"|
                           plano$variables$b06_cor=="Parda (mulata, cabocla, cafuza, mameluca ou mestiça)"]<-"Negro"

table(plano$variables$b06_cor,plano$variables$raca_cor)

# criando variáveis
table(plano$variables$raca_cor,plano$variables$F03_D_G_VALOR_C)

plano$variables$D_negro<-NA
plano$variables$D_negro[plano$variables$F03_D_G_VALOR_C=="Reagente"& 
                          plano$variables$raca_cor== "Negro"]<-"pos_negro"
plano$variables$D_negro[plano$variables$F03_D_G_VALOR_C=="Não reagente"& 
                          plano$variables$raca_cor== "Negro"]<-"neg_negro"
plano$variables$D_nnegro<-NA
plano$variables$D_nnegro[plano$variables$F03_D_G_VALOR_C=="Reagente"& 
                           plano$variables$raca_cor== "Não negro"] <-"pos_nn"
plano$variables$D_nnegro[plano$variables$F03_D_G_VALOR_C=="Não reagente"& 
                           plano$variables$raca_cor== "Não negro"] <-"neg_nn"
table(plano$variables$raca_cor,plano$variables$F03_D_G_VALOR_C)
table(plano$variables$D_negro)
table(plano$variables$D_nnegro)

####  negro  ####

# n
table(plano$variables$D_negro)

# N
svytable(~D_negro, plano, round=TRUE)
svytotal(~D_negro, design=plano, na.rm=T)

## prevalência e intervalo de conf
#test(921,2108,"d")
#test(921,1587,"d")
test(921,1194,"d")

####  não negro  ####

# n
table(plano$variables$D_nnegro)

# N
svytable(~D_nnegro, plano, round=TRUE)
svytotal(~D_nnegro, design=plano, na.rm=T)

## prevalência e intervalo de conf
#test(666,2108,"d")
#test(666,1587,"d")
test(666,914,"d")

### vacina febre amarela

# criando variaveis
table(bancoAdj$variables$F03_D_G_VALOR_C,bancoAdj$variables$D13_FEBRE_VACINA)

bancoAdj$variables$D_tomou<-NA
bancoAdj$variables$D_tomou[bancoAdj$variables$F03_D_G_VALOR_C=="Reagente"& 
                             bancoAdj$variables$D13_FEBRE_VACINA== "Sim"]<-"pos_tomou"
bancoAdj$variables$D_tomou[bancoAdj$variables$F03_D_G_VALOR_C=="Não reagente"& 
                             bancoAdj$variables$D13_FEBRE_VACINA== "Sim"]<-"neg_tomou"
bancoAdj$variables$D_ntomou<-NA
bancoAdj$variables$D_ntomou[bancoAdj$variables$F03_D_G_VALOR_C=="Reagente"& 
                              bancoAdj$variables$D13_FEBRE_VACINA== "Não"] <-"pos_ntomou"
bancoAdj$variables$D_ntomou[bancoAdj$variables$F03_D_G_VALOR_C=="Não reagente"& 
                              bancoAdj$variables$D13_FEBRE_VACINA== "Não"] <-"neg_ntomou"
table(bancoAdj$variables$D13_FEBRE_VACINA,bancoAdj$variables$F03_D_G_VALOR_C)
table(bancoAdj$variables$D_tomou)
table(bancoAdj$variables$D_ntomou)

#### tomou a vacina ####

# n
table(bancoAdj$variables$D_tomou)

# N
svytable(~D_tomou, bancoAdj, round=TRUE)
svytotal(~D_tomou, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(868,2108,"d")
#test(868,1564,"d")
test(868,1181,"d")

#### n tomou a vacina ####

# n
table(bancoAdj$variables$D_ntomou)

# N
svytable(~D_ntomou, bancoAdj, round=TRUE)
svytotal(~D_ntomou, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(696,2108,"d")
#test(696,1564,"d")
test(696,893,"d")


##############################     ZIKA     ##################################


#### geral ####

# n
table(bancoAdj$variables$F07_Z_G_VALOR_C)

# N
svytable(~F07_Z_G_VALOR_C, bancoAdj, round=TRUE)
svytotal(~F07_Z_G_VALOR_C, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
test(1072,2084,"z")


#### sexos

# n
table(bancoAdj$variables$F07_Z_G_VALOR_C,bancoAdj$variables$B04_SEXO)

# N 
svytable(~F07_Z_G_VALOR_C+B04_SEXO, bancoAdj, round=TRUE)
svytotal(~interaction(F07_Z_G_VALOR_C,B04_SEXO), design=bancoAdj, na.rm=T)

# criando variaveis
bancoAdj$variables$Zika_fem<-NA
bancoAdj$variables$Zika_fem[bancoAdj$variables$F07_Z_G_VALOR_C=="Reagente"& 
                              bancoAdj$variables$B04_SEXO== "Feminino"]<-"positivo_f"
bancoAdj$variables$Zika_fem[bancoAdj$variables$F07_Z_G_VALOR_C=="Não reagente"& 
                              bancoAdj$variables$B04_SEXO== "Feminino"]<-"negativo_f"
bancoAdj$variables$Zika_masc<-NA
bancoAdj$variables$Zika_masc[bancoAdj$variables$F07_Z_G_VALOR_C=="Reagente"& 
                               bancoAdj$variables$B04_SEXO== "Masculino"] <-"positivo_m"
bancoAdj$variables$Zika_masc[bancoAdj$variables$F07_Z_G_VALOR_C=="Não reagente"& 
                               bancoAdj$variables$B04_SEXO== "Masculino"] <-"negativo_m"
table(bancoAdj$variables$Zika_fem)
table(bancoAdj$variables$Zika_masc)


#### feminino ####

# n
table(bancoAdj$variables$Zika_fem)

# N
svytable(~Zika_fem, bancoAdj, round=TRUE)
svytotal(~Zika_fem, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(625,2084,"z")
#test(625,1072,"z")
test(625,1239,"z")

#### masculino ####

# n
table(bancoAdj$variables$Zika_masc)

# N
svytable(~Zika_masc, bancoAdj, round=TRUE)
svytotal(~Zika_masc, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(447,2084,"z")
#test(447,1072,"z")
test(447,845,"z")

#### faixas etárias

# n
table(bancoAdj$variables$F07_Z_G_VALOR_C,bancoAdj$variables$c_idade)

# N 
svytable(~F07_Z_G_VALOR_C+c_idade, bancoAdj, round=TRUE)
svytotal(~interaction(F07_Z_G_VALOR_C,c_idade), design=bancoAdj, na.rm=T)


## criando variaveis

table(bancoAdj$variables$c_idade,bancoAdj$variables$F07_Z_G_VALOR_C)

bancoAdj$variables$Z_idade0a11<-NA
bancoAdj$variables$Z_idade0a11[bancoAdj$variables$F07_Z_G_VALOR_C=="Reagente"& 
                                 bancoAdj$variables$c_idade== "[0,11]"]<-"positivo_0a11"
bancoAdj$variables$Z_idade0a11[bancoAdj$variables$F07_Z_G_VALOR_C=="Não reagente"& 
                                 bancoAdj$variables$c_idade== "[0,11]"]<-"negativo_0a11"
bancoAdj$variables$Z_idade12a18<-NA
bancoAdj$variables$Z_idade12a18[bancoAdj$variables$F07_Z_G_VALOR_C=="Reagente"& 
                                  bancoAdj$variables$c_idade== "(11,18]"]<-"positivo_12a18"
bancoAdj$variables$Z_idade12a18[bancoAdj$variables$F07_Z_G_VALOR_C=="Não reagente"& 
                                  bancoAdj$variables$c_idade== "(11,18]"]<-"negativo_12a18"
bancoAdj$variables$Z_idade19a29<-NA
bancoAdj$variables$Z_idade19a29[bancoAdj$variables$F07_Z_G_VALOR_C=="Reagente"& 
                                  bancoAdj$variables$c_idade== "(18,29]"]<-"positivo_19a29"
bancoAdj$variables$Z_idade19a29[bancoAdj$variables$F07_Z_G_VALOR_C=="Não reagente"& 
                                  bancoAdj$variables$c_idade== "(18,29]"]<-"negativo_19a29"
bancoAdj$variables$Z_idade30a59<-NA
bancoAdj$variables$Z_idade30a59[bancoAdj$variables$F07_Z_G_VALOR_C=="Reagente"& 
                                  bancoAdj$variables$c_idade== "(29,59]"]<-"positivo_30a59"
bancoAdj$variables$Z_idade30a59[bancoAdj$variables$F07_Z_G_VALOR_C=="Não reagente"& 
                                  bancoAdj$variables$c_idade== "(29,59]"]<-"negativo_30a59"
bancoAdj$variables$Z_idade60<-NA
bancoAdj$variables$Z_idade60[bancoAdj$variables$F07_Z_G_VALOR_C=="Reagente"& 
                               bancoAdj$variables$c_idade== "(59,120]"]<-"positivo_60"
bancoAdj$variables$Z_idade60[bancoAdj$variables$F07_Z_G_VALOR_C=="Não reagente"& 
                               bancoAdj$variables$c_idade== "(59,120]"]<-"negativo_60"

## verificando
table(bancoAdj$variables$c_idade,bancoAdj$variables$F07_Z_G_VALOR_C)
table(bancoAdj$variables$Z_idade0a11)
table(bancoAdj$variables$Z_idade12a18)
table(bancoAdj$variables$Z_idade19a29)
table(bancoAdj$variables$Z_idade30a59)
table(bancoAdj$variables$Z_idade60)

####  0 a 11  ####

# n
table(bancoAdj$variables$Z_idade0a11)

# N
svytable(~Z_idade0a11, bancoAdj, round=TRUE)
svytotal(~Z_idade0a11, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(33,2084,"z")
#test(33,1072,"z")
test(33,128,"z")

####  12 a 18  ####

# n
table(bancoAdj$variables$Z_idade12a18)

# N
svytable(~Z_idade12a18, bancoAdj, round=TRUE)
svytotal(~Z_idade12a18, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(61,2084,"z")
#test(61,1072,"z")
test(61,161,"z")

####  19 a 29  ####

# n
table(bancoAdj$variables$Z_idade19a29)

# N
svytable(~Z_idade19a29, bancoAdj, round=TRUE)
svytotal(~Z_idade19a29, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(140,2084,"z")
#test(140,1072,"z")
test(140,255,"z")

####  30 a 59  ####

# n
table(bancoAdj$variables$Z_idade30a59)

# N
svytable(~Z_idade30a59, bancoAdj, round=TRUE)
svytotal(~Z_idade30a59, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(512,2084,"z")
#test(512,1072,"z")
test(512,948,"z")

####  60+  ####

# n
table(bancoAdj$variables$Z_idade60)

# N
svytable(~Z_idade60, bancoAdj, round=TRUE)
svytotal(~Z_idade60, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(326,2084,"z")
#test(326,1072,"z")
test(326,592,"z")

#### Raça/cor

# criando variáveis
table(plano$variables$raca_cor,plano$variables$F07_Z_G_VALOR_C)

plano$variables$Z_negro<-NA
plano$variables$Z_negro[plano$variables$F07_Z_G_VALOR_C=="Reagente"& 
                          plano$variables$raca_cor== "Negro"]<-"pos_negro"
plano$variables$Z_negro[plano$variables$F07_Z_G_VALOR_C=="Não reagente"& 
                          plano$variables$raca_cor== "Negro"]<-"neg_negro"
plano$variables$Z_nnegro<-NA
plano$variables$Z_nnegro[plano$variables$F07_Z_G_VALOR_C=="Reagente"& 
                           plano$variables$raca_cor== "Não negro"] <-"pos_nn"
plano$variables$Z_nnegro[plano$variables$F07_Z_G_VALOR_C=="Não reagente"& 
                           plano$variables$raca_cor== "Não negro"] <-"neg_nn"
table(plano$variables$raca_cor,plano$variables$F07_Z_G_VALOR_C)
table(plano$variables$Z_negro)
table(plano$variables$Z_nnegro)

####  negro  ####

# n
table(plano$variables$Z_negro)

# N
svytable(~Z_negro, plano, round=TRUE)
svytotal(~Z_negro, design=plano, na.rm=T)

## prevalência e intervalo de conf
#test(654,2084,"z")
#test(654,1072,"z")
test(654,1179,"z")

####  não negro  ####

# n
table(plano$variables$Z_nnegro)

# N
svytable(~Z_nnegro, plano, round=TRUE)
svytotal(~Z_nnegro, design=plano, na.rm=T)

## prevalência e intervalo de conf
#test(418,2084,"z")
#test(418,1072,"z")
test(418,905,"z")

### vacina febre amarela

# criando variaveis
table(bancoAdj$variables$F07_Z_G_VALOR_C,bancoAdj$variables$D13_FEBRE_VACINA)

bancoAdj$variables$Z_tomou<-NA
bancoAdj$variables$Z_tomou[bancoAdj$variables$F07_Z_G_VALOR_C=="Reagente"& 
                             bancoAdj$variables$D13_FEBRE_VACINA== "Sim"]<-"pos_tomou"
bancoAdj$variables$Z_tomou[bancoAdj$variables$F07_Z_G_VALOR_C=="Não reagente"& 
                             bancoAdj$variables$D13_FEBRE_VACINA== "Sim"]<-"neg_tomou"
bancoAdj$variables$Z_ntomou<-NA
bancoAdj$variables$Z_ntomou[bancoAdj$variables$F07_Z_G_VALOR_C=="Reagente"& 
                              bancoAdj$variables$D13_FEBRE_VACINA== "Não"] <-"pos_ntomou"
bancoAdj$variables$Z_ntomou[bancoAdj$variables$F07_Z_G_VALOR_C=="Não reagente"& 
                              bancoAdj$variables$D13_FEBRE_VACINA== "Não"] <-"neg_ntomou"
table(bancoAdj$variables$D13_FEBRE_VACINA,bancoAdj$variables$F07_Z_G_VALOR_C)
table(bancoAdj$variables$Z_tomou)
table(bancoAdj$variables$Z_ntomou)

#### tomou a vacina ####

# n
table(bancoAdj$variables$Z_tomou)

# N
svytable(~Z_tomou, bancoAdj, round=TRUE)
svytotal(~Z_tomou, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(566,2084,"z")
#test(566,1056,"z")
test(566,1162,"z")

#### n tomou a vacina ####

# n
table(bancoAdj$variables$Z_ntomou)

# N
svytable(~Z_ntomou, bancoAdj, round=TRUE)
svytotal(~Z_ntomou, design=bancoAdj, na.rm=T)

## prevalência e intervalo de conf
#test(490,2084,"z")
#test(490,1056,"z")
test(490,888,"z")




###################################  MET 2  ###################################

#### TR Chikungunya e vacinação - febre amarela ####

## tabela de dupla entrada
tab_chik <-table(sangue$F11_C_G_VALOR_C, sangue$D13_FEBRE_VACINA)
tab_chik
addmargins(tab_chik)
prop.table(tab_chik)
addmargins(prop.table(tab_chik))

## teste de qui-quadrado p/ independência
chisq.test(sangue$F11_C_G_VALOR_C, sangue$D13_FEBRE_VACINA)

#chisq.test(sangue$F11_C_G_VALOR_C, sangue$D13_FEBRE_VACINA, simulate.p.value = T)
#chisq.test(sangue$F11_C_G_VALOR_C, sangue$D13_FEBRE_VACINA,simulate.p.value = TRUE, B = 10000)


## Coeficiente de contigência 
CramerV(tab_chik)
GoodmanKruskalGamma(tab_chik)

#### TR Dengue e vacinação - febre amarela ####

## tabela de dupla entrada
tab_deng <-table(sangue$F03_D_G_VALOR_C, sangue$D13_FEBRE_VACINA)
tab_deng
addmargins(tab_deng)
prop.table(tab_deng)
addmargins(prop.table(tab_deng))

## teste de qui-quadrado p/ independência
chisq.test(sangue$F03_D_G_VALOR_C, sangue$D13_FEBRE_VACINA)

#chisq.test(sangue$F03_D_G_VALOR_C, sangue$D13_FEBRE_VACINA, simulate.p.value = T)
#chisq.test(sangue$F03_D_G_VALOR_C, sangue$D13_FEBRE_VACINA,simulate.p.value = TRUE, B = 10000)


## Coeficiente de contigência 
CramerV(tab_deng)
GoodmanKruskalGamma(tab_deng)

#### TR Zika e vacinação - febre amarela ####

## tabela de dupla entrada
tab_zika <-table(sangue$F07_Z_G_VALOR_C, sangue$D13_FEBRE_VACINA)
tab_zika
addmargins(tab_zika)
prop.table(tab_zika)
addmargins(prop.table(tab_zika))

## teste de qui-quadrado p/ independência
chisq.test(sangue$F07_Z_G_VALOR_C, sangue$D13_FEBRE_VACINA)

#chisq.test(sangue$F07_Z_G_VALOR_C, sangue$D13_FEBRE_VACINA, simulate.p.value = T)
#chisq.test(sangue$F07_Z_G_VALOR_C, sangue$D13_FEBRE_VACINA,simulate.p.value = TRUE, B = 10000)


## Coeficiente de contigência 
CramerV(tab_zika)
GoodmanKruskalGamma(tab_zika)


