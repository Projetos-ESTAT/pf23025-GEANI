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

pacman::p_load(dplyr, DescTools)

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
             DENGUE3_PRNT20 %in% c("NEG", NA) ~ "NEG") %>% 
           as.factor(),
         DENGUE_PRNT20_2 = case_when(
           DENGUE1_PRNT20 == "POS" | 
             DENGUE2_PRNT20 == "POS" | 
             DENGUE3_PRNT20 == "POS" ~ "POS", 
           DENGUE1_PRNT20 == "NEG" & 
             DENGUE2_PRNT20 == "NEG" & 
             DENGUE3_PRNT20 == "NEG" ~ "NEG",
         is.na(DENGUE1_PRNT20) & 
           is.na(DENGUE2_PRNT20) & 
           is.na(DENGUE3_PRNT20) ~ "IND")%>% 
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

#### adicionando os pesos amostrais ####

bancoAdj <- srvyr::as_survey_design(sangue, ids = c(ids = p06_upa, x06_domicilio), 
                                    strata = p05_estrato, 
                                    data = sangue,
                                    weights = peso_teste, 
                                    nest = TRUE)

################################  AMOSTRAGEM  ################################

# -------------------------------- Parte 1 ----------------------------------- #

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
    sens_adjusted = sens_adjusted, 
    spec_adjusted = spec_adjusted,
    RVP_adjusted = sens_adjusted/(1-spec_adjusted),
    RVN_adjusted = (1-sens_adjusted)/spec_adjusted
  )
  )
}

#### sensibilidade, especificidade, RVP e RVN - DENGUE ####

### NEG e NA juntos
svy_bgadjust(
  bancoAdj, 
  DENGUE_PRNT20, 
  "POS",
  "NEG",
  F03_D_G_VALOR_C,
  "Reagente",
  "Não reagente"
)
# sensibilidade = 98,26%
# especificidade = 34,53%
# RVP = 1,5
# RVN = 0,05

### NEG e NA separados
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

### NEG e NA juntos
svy_bgadjust(
  bancoAdj, 
  ZIK_PRNT20_2, 
  "POS",
  "NEG",
  F07_Z_G_VALOR_C,
  "Reagente",
  "Não reagente"
)
# sensibilidade = 99,61%
# especificidade = 64,56%
# RVP = 2,81
# RVN = 0,01

### NEG e NA separados
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





































###################################  MET 2  ###################################

#### TR Chikungunya e vacinação - febre amarela ####

## tabela de dupla entrada
tab_chik <-table(sangue$F11_C_G_VALOR_C, sangue$D13_FEBRE_VACINA)
tab_chik
addmargins(tab_chik)

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

## teste de qui-quadrado p/ independência
chisq.test(sangue$F07_Z_G_VALOR_C, sangue$D13_FEBRE_VACINA)

#chisq.test(sangue$F07_Z_G_VALOR_C, sangue$D13_FEBRE_VACINA, simulate.p.value = T)
#chisq.test(sangue$F07_Z_G_VALOR_C, sangue$D13_FEBRE_VACINA,simulate.p.value = TRUE, B = 10000)


## Coeficiente de contigência 
CramerV(tab_zika)
GoodmanKruskalGamma(tab_zika)


