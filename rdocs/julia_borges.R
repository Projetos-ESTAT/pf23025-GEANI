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



library(survey)
library(tidyverse)
library(srvyr)
require(dplyr)
library(epiDisplay)

plano2 <- plano$variables |>
  dplyr::select(x01_uf,
                x02_munic,
                x03_distrito,
                x04_subdistrito,
                x05_setor,
                x06_domicilio,
                b00_numero,
                p06_upa, 
                x06_domicilio,
                p05_estrato,
                peso_teste)

dados2 <- 
  dados |> 
  dplyr::mutate(dplyr::across(c(X01_UF, 
                                X02_MUNIC, 
                                X03_DISTRITO, 
                                X04_SUBDISTRITO, 
                                X05_SETOR, 
                                X06_DOMICILIO, 
                                B00_NUMERO ), 
                              \(x) as.integer(x)), 
                DENGUE_PRNT20 = dplyr::case_when(
                  DENGUE1_PRNT20 == "POS" | 
                    DENGUE2_PRNT20 == "POS" | 
                    DENGUE3_PRNT20 == "POS" ~ "POS", 
                  DENGUE1_PRNT20 %in% c("NEG", NA) & 
                    DENGUE2_PRNT20 %in% c("NEG", NA) & 
                    DENGUE3_PRNT20 %in% c("NEG", NA) ~ "NEG") |> 
                  as.factor(),
                F03_D_G_VALOR_C = dplyr::case_when(
                  dplyr::between(F03_D_G_VALOR, -Inf,18)~"Não reagente",
                  dplyr::between(F03_D_G_VALOR, 22, Inf)~"Reagente"),
                ZIK_PRNT20_2 = dplyr::case_when(
                  ZIK_PRNT20 == "POS" ~ "POS", 
                  ZIK_PRNT20 %in% c("NEG", NA) ~ "NEG"),
                F07_Z_G_VALOR_C = dplyr::case_when(
                  dplyr::between(F07_Z_G_VALOR, 0, 22) ~ "Não reagente", 
                  dplyr::between(F07_Z_G_VALOR, 28, Inf) ~ "Reagente"
                ))

'DENGUE TESTE RAPIDO = F03_D_G_VALOR_C
ZIKA TESTE RAPIDO = F07_Z_G_VALOR_C
CHIK TESTE RAPIDO = F11_C_G_VALOR
DENGUE PRNT = DENGUE_PRNT20 (variavel criada no script junta banco.R
ZIKA PRNT = ZIK_PRNT20_2
CHIK PRNT = CHIK_PRNT20'

'*Dengue*
Variável: F03_D_G_VALOR
Ponto de cortes
Negativo < ou = 18
Indeterminado >18 a <22
Positivo > ou = 22

*Zika*
Variável: F07_Z_G_VALOR
Negativo 0 a 22
Indeterminado >23 a <27
Positivo > ou = 28

*Chikungunya*
Variável: F11_C_G_VALOR
Negativo < ou = 16
Indeterminado >17 a <19
Positivo > 20'

sangue <- dplyr::left_join(
  plano2, dados2,
  by = dplyr::join_by("x01_uf" == "X01_UF", 
                      "x02_munic" == "X02_MUNIC", 
                      "x03_distrito" == "X03_DISTRITO","x04_subdistrito" == "X04_SUBDISTRITO", 
                      "x05_setor" == "X05_SETOR", 
                      "x06_domicilio" == "X06_DOMICILIO",
                      "b00_numero" == "B00_NUMERO")) 

bancoAdj <- srvyr::as_survey_design(sangue, ids = c(ids = p06_upa, x06_domicilio), 
                                    strata = p05_estrato, 
                                    data = sangue,
                                    weights = peso_teste, 
                                    nest = TRUE)

#Begg e grenees#########

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
    spec_adjusted = spec_adjusted
  )
  )
}

#######Acuacia Zika###########
svy_bgadjust(
  bancoAdj, 
  ZIK_PRNT20_2, 
  "POS",
  "NEG",
  F07_Z_G_VALOR_C,
  "Reagente",
  "Não reagente"
)

#################Acuracia Dengue#######
svy_bgadjust(
  bancoAdj, 
  DENGUE_PRNT20, 
  "POS",
  "NEG",
  F03_D_G_VALOR_C,
  "Reagente",
  "Não reagente"
)