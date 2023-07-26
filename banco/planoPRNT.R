
library(survey)
library(tidyverse)
library(srvyr)
require(dplyr)
library(epiDisplay)

setwd('D:/Downloads/ESTAT - Geani')
dados<-readRDS('DB_ZDC_FASE2.rds')
plano<-readRDS('plano.rds')

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
                  dplyr::between(F07_Z_G_VALOR, -Inf, 22) ~ "Não reagente", # Verificar se estes sao os pontos de corte que serao utilizados
                  dplyr::between(F07_Z_G_VALOR, 28, Inf) ~ "Reagente"
                ))
  
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


##Negativos = negativos do TR/prnt e NA #########

table(bancoAdj$variables$DENGUE_PRNT20)

survey::svytable(~DENGUE_PRNT20,bancoAdj, round = TRUE)
 
survey::svytable(~DENGUE_PRNT20, bancoAdj, Ntotal = 100)
 
D1 <- survey::svyciprop(~I(DENGUE_PRNT20=="POS"), bancoAdj, method = "logit")
(D7 <- rbind((attr(D1, "ci") *100)))

##Dengue1
table(bancoAdj$variables$DENGUE1_PRNT20)

survey::svytable(~DENGUE1_PRNT20,bancoAdj, round = TRUE)

survey::svytable(~DENGUE1_PRNT20, bancoAdj, Ntotal = 100)

D1 <- survey::svyciprop(~I(DENGUE1_PRNT20=="POS"), bancoAdj, method = "logit")
(D7 <- rbind((attr(D1, "ci") *100)))

##Dengue2
table(bancoAdj$variables$DENGUE2_PRNT20)

survey::svytable(~DENGUE2_PRNT20,bancoAdj, round = TRUE)

survey::svytable(~DENGUE2_PRNT20, bancoAdj, Ntotal = 100)

D1 <- survey::svyciprop(~I(DENGUE2_PRNT20=="POS"), bancoAdj, method = "logit")
(D7 <- rbind((attr(D1, "ci") *100)))

##Dengue3
table(bancoAdj$variables$DENGUE3_PRNT20)

survey::svytable(~DENGUE3_PRNT20,bancoAdj, round = TRUE)

survey::svytable(~DENGUE3_PRNT20, bancoAdj, Ntotal = 100)

D1 <- survey::svyciprop(~I(DENGUE3_PRNT20=="POS"), bancoAdj, method = "logit")
(D7 <- rbind((attr(D1, "ci") *100)))

##Zika
table(bancoAdj$variables$ZIK_PRNT20_2)

survey::svytable(~ZIK_PRNT20_2, bancoAdj, round = TRUE)
survey::svytable(~ZIK_PRNT20_2, bancoAdj, Ntotal = 100)

Z1 <- survey::svyciprop(~I(ZIK_PRNT20_2=="POS"), bancoAdj, method = "logit")
(Z7 <- rbind((attr(Z1, "ci") *100)))

################################################################
#DENGUE e ZIKA#####
table(bancoAdj$variables$ZIK_PRNT20_2,bancoAdj$variables$DENGUE_PRNT20)
svytable(~ZIK_PRNT20_2 + DENGUE_PRNT20, bancoAdj, round=TRUE)
#############################

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

###########################

library(knitr)

########Prevalencia ajustada manual sem peso amostral#####

####Dengue
prevAjusD <- ( 0.75 + 0.34 - 1)/(0.98 + 0.34 - 1)
prevAjusD
prevAjusD = ( 0.748 + 0.345 - 1)/(0.98 + 0.345 - 1)
prevAjusD
variancia = .748*(1-.748)/(2120*(.982+.345-1)^2)
variancia
IC95inferior = .286-qnorm(.975)*(0.00083151)^2
IC95inferior
IC95superior = .286 +qnorm(.975)*(0.00083151)^2
IC95superior

#prevalencia_ajustada = (prevalencia_bruta + especificidade - 1)/(sensibilidade + especificidade - 1)
#variancia = prevalencia_bruta * (1 - prevalencia_bruta) / (N total * (sensibilidade + especificidade - 1)^2)

#IC95% inferior = prevalencia_ajustada - Z * raiz_da_variancia
#IC95% superior = prevalencia_ajustada + Z * raiz_da_variancia
#O z  usar qnorm(.975)

####### Zika sem peso aostral######
prevAjus = (0.50 + 0.64 - 1)/(0.99 + 0.64 - 1)
prevAjus
variancia = 51*(1-51)/(2.120*(99.6+64.5-1)^2)
variancia
