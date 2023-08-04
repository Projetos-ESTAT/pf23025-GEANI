source("rdocs/source/packages.R")
pacman::p_load("readxl", "dplyr", "purrr",
               "glue", "ggplot2", "ggpubr",
               "kableExtra", "caret", "pROC",
               "plotROC", "tibble", "reticulate", "glmtoolbox",
               "survey", "MASS", "caret")

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


plano <- readRDS("banco/plano.rds")

#PARTE NÃO UTILIZADA

bancoRed <- plano$variables

bancoRed <- bancoRed %>% dplyr::filter(is.na(CHIK_PRNT20)==F)

bancoRed <- bancoRed %>% dplyr::select(CHIK_PRNT20,peso_teste,Sexo,b05_idade,
                                       d13_febre_vacina,b06_cor,c01_tipo,f11_c_g_valor)

bancoRed <- bancoRed %>% dplyr::filter(d13_febre_vacina!="Não sabe")

#AGRUPAR RAÇA NEGRO E NÃO NEGRO

#agrupei parda com indígia para serem 3 níveis
bancoRed <- bancoRed %>% dplyr::mutate(b06_cor = case_when(
  b06_cor == 'Branca' ~ 'nao negro',
  b06_cor == 'Indígena' ~ 'nao negro',
  b06_cor == 'Amarela (origem japonesa, chinesa, coreana etc.)' ~ 'nao negro',
  b06_cor == 'Preta' ~ 'negro',
  b06_cor == 'Parda (mulata, cabocla, cafuza, mameluca ou mestiça)' ~ 'negro'
))  %>% dplyr::select(CHIK_PRNT20,peso_teste,Sexo,b05_idade,
                      d13_febre_vacina,c01_tipo,f11_c_g_valor,b06_cor)

bancoRed <- bancoRed %>% dplyr::mutate(CHIK_PRNT20 = case_when(
  CHIK_PRNT20 == 'POS' ~ 1,
  CHIK_PRNT20 == 'NEG' ~ 0))

plano$variables <- plano$variables %>%
  mutate(b06_cor = case_when(
    b06_cor == 'Branca' ~ 'nao negro',
    b06_cor == 'Indígena' ~ 'nao negro',
    b06_cor == 'Amarela (origem japonesa, chinesa, coreana etc.)' ~ 'nao negro',
    b06_cor == 'Preta' ~ 'negro',
    b06_cor == 'Parda (mulata, cabocla, cafuza, mameluca ou mestiça)' ~ 'negro',
  ))

plano$variables <- plano$variables %>%
  mutate(d13_febre_vacina = na_if(d13_febre_vacina, 'Não sabe'))

#modelo todas var
mod1 <- svyglm(factor(CHIK_PRNT20) ~ Sexo + b05_idade + d13_febre_vacina + c01_tipo
               +f11_c_g_valor + factor(b06_cor),family = binomial, design = plano)

summary(mod1)

mod1$aic

corte <- seq(from = 0, to =1, by = 0.01)
acc <- c()
j=0
for(i in corte){
  j=j+1
acc[j] <- sum((predict(mod1,bancoRed, type = "response")>=i)==bancoRed$CHIK_PRNT20)/nrow(bancoRed)
}

plot(acc, type = 'l')
points(max(acc),which.max(acc), col = 'red', pch = 16)
data.frame(corte = corte, acc = acc)


roc(bancoRed$CHIK_PRNT20, predict(mod1,bancoRed, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)


#melhor modelo AIC stepwise

modAIC <- step(mod1)

mod <- summary(modAIC)
confint(modAIC)
modAIC$aic

#melhor valor de corte 0.25 (testar 0.07)
modAIC$acc <- sum((predict(modAIC, type = "response")>=0.1)==bancoRed$CHIK_PRNT20)/nrow(bancoRed)

modAIC$acc

roc(bancoRed$CHIK_PRNT20, predict(modAIC, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

#melhor valor de corte 0.25 (testar 0.07)
conf_mat <- confusionMatrix(factor(as.numeric(predict(mod1, type = "response")>=0.25)),
                            factor(bancoRed$CHIK_PRNT20), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]


#Acurácia dado corte 18 e 22

sum((bancoRed$f11_c_g_valor > 18)==bancoRed$CHIK_PRNT20)/nrow(bancoRed)

conf_mat <- confusionMatrix(factor(as.numeric((bancoRed$f11_c_g_valor > 18))),
                            factor(bancoRed$CHIK_PRNT20), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]
