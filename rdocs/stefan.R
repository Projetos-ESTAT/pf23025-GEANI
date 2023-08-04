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

#Inicialização----

source("rdocs/source/packages.R")
pacman::p_load("readxl", "dplyr", "purrr",
               "glue", "ggplot2", "ggpubr",
               "kableExtra", "caret", "pROC",
               "plotROC", "tibble", "reticulate", "glmtoolbox",
               "survey", "MASS", "caret")

plano <- readRDS("banco/plano.rds")

#Banco apenas as com variáveis---- 


bancoRed <- plano$variables

bancoRed <- bancoRed %>% dplyr::filter(is.na(CHIK_PRNT20)==F)

bancoRed <- bancoRed %>% dplyr::select(CHIK_PRNT20,peso_teste,Sexo,b05_idade,
                                       d13_febre_vacina,b06_cor,c01_tipo,f11_c_g_valor)

bancoRed <- bancoRed %>% dplyr::filter(d13_febre_vacina!="Não sabe")

#AGRUPAR RAÇA NEGRO E NÃO NEGRO
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



#Arrumando plano---- 

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



#Modelos---- 

#modelo todas var
mod1 <- svyglm(factor(CHIK_PRNT20) ~ Sexo + b05_idade + d13_febre_vacina + c01_tipo
               +f11_c_g_valor + factor(b06_cor),family = binomial, design = plano)

summary(mod1)

mod1$aic

#Curva ROC
roc(bancoRed$CHIK_PRNT20, predict(mod1, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)

#melhor valor de corte 
mod1$acc <- sum((predict(mod1, type = "response")>=0.1)==bancoRed$CHIK_PRNT20)/nrow(bancoRed)

mod1$acc



#Matriz de confusão
conf_mat <- confusionMatrix(factor(as.numeric(predict(mod1, type = "response")>=0.1)),
                            factor(bancoRed$CHIK_PRNT20), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]




#modelo StepAIC
modAIC <- step(mod1,direction = "backward")

summary(modAIC)

modAIC$aic

#Curva ROC
roc(bancoRed$CHIK_PRNT20, predict(modAIC, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T,
    print.thres=T)


#melhor valor de corte 
modAIC$acc <- sum((predict(modAIC, type = "response")>=0.1)==bancoRed$CHIK_PRNT20)/nrow(bancoRed)

modAIC$acc


#Matriz de Confusão
conf_mat <- confusionMatrix(factor(as.numeric(predict(modAIC, type = "response")>=0.065)),
                            factor(bancoRed$CHIK_PRNT20), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]





#variável f11_c_g_valor
#Acurácia dado corte 18

sum((bancoRed$f11_c_g_valor > 18)==bancoRed$CHIK_PRNT20)/nrow(bancoRed)

#Curva ROC
roc(bancoRed$CHIK_PRNT20, bancoRed$f11_c_g_valor,
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T)

#Matriz de confusão
conf_mat <- confusionMatrix(factor(as.numeric((bancoRed$f11_c_g_valor > 18))),
                            factor(bancoRed$CHIK_PRNT20), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]