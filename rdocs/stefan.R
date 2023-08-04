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


#tirar uma amostra pra validação
set.seed(1234)
n <- 100 #tamanho da validação
indexValid <- sample(1:nrow(bancoRed),n, replace = F)
indexTreino <- setdiff(1:nrow(bancoRed),indexValid)

#separando bancos
bancoRedTreino <- bancoRed[indexTreino,]
bancoRedValid <- bancoRed[indexValid,]
planoTreino <- plano[indexTreino]
planoValid <- plano[indexValid]


#modelo todas var
mod1 <- svyglm(factor(CHIK_PRNT20) ~ Sexo + b05_idade + d13_febre_vacina + c01_tipo
               +f11_c_g_valor + factor(b06_cor),family = binomial, design = planoTreino)

summary(mod1)

mod1$aic

mod1$acc <- sum((predict(mod1,bancoRedValid, type = "response")>=0.5)==bancoRedValid$CHIK_PRNT20)/nrow(bancoRedValid)

mod1$acc

roc(bancoRedValid$CHIK_PRNT20, predict(mod1,bancoRedValid, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T)

#modelo sem sexo
mod2 <- svyglm(factor(CHIK_PRNT20) ~ b05_idade  + d13_febre_vacina + c01_tipo
               +f11_c_g_valor + factor(b06_cor),family = binomial, design = planoTreino)

summary(mod2)

mod2$aic

mod2$acc <- sum((predict(mod2,bancoRedValid, type = "response")>=0.5)==bancoRedValid$CHIK_PRNT20)/nrow(bancoRedValid)

mod2$acc

roc(bancoRedValid$CHIK_PRNT20, predict(mod2,bancoRedValid, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T)


#modelo sem sexo e idade
mod3 <- svyglm(factor(CHIK_PRNT20) ~ d13_febre_vacina + c01_tipo
               +f11_c_g_valor + factor(b06_cor), family = binomial, design = planoTreino)

summary(mod3)

mod3$aic

mod3$acc <- sum((predict(mod3,bancoRedValid, type = "response")>=0.5)==bancoRedValid$CHIK_PRNT20)/nrow(bancoRedValid)

mod3$acc

roc(bancoRedValid$CHIK_PRNT20, predict(mod3, bancoRedValid, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T)


#Modelo com 1 var
mod4 <- svyglm(factor(CHIK_PRNT20) ~ f11_c_g_valor,family = binomial, design = planoTreino)

summary(mod4)

mod4$aic

mod4$acc <- sum((predict(mod4,bancoRedValid, type = "response")>=0.5)==bancoRedValid$CHIK_PRNT20)/nrow(bancoRedValid)

mod4$acc

roc(bancoRedValid$CHIK_PRNT20, predict(mod4,bancoRedValid, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T)



#melhor modelo AIC stepwise
modAIC <- step(mod1)

summary(modAIC)

modAIC$aic

modAIC$acc <- sum((predict(modAIC,bancoRedValid, type = "response")>=0.5)==bancoRedValid$CHIK_PRNT20)/nrow(bancoRedValid)

modAIC$acc

roc(bancoRedValid$CHIK_PRNT20, predict(modAIC,bancoRedValid, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T)







#---- Todas observações

#modelo todas var
mod1 <- svyglm(factor(CHIK_PRNT20) ~ Sexo + b05_idade + d13_febre_vacina + c01_tipo
               +sqrt(f11_c_g_valor) + factor(b06_cor),family = binomial, design = plano)

summary(mod1)

mod1$aic

#melhor valor de corte 0.3
mod1$acc <- sum((predict(mod1, type = "response")>=0.3)==bancoRed$CHIK_PRNT20)/nrow(bancoRed)

mod1$acc

roc(bancoRed$CHIK_PRNT20, predict(mod1, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T)

#melhor valor de corte 0.3 (testar 0.7)
conf_mat <- confusionMatrix(factor(as.numeric(predict(mod1, type = "response")>=0.3)),
                            factor(bancoRed$CHIK_PRNT20), positive = "1")
conf_mat$table
conf_mat$byClass[c("Sensitivity", "Specificity")]



#modelo sem sexo
mod2 <- svyglm(factor(CHIK_PRNT20) ~ b05_idade  + d13_febre_vacina + c01_tipo
               +f11_c_g_valor + factor(b06_cor),family = binomial, design = plano)

summary(mod2)

mod2$aic

mod2$acc <- sum((predict(mod2, type = "response")>=0.5)==bancoRed$CHIK_PRNT20)/nrow(bancoRed)

mod2$acc

roc(bancoRed$CHIK_PRNT20, predict(mod2, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T)


#modelo sem sexo e idade
mod3 <- svyglm(factor(CHIK_PRNT20) ~ d13_febre_vacina + c01_tipo
               +f11_c_g_valor + factor(b06_cor), family = binomial, design = plano)

summary(mod3)

mod3$aic

mod3$acc <- sum((predict(mod3, type = "response")>=0.5)==bancoRed$CHIK_PRNT20)/nrow(bancoRed)

mod3$acc

roc(bancoRed$CHIK_PRNT20, predict(mod3, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T)


#Modelo com 1 var
bancoRed2 <- plano$variables

bancoRed2 <- bancoRed2 %>% dplyr::filter(is.na(CHIK_PRNT20)==F)

bancoRed2 <- bancoRed2 %>% dplyr::select(CHIK_PRNT20,peso_teste,Sexo,b05_idade,
                                         d13_febre_vacina,b06_cor,c01_tipo,f11_c_g_valor)

bancoRed2 <- bancoRed2 %>% dplyr::mutate(CHIK_PRNT20 = case_when(
  CHIK_PRNT20 == 'POS' ~ 1,
  CHIK_PRNT20 == 'NEG' ~ 0))

#tirar uma amostra pra validação
#fiz as duas separações do mesmo tamanho das anteriores
# bancoRed2Treino <- bancoRed2[indexTreino,]
# bancoRed2Valid <- bancoRed2[indexValid,]


mod4 <- svyglm(factor(CHIK_PRNT20) ~ f11_c_g_valor,family = binomial, design = plano)

summary(mod4)

mod4$aic

mod4$acc <- sum((predict(mod4,bancoRed, type = "response")>=0.5)==bancoRed$CHIK_PRNT20)/nrow(bancoRed)

mod4$acc

roc(bancoRed2$CHIK_PRNT20, predict(mod4,bancoRed2, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T)



#melhor modelo AIC stepwise
modAIC <- step(mod1)

summary(modAIC)

modAIC$aic

#melhor valor de corte 0.25 (testar 0.07)
modAIC$acc <- sum((predict(modAIC, type = "response")>=0.25)==bancoRed$CHIK_PRNT20)/nrow(bancoRed)

modAIC$acc

roc(bancoRed$CHIK_PRNT20, predict(modAIC, type = "response"),
    percent=TRUE,
    col = "red",
    print.auc = TRUE,
    plot=T)

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
