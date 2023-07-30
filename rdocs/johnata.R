source("rdocs/source/packages.R")
pacman::p_load(effects,car,erer,VGAM, caret, 
               janitor,ltm,FSA,rcompanion,
               PerformanceAnalytics, MASS,survey)
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

plano$variables <- plano$variables %>%
  mutate(b06_cor = case_when(
    b06_cor == 'Branca' ~ 'nao negro',
    b06_cor == 'Indígena ' ~ 'nao negro',
    b06_cor == 'Amarela (origem japonesa, chinesa, coreana etc.)' ~ 'nao negro',
    b06_cor == 'Preta' ~ 'negro',
    b06_cor == 'Parda (mulata, cabocla, cafuza, mameluca ou mestiça)' ~ 'negro',
  ))
plano$variables <- plano$variables %>%
  mutate(d13_febre_vacina = na_if(d13_febre_vacina, 'Não sabe'))

mod1 <- svyglm(factor(CHIK_PRNT20) ~ factor(Sexo) + b05_idade + d13_febre_vacina + c01_tipo
       +f11_c_g_valor + factor(b06_cor),family = binomial,design = plano)
summary(mod1)







