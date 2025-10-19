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

## Excel
Infos_Clientes <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_clientes")

## Renomeando as variáveis do banco
rename(Infos_Clientes, ClientID = Cli3ntID)

## Transformando lbs em kg, dm em cm
Infos_Clientes2 <- Infos_Clientes %>% mutate(Weight_kg = Weight_lbs * 0.4536)
Infos_Clientes3 <- Infos_Clientes2 %>% mutate(Height_cm = Height_dm * 10)

## Criando um dataframe somente com as variáveis de interesse
Infos_Clientes4 <- Infos_Clientes3 %>% group_by(Name) %>% summarise(Weight_kg, Height_cm)

## Fazendo um gráfico para analisar a relação entre peso X altura entre os clientes
GraficoPesoAltura <- ggplot(Infos_Clientes4) +
  aes(x = Weight_kg, y = Height_cm) +
  geom_point(colour = "#A11D21", size = 1) +
  labs(
    x = "Peso dos Clientes (Kg)",
    y = "Altura dos Clientes (Cm)"
  ) +
  theme_estat() + geom_smooth()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")
GraficoPesoAltura

## Fazendo o Coeficiente de Correlação de Pearson
cor(Infos_Clientes4$Weight_kg, Infos_Clientes4$Height_cm)
