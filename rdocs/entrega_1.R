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

library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)

## Instalação Tinytex
library(tinytex)
tinytex::install_tinytex()

## Excel
Relatorio_Vendas <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "relatorio_vendas")
Infos_Vendas <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_vendas")
Infos_Produtos <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_produtos")

## Renomeando as variáveis do banco
Infos_Produtos <- rename(Infos_Produtos, ItemID = Ite3ID)
Infos_Vendas <- rename(Infos_Vendas, SaleID = Sal3ID)

## Juntando as variáveis do banco de dados
dados_inner <- dplyr::inner_join(Infos_Produtos, Infos_Vendas, by = "ItemID")
dados_inner2 <- dplyr::inner_join(Relatorio_Vendas, dados_inner, by = "SaleID")

## Adicionando uma coluna nova no banco de dados
dados_inner3 <- dados_inner2 %>% mutate(PreçoTotal = Quantity * UnityPrice)

## Modificando a coluna do banco de dados
dados_inner4 <- dados_inner3 %>% mutate(Date = year(Date))

## Adicionando o preço em reais
dados_inner5 <- dados_inner4 %>% mutate(PreçoTotalReais = PreçoTotal * 5.31)

## Fazendo a média das lojas registradas nos anos de 1880 até 1889
dados_inner6 <- dados_inner5 %>% group_by(Date) %>% summarise(PreçoFinal = mean(PreçoTotalReais))

##Mostrando, por meio de um gráfico, a análise feita das médias anuais das lojas
GraficoAnalítico <- ggplot(dados_inner6) +
  aes(x=Date, y=PreçoFinal, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Período de Tempo(Anos)", y="Média das lojas(Reais)") +
  theme_estat() +
  scale_x_continuous(breaks = seq(1880,1889))
ggsave("series_uni.pdf", width = 158, height = 93, units = "mm")
GraficoAnalítico
