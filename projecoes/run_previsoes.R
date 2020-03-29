
# Dependências
# install.packages('deSolve')
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages('scales')
# install.packages("data.table")

library(deSolve)

## Carregando funçoes
func <- new.env()
source('projecoes/helper/funcoes.R', local = func, encoding = 'UTF-8')

## Carregando dados
estados <- readRDS('projecoes/dados/processado/covid-estado.rds')
cidades <- readRDS('projecoes/dados/processado/covid-cidades.rds')

## Definindo parâmetros, caso Distrito Federal
valores_iniciais <- func$calcular_valores_iniciais(
  infectados = 242,
  recuperados = 0,
  expostos = 2,
  populacao = 3015268
) 

parametros <- func$calcular_parametros(
  taxa_contato = 10,
  prob_transmissao = .17,
  periodo_infeccao = 8,
  periodo_latencia = 7
)

## Fazendo previsões
previsoes <- func$prever(valores_iniciais, parametros, periodos = 10)

## Processando previsões
previsoes_ <- func$processar_previsoes( # testando para Distrito Federal
  previsoes = previsoes,
  col_time = 'time',
  data_inicio = '2020-03-26',
  ordem_grand = 1000 # Dividido população por 1.000
)

## Plotando previsões
previsoes2_ <- previsoes_ %>% 
  filter(tipo != 'Suscetíveis')

func$plotar_previsoes(
  previsoes = previsoes2_,
  subtitulo = 'Distrito Federal',
  ordem_grand = 'Mil '
)

