
# install.packages('nCov2019')
# install.packages('deSolve')

require(nCov2019)
require(deSolve)

func <- new.env()
source('projecoes/helper/funcoes.R', local = func, encoding = 'UTF-8')

if (!exists('data_br')) {
  data <- load_nCov2019()
  data_global <- data["global"] #extract global data
  data_br <- filter(data_global, country == 'Brazil')
}

valores_iniciais <- func$calcular_valores_iniciais() # Vem dos dados
parametros <- func$calcular_parametros() # Vem dos inputs apps
tempo <- 1:100
previsoes <- func$prever(valores_iniciais, tempo, parametros)

previsoes_ <- func$processar_previsoes(
  previsoes = previsoes,
  col_time = 'time',
  data_inicio = '2020-02-26'
)

func$plotar_previsoes(previsoes_)


obter_infectados <- function(resumo_estado, estado_){
  
  resumo_estado %>% 
    filter(variavel == 'Confirmados') %>%
    filter(estado == estado_) %>% 
    arrange(date) %>% 
    .[['quantidade']] %>% 
    tail(1)
  
}

obter_infectados(resumo_estado, 'Distrito Federal')


