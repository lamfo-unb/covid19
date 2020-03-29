library(ggplot2)
library(dplyr)



calcular_valores_iniciais <- function(
  infectados = 1, recuperados = 0, expostos = 2, populacao = 211289547){
  
  N <- populacao
  X = infectados          # infeccioso (infectados)
  Y = recuperados                    # recuperados
  Z = expostos                    # expostos (2 ?)
  W = N - (X + Z)
  
  init_values = c(S = W/N, E = X/N, I = Y/N, R = Z/N)
  return(init_values)
}

calcular_parametros <- function(
  taxa_contato = 10, prob_transmissao = .17, periodo_infeccao = 8, periodo_latencia = 7){
  beta_value = taxa_contato * prob_transmissao
  gamma_value = 1 / periodo_infeccao
  delta_value = 1 / periodo_latencia
  #Ro = beta_value / gamma_value
  par_list = c(beta = beta_value, gamma = gamma_value, delta = delta_value)
  
  return(par_list)
}

SEIR <- function(current_timepoint, state_values, parameters){
  # criando variaveis de estado (local variables)
  S = state_values[1]        # suscetiveis
  E = state_values[2]        # expostos
  I = state_values[3]        # infecciosos
  #R = state_values[4]        # recuperados
  
  with( 
    as.list(parameters),     # parameters recebe variaveis nomeadas 
    {
      # calcula derivadas
      dS = (-beta * S * I)
      dE = (beta * S * I) - (delta * E)
      dI = (delta * E) - (gamma * I)
      dR = (gamma * I)
      
      # combine results
      results = c(dS, dE, dI, dR)
      list(results)
    }
  )
}

adicionar_datas <- function(previsoes, data_inicial, col_time = 'time'){
  num_dias <- nrow(previsoes)
  colunas <- base::setdiff(names(previsoes), col_time)
  previsoes <- previsoes[,colunas]
  date <- seq.Date(as.Date(data_inicial), by = 'day', length.out = num_dias)
  
  data.frame(
    time = date,
    previsoes,
    stringsAsFactors = FALSE
  )
  
}

obter_populacao_absoluta <- function(previsoes, col_time = 'time', pop = NULL){
  pop_brasil <- 211289547
  if (is.null(pop)) pop <- pop_brasil
  
  date <- previsoes[[col_time]]
  colunas <- base::setdiff(names(previsoes), col_time)
  previsoes <- previsoes[,colunas]
  previsoes[] <- lapply(previsoes, function(x){x*pop})
  
  data.frame(
    time = date,
    previsoes,
    stringsAsFactors = FALSE
  )
}

renomear_variaveis <- function(previsoes){
  cols <- c('I' = 'Infectados', 'R' = 'Recuperados', 'S' = 'Suscetíveis',
            'E' = 'Expostos', 'time' = 'time')
  
  colunas <- names(previsoes)
  nov_cols <- lapply(colunas, function(x){cols[[x]]})
  names(previsoes) <- nov_cols
  return(previsoes)
}

pivotear_long <- function(previsoes){
  colunas_var <- c('Infectados','Recuperados','Suscetíveis','Expostos')
  tidyr::pivot_longer(previsoes, cols = colunas_var, names_to = 'tipo', values_to = 'casos')
}

processar_previsoes <- function(previsoes, col_time, data_inicio, pop = NULL){
  previsoes <- obter_populacao_absoluta(previsoes, col_time, pop)
  previsoes <- adicionar_datas(previsoes, data_inicio, col_time)
  previsoes <- renomear_variaveis(previsoes)
  pivotear_long(previsoes)
}

plotar_previsoes <- function(previsoes){
  previsoes %>% 
    ggplot(aes(x = time, y = casos, color = tipo)) +
    geom_line() +
    labs(
      x = NULL,
      y = 'Casos',
      color = NULL
    )
}


prever <- function(valores_iniciais, tempo, parametros){
  output <- deSolve::lsoda(valores_iniciais, tempo, SEIR, parametros)
  previsoes <- as.data.frame(output)
  return(previsoes)
}
