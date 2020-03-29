library(dplyr)
library(ggplot2)

colunas <- c(
  "city", "city_ibge_code", "confirmed", 
  "date", "deaths", "estimated_population_2019",
  "place_type", "state"
)

dados <- data.table::fread(
  file = 'projecoes/dados/bruto/2020-03-28-brasil-covid19-brasil-io.csv',
  sep = ';', dec = ',', data.table = FALSE, select = colunas
  )

estado <- dados %>% 
  filter(place_type == 'state') %>% 
  select(-city) %>% 
  rename(estate_ibge_code = city_ibge_code) %>% 
  mutate(date = as.Date(date))

cidades <- dados %>% 
  filter(place_type == 'city') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(deaths = if_else(is.na(deaths), 0, deaths))

saveRDS(estado, file = 'projecoes/dados/processado/covid-estado.rds')
saveRDS(cidades, file = 'projecoes/dados/processado/covid-cidades.rds')
