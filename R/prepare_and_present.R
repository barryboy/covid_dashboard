library(ggplot2)
library(ggrepel)
#data <- read.csv(url('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv'))

ma <- function(x, n = 5){
  stats::filter(x, rep(1 / n, n), sides = 2)
}

prepare_data <- function(country, data, window){
  df <- data[data$countriesAndTerritories == country, c('dateRep', 'cases', 'deaths')]
  df$dateRep <- as.character(strptime(df$dateRep, '%d/%m/%Y'))
  df <- df[order(df$dateRep),]
  df$cumCases <- cumsum(df$cases)
  df$cumDeaths <- cumsum(df$deaths)
  df$avgCases <- ma(df$cases, window)
  df$avgDeaths <- ma(df$deaths, window)
  df$country <- country
  df$label <- ''
  df <- df[complete.cases(df),]
  max_date <- max(df$dateRep)
  df[df$dateRep == max_date,]$label <- stringr::str_replace_all(country, '_', ' ')
  df
}

data_countries <- lapply(list('Poland', 'Italy', 'United_States_of_America', 'Romania', 'United_Kingdom', 'Sweden', 'Germany'), prepare_data, data, 5)
df <- do.call('rbind', data_countries)
df <- df[df$cumCases > 100 & df$cumDeaths > 10,]


ggplot(df, aes(x = log10(cumDeaths), y = log10(avgDeaths), color = country, fill = country, label=label)) +
  geom_point() +
  stat_smooth(se = F, method = 'loess') +
  geom_text_repel() +
  theme(legend.position = 'none')

ggplot(df, aes(x = log10(cumCases), y = log10(avgDeaths), color = country, fill = country, label=label)) +
  geom_point() +
  stat_smooth(se = F, method = 'loess') +
  geom_text_repel() +
  theme(legend.position = 'none')

ggplot(df, aes(x = dateRep, y = log10(cumCases), color = country, fill = country, group = country, label=label)) +
  geom_point() +
  geom_line() +
  geom_text_repel() +
  theme(legend.position = 'none')

ggplot(df, aes(x = dateRep, y = log10(avgCases), color = country, fill = country, group = country, label=label)) +
  geom_point() +
  geom_line() +
  geom_text_repel() +
  theme(legend.position = 'none')

ggplot(df, aes(x = dateRep, y = log10(avgDeaths), color = country, fill = country, group = country, label=label)) +
  geom_point() +
  geom_line() +
  geom_text_repel() +
  theme(legend.position = 'none')
