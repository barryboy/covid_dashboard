library(ggplot2)
data <- read.csv(url('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv'))

ma <- function(x, n = 5){
  stats::filter(x, rep(1 / n, n), sides = 2)
}

prepare_data <- function(country, data, window){
  df <- data[data$countriesAndTerritories == country, c('dateRep', 'cases', 'deaths')]
  df$dateRep <- strptime(df$dateRep, '%d/%m/%Y')
  df <- df[order(df$dateRep),]
  df$cumCases <- cumsum(df$cases)
  df$cumDeaths <- cumsum(df$deaths)
  df$avgCases <- ma(df$cases, window)
  df$country <- country
  df <- df[complete.cases(df),]
  df
}

data_countries <- lapply(list('Poland', 'Spain', 'United_States_of_America', 'Romania', 'United_Kingdom'), prepare_data, data, 10)
df <- do.call('rbind', data_countries)
df <- df[df$cumCases > 100,]

ggplot(df, aes(x = log10(cumCases), y = log10(avgCases), color = country, fill = country)) +
  geom_point() +
  stat_smooth(se = F, method = 'loess')

ggplot(df, aes(x = dateRep, y = log10(cumCases), color = country, fill = country, group = country)) +
  geom_point() +
  geom_line()
