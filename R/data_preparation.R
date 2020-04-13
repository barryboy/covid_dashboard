data <- read.csv(url('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv'), stringsAsFactors = F)


prepare_data <- function(country, data){
  df <- data[data$countriesAndTerritories == country, c('dateRep', 'cases', 'deaths', 'popData2018')]
  if (nrow(df) < 10) {return(NULL)}
  
  df$country <- country
  df$population <- df$popData2018
  
  df$date <- as.Date(strptime(df$dateRep, '%d/%m/%Y'))
  df$dateRep <- NULL
  
  df$label <- ''
  df[df$date == max(df$date),]$label <- gsub("_", " ", country)
  
  df <- df[order(df$date),]
  df$cum_cases <- cumsum(df$cases)
  df$cum_deaths <- cumsum(df$deaths)
  
  df <- df[df$cum_cases > 0,]
  
  date_0 <- min(df$date)
  date_100 <- min(df[df$cum_cases>=100,]$date)
  date_1000 <- min(df[df$cum_cases>=1000,]$date)
  
  df$n_day_0 <- df$date - date_0 + 1
  df$n_day_100 <- df$date - date_100 + 1
  df$n_day_1000 <- df$date - date_1000 + 1
  
  df$avg_cases_3 <- as.numeric(stats::filter(df$cases, rep(1 / 3, 3), sides = 1))
  df$avg_cases_5 <- as.numeric(stats::filter(df$cases, rep(1 / 5, 5), sides = 1))
  
  df$avg_deaths_3 <- as.numeric(stats::filter(df$deaths, rep(1 / 3, 3), sides = 1))
  df$avg_deaths_5 <- as.numeric(stats::filter(df$deaths, rep(1 / 5, 5), sides = 1))
  
  df[, c('country', 'date', 'n_day_0', 'n_day_100', 'n_day_1000', 'population', 'label'
         , 'cases', 'deaths', 'cum_cases', 'cum_deaths'
         , 'avg_cases_3', 'avg_cases_5', 'avg_deaths_3', 'avg_deaths_5')]
}

data_countries <- lapply(unique(data$countriesAndTerritories), prepare_data, data)
df <- do.call('rbind', data_countries)
