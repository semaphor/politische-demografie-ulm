library(dplyr)

data <- read.csv('daten/raw_kandidat_innen.csv')

cleaned_data <- data %>% select(-any_of(c('Name')))

write.csv(cleaned_data, 'daten/kandidat_innen-2024-09-06.csv')



data <- read.csv('daten/raw_aktueller_gemeinderat_2024-03.csv')

cleaned_data <- data %>% select(-any_of(c('Familienname..Vornamen..ggf..zusätzliche.Angaben')))

write.csv(cleaned_data, 'daten/aktueller_gemeinderat_2024-03.csv')
