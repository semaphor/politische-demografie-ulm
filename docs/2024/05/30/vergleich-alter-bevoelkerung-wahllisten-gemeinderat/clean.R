library(dplyr)


data <- read.csv('../../daten/raw_kandidat_innen.csv')

cleaned_data <- data %>% select(-any_of(c('Lfd...Nr.','Name')))

write.csv(cleaned_data, '../../daten/kandidat_innen-2024-06-09.csv', row.names=FALSE)



data2 <- read.csv('../../daten/raw_aktueller_gemeinderat_2024-03.csv')

cleaned_data2 <- data %>% select(-any_of(c('Familienname..Vornamen..ggf..zusätzliche.Angaben')))

write.csv(cleaned_data, '../../daten/gemeinderat_2024-03_aktueller.csv')
