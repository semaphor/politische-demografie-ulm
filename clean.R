library(dplyr)

data <- read.csv('raw_kandidat_innen.csv')

cleaned_data <- data %>% select(-any_of(c('Name')))

write.csv(cleaned_data, 'kandidat_innen.csv')