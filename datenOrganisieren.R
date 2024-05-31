library(dplyr)
library(stringr)
library(wiesbaden)


# Altersdaten von der Regionaldatenbank Deutschland herunterladen und aufbereiten:

download  <- retrieve_data(
  genesis = c(user="", password="", db="regio"),
  tablename = "12411KJ013",
  regionalmerkmal = "KREISE", regionalschluessel = "08421",
  startyear = "2018", endyear = "2018" 
  )
tmp <- download
rm(download)
tmp <- tmp %>%
                          mutate(Alter.von = ALTX76)
tmp$Alter.von <- tmp$Alter.von %>%
                          str_replace("^ALT0", "")
demografieUlm2018regio <- tmp[tmp$GES == "GESM", ] %>%
                          mutate(Wohnbevölkerung..männlich = BEVSTD_val)
# TODO besser Funktion statt hart gecodetem 6:
demografieUlm2018regio$Wohnbevölkerung..weiblich <- tmp[tmp$GES == "GESW", 6]
rm(tmp)
# Für Altersgruppen 75-79, 80-84, 85-89 und über 90 Anzahl Personen auf die jew. 5 Jahre gleichm. verteilen:
for (i in c("75B80", "80B85", "85B90","90UM")) {
  j = as.numeric(i %>% str_replace("[BU].*.$", ""))
  for (k in seq(j + 4, j)) {
    demografieUlm2018regio <-  demografieUlm2018regio %>%
      rbind(
        demografieUlm2018regio[demografieUlm2018regio$Alter.von == i, ] %>%
        mutate(
          Alter.von = k,
          Wohnbevölkerung..männlich = round(Wohnbevölkerung..männlich/5, 0),
          Wohnbevölkerung..weiblich = round(Wohnbevölkerung..weiblich/5, 0)
        )
      )
  }
  demografieUlm2018regio <- demografieUlm2018regio[!(demografieUlm2018regio$Alter.von == i),]
}
rm(i, j, k)
demografieUlm2018regio$Alter.von <- as.numeric(demografieUlm2018regio$Alter.von)
demografieUlm2018regio$Wohnbevölkerung <- demografieUlm2018regio$Wohnbevölkerung..weiblich + demografieUlm2018regio$Wohnbevölkerung..männlich

write.csv(demografieUlm2018regio, file="daten/demografie_ulm-2018-12-31-rdb.csv")

