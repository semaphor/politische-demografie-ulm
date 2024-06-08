# DIE SCHULTERN VIELER ANDERER, DANKE
# ===================================

library(ggridges)
library(ggplot2)
library(knitr)
library(viridis)
library(hrbrthemes)
library(dplyr)
library(DT)
library(forcats)
library(ggpubr)
library(ragg)


# OPTIONEN
# ========

# R:
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/options.html
options(
  OutDec = ",",
  #warn = 10, # Auf negativen Wert setzen, um alle Warnungen zu unterdrücken.
  #TODO in den R chunks Einstellung für warn, error, message prüfen und sauber setzen.
  useFancyQuotes = "UTF-8"
)

# Knittr:
# https://yihui.org/knitr/options/
# https://yihui.org/knitr/hooks/
knit_hooks$set(inline = function(x) {
  prettyNum(x, big.mark=".")
})


#TODO Funktioniert das auch global für den output der R chunks?

# DATEN EINLESEN UND AUFBEREITEN
# ==============================

# Ausgangsdaten im Unterordner /daten.
# Zur Datenminimierung wurde vor dem Hochladen clean.R auf unseren Ausgangsdaten ausgeführt.
# Das Skript für den Download von Demografiedaten über die API der Statistischen Ämter findet sich in datenOrganisieren.R.


# KANDIDAT_INNEN/LISTEN:
#TODO Lfd.-Nr. wieder reinnehmen?
listen <- read.csv('daten/kandidat_innen-2024-06-09.csv')
colnames(listen) <- c('Beruf', 'Geburtsjahr', 'Stadtteil', 'Liste')
listen$Stadtteil <- as.factor(listen$Stadtteil)
listen$Liste <- as.factor(listen$Liste)
# Alter aus Geburtsjahr errechnen:
listen <- listen %>% mutate(Alter = 2024 - Geburtsjahr)
# Urspr. Berufsangabe in neue Spalte sichern:
listenHelper <- cbind(NA, listen$Beruf, listen)
names(listenHelper) <- c("Tätigkeit", "BerufAngabe", names(listen))
listen <- listenHelper
rm(listenHelper)
# Nach Median aufsteigend sortieren (schonmal hier, wird in den meisten Plots so gemacht):
listen <- listen %>% 
  mutate(Liste = fct_reorder(Liste, Alter, .fun='median'))
# n zählen:
listenLabels <- listen %>%
  group_by(Liste) %>%
  summarise(n = n(), md = median(Alter)) %>%
  mutate(text = paste0(Liste, '\n(n=', n, ', MD=', md, ')'))


# WAHLBERECHTIGTE:
#TODO offizielle Quelle finden/anfragen
wahlberechtigte=92000 # SWP-Artikel [@swp:wahlunterlagen]


#AKTUELLER GEMEINDERAT:
aktRat <- read.csv('daten/gemeinderat_2024-03_aktueller.csv')
aktRat <- aktRat %>% mutate(Alter = 2024 - Geburtsjahr)


#AKTUELLER GEMEINDERAT:
ratNeu <- read.csv('daten/gemeinderat_2024-06-09-neuer.csv')
ratNeu <- ratNeu %>% mutate(Alter = 2024 - Geburtsjahr)


# ALTER ULM, Ulmer Statisitik 2023:
demografieUlm2023 <- read.csv('daten/demografie_ulm-2023-12-31-ulmer_statistik.csv')
names(demografieUlm2023)[11] <- "Wohnbevölkerung"
# Upsampling, so irgendwie (das geht sicher besser?)
demografieUlm2023Distr <- vector()
for (i in seq_along(demografieUlm2023$Wohnbevölkerung)) {
  demografieUlm2023Distr <- c(demografieUlm2023Distr, rep(demografieUlm2023$Alter.von[i], round(demografieUlm2023$Wohnbevölkerung[i])))
}
demografieUlm2023Distr <- as.data.frame(demografieUlm2023Distr)
demografieUlm2023Distr <- cbind(demografieUlm2023Distr,'Ulm')
names(demografieUlm2023Distr) <- c('Alter', 'Liste')
demografieUlm2023Distr <- cbind(NA, NA, NA, NA, NA, demografieUlm2023Distr)
names(demografieUlm2023Distr) <- c('Tätigkeit', 'BerufAngabe', 'Beruf', 'Geburtsjahr', 'Stadtteil', 'Alter', 'Liste')

# Extra data.frame: listen + demografieUlmDistr:
listenMitDemografieUlm2023 <- rbind(demografieUlm2023Distr, listen)

# Median:
demografieUlm2023Median = median(demografieUlm2023Distr$Alter)

# Gesamtzahl, mit einem kleinen Check:
if (sum(demografieUlm2023$Wohnbevölkerung) != length(demografieUlm2023Distr$Alter)) print("FEHLER: Ups, da ist was schief gegangen.")
gesamt <- sum(demografieUlm2023$Wohnbevölkerung)

unterWahlalter = sum(demografieUlm2023$Wohnbevölkerung[demografieUlm2023$Alter.von <= 15]);


# ALTER ULM, 2018, RDB;
# (Regionaldatenbank Deutschland, regionalstatistik.de):
demografieUlm2018regio <- read.csv(file="daten/demografie_ulm-2018-12-31-rdb.csv")
# Upsampling, so irgendwie (das geht sicher besser?)
demografieUlm2018regioDistr <- vector()
for (i in seq_along(demografieUlm2018regio$Wohnbevölkerung)) {
  demografieUlm2018regioDistr <- c(demografieUlm2018regioDistr, rep(demografieUlm2018regio$Alter.von[i], round(demografieUlm2018regio$Wohnbevölkerung[i])))
}
demografieUlm2018regioDistr <- as.data.frame(demografieUlm2018regioDistr)
demografieUlm2018regioDistr <- cbind(demografieUlm2018regioDistr,'Ulm')
names(demografieUlm2018regioDistr) <- c('Alter', 'Liste')
demografieUlm2018regioDistr <- cbind(NA, NA, NA, NA, NA, demografieUlm2018regioDistr)
names(demografieUlm2018regioDistr) <- c('Tätigkeit', 'BerufAngabe', 'Beruf', 'Geburtsjahr', 'Stadtteil', 'Alter', 'Liste')


# ALTER ULM, 2022, DESTATIS:
demografieUlm2022deGenderBi <- read.csv('daten/demografie_ulm-2022-12-31-statistisches_bundesamt.csv')
demografieUlm2022deGenderBi$Geschlecht <- as.factor(demografieUlm2022deGenderBi$Geschlecht)
# Nur zwei Geschlechter? Raus damit:
demografieUlmAnzAltersgruppen <- length(unique(demografieUlm2022deGenderBi$Altersgruppe))
demografieUlmMtx <- matrix(nrow = demografieUlmAnzAltersgruppen, ncol = 2)
demografieUlmMtx[,1] <- sort(unique(demografieUlm2022deGenderBi$Altersgruppe))
for (pos in seq(demografieUlmAnzAltersgruppen)) {
  demografieUlmMtx[pos,2] <- sum(demografieUlm2022deGenderBi$Personen[demografieUlm2022deGenderBi$Altersgruppe == demografieUlmMtx[pos,1]])
}
demografieUlm2022de <- data.frame(demografieUlmMtx)
names(demografieUlm2022de) <- names(demografieUlm2022deGenderBi)[2:3]
# Altersgruppen in Schritten von zehn Jahren:
demografieUlm2022de <- demografieUlm2022de %>% mutate(AltersgruppeZehner = cut(Altersgruppe, breaks = seq(from = 0, to = 80, by = 10), right = FALSE))
levels(demografieUlm2022de$AltersgruppeZehner)[levels(demografieUlm2022de$AltersgruppeZehner) == "[70,80)"]  <- "[70,...)" 
demografieUlm2022deZehner <- demografieUlm2022de %>% select(any_of(c("AltersgruppeZehner", "Personen"))) %>%
  group_by(AltersgruppeZehner) %>% 
  summarise(across(everything(), sum))
rm(pos, demografieUlmMtx, demografieUlmAnzAltersgruppen)
# Upsampling, so irgendwie (das geht sicher besser?)
demografieUlm2022deDistr <- vector()
for (i in seq_along(demografieUlm2022de$Personen)) {
  mittleresAlter <- demografieUlm2022de$Altersgruppe[i] + (demografieUlm2022de$Altersgruppe[i+1] - demografieUlm2022de$Altersgruppe[i]) / 2
  if (is.na(mittleresAlter)) mittleresAlter <- 75
  demografieUlm2022deDistr <- c(demografieUlm2022deDistr, rep(mittleresAlter ,round(demografieUlm2022de$Personen[i])))
}
demografieUlm2022deDistr <- as.data.frame(demografieUlm2022deDistr)
demografieUlm2022deDistr <- cbind(NA, NA, NA, NA, 'Ulm', demografieUlm2022deDistr)
names(demografieUlm2022deDistr) <- c('BerufAngabe', 'Beruf', 'Geburtsjahr', 'Stadtteil', 'Liste', 'Alter')
