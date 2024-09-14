# Pakete laden ------------------------------------------------------------

# Tidyverse
library(tidyverse) # Sammlung von Paketen für Data Science

# API
library(rdwd) # Wetterdaten vom DWD


# Grundeinstellungen ------------------------------------------------------

# Arbeitsverzeichnis festlegen
setwd("/Users/davidtrogemann/Documents/Data Science/Vogelbeobachtung/")

# Verzeichnisse definieren
pfad_arbeitsverzeichnis <- getwd()
pfad_datensatz <- file.path(pfad_arbeitsverzeichnis, "Datensatz")
pfad_zwischenergebnisse <- file.path(pfad_arbeitsverzeichnis, "Zwischenergebnisse")

# Sprache ändern
Sys.setlocale("LC_TIME", "de_DE.UTF-8")

# Aktuelles Datum
heutiges_datum <- Sys.Date() # für Dateinamen


# Datenimport -------------------------------------------------------------

# Daten importieren
erkennungen_rohdaten <- read_csv(file.path(pfad_datensatz, "detections.csv"))

glimpse(erkennungen_rohdaten)


# Datenbereinigung --------------------------------------------------------

# Datentypen korrigieren
erkennungen_bereinigt <- 
  erkennungen_rohdaten |> 
  rename(
    datum = Date,
    uhrzeit = Time,
    wissenschaftlicher_name = Sci_Name,
    deutscher_name = Com_Name,
    breitengrad = Lat,
    laengengrad = Lon,
    empfindlichkeit = Sens,
    ueberlappung = Overlap,
    dateiname = File_Name,
    erkennungswahrscheinlichkeit = Confidence,
    schwellenwert = Cutoff,
    kalenderwoche = Week
  ) |>
  mutate(
    wissenschaftlicher_name = as.factor(wissenschaftlicher_name),
    deutscher_name = as.factor(deutscher_name),
    kalenderwoche = as.integer(kalenderwoche),
  ) |>
  select(-dateiname, -ueberlappung) |>
  # Doppelte Datensätze entfernen
  distinct()

# Unterbrechungen in Erkennungen feststellen
unterbrechungen <- 
  erkennungen_bereinigt |>
  mutate(
    tage_differenz = as.integer(coalesce(c(NA, diff(datum)), 0))
  ) |>
  filter(tage_differenz > 1) |>
  mutate(
    ende = datum,
    beginn = datum - tage_differenz,
    # + 2 um Beginn und Ende als Tage einzubeziehen
    dauer_in_tage = as.integer(tage_differenz - 1 + 2)
  ) |>
  select(beginn, ende, dauer_in_tage)

print(unterbrechungen)

# Beginn und Ende des Datensatzes festlegen
beginn_erkennungen <- 
  erkennungen_bereinigt |>
  group_by(datum) |>
  summarise(
    anzahl_erkennungen = n(),
    .groups = "drop"
  ) |> 
  # Näherungswert für vollständige Tage
  filter(anzahl_erkennungen >= 1000) |> 
  slice_min(datum) |> 
  pull(datum) |>
  # Frühsten Beginn soll 01.04.2024 sein
  pmax(as_date("2024-03-01"))

ende_erkennungen <- 
  erkennungen_bereinigt |>
  summarise(max_datum = max(datum)) |>
  mutate(ende_datum = max_datum - days(1)) |>
  pull(ende_datum)

# Nur Vollständige Tage brücksichtigen
erkennungen_bereinigt <- 
  erkennungen_bereinigt |>
  filter(
    # Filtere Datensätze mit Beginn und Ende der Unterbrechung heraus
    # Unterbrochene Tage sind bereits im Datensatz nicht vorhanden
    !(datum %in% unterbrechungen$beginn | datum %in% unterbrechungen$ende),
    # Filtere Datensätze vor Beginn und nach Ende heraus
    datum >= beginn_erkennungen & datum <= ende_erkennungen
  )


# Datenzusammenstellung ---------------------------------------------------

# Jahreszeiten ergänzen
erkennungen_bereinigt_mit_jahreszeiten <- 
  erkennungen_bereinigt |>
  mutate(
    jahreszeit = case_when(
      datum >= make_date(year(datum), 3, 20) & datum < make_date(year(datum), 6, 21) ~ "Frühling",
      datum >= make_date(year(datum), 6, 21) & datum < make_date(year(datum), 9, 23) ~ "Sommer",
      datum >= make_date(year(datum), 9, 23) & datum < make_date(year(datum), 12, 21) ~ "Herbst",
      datum >= make_date(year(datum) - 1, 12, 21) & datum < make_date(year(datum), 3, 20) ~ "Winter"
    ),
    jahreszeit = factor(jahreszeit, levels = c("Frühling", "Sommer", "Herbst", "Winter"))
  )

# Wetterdaten vom DWD ergänzen

# Längen- und Breitengrad der Erkennungen ermitteln
laengengrad <- 
  erkennungen_bereinigt_mit_jahreszeiten |>
  pull(laengengrad) |>
  last()

breitengrad <- 
  erkennungen_bereinigt_mit_jahreszeiten |>
  pull(breitengrad) |>
  last()

# Auswahl der Variablen gemäß Dokumentation
relevante_wettervariablen <- 
  c(
    "air_temperature", 
    "pressure",
    "soil_temperature", 
    "sun",
    "cloudiness",
    "precipitation",
    "wind"
  )

# Passende Wetterstation mit entsprechenden Daten finden
wetterstationen <- 
  as_tibble(
    nearbyStations(
      lat = breitengrad, 
      lon = laengengrad, 
      radius = 50, 
      res = "hourly", 
      per = "recent", 
      var = relevante_wettervariablen
    )
  ) |>
  # Fehlerhaften Datensatz entfernen
  drop_na() |>
  arrange(desc(bis_datum), dist) |>
  group_by(var) |>
  # Auswahl der nächsten Station mit der relevanten Variable
  slice_max(
    order_by = bis_datum, 
    n = 1, 
    with_ties = FALSE
  ) |>
  ungroup() |>
  select(
    station = Stations_id,
    ort = Stationsname,
    variable = var,
    entfernung = dist,
    link = url
  ) |> 
  arrange(entfernung)

print(wetterstationen)

# Funktion zur Datenabfrage und -konvertierung
wetterdaten_laden <- 
  function(link) {
    dataDWD(link, read = FALSE, dir = tempdir()) |>
    readDWD(varnames = TRUE) |>
    as_tibble() |>
    select(MESS_DATUM, where(is.numeric), -STATIONS_ID)
  }

# Kombinierte Wetterdaten aus verschiedenen Quellen
wetterdaten <- 
  wetterstationen |>
  # Ein Tibble pro Variable erzeugen
  mutate(einzelne_wetterdaten = map(link, wetterdaten_laden)) |>
  # Tibbles kombinieren
  select(einzelne_wetterdaten) |>
  unnest(cols = c(einzelne_wetterdaten)) |>
  # Alle Werte aufsummieren um Unterscheidung nach Station zu entfernen
  group_by(MESS_DATUM) |>
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) |>
  ungroup() |>
  mutate(
    datum = as.Date(MESS_DATUM),
    uhrzeit = format(MESS_DATUM, "%H:%M:%S"),
    niederschlagsindikator = factor(
      RS_IND.Niederschlagsindikator, 
      levels = c(0:1), 
      labels = c(
        "Kein Niederschlag", 
        "Niederschlag"
      )
    ),
    # Im Datensatz sind offenbar NA als -1 angegeben
    bedeckungsgrad_in_okta = ifelse(
      V_N.Bedeckungsgrad == -1, NA, V_N.Bedeckungsgrad
    ),
    bedeckungsgrad_in_okta = factor(
      bedeckungsgrad_in_okta,
      levels = c(0:8),
      labels = c(
        "wolkenlos", 
        "sonnig", 
        "heiter", 
        "leicht bewölkt", 
        "wolkig", 
        "bewölkt", 
        "stark bewölkt", 
        "fast bedeckt", 
        "bedeckt"
      )
    )
  ) |>
  select(
    datum, 
    uhrzeit, 
    lufttemperatur_in_grad = TT_TU.Lufttemperatur,
    luftfeuchtigkeit_in_prozent = RF_TU.Relative_Feuchte,
    luftdruck_in_hpa = P.Luftdruck_NN,
    bodentemperatur_in_grad = V_TE005.Erdbodentemperatur_005cm,
    niederschlagsindikator,
    niederschlagshoehe_in_mm = R1.Niederschlagshoehe,
    sonnenscheindauer_in_min = SD_SO.Sonnenscheindauer,
    bedeckungsgrad_in_okta,
    windgeschwindigkeit_in_m_pro_s = F.Windgeschwindigkeit
  )

summary(wetterdaten)
glimpse(wetterdaten)

# Wetterbedigungen ergänzen
erkennungen_bereinigt_mit_jahreszeiten_mit_wetter <- 
  erkennungen_bereinigt_mit_jahreszeiten |>
  mutate(stunde = hour(hms(uhrzeit))) |>
  left_join(
    wetterdaten |>
      mutate(stunde = hour(hms(uhrzeit))),
    by = c("datum", "stunde")
  ) |>
  select(-stunde, -uhrzeit.y) |>
  rename(uhrzeit = uhrzeit.x)

erkennungen <- erkennungen_bereinigt_mit_jahreszeiten_mit_wetter

summary(erkennungen)
glimpse(erkennungen)
view(erkennungen)


# Zwischenergebnisse ------------------------------------------------------

# Speichern als CSV
write_csv(
  erkennungen, 
  file.path(
    pfad_zwischenergebnisse, str_glue("erkennungen_{heutiges_datum}.csv")
  )
)