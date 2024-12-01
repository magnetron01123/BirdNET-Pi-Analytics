# Pakete laden ------------------------------------------------------------

# Tidyverse
library(tidyverse) # Sammlung von Paketen für Data Science

# API
library(rdwd) # Wetterdaten vom DWD


# Verzeichnisse definieren ------------------------------------------------

pfad_arbeitsverzeichnis <- getwd()

pfad_datensatz <- file.path(pfad_arbeitsverzeichnis, "Datensatz")

pfad_zwischenergebnisse <- file.path(pfad_arbeitsverzeichnis, "Zwischenergebnisse")


# Datenimport -------------------------------------------------------------

# Daten importieren
erkennungen_rohdaten <- read_csv(file.path(pfad_datensatz, "detections.csv"))

glimpse(erkennungen_rohdaten)


# Datenbereinigung --------------------------------------------------------

# Variablen benennen und Datentypen korrigieren
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
  select(-dateiname, -ueberlappung)

# Unterbrechungen in Erkennungen feststellen
unterbrechungen <- 
  erkennungen_bereinigt |>
  mutate(
    tage_differenz = as.integer(coalesce(c(NA, diff(datum)), 0))
  ) |>
  filter(tage_differenz > 1) |>
  transmute(
    ende = datum,
    beginn = datum - tage_differenz,
    dauer_in_tage = as.integer(ende - beginn + 1)
  )

print(unterbrechungen)

# Beginn und Ende des Datensatzes festlegen
beginn_erkennungen <- 
  erkennungen_bereinigt |>
  filter(datum >= as.Date("2024-03-01")) |>
  count(datum, name = "anzahl_erkennungen") |>
  # In dem Datensatz sind Testdaten vorhanden
  # 500 ist ein Näherungswert für vollständige Tage
  filter(anzahl_erkennungen >= 250) |>
  slice_min(datum) |>
  pull(datum)

ende_erkennungen <- 
  erkennungen_bereinigt |> 
  pull(datum) |> 
  # Das aktuellste Datum könnte eventuell noch nicht abgeschlossen sein
  max() - 1

# Nur Vollständige Tage ohne Testdaten berücksichtigen
erkennungen_bereinigt <- 
  erkennungen_bereinigt |>
  filter(
    # Filtere Datensätze mit Beginn und Ende der Unterbrechung heraus
    # Unterbrochene Tage sind bereits im Datensatz nicht vorhanden
    !(datum %in% unterbrechungen$beginn | datum %in% unterbrechungen$ende),
    # Filtere Datensätze vor Beginn und nach Ende heraus
    datum >= beginn_erkennungen & datum <= ende_erkennungen
  )


# Wetterdaten -------------------------------------------------------------

# Längen- und Breitengrad der Erkennungen ermitteln
laengengrad <- 
  erkennungen_bereinigt |>
  pull(laengengrad) |>
  last()

breitengrad <- 
  erkennungen_bereinigt |>
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

# Funktion zur Datenabfrage vom DWD
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
  transmute(einzelne_wetterdaten = map(link, wetterdaten_laden)) |>
  # Tibbles kobinieren
  unnest(cols = c(einzelne_wetterdaten)) |>
  # Alle Werte aufsummieren um Unterscheidung nach Station zu entfernen
  group_by(MESS_DATUM) |>
  summarise(
    across(everything(), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(
    datum = as_date(MESS_DATUM),
    uhrzeit = format(MESS_DATUM, "%H:%M:%S"),
    niederschlagsindikator = factor(
      if_else(
        RS_IND.Niederschlagsindikator %in% 0:1,
        RS_IND.Niederschlagsindikator,
        NA
      ),
      levels = c(0:1), 
      labels = c(
        "Kein Niederschlag", 
        "Niederschlag"
      )
    ),
    # Im Datensatz sind offenbar NA als -1 angegeben
    bedeckungsgrad_in_okta = factor(
      if_else(
        V_N.Bedeckungsgrad %in% 0:8,
        V_N.Bedeckungsgrad,
        NA
      ),
      levels = 0:8,
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

# Wetterbedigungen ergänzen
erkennungen_bereinigt_mit_jahreszeiten_mit_wetter <- 
  erkennungen_bereinigt_mit_jahreszeiten |>
  mutate(stunde = hour(hms(uhrzeit))) |>
  left_join(
    wetterdaten |>
      mutate(
        stunde = hour(hms(uhrzeit)),
        uhrzeit_wetter = uhrzeit  # Umbenennen der 'uhrzeit'-Spalte
      ) |>
      select(-uhrzeit),
    by = c("datum", "stunde")
  ) |>
  select(-stunde)

erkennungen <- erkennungen_bereinigt_mit_jahreszeiten_mit_wetter

summary(erkennungen)
glimpse(erkennungen)
view(erkennungen)


# Zwischenergebnisse ------------------------------------------------------

heutiges_datum <- Sys.Date()

# Speichern als CSV
write_csv(
  erkennungen, 
  file.path(
    pfad_zwischenergebnisse, 
    str_glue("erkennungen_{heutiges_datum}.csv")
  )
)