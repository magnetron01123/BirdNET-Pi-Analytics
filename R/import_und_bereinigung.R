# Pakete laden ------------------------------------------------------------

library(tidyverse)
library(rdwd)


# Verzeichnisse definieren ------------------------------------------------

pfad_datensatz <- file.path(getwd(), "Datensatz")
pfad_zwischenergebnisse <- file.path(getwd(), "Zwischenergebnisse")


# Datenimport -------------------------------------------------------------

# Daten importieren
erkennungen_rohdaten <- read_csv(file.path(pfad_datensatz, "detections.csv"))

glimpse(erkennungen_rohdaten)


# Datenbereinigung --------------------------------------------------------

# Variablen benennen und Datentypen korrigieren
erkennungen_bereinigt <- 
  erkennungen_rohdaten |>
  transmute(
    zeitstempel = ymd_hms(paste(Date, Time)),
    wissenschaftlicher_name = as.factor(Sci_Name),
    deutscher_name = as.factor(Com_Name),
    erkennungswahrscheinlichkeit = Confidence,
    schwellenwert = Cutoff,
    empfindlichkeit = Sens,
    breitengrad = Lat,
    laengengrad = Lon,
    ueberlappung = Overlap
  )

# Unterbrechungen in Erkennungen feststellen
zeitraum_unterbrechungen <- 
  erkennungen_bereinigt |>
  arrange(zeitstempel) |> 
  mutate(
    datum = date(zeitstempel),
    tage_differenz = datum - lag(datum)
  ) |> 
  drop_na() |> 
  filter(tage_differenz > 1) |> 
  transmute(
    beginn = datum - tage_differenz,
    ende = datum,
    # +1 damit Beginn und Ende als Tag berücksichtigt werden
    dauer = ende - beginn + 1
  )

print(zeitraum_unterbrechungen)

# Beginn und Ende des Datensatzes festlegen
zeitraum_erkennungen <- 
  tibble(
    beginn = erkennungen_bereinigt  |> 
      filter(zeitstempel >= as_datetime("2024-03-01")) |> 
      count(datum = date(zeitstempel)) |> 
      filter(n >= 250) |> 
      summarise(beginn = min(datum)) |> 
      pull(beginn),
    ende = date(max(erkennungen_bereinigt$zeitstempel)) - 1
  )

print(zeitraum_erkennungen)

# Nur Vollständige Tage ohne Testdaten berücksichtigen
erkennungen_bereinigt <- 
  erkennungen_bereinigt |> 
  filter(
    between(date(zeitstempel), zeitraum_erkennungen$beginn, zeitraum_erkennungen$ende) & 
      # Es müssen nur noch die unvollständigen Tage mit Beginn und Ende der Unterbrechung berücksichtigt werden
      !(date(zeitstempel) %in% zeitraum_unterbrechungen$beginn 
        | date(zeitstempel) %in% zeitraum_unterbrechungen$ende)
  )


# Wetterdaten -------------------------------------------------------------

# Passende Wetterstation mit entsprechenden Daten finden
wetterstationen <- 
  as_tibble(
    nearbyStations(
      lat = last(pull(erkennungen_bereinigt, breitengrad)),
      lon = last(pull(erkennungen_bereinigt, laengengrad)),
      radius = 50, 
      res = "hourly", 
      per = "recent", 
      var = c(
        "air_temperature", 
        "pressure",
        "soil_temperature", 
        "sun",
        "cloudiness",
        "precipitation",
        "wind"
      )
    )
  ) |>
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
  transmute(einzelne_wetterdaten = map(link, wetterdaten_laden)) |>
  unnest(cols = c(einzelne_wetterdaten)) |>
  # Unterscheidung nach Station aufheben
  group_by(MESS_DATUM) |>
  summarise(
    across(everything(), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  transmute(
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
    bedeckungsgrad= factor(
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
    ),
    lufttemperatur = TT_TU.Lufttemperatur,
    luftfeuchtigkeit = RF_TU.Relative_Feuchte,
    luftdruck = P.Luftdruck_NN,
    bodentemperatur = V_TE005.Erdbodentemperatur_005cm,
    niederschlagshoehe = R1.Niederschlagshoehe,
    sonnenscheindauer = SD_SO.Sonnenscheindauer,
    windgeschwindigkeit = F.Windgeschwindigkeit,
    wetterzeit = MESS_DATUM
  )

summary(wetterdaten)
glimpse(wetterdaten)


# Daten ergänzen ----------------------------------------------------------

erkennungen <-
  erkennungen_bereinigt |>
  # Wetterdaten ergänzen
  # Join nach Datum und Stunde wegen Wetterdaten auf Studenbasis
  mutate(wetterzeit = ceiling_date(zeitstempel, "hour")) |>
  left_join(wetterdaten) |> 
  # Jahreszeiten ergänzen
  mutate(
    jahreszeit = case_when(
      date(zeitstempel) >= make_date(year(zeitstempel), 3, 20) & 
        date(zeitstempel) < make_date(year(zeitstempel), 6, 21) ~ "Frühling",
      date(zeitstempel) >= make_date(year(zeitstempel), 6, 21) & 
        date(zeitstempel) < make_date(year(zeitstempel), 9, 23) ~ "Sommer",
      date(zeitstempel) >= make_date(year(zeitstempel), 9, 23) & 
        date(zeitstempel) < make_date(year(zeitstempel), 12, 21) ~ "Herbst",
      (date(zeitstempel) >= make_date(year(zeitstempel) - 1, 12, 21) & 
         date(zeitstempel) < make_date(year(zeitstempel), 3, 20)) ~ "Winter"
    ),
    jahreszeit = factor(jahreszeit, levels = c("Frühling", "Sommer", "Herbst", "Winter"))
  )


# Export und Import -------------------------------------------------------

# Daten werden nur 1 Jahr vom DWD vorgehalten und daher Export nötig

heutiges_datum <- Sys.Date()

# Aktuelle Ergebnisse als CSV exportieren
write_csv(
  erkennungen, 
  file.path(
    pfad_zwischenergebnisse, 
    str_glue("erkennungen_{heutiges_datum}.csv")
  )
)

# Ergebnisse aus der Vergangenheit anhängen
erkennungen <- 
  bind_rows(
    erkennungen,
    list.files(
      path = pfad_zwischenergebnisse,  
      pattern = "\\.csv$",             
      full.names = TRUE                
    ) |> 
    map_dfr(~ read_csv(.x, locale = locale(encoding = "UTF-8")))
  ) |> 
  distinct()

print(erkennungen)
view(erkennungen)