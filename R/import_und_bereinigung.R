# Pakete laden ------------------------------------------------------------

library(tidyverse)
library(janitor)
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
  clean_names() |> 
  transmute(
    zeitstempel = ymd_hms(paste(date, time)),
    wissenschaftlicher_name = as_factor(sci_name),
    deutscher_name = as_factor(com_name),
    erkennungswahrscheinlichkeit = confidence,
    schwellenwert = cutoff,
    empfindlichkeit = sens,
    breitengrad = lat,
    laengengrad = lon,
    ueberlappung = overlap
  )

# Unterbrechungen in Erkennungen feststellen
zeitraum_unterbrechungen <- 
  erkennungen_bereinigt |>
  arrange(zeitstempel) |> 
  mutate(
    datum = date(zeitstempel),
    abstand = datum - lag(datum)
  ) |> 
  drop_na() |> 
  filter(abstand > 1) |> 
  transmute(
    beginn = datum - abstand,
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
    ende = date(max(erkennungen_bereinigt$zeitstempel)),
    dauer = ende - beginn + 1
  )

print(zeitraum_erkennungen)

# Nur Vollständige Tage ohne Testdaten berücksichtigen
erkennungen_bereinigt <- 
  erkennungen_bereinigt |> 
  filter(
    between(date(zeitstempel), zeitraum_erkennungen$beginn, zeitraum_erkennungen$ende) & 
    # Es müssen nur noch die unvollständigen Tage mit Beginn und Ende der Unterbrechung berücksichtigt werden
    !(date(zeitstempel) %in% zeitraum_unterbrechungen$beginn | date(zeitstempel) %in% zeitraum_unterbrechungen$ende)
  )


# Wetterdaten -------------------------------------------------------------

# Variablen gemäß Dokumentation der API
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
  nearbyStations(
    lat = last(pull(erkennungen_bereinigt, breitengrad)),
    lon = last(pull(erkennungen_bereinigt, laengengrad)),
    # Hoher Radius aufgrund weniger Stationen in der Nähe
    radius = 50, 
    res = "hourly", 
    per = "recent", 
    var = relevante_wettervariablen
  ) |> 
  as_tibble() |>
  drop_na() |>
  clean_names() |> 
  arrange(desc(bis_datum), dist) |>
  group_by(var) |>
  # Auswahl der nächsten Station mit der relevanten Variable
  slice_max(
    order_by = bis_datum, 
    with_ties = FALSE
  ) |>
  ungroup() |>
  transmute(
    station = as_factor(stations_id),
    ort = as_factor(stationsname),
    wettervariable = as_factor(var),
    entfernung = as_factor(dist),
    link = url
  )

print(wetterstationen)

# Funktion zur Datenabfrage vom DWD
wetterdaten_laden <- 
  function(link) {
    dataDWD(link, read = FALSE, dir = tempdir()) |>
    readDWD(varnames = TRUE) |>
    as_tibble() |>
    clean_names(transliterations = "german")
  }

# Kombinierte Wetterdaten aus verschiedenen Quellen
wetterdaten <- 
  wetterstationen |>
  mutate(wetterdaten = map(link, wetterdaten_laden)) |>
  unnest(wetterdaten) |>
  # Unterscheidung nach Station aufheben
  group_by(mess_datum) |>
  summarise(
    across(where(is.numeric), ~sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  transmute(
    wetterzeit = mess_datum,
    niederschlagsindikator = factor(
      if_else(
        rs_ind_niederschlagsindikator %in% 0:1,
        rs_ind_niederschlagsindikator,
        NA
      ),
      levels = c(0:1), 
      labels = c(
        "Kein Niederschlag", 
        "Niederschlag"
      )
    ),
    bedeckungsgrad = factor(
      if_else(
        v_n_bedeckungsgrad %in% 0:8,
        v_n_bedeckungsgrad,
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
    lufttemperatur = tt_tu_lufttemperatur,
    luftfeuchtigkeit = rf_tu_relative_feuchte,
    luftdruck = p_luftdruck_nn,
    bodentemperatur = v_te005_erdbodentemperatur_005cm,
    niederschlagshoehe = r1_niederschlagshoehe,
    sonnenscheindauer = sd_so_sonnenscheindauer,
    windgeschwindigkeit = f_windgeschwindigkeit
  )

glimpse(wetterdaten)
summary(wetterdaten)


# Daten ergänzen ----------------------------------------------------------

# Wetterdaten und Jahreszeiten hinzufügen

erkennungen <-
  erkennungen_bereinigt |>
  # Join By nach Wetterzeit aufgrund stündlicher Datenbasis
  mutate(wetterzeit = floor_date(zeitstempel, "hour")) |>
  # Inner Join damit Erkennungen und Wetter den gleichen Datenstand haben
  inner_join(wetterdaten) |> 
  mutate(
    jahreszeit = fct(
      # Meteorlogische Ermittlung der Jahreszeiten - gleichmäßige Aufteilung der Monate
      case_when(
        month(zeitstempel) %in% 3:5  ~ "Frühling",
        month(zeitstempel) %in% 6:8  ~ "Sommer",
        month(zeitstempel) %in% 9:11 ~ "Herbst",
        month(zeitstempel) %in% c(12, 1, 2) ~ "Winter",
        TRUE ~ NA
      ),
      levels = c("Frühling", "Sommer", "Herbst", "Winter")
    )
  ) |> 
  arrange(desc(zeitstempel))


# Export und Import -------------------------------------------------------

# Daten werden nur 1 Jahr vom DWD vorgehalten und daher Export nötig

# Aktuelle Ergebnisse als CSV exportieren
write_csv(
  erkennungen, 
  file.path(
    pfad_zwischenergebnisse, 
    paste0("erkennungen_", Sys.Date(), ".csv")
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
    map_dfr(~read_csv(.x, locale = locale(encoding = "UTF-8")))
  ) |> 
  distinct() |> 
  arrange(desc(zeitstempel))

print(erkennungen)
view(erkennungen)