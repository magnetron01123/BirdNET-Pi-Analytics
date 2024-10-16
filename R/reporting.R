# Pakete laden ------------------------------------------------------------

# Tidyverse
library(tidyverse) # Sammlung von Paketen für Data Science
library(tidymodels) # Modellierung im Tidyverse
library(corrr) # Korrelationen im Tidyverse

# Tabellen und Diagramme
library(scales) # Beschriftungen von Skalen bei ggplot2
library(gt) # Tabellen designen
library(patchwork) # Mehrere Diagramme in einem Layout


# Grundeinstellungen ------------------------------------------------------

# Weitere Verzeichnisse definieren
pfad_arbeitsverzeichnis <- getwd()
pfad_skripte <- file.path(pfad_arbeitsverzeichnis, "R")
pfad_diagramme <- file.path(pfad_arbeitsverzeichnis, "Diagramme")
pfad_tabellen <- file.path(pfad_arbeitsverzeichnis, "Tabellen")

# Sprache ändern
Sys.setlocale("LC_TIME", "de_DE.UTF-8")


# Datenimport und -bereinigung --------------------------------------------

source(file.path(pfad_skripte, "import_und_bereinigung.R"))


# Datenanalysen -----------------------------------------------------------

# Unterschiedliche Vogelarten zählen
mindestbeobachtungen <- 10

anzahl_vogelarten <- 
  erkennungen |>
  group_by(deutscher_name) |> 
  summarise(
    anzahl_erkennungen = n(),
    .groups = "drop"
  ) |> 
  filter(
    anzahl_erkennungen >= mindestbeobachtungen
  ) |> 
  nrow()

print(anzahl_vogelarten)

# Erkennungen am Tag
erkennungen_am_tag <-
  erkennungen |> 
  group_by(
    datum
  ) |> 
  summarise(
    erkennungen = n(),
    vogelarten = n_distinct(deutscher_name),
    .groups = "drop"
  ) |> 
  arrange(desc(datum))

print(erkennungen_am_tag)

# Top 10 der häufigsten Vogelarten
top10_vogelarten <- 
  erkennungen |>
  group_by(deutscher_name, wissenschaftlicher_name) |> 
  summarise(
    absolute_haeufigkeit = n(),
    .groups = "drop"
  ) |> 
  mutate(
    relative_haeufigkeit = absolute_haeufigkeit / sum(absolute_haeufigkeit) * 100
  ) |> 
  slice_max(
    n = 10, 
    order_by = absolute_haeufigkeit, 
    with_ties = FALSE
  )

print(top10_vogelarten)

# Bottom 10 der häufigsten Vogelarten
bottom10_vogelarten <- 
  erkennungen |>
  group_by(deutscher_name, wissenschaftlicher_name) |> 
  summarise(
    absolute_haeufigkeit = n(),
    .groups = "drop"
  ) |> 
  mutate(
    relative_haeufigkeit = absolute_haeufigkeit / sum(absolute_haeufigkeit) * 100
  ) |> 
  filter(absolute_haeufigkeit >= mindestbeobachtungen) |> 
  slice_min(
    n = 10, 
    order_by = absolute_haeufigkeit, 
    with_ties = FALSE
  )

print(bottom10_vogelarten)

vogelarten_haeufigkeiten <-
  erkennungen |> 
  group_by(deutscher_name, wissenschaftlicher_name) |> 
  summarise(
    absolut = n(),
    .groups = "drop"
  ) |> 
  mutate(
    relativ = absolut / sum(absolut) * 100
  ) |> 
  filter(absolut >= mindestbeobachtungen) |> 
  arrange(desc(absolut))

print(vogelarten_haeufigkeiten)

# Neue Vogelarten bestimmen
neue_vogelarten <- 
  erkennungen |>
  group_by(deutscher_name) |>
  filter(n() >= mindestbeobachtungen) |>
  ungroup() |>
  group_by(jahr = year(datum), kalenderwoche = week(datum)) |>
  summarise(vogelarten = list(unique(deutscher_name)), .groups = "drop") |>
  mutate(
    alle_vogelarten = accumulate(vogelarten, union, .init = list())[-1],
    neue_vogelarten = map2(vogelarten, lag(alle_vogelarten, default = list(list())), ~ setdiff(.x, .y))
  ) |>
  select(jahr, kalenderwoche, neue_vogelarten) |>
  unnest_longer(neue_vogelarten) |> 
  arrange(desc(jahr), desc(kalenderwoche))

print(neue_vogelarten)

# Wöchentliche Änderungen in Prozent
woechentliche_veraenderungen <- 
  erkennungen |> 
  mutate(
    jahr = isoyear(datum),
    kalenderwoche = isoweek(datum)
  ) |> 
  filter(
    kalenderwoche < isoweek(heutiges_datum)
  ) |> 
  group_by(jahr, kalenderwoche, deutscher_name) |> 
  summarise(
    erkennungen = n(),
    .groups = "drop"
  ) |> 
  arrange(deutscher_name, jahr, kalenderwoche) |> 
  complete(
    jahr, kalenderwoche, deutscher_name, 
    fill = list(erkennungen = 0)
  ) |> 
  group_by(deutscher_name) |> 
  mutate(
    erkennungen_vorwoche = lag(erkennungen),
    prozentuale_veraenderung = ifelse(
      is.na(erkennungen_vorwoche) | erkennungen_vorwoche == 0, 
      0, 
      (erkennungen - erkennungen_vorwoche) / erkennungen_vorwoche * 100
    )
  ) |> 
  ungroup() |> 
  drop_na()

print(woechentliche_veraenderungen)

# Modellierung ------------------------------------------------------------

# Interkorrelationen mit Top 10 Vogelarten
vogelarten_korrelationsmatrix <- 
  erkennungen |>
  filter(deutscher_name %in% top10_vogelarten$deutscher_name) |>
  # 0 Uhr in 24 Uhr mappen
  mutate(stunde = if_else(hour(uhrzeit) == 0, 24, hour(uhrzeit))) |>
  group_by(deutscher_name, datum, stunde) |>
  summarise(
    anzahl_erkennungen = n(), 
    .groups = "drop"
  ) |>
  # Daten für Korrelation transformieren
  pivot_wider(
    names_from = deutscher_name, 
    values_from = anzahl_erkennungen, 
    # 0 für keine Beobachtungen in der Stunde
    values_fill = list(anzahl = 0)
  ) |>
  select(-datum, -stunde) |>
  # Daten sind nicht normalverteilt
  correlate(method = "kendall") |> # Daten sind nicht normalverteilt
  rearrange() |>
  shave()

print(vogelarten_korrelationsmatrix)

# Lineare Regression

# Erkennungen und Wetterbedigungen
daten_lm_erkennungen_wetterbedingungen <- 
  erkennungen |>
  mutate(stunde = if_else(hour(uhrzeit) == 0, 24, hour(uhrzeit))) |>
  group_by(datum, stunde) |>
  summarise(
    anzahl_erkennungen = n(),
    lufttemperatur_in_grad = mean(lufttemperatur_in_grad, na.rm = TRUE),
    luftfeuchtigkeit_in_prozent = mean(luftfeuchtigkeit_in_prozent, na.rm = TRUE),
    luftdruck_in_hpa = mean(luftdruck_in_hpa, na.rm = TRUE),
    bodentemperatur_in_grad = mean(bodentemperatur_in_grad, na.rm = TRUE),
    niederschlagshoehe_in_mm = sum(niederschlagshoehe_in_mm, na.rm = TRUE),
    sonnenscheindauer_in_min = sum(sonnenscheindauer_in_min, na.rm = TRUE),
    bedeckungsgrad_in_okta = mean(as.numeric(bedeckungsgrad_in_okta), na.rm = TRUE), 
    windgeschwindigkeit_in_m_pro_s = mean(windgeschwindigkeit_in_m_pro_s, na.rm = TRUE),
    .groups = "drop"
  )

lm_erkennungen_wetterbedingungen <- 
  workflow() |>
  add_recipe(
    recipe(
      anzahl_erkennungen ~ 
        lufttemperatur_in_grad + 
        luftfeuchtigkeit_in_prozent + 
        luftdruck_in_hpa + 
        bodentemperatur_in_grad + 
        niederschlagshoehe_in_mm + 
        sonnenscheindauer_in_min +
        bedeckungsgrad_in_okta +
        windgeschwindigkeit_in_m_pro_s,
      data = daten_lm_erkennungen_wetterbedingungen
    ) |> 
      step_normalize(everything())
  ) |>
  add_model(
    linear_reg() |> 
      set_engine("lm")
  ) |>
  fit(data = daten_lm_erkennungen_wetterbedingungen)

zusammenfassung_lm_erkennungen_wetterbedingungen <- glance(lm_erkennungen_wetterbedingungen)
ergebnisse_lm_erkennungen_wetterbedingungen <- tidy(lm_erkennungen_wetterbedingungen)

print(zusammenfassung_lm_erkennungen_wetterbedingungen)
print(ergebnisse_lm_erkennungen_wetterbedingungen)

# Erkennungswahrscheinlichkeit und Wetterbedingungen

daten_lm_erkennungswahrscheinlichkeit_wetterbedingungen <- 
  erkennungen |>
  mutate(stunde = if_else(hour(uhrzeit) == 0, 24, hour(uhrzeit))) |>
  group_by(datum, stunde) |>
  summarise(
    erkennungswahrscheinlichkeit_in_prozent = mean(erkennungswahrscheinlichkeit, na.rm = TRUE),
    lufttemperatur_in_grad = mean(lufttemperatur_in_grad, na.rm = TRUE),
    luftfeuchtigkeit_in_prozent = mean(luftfeuchtigkeit_in_prozent, na.rm = TRUE),
    luftdruck_in_hpa = mean(luftdruck_in_hpa, na.rm = TRUE),
    bodentemperatur_in_grad = mean(bodentemperatur_in_grad, na.rm = TRUE),
    niederschlagshoehe_in_mm = sum(niederschlagshoehe_in_mm, na.rm = TRUE),
    sonnenscheindauer_in_min = sum(sonnenscheindauer_in_min, na.rm = TRUE),
    bedeckungsgrad_in_okta = mean(as.numeric(bedeckungsgrad_in_okta), na.rm = TRUE), 
    windgeschwindigkeit_in_m_pro_s = mean(windgeschwindigkeit_in_m_pro_s, na.rm = TRUE),
    .groups = "drop"
  )

lm_erkennungswahrscheinlichkeit_wetterbedingungen <- 
  workflow() |>
  add_recipe(
    recipe(
      erkennungswahrscheinlichkeit_in_prozent ~ lufttemperatur_in_grad + 
        luftfeuchtigkeit_in_prozent + 
        luftdruck_in_hpa + 
        bodentemperatur_in_grad + 
        niederschlagshoehe_in_mm + 
        sonnenscheindauer_in_min +
        bedeckungsgrad_in_okta +
        windgeschwindigkeit_in_m_pro_s,
      data = daten_lm_erkennungswahrscheinlichkeit_wetterbedingungen
    ) |> 
      step_normalize(everything())
  ) |>
  add_model(
    linear_reg() |> 
      set_engine("lm")
  ) |>
  fit(data = daten_lm_erkennungswahrscheinlichkeit_wetterbedingungen)

zusammenfassung_lm_erkennungswahrscheinlichkeit_wetterbedingungen <- glance(lm_erkennungswahrscheinlichkeit_wetterbedingungen)
ergebnisse_lm_erkennungswahrscheinlichkeit_wetterbedingungen <- tidy(lm_erkennungswahrscheinlichkeit_wetterbedingungen)

print(zusammenfassung_lm_erkennungswahrscheinlichkeit_wetterbedingungen)
print(ergebnisse_lm_erkennungswahrscheinlichkeit_wetterbedingungen)


# Visualisierung ----------------------------------------------------------

# Liste für Diagramme
diagramme <- list() # Liste für alle Diagramme

# Entwicklung der täglichen Vogelerkennungen

diagramme$entwicklung_taegliche_vogelerkennungen <- {
  
  # Definiere Jahreszeitenfarben und Zeiträume
  jahreszeiten_farben <- c(
    "Winter" = "blue", 
    "Frühling" = "green", 
    "Sommer" = "yellow", 
    "Herbst" = "orange"
  )
  
  # Variable für Abschnitte im Diagramm
  jahreszeiten_zeitraeume <- 
    erkennungen |>
    group_by(datum) |>
    summarise(
      erkennungen = n(),
      .groups = "drop"
    ) |> 
    #Datensatz vervollständigen und ergänzen
    complete(datum = seq(min(datum), max(datum), by = "day")) |>
    mutate(
      jahreszeit = case_when(
        datum >= make_date(year(datum), 3, 1) & datum <= make_date(year(datum), 5, 31) ~ "Frühling",
        datum >= make_date(year(datum), 6, 1) & datum <= make_date(year(datum), 8, 31) ~ "Sommer",
        datum >= make_date(year(datum), 9, 1) & datum <= make_date(year(datum), 11, 30) ~ "Herbst",
        datum >= make_date(year(datum) - 1, 12, 1) & datum <= make_date(year(datum), 2, 29) ~ "Winter"
      ),
      jahreszeit = factor(jahreszeit, levels = c("Frühling", "Sommer", "Herbst", "Winter"))
    ) |> 
    group_by(jahreszeit) |> 
    summarise(
      beginn = min(datum), 
      ende = max(datum), 
      mitte = mean(c(max(datum), min(datum))), 
      .groups = "drop"
    ) |> 
    # 1 Tag addieren um Lücke zu schließen
    mutate(
      ende = if_else(
        ende < max(erkennungen$datum), 
        ende + days(1), ende
      )
    )
  
  # Funktion für die Erstellung von Abschnitten für Jahreszeiten
  create_jahreszeiten_abschnitt <- 
    function(with_labels = TRUE) {
      # Mit Labels für das obere Diagramm und ohne für das untere Diagramm
      if (with_labels) {
        list(
          geom_rect(
            data = jahreszeiten_zeitraeume, 
            aes(xmin = beginn, xmax = ende, ymin = -Inf, ymax = Inf, fill = jahreszeit), 
            alpha = 0.1, 
            inherit.aes = FALSE
          ),
          geom_text(
            data = jahreszeiten_zeitraeume, 
            aes(x = mitte, y = Inf, label = jahreszeit), 
            vjust = 1.5, 
            inherit.aes = FALSE
          ),
          scale_fill_manual(
            values = jahreszeiten_farben, 
            name = "Jahreszeit"
          )
        )
      } else {
        list(
          geom_rect(
            data = jahreszeiten_zeitraeume, 
            aes(xmin = beginn, xmax = ende, ymin = -Inf, ymax = Inf, fill = jahreszeit), 
            alpha = 0.1, 
            inherit.aes = FALSE
          ),
          scale_fill_manual(
            values = jahreszeiten_farben, 
            name = "Jahreszeit"
          )
        )
      }
    }
  
  # Funktion für Anmerkungen in Unterbrechungen
  unterbrechungen_anmerken <- 
    function(unterbrechungen) {
      unterbrechungen |>
        filter(beginn >= beginn_erkennungen) |> 
        rowwise() |>
        mutate(
          mittelwert = mean(c(beginn, ende)),
          label = str_glue("{dauer_in_tage} Tage\nUnterbrechung")
        ) |>
        pmap(function(beginn, ende, mittelwert, label, ...) {
          annotate(
            "text",
            label = label,
            x = mittelwert,
            y = 0,
            color = "red",
            size = 3
          )
        })
    }
  
  # Liniendiagramm
  liniendiagramm_taegliche_erkennungen <- 
    erkennungen |>
    group_by(datum) |> 
    summarise(
      anzahl_erkennungen = n(),
      .groups = "drop"
    ) |> 
    # Datensatz vervollständigen damit Unterbrechungen dargestellt werden
    complete(datum = seq(min(datum), max(datum), by = "day")) |>
    ggplot(aes(x = datum, y = anzahl_erkennungen)) +
    geom_line(na.rm = TRUE) +
    geom_smooth(
      method = "glm", 
      color = "red", 
      linetype = "dashed", 
      se = FALSE,
      method.args = list(family = poisson)
    ) +
    scale_x_date(
      date_labels = "%B %Y",
      date_breaks = "1 month"
    ) +
    scale_y_continuous(
      labels = label_number(big.mark = ".", decimal.mark = ","),
      breaks = function(x) seq(0, max(x), by = 1000),
    ) +
    labs(
      x = "Datum der Erkennungen", 
      y = "Anzahl der Erkennungen"
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      plot.title = element_blank(),
      axis.title.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 5))
    ) +
    create_jahreszeiten_abschnitt(with_labels = TRUE) +
    unterbrechungen_anmerken(unterbrechungen)
  
  # Flächendiagramm
  sauelendiagramm_taegliche_vogelarten <- 
    erkennungen |>
    group_by(datum) |>
    summarise(
      anzahl_vogelarten = n_distinct(deutscher_name),
      .groups = "drop"
    ) |>
    # Datensatz vervollständigen damit Unterbrechungen dargestellt werden
    complete(datum = seq(min(datum), max(datum), by = "day")) |>
    ggplot(aes(x = datum, y = anzahl_vogelarten)) +
    # Bei geom_area werden NA nicht als Unterbrechungen dargestellt
    # Min und Max müssen bei geom_ribbon angegeben werden
    geom_ribbon(aes(ymin = 0, ymax = anzahl_vogelarten), na.rm = FALSE) +
    scale_x_date(
      date_labels = "%B %Y",
      date_breaks = "1 month"
    ) +
    scale_y_continuous(
      labels = label_number(big.mark = ".", decimal.mark = ","),
      breaks = function(x) seq(0, max(x), by = 10)
    ) +
    geom_smooth(
      method = "glm", 
      color = "red", 
      linetype = "dashed", 
      se = FALSE,
      method.args = list(family = poisson)
    ) +
    labs(
      x = "Datum der Erkennungen",
      y = "Anzahl der Vogelarten",
      title = "Tägliche Anzahl der beobachteten Vogelarten"
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.title.x = element_text(margin = margin(t = 5)),
      plot.title = element_blank(),
      axis.title.y = element_text(margin = margin(r = 5))
    ) +
    create_jahreszeiten_abschnitt(with_labels = FALSE)
  
  liniendiagramm_taegliche_erkennungen + sauelendiagramm_taegliche_vogelarten +
    plot_layout(
      nrow = 2,
      heights = c(2, 1),
      axes = "collect",
    ) +
    plot_annotation(
      title = "Entwicklung der täglichen Vogelerkennungen und Artenvielfalt",
      subtitle = str_glue(
        "Vogelerkennungen vom {format(beginn_erkennungen, '%d.%m.%Y')} bis {format(ende_erkennungen, '%d.%m.%Y')} ",
        "in Büngern (Rhede)"),
      caption = str_glue(
        "N = {format(nrow(erkennungen), big.mark = '.', decimal.mark = ',')}; ",
        "Poisson-Regressions-Trend als gestrichelte Linie"
      ),
    )
}

# Häufigkeiten von Vogelarten
diagramme$haeufiste_vogelarten <- {
  
  vogelarten_anteile <- 
    erkennungen |> 
    group_by(deutscher_name) |> 
    summarise(absolut = n()) |> 
    mutate(relativ = absolut / nrow(erkennungen) * 100) |> 
    filter(deutscher_name %in% top10_vogelarten$deutscher_name) |> 
    arrange(desc(relativ))
  
  haeufigkeit_vogelarten <- 
    vogelarten_anteile |> 
    mutate(deutscher_name = factor(deutscher_name, levels = rev(vogelarten_anteile$deutscher_name))) |> 
    ggplot(aes(x = deutscher_name, y = relativ)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Top 10 Vogelarten",
      subtitle = "Alle Erkennungen"
    ) +
    scale_y_continuous(
      labels = label_number(
        big.mark = ".", 
        decimal.mark = ",", 
        suffix = "%"
      )
    ) +
    theme_classic() +
    theme(
      axis.title = element_blank(),
    )
  
  verteilungen_vogelarten <- 
    erkennungen |> 
    group_by(deutscher_name, datum) |> 
    summarise(
      anzahl = n(),
      .groups = "drop"
    ) |> 
    filter(deutscher_name %in% vogelarten_anteile$deutscher_name) |> 
    mutate(deutscher_name = factor(deutscher_name, levels = vogelarten_anteile$deutscher_name)) |> 
    ggplot(aes(x = datum, y = anzahl, fill = deutscher_name, color = deutscher_name)) +
    geom_area() +
    geom_smooth(
      method = "glm", 
      se = FALSE, 
      color = "black", 
      linetype = "dashed",
      linewidth = 0.75,
      method.args = list(family = poisson)
    ) +
    labs(
      title = "Top 10 Vogelarten im Zeitverlauf",
      subtitle = "Tägliche Erkennungen",
      x = "Datum der Erkennungen",
      y = "Anzahl der Erkennungen"
    ) +
    scale_y_continuous(
      labels = label_number(big.mark = ".", decimal.mark = ","),
      expand = c(0, 0)
    ) +
    facet_wrap(~ deutscher_name, scales = "free_y") +
    theme_classic() +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      axis.line = element_blank(),
      axis.title.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 5))
    )
  
  haeufigkeit_vogelarten + verteilungen_vogelarten +
    plot_layout(widths = c(1, 3)) +
    plot_annotation(
      title = "Häufigkeiten von Vogelarten im Zeitverlauf",
      subtitle = str_glue(
        "Vogelerkennungen vom {format(beginn_erkennungen, '%d.%m.%Y')} bis {format(ende_erkennungen, '%d.%m.%Y')} ",
        "in Büngern (Rhede)"),
      caption = str_glue(
        "N = {format(nrow(erkennungen), big.mark = '.', decimal.mark = ',')}; ",
        "Poisson-Regressions-Trend als gestrichelte Linie"
      )
    )
}

# Heatmaps mit Vogelaktivität
diagramme$heatmap_vogelaktivitaet <- {
  
  # Funktion zur Generierung der Stundenlabels
  stunden_labels <- 
    function(breaks) {
      paste0(breaks, " Uhr")
    }
  
  # Diagramm für Erkennungen
  heatmap_datum_stunde_erkennungen <- 
    erkennungen |>
    mutate(stunde = if_else(hour(uhrzeit) == 0, 24, hour(uhrzeit))) |>
    group_by(datum, stunde) |>
    summarise(
      anzahl_erkennungen = n(),
      .groups = "drop"
    ) |>
    complete(
      datum, stunde = full_seq(1:24, 1), 
      fill = list(anzahl_erkennungen = 0)
    ) |> 
    complete(
      datum = seq(min(datum), max(datum), by = "day"), stunde = 1:24,
      fill = list(anzahl_erkennungen = NA)
    ) |>
    ggplot(aes(x = datum, y = stunde, fill = anzahl_erkennungen)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(
      colors = c("white", "red"),
      na.value = "grey"
    ) +
    scale_x_date(
      date_labels = "%B %Y", 
      date_breaks = "1 month", 
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(1, 24, by = 3), 
      labels = stunden_labels, 
      expand = c(0, 0)
    ) +
    labs(
      fill = "Vogelerkennungen",
      x = "Datum der Erkennungen",
      y = "Uhrzeit der Erkennungen"
    ) +
    theme(
      axis.line = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      legend.ticks = element_blank(),
      plot.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 5)),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = c(0, 1),
      legend.margin = margin(t = 0, b = 0)
    ) +
    guides(
      fill = guide_colorbar(
        barwidth = 10, barheight = 0.5, 
        title.position = "top", title.hjust = 0.5
      )
    )
  
  # Diagramm für Vogelarten
  heatmap_datum_stunde_vogelarten <- 
    erkennungen |>
    mutate(stunde = if_else(hour(uhrzeit) == 0, 24, hour(uhrzeit))) |>
    group_by(datum, stunde) |>
    summarise(
      anzahl_vogelarten = n_distinct(deutscher_name),
      .groups = "drop"
    ) |>
    complete(
      datum, stunde = full_seq(1:24, 1), 
      fill = list(anzahl_vogelarten = 0)
    ) |> 
    complete(
      datum = seq(min(datum), max(datum), by = "day"), stunde = 1:24,
      fill = list(anzahl_vogelarten = NA)
    ) |>
    ggplot(aes(x = datum, y = stunde, fill = anzahl_vogelarten)) +
    geom_tile(color = "white") +
    scale_fill_gradient(
      low = "white", 
      high = "red",
      na.value = "grey"
    ) +
    scale_x_date(
      date_labels = "%B %Y", 
      date_breaks = "1 month", 
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(1, 24, by = 3), 
      labels = stunden_labels, 
      expand = c(0, 0)
    ) +
    labs(
      fill = "Vogelarten",
      x = "Datum der Erkennungen",
      y = "Uhrzeit der Erkennungen"
    ) +
    theme(
      axis.line = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      legend.ticks = element_blank(),
      plot.title = element_blank(),
      axis.title.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 5)), 
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = c(0, 1),
      legend.margin = margin(t = 0, b = 0)
    ) +
    guides(
      fill = guide_colorbar(
        barwidth = 10, barheight = 0.5, 
        title.position = "top", title.hjust = 0.5
      )
    )
  
  heatmap_datum_stunde_erkennungen + heatmap_datum_stunde_vogelarten +
    plot_annotation(
      title = "Zeitliche Verteilung der Vogelerkennungen und Artenvielfalt",
      subtitle = str_glue(
        "Vogelerkennungen vom {format(beginn_erkennungen, '%d.%m.%Y')} bis {format(ende_erkennungen, '%d.%m.%Y')} ",
        "in Büngern (Rhede)"),
      caption = str_glue(
        "N = {format(nrow(erkennungen), big.mark = '.', decimal.mark = ',')}; ",
        "Unterbrechungen in grau"
      )
    ) +
    plot_layout(
      nrow = 2,
      axes = "collect"
    )
}

# Heatmap mit Korrelationen
diagramme$heatmap_vogelarten_korrelationen <- 
  vogelarten_korrelationsmatrix |>
  pivot_longer(
    cols = !term, 
    names_to = "vogelart", 
    values_to = "korrelation"
  ) |>
  filter(!is.na(korrelation)) |>  # Filtere NA-Werte heraus
  mutate(
    term = factor(term, levels = unique(vogelarten_korrelationsmatrix$term)),
    vogelart = factor(vogelart, levels = unique(vogelarten_korrelationsmatrix$term))
  ) |>
  ggplot(aes(x = term, y = vogelart, fill = korrelation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = format(round(korrelation, 2), nsmall = 2)), size = 4) +
  scale_fill_gradient2(
    low = "red", high = "green", mid = "white", midpoint = 0,
    limit = c(-1, 1), name = "Korrelation\nnach Kendall-Tau"
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    x = "Top 10 Vogelarten",
    y = "Top 10 Vogelarten",
    title = "Interaktionen der häufigsten Vogelarten basierend auf Erkennungen",
    subtitle = str_glue(
      "Vogelerkennungen vom {format(beginn_erkennungen, '%d.%m.%Y')} bis {format(ende_erkennungen, '%d.%m.%Y')} ",
      "in Büngern (Rhede)"),
    caption = str_glue(
      "N = {format(nrow(erkennungen), big.mark = '.', decimal.mark = ',')}; ",
      "Top 10 Vogelarten nach Häufigkeit; Stündliche Betrachtung"
    )
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    panel.grid = element_blank(),
    plot.background = element_blank(),
    legend.ticks = element_blank(),
    axis.ticks = element_line(color = "black"),
    legend.justification = c(1, 0),
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.75),
    legend.direction = "horizontal",
    axis.title = element_blank(),
    plot.caption =  element_text(margin = margin(t = 20))
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 10, barheight = 0.5, 
      title.position = "top", title.hjust = 0.5
    )
  )

# Visualisierung der Regressionskoeffizienten
diagramme$forest_plot_wetterbedigungen_regressionsanalyse <- 
  ergebnisse_lm_erkennungen_wetterbedingungen |>
  filter(term != "(Intercept)") |>
  mutate(
    signifikant = p.value <= 0.05,
    term = recode(
      term,
      "lufttemperatur_in_grad" = "Lufttemperatur",
      "luftfeuchtigkeit_in_prozent" = "Luftfeuchtigkeit",
      "luftdruck_in_hpa" = "Luftdruck",
      "bodentemperatur_in_grad" = "Erdbodentemperatur",
      "niederschlagshoehe_in_mm" = "Niederschlagshöhe",
      "sonnenscheindauer_in_min" = "Sonnenscheindauer",
      "bedeckungsgrad_in_okta" = "Bewölkung",
      "windgeschwindigkeit_in_m_pro_s" = "Windgeschwindigkeit"
    )
  ) |>
  arrange(signifikant, estimate) |>
  mutate(term = factor(term, levels = term)) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_point(aes(color = "Regressionskoeffizienten")) +
  geom_text(
    aes(
      label = str_glue("p {if_else(p.value < 0.01, '< 0.01', paste('=', number(p.value, accuracy = 0.01)))}"),
      hjust = if_else(estimate < 0, 1.25, -0.25),
      vjust = -1.5
    )
  ) +
  geom_errorbar(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error,
      color = "Konfidenzintervalle"
    ),
    width = 0.25
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Einfluss der Wetterbedingungen auf Vogelerkennungen",
    subtitle = str_glue(
      "Vogelerkennungen vom {format(beginn_erkennungen, '%d.%m.%Y')} bis {format(ende_erkennungen, '%d.%m.%Y')} ",
      "in Büngern (Rhede) ",
      "und Wetterdaten vom DWD"
    ),
    caption = str_glue(
      "N = {format(nrow(erkennungen), big.mark = '.', decimal.mark = ',')}; ",
      "R² = {format(round(zusammenfassung_lm_erkennungen_wetterbedingungen$r.squared, 2), big.mark = '.', decimal.mark = ',')}; ",
      "Alle Variablen wurden standadisiert; ",
      "Geschnittene Konfidenzintervalle sind nicht signifikant; ",
      "Stündliche Betrachtung"
    ),
    color = "Multiple Regressionsanalyse",
  ) +
  scale_y_continuous(labels = label_number(accuracy = 0.01, decimal.mark = ",")) +
  scale_color_manual(
    values = c("Regressionskoeffizienten" = "black", "Konfidenzintervalle" = "black"), 
    labels = c("95%-Konfidenzintervalle", "Regressionskoeffizienten")
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.position = "right",
    axis.title = element_blank(),
    plot.caption =  element_text(margin = margin(t = 20))
  )


# Tabellen ----------------------------------------------------------------

tabellen <- list()

# Top 10 Vogelarten
tabellen$top_bottom_vogelarten <- 
  tibble(
    rang = seq(1, 10, by = 1),
    top10 = top10_vogelarten$deutscher_name,
    top10_haeufigkeit = top10_vogelarten$absolute_haeufigkeit,
    bottom10 = bottom10_vogelarten$deutscher_name,
    bottom10_haeufigkeit = bottom10_vogelarten$absolute_haeufigkeit
  ) |> 
  gt() |>
  fmt_number(
    columns = c(top10_haeufigkeit, bottom10_haeufigkeit),
    decimals = 0,
    use_seps = TRUE,
    sep_mark = ".",
    dec_mark = ","
  ) |>
  cols_label(
    rang = "Rang",
    top10 = "Vogelarten",
    top10_haeufigkeit = "Erkennungen",
    bottom10 = "Vogelarten",
    bottom10_haeufigkeit = "Erkennungen"
  ) |>
  tab_header(
    title = "Top und Bottom 10 Vogelarten in Büngern (Rhede)",
    subtitle = str_glue(
      "Vogelerkennungen vom {format(beginn_erkennungen, '%d.%m.%Y')} bis zum {format(ende_erkennungen, '%d.%m.%Y')}"
    )
  ) |>
  tab_spanner(
    label = "Top 10",
    columns = c(top10, top10_haeufigkeit)
  ) |> 
  tab_spanner(
    label = "Bottom 10",
    columns = c(bottom10, bottom10_haeufigkeit)
  ) |> 
  tab_footnote(
    footnote = str_glue(
      "Mindestens {mindestbeobachtungen} Erkennungen pro Vogelart"
    ),
    locations = cells_column_spanners(spanners = "Bottom 10")
  ) 

# Häufigkeiten der Vogelarten
vogelarten_haeufigkeiten |> 
  mutate(rang = row_number()) |> 
  relocate(rang, .before = 1) |> 
  gt() |> 
  fmt_number(
    columns = c(absolut, relativ),
    decimals = 0,
    use_seps = TRUE,
    sep_mark = ".",
    dec_mark = ","
  ) |>
  cols_label(
    rang = "Rang",
    deutscher_name = "Deutscher Name",
    wissenschaftlicher_name = "Wissenschaftlicher Name",
    absolut = "Absolut",
    relativ = "Relativ"
  ) |>
  tab_header(
    title = "Vogelarten in Büngern (Rhede)",
    subtitle = str_glue(
      "Vogelerkennungen vom {format(beginn_erkennungen, '%d.%m.%Y')} bis zum {format(ende_erkennungen, '%d.%m.%Y')}"
    )
  ) |> 
  tab_spanner(
    label = "Vogelarten",
    columns = c(deutscher_name, wissenschaftlicher_name)
  ) |> 
  tab_spanner(
    label = "Häufigkeiten",
    columns = c(absolut, relativ)
  ) |> 
  tab_footnote(
    footnote = str_glue(
      "Mindestens {mindestbeobachtungen} Erkennungen pro Vogelart"
    ),
    locations = cells_title(groups = "subtitle")
  ) |> 
  tab_footnote(
    footnote = str_glue(
      "Die Konfidenz für die Vogelerkennung liegt bei {last(erkennungen$schwellenwert) *100} %"
    ),
    locations = cells_title(groups = "subtitle")
  )

# Wöchentliche Veränderungen
tabellen$woechentliche_veraenderungen <- 
  woechentliche_veraenderungen |> 
  arrange(jahr, kalenderwoche) |> 
  select(-jahr, -erkennungen, -erkennungen_vorwoche) |> 
  mutate(
    kalenderwoche = str_glue("KW {kalenderwoche}")
  ) |> 
  pivot_wider(
    names_from = kalenderwoche,
    values_from = prozentuale_veraenderung
  ) |> 
  filter(deutscher_name %in% top10_vogelarten$deutscher_name) |> 
  select(deutscher_name, last_col(3):last_col()) |> 
  arrange(desc(across(last_col()))) |> 
  gt() |> 
  fmt_number(
    columns = where(is.numeric),
    decimals = 0
  ) |> 
  cols_label(
    deutscher_name = "Vogelart"
  ) |> 
  tab_spanner(
    label = "Vergleich zu Vorwoche in %",
    columns = starts_with("KW")
  ) |> 
  tab_header(
    title = "Veränderungen der Erkennungen in Rhede (Büngern)",
  ) |> 
  tab_footnote(
    footnote = "Sortierung nach Anzahl der Erkennungen in der aktuellsten Woche",
    locations = cells_column_labels(
      columns = "deutscher_name"
    )
  )

# Neue Vogelarten
tabellen$neue_vogelarten <-
  neue_vogelarten |>
  arrange(jahr, kalenderwoche, neue_vogelarten) |> 
  mutate(
    kalenderwoche = str_glue("KW {kalenderwoche}")
  ) |> 
  group_by(kalenderwoche) |>
  mutate(
    nummer = row_number(),
    .before = everything()
  ) |>
  pivot_wider(names_from = kalenderwoche, values_from = neue_vogelarten) |> 
  select(-jahr) |> 
  gt() |> 
  cols_label(
    nummer = "Nr."
  ) |> 
  tab_header(
    title = "Erstmals erkannte Vogelarten je Kalenderwoche",
    subtitle = str_glue(
      "Vogelerkennungen vom {format(beginn_erkennungen, '%d.%m.%Y')} bis {format(ende_erkennungen, '%d.%m.%Y')} ",
      "in Rhede (Büngern)"
    )
  ) |> 
  tab_spanner(
    label = "Kalenderwochen",
    columns = everything()
  ) |>
  tab_footnote(
    footnote = str_glue(
      "Die Konfidenz für die Vogelerkennung liegt bei {last(erkennungen$schwellenwert) *100} %"
    ),
    locations = cells_title(groups = "subtitle")
  ) |> 
  tab_footnote(
    footnote = str_glue(
      "Mindestens {mindestbeobachtungen} Erkennungen pro Vogelart"
    ),
    locations = cells_title(groups = "subtitle")
  ) |> 
  sub_missing(
    missing_text = ""
  )

tabellen$unterbrechungen <-
  unterbrechungen |> 
  mutate(
    beginn = format(as.Date(beginn), "%d.%m.%Y"),
    ende = format(as.Date(ende), "%d.%m.%Y")
  ) |> 
  gt() |> 
  cols_label(
    beginn = "Beginn",
    ende = "Ende",
    dauer_in_tage = "Dauer in tage"
  ) |> 
  tab_header(
    title = "Unterbrechungen durch Ausfälle der Station",
    subtitle = "Beispielsweise durch Stromausfall oder Ähnliches"
  )


# Dateien speichern -------------------------------------------------------

# Plots in einer Schleife speichern und ausführen
walk2(rev(seq_along(diagramme)), rev(diagramme), function(nummer_diagramm, plot) {
  name_diagramm <- names(diagramme)[nummer_diagramm]
  name_datei <- str_glue("{nummer_diagramm}_{name_diagramm}.jpg")
  ggsave(
    filename = file.path(pfad_diagramme, name_datei),
    plot = plot,
    device = "jpeg"
  )
  print(plot)
})

# Tabellen in einer schleife speichern und ausführen
walk2(rev(seq_along(tabellen)), rev(tabellen), function(nummer_tabelle, tabelle) {
  name_tabelle <- names(tabellen)[nummer_tabelle]
  name_datei <- str_glue("{nummer_tabelle}_{name_tabelle}.html")
  gtsave(
    data = tabelle,
    filename = file.path(pfad_tabellen, name_datei)
  )
  print(tabelle)
})
