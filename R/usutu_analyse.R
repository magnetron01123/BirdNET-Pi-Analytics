# Pakete laden ------------------------------------------------------------
library(tidyverse)
library(scales)

library(ggtext)


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

erkennungen_amseln <-
  erkennungen |> 
  filter(datum >= as_date("2024-03-01") & datum <= as_date("2024-12-31")) |> 
  mutate(woche = floor_date(datum, "week")) |> 
  group_by(woche) |> 
  summarise(
    erkennungen = n(),
    erkennungen_amseln = sum(if_else(deutscher_name == "Amsel", 1, 0)),
    .groups = "drop"
  ) |> 
  mutate(anteil_amseln = erkennungen_amseln / erkennungen)

erkennungen_amseln |> 
  ggplot(
    aes(
      x = woche,
      y = anteil_amseln
    )
  ) +
  geom_line() +
  geom_point() +
  geom_rect(
    xmin = as_date("2024-05-1"),
    xmax = max(erkennungen$datum),
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.01
  ) +
  geom_text(
    data = erkennungen_amseln |> 
      filter(anteil_amseln %in% range(anteil_amseln)),
    aes(
      label = paste0(
      label_number(big.mark = ".", decimal.mark = ",")(erkennungen_amseln), 
      " Amseln erkannt"
      )
    ),
    vjust = -1.5
  ) +
  scale_x_date(
    date_labels = "%b %y",
    date_breaks = "1 month"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.2)),
  ) +
  labs(
    title = "Rückgang von Amseln im Garten in Rhede",
    subtitle = "Einfluss des Usutu-Virus in 2024 im markierten Zeitfenster",
    caption = str_glue(
      "**Datenquelle:**<br>",
      "BirdNET-Pi, ein Gerät zur automatischen Erkennung von Vogelgesängen.<br>",
      "Der Datensatz umfasst im Jahr 2024 insgesamt 
      {label_number(big.mark = '.', accuracy = 1)(sum(erkennungen_amseln$erkennungen))} 
      Vogelbeobachtungen.<br>"
    ),
    x = "Wöchentliche Erfassung",
    y = "Anteil der Amseln"
  ) +
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.border = element_rect(colour = "black", fill = NA),
    plot.caption = element_markdown()
  )