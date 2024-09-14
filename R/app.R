
# Pakete laden ------------------------------------------------------------

# Tidyverse
library(tidyverse) # dyployr, gpplot 2 etc.

# Shiny App
library(shiny) # Erstellung von Web Apps
library(bslib) # UI aus Bootstrap
library(bsicons) # Icons aus Bootstrap

library(scales) # Beschriftungen für Plots
library(rnaturalearth) # Karten für Plots

# Grundeinstellungen ------------------------------------------------------

# Arbeitsverzeichnis festlegen
setwd("/Users/davidtrogemann/Documents/Data Science/Vogelbeobachtung/")

# Weitere Verzeichnisse definieren
pfad_arbeitsverzeichnis <- getwd()
pfad_datensatz <- file.path(pfad_arbeitsverzeichnis, "Datensatz")
pfad_skripte <- file.path(pfad_arbeitsverzeichnis, "R")

# Sprache ändern
Sys.setlocale("LC_TIME", "de_DE.UTF-8")


# Datenimport und -bereinigung --------------------------------------------

source(file.path(pfad_skripte, "import_und_bereinigung.R"))


# Allgemeine Variablen ----------------------------------------------------

mindesterkennungen <- 10

anzahl_vogelarten <- 
  erkennungen |>
  group_by(deutscher_name) |> 
  summarise(
    anzahl_erkennungen = n(),
    .groups = "drop"
  ) |> 
  filter(anzahl_erkennungen >= mindesterkennungen) |> 
  nrow()

erkennungen_am_tag <-
  erkennungen |> 
  group_by(datum) |> 
  summarise(
    erkennungen = n(),
    vogelarten = n_distinct(deutscher_name),
    .groups = "drop"
  ) |> 
  arrange(desc(datum))

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
  filter(absolut >= mindesterkennungen) |> 
  arrange(desc(absolut))


# UI ----------------------------------------------------------------------

ui <- page_navbar(
  title = "Vogelerkennungen in Büngern",
  fillable = FALSE,
  
  # Tab 1
  nav_panel(
    title = "Entwicklungen",
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title = "Vogelerkennungen insgesamt:",
        value = str_glue(
          "{format(nrow(erkennungen), big.mark = '.', decimal.mark = ',')}"
        ),
        showcase = bs_icon("list-ol"),
        theme = "primary"
      ),
      value_box(
        title = "Unterschiedliche Vogelarten:",
        value = str_glue("{anzahl_vogelarten}"),
        p(
          str_glue("mit mindestens {mindesterkennungen} Erkennungen")
        ),
        showcase = bs_icon("feather"),
        theme = "primary"
      ),
      value_box(
        title = "Unterbrechung der Aufzeichnung",
        value = sum(unterbrechungen$dauer_in_tage),
        showcase = bs_icon("calendar-day"),
        theme = "danger",
        p("in Tage")
      )
    ),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("BirdNET-Pi in Büngern"),
        card_body(plotOutput("diagramm_deutschlandkarte")),
        card_body(
          card_title("BirdNET Pi"),
          gap = 0,
          p("Der BirdNET-Pi ist ein System, das mit Hilfe eines Raspberry Pi ", 
            "und der BirdNET-Technologie Vogelstimmen im Garten erkennt und ", 
            "identifiziert. Die Erkennungen erfolgen durch ein Mikrofon, das ",
            "die Geräusche aufzeichnet, und eine KI, die die Vogelarten ",
            "anhand der aufgenommenen Stimmen bestimmt.")
        ),
        card_footer(
          a(
            "Station 2636 bei Birdweather.com",
            href = "https://app.birdweather.com/stations/2636",
            target = "_blank"
          )
        )
      ),
      card(
        full_screen = TRUE,
        card_header(
          "Entwicklung der täglichen Vogelerkennungen",
          popover(
            title = "Diagrammeinstellungen",
            bs_icon("gear"),
            sliderInput(
              inputId = "zeitraum_erkennungen",
              label = "Zeitraum der Erkennungen",
              min = min(erkennungen$datum),
              max = max(erkennungen$datum),
              value = c(min(erkennungen$datum), max(erkennungen$datum)),
              timeFormat = "%d.%m.%y",
            ),
            selectizeInput(
              inputId = "einschraenkung_vogelarten",
              label = "Einschränkung nach Vogelarten",
              choices = sort(unique(vogelarten_haeufigkeiten$deutscher_name)),
              multiple = TRUE,
              options = list(placeholder = "Wähle Vogelarten aus")
            )
          )
        ),
        card_body(
          card_title("Vogelerkennungen"),
          gap = 0,
          plotOutput("diagramm_entwicklung_erkennungen")
        ),
        card_body(
          card_title("Vogelarten"),
          gap = 0,
          plotOutput("diagramm_entwicklung_vogelarten")
        ),
        card_footer(
          "Anmerkung: Poisson-Regressions-Trend als gestrichelte Linie"
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        full_screen = TRUE,
        card_header(
          "Entwicklung der Vogelarten",
          popover(
            title = "Diagrammeinstellungen",
            bs_icon("gear"),
            sliderInput(
              inputId = "zeitraum_erkennungen",
              label = "Zeitraum der Erkennungen",
              min = min(erkennungen$datum),
              max = max(erkennungen$datum),
              value = c(min(erkennungen$datum), max(erkennungen$datum)),
              timeFormat = "%d.%m.%y",
            ),
            sliderInput(
              inputId = "anzahl_vogelarten",
              label = "Anzahl der Vogelarten",
              min = 1,
              max = anzahl_vogelarten,
              value = 10
            ),
            radioButtons(
              inputId = "sortierung_vogelarten",
              label = "Sortierung nach Häufigkeit",
              choices = list(
                "Absteigend" = "absteigend",
                "Aufsteigend" = "aufsteigend"
              ),
              selected = "absteigend"
            ),
            selectizeInput(
              inputId = "einschraenkung_vogelarten",
              label = "Einschränkung nach Vogelarten",
              choices = sort(unique(vogelarten_haeufigkeiten$deutscher_name)),
              multiple = TRUE,
              options = list(placeholder = "Wähle Vogelarten aus")
            )
          ),
        ),
        card_body(),
        card_footer()
      )
    )
  ),
  
  # Tab 2
  nav_panel(
    title = "Vogelarten",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput(
          inputId = "zeitraum_erkennungen",
          label = "Zeitraum der Erkennungen",
          min = min(erkennungen$datum),
          max = max(erkennungen$datum),
          value = c(min(erkennungen$datum), max(erkennungen$datum)),
          timeFormat = "%d.%m.%y",
        ),
        sliderInput(
          inputId = "anzahl_vogelarten",
          label = "Anzahl der Vogelarten",
          min = 1,
          max = anzahl_vogelarten,
          value = 10
        ),
        radioButtons(
          inputId = "sortierung_vogelarten",
          label = "Sortierung nach Häufigkeit",
          choices = list(
            "Absteigend" = "absteigend",
            "Aufsteigend" = "aufsteigend"
          ),
          selected = "absteigend"
        ),
        selectizeInput(
          inputId = "einschraenkung_vogelarten",
          label = "Einschränkung nach Vogelarten",
          choices = sort(unique(vogelarten_haeufigkeiten$deutscher_name)),
          multiple = TRUE,
          options = list(placeholder = "Wähle Vogelarten aus")
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header(
            "Häufigkeiten der Vogelarten"
          ),
          card_body(
            tableOutput("tabelle_haeufigkeiten_vogelarten")
          ),
          card_footer(
            str_glue(
              "Mindestens {mindesterkennungen} Erkennungen pro Vogelart"
            )
          )
        )
      ),
      layout_columns(
        card(
          card_header("Neue Vogelarten"),
          card_body(tableOutput("tabelle_neue_vogelarten")),
          card_footer()
        )
      )
    )
  ),
  
  # Tab 3
  nav_panel(
    title = "Modelle",
    layout_sidebar(
      sidebar = sidebar(
        h5("Erkennungen"),
        sliderInput(
          inputId = "zeitraum_erkennungen",
          label = "Zeitraum",
          min = min(erkennungen$datum),
          max = max(erkennungen$datum),
          value = c(min(erkennungen$datum), max(erkennungen$datum)),
          timeFormat = "%d.%m.%y",
        ),
        radioButtons(
          inputId = "gruppierung_erkennungen",
          label = "Gruppierung",
          choices = list(
            "Stündlich" = "stuendlich",
            "Täglich" = "taeglich",
            "Wöchentlich" = "woechenlich"
          ),
          selected = "stuendlich"
        ),
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Wetterbedigungen als unabhängige Variable"),
            layout_sidebar(
              sidebar = sidebar(
                h5("Variablen"),
                input_switch(
                  id = "variable_sonnenscheindauer",
                  label = "Sonnenscheindauer",
                  value = TRUE
                ),
                input_switch(
                  id = "variable_lufttemperatur",
                  label = "Lufttemperatur",
                  value = TRUE
                ),
                input_switch(
                  id = "variable_bewoelkung",
                  label = "Bewölkung",
                  value = TRUE
                ),
                input_switch(
                  id = "variable_niederschlagshoehe",
                  label = "Niederschlagshöhe",
                  value = TRUE
                ),
                input_switch(
                  id = "variable_luftfeuchtigkeit",
                  label = "Luftfeuchtigkeit",
                  value = TRUE
                ),
                input_switch(
                  id = "variable_windgeschwindigkeit",
                  label = "Windgeschwindigkeit",
                  value = TRUE
                ),
                input_switch(
                  id = "variable_luftdruck",
                  label = "Luftdruck",
                  value = TRUE
                ),
                input_switch(
                  id = "variable_erbodentemperatur",
                  label = "Erdbodentemperatur",
                  value = TRUE
                )
              ),
              card_body(
                card_title("Vorhersage von Vogelerkennungen"),
                gap = 0
              ),
              card_body(
                card_title("Vorhersage von Erkennungenwahrscheinlichkeiten"),
                gap = 0
              )
            ),
          card_footer()
        )
      )
    )

  ),
  
  # Tab 4
  nav_panel(
    title = "Sonstiges",
    layout_columns(
      card(
        card_header("Unterbrechungen"),
        card_body(
          tableOutput("tabelle_unterbrechungen")
        )
      )
    )
  )
)

# R Session ------------------------------------------------------------------

server <- function(input, output) {
  
  # Tab 1
  
  # Deutschlandkarte
  output$diagramm_deutschlandkarte <- renderPlot({
    ne_countries(scale = "medium", returnclass = "sf") |>
      filter(name == "Germany") |> 
      ggplot() +
      geom_sf(fill = "white", color = "black") +
      geom_point(
        aes(
          x = laengengrad, 
          y = breitengrad
        ), 
        size = 7.5, 
        color = "#dc3545"
      ) +
      geom_curve(
        aes(
          x = 10, 
          y = 50 + 1,
          xend = laengengrad + 1,
          yend = breitengrad
        ),
        curvature = 0.2,
        arrow = arrow(length = unit(0.03, "npc"))
      ) +
      geom_label(
        aes(
          x = 10,
          y = 50,
          label = str_glue(
            "{format(first(erkennungen_am_tag$erkennungen), big.mark = '.', decimal.mark = ',')} Erkennungen\n",
            "und {format(first(erkennungen_am_tag$vogelarten), big.mark = '.', decimal.mark = ',')} Vogelarten\n",
            "am {format(first(erkennungen_am_tag$datum), '%d.%m.%Y')}"
          )
        ),
        fill = "#198754",
        color = "white",
        size = 4,
        label.size = 0.25,
        label.padding = unit(1, "lines"),
        label.r = unit(0.5, "lines")
      ) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        text = element_text(family = "Open Sans")
      )
  }, res = 96)
  
  # Entwicklung Erkennungen
  output$diagramm_entwicklung_erkennungen <- renderPlot({
    erkennungen |> 
      filter(
        length(input$einschraenkung_vogelarten) == 0 | 
        deutscher_name %in% input$einschraenkung_vogelarten
      ) |> 
      filter(
        datum >= input$zeitraum_erkennungen[1] & 
        datum <= input$zeitraum_erkennungen[2]
      ) |>
      group_by(datum) |> 
      summarise(
        anzahl_erkennungen = n(),
        .groups = "drop"
      ) |> 
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
      scale_y_continuous(
        labels = label_number(big.mark = ".", decimal.mark = ",")
      ) +
      labs(
        x = "Datum der Erkennungen", 
        y = "Anzahl der Erkennungen"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_blank(),
        axis.title = element_blank()
      )
  }, res = 96)
  
  output$diagramm_entwicklung_vogelarten <- renderPlot({
      erkennungen |>
      filter(
        length(input$einschraenkung_vogelarten) == 0 | 
          deutscher_name %in% input$einschraenkung_vogelarten
      ) |> 
      group_by(datum) |>
      summarise(
        anzahl_vogelarten = n_distinct(deutscher_name),
        .groups = "drop"
      ) |>
      filter(
        datum >= input$zeitraum_erkennungen[1] & 
        datum <= input$zeitraum_erkennungen[2]
      ) |>
      complete(datum = seq(min(datum), max(datum), by = "day")) |>
      ggplot(aes(x = datum, y = anzahl_vogelarten)) +
      geom_ribbon(aes(ymin = 0, ymax = anzahl_vogelarten), na.rm = FALSE) +
      geom_smooth(
        method = "glm", 
        color = "#dc3545", 
        linetype = "dashed", 
        se = FALSE,
        method.args = list(family = poisson)
      ) +
      scale_y_continuous(
        labels = label_number(big.mark = ".", decimal.mark = ",")
      ) +
      labs(
        x = "Datum der Erkennungen",
        y = "Anzahl der Vogelarten",
        title = "Tägliche Anzahl der beobachteten Vogelarten"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_blank(),
        axis.title = element_blank()
      )
  }, res = 96)
  
  # Entwicklung Vogelarten
  output$diagramm_entwicklung_test <- renderPlot({
    erkennungen |> 
      group_by(datum, deutscher_name) |> 
      summarise(
        anzahl = n(),
        .groups = "drop"
      ) |> 
      left_join(vogelarten_haeufigkeiten, by = "deutscher_name") |> 
      mutate(deutscher_name = factor(deutscher_name, levels = vogelarten_haeufigkeiten$deutscher_name)) |> 
      filter(
        datum >= input$zeitraum_erkennungen[1] & 
        datum <= input$zeitraum_erkennungen[2]
      ) |>
      arrange(
        if (input$sortierung_vogelarten == "aufsteigend") absolut
        else desc(absolut)
      ) |>
      ggplot(
        aes(
          x = datum, y = anzahl, 
          fill = deutscher_name, color = deutscher_name
        )
      ) +
      geom_area() +
      geom_smooth(
        method = "glm", 
        se = FALSE, 
        color = "black", 
        linetype = "dashed",
        linewidth = 0.75,
        method.args = list(family = poisson)
      ) +
      facet_wrap(~ deutscher_name, scales = "free_y") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.title = element_blank()
      )
  }, res = 96)
  
  # Tab 2
  
  # Häufigkeiten der Vogelarten
  output$tabelle_haeufigkeiten_vogelarten <- renderTable({
    erkennungen |>
      filter(
        datum >= input$zeitraum_erkennungen[1] & 
        datum <= input$zeitraum_erkennungen[2]
      ) |>
      filter(
        length(input$einschraenkung_vogelarten) == 0 | 
        deutscher_name %in% input$einschraenkung_vogelarten
      ) |> 
      group_by(deutscher_name, wissenschaftlicher_name) |>
      summarise(
        absolute_haeufigkeit = n(), 
        .groups = 'drop'
      ) |>
      mutate(
        relative_haeufigkeit = absolute_haeufigkeit / sum(absolute_haeufigkeit) * 100
      ) |>
      filter(
        absolute_haeufigkeit >= mindesterkennungen
      ) |>
      arrange(
        if (input$sortierung_vogelarten == "aufsteigend") absolute_haeufigkeit 
        else desc(absolute_haeufigkeit)
      ) |>
      mutate(rang = row_number()) |> 
      slice_head(n = input$anzahl_vogelarten) |> 
      select(
        rang,
        deutscher_name,
        wissenschaftlicher_name,
        relative_haeufigkeit,
        absolute_haeufigkeit
      )
  })
  
  # Neue Vogelarten
  output$tabelle_neue_vogelarten <- renderTable({
    erkennungen |>
      group_by(deutscher_name) |>
      filter(n() >= mindesterkennungen) |>
      ungroup() |>
      group_by(
        jahr = year(datum), 
        kalenderwoche = week(datum)
      ) |>
      summarise(
        vogelarten = list(unique(deutscher_name)), 
        .groups = "drop"
      ) |>
      mutate(
        alle_vogelarten = accumulate(vogelarten, union, .init = list())[-1],
        neue_vogelarten = map2(vogelarten, lag(alle_vogelarten, default = list(list())), ~ setdiff(.x, .y))
      ) |>
      select(jahr, kalenderwoche, neue_vogelarten) |>
      unnest_longer(neue_vogelarten) |>
      arrange(desc(jahr), kalenderwoche, neue_vogelarten) |>
      mutate(
        kalenderwoche = str_glue("KW {kalenderwoche}")
      ) |>
      group_by(kalenderwoche) |>
      mutate(
        nummer = row_number(),
        .before = everything()
      ) |>
      pivot_wider(names_from = kalenderwoche, values_from = neue_vogelarten) |>
      select(-jahr, -nummer)
  })
  
  # Tab 3
  
  
}

# Shiny-App starten
shinyApp(ui = ui, server = server)