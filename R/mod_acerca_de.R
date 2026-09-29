# ============================================================
# mod_acerca_de.R — Información sobre StatRoad
# StatRoad · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

mod_acerca_de_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "py-4 px-3",
      style = "max-width: 780px; margin: 0 auto;",

      h4(
        bs_icon("info-circle", class = "me-2"),
        "Acerca de StatRoad",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(class = "text-muted mb-4",
        "StatRoad es la aplicación de StatSuite para analizar atropellos de ",
        "fauna en redes viales, desarrollada en el ICOMVIS de la Universidad ",
        "Nacional, Costa Rica. Integra la definición de la escala de agregación ",
        "(K de Ripley en red), la identificación de puntos críticos mediante ",
        "estimación de densidad de kernel en red y Getis-Ord Gi*, y análisis ",
        "exploratorios por tramo, taxón y fecha."
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header(bs_icon("collection", class = "me-1"),
                      "StatSuite — Ecosistema completo"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("StatDesign"),    " — Diseño de estudios y muestreo"),
              tags$li(strong("StatFlow"),      " — Primeros análisis y visualización"),
              tags$li(strong("StatGeo"),       " — Análisis espacial y mapas"),
              tags$li(strong("StatMonitor"),   " — Monitoreo poblacional"),
              tags$li(strong("StatModels"),    " — Modelos estadísticos"),
              tags$li(strong("StatDiversity"), " — Diversidad de especies y funcional"),
              tags$li(strong("StatRoad"),      " — Atropellos de fauna en redes viales ← aquí")
            )
          )
        ),

        card(
          card_header(bs_icon("box-seam", class = "me-1"),
                      "Ecosistema R utilizado"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("sf"), " + ", strong("sfnetworks"),
                      " — red vial, limpieza y ajuste de puntos a la vía"),
              tags$li(strong("spNetwork"),
                      " — KDE en red y K de Ripley en red"),
              tags$li(strong("sfdep"),
                      " — Getis-Ord Gi* sobre segmentos"),
              tags$li(strong("ggplot2"), " + ", strong("DT"),
                      " — visualización y tablas interactivas")
            )
          )
        )
      ),

      # Desarrollo
      card(
        class = "mt-3",
        card_header(bs_icon("code-slash", class = "me-1"),
                    "Desarrollo"),
        card_body(
          p(class = "small mb-2",
            bs_icon("person-fill", class = "me-1"),
            strong("Autor:"), " Manuel Spínola — ICOMVIS, ",
            "Universidad Nacional, Costa Rica."),
          p(class = "small mb-2",
            bs_icon("robot", class = "me-1"),
            strong("Asistencia en desarrollo:"), " StatRoad fue desarrollado ",
            "con asistencia de ", strong("Claude (Anthropic)"),
            " para la estructura de módulos, interfaz de usuario, ",
            "lógica del servidor."),
          p(class = "small mb-0",
            bs_icon("building", class = "me-1"),
            strong("Institución:"), " Instituto Internacional en ",
            "Conservación y Manejo de Vida Silvestre (ICOMVIS), ",
            "Universidad Nacional de Costa Rica.")
        )
      ),

      # Cómo citar (datos tomados de DESCRIPTION; ver helpers.R)
      tarjeta_cita("StatRoad", ns),

      div(
        class = "alert alert-info small mt-3 mb-0",
        bs_icon("envelope", class = "me-1"),
        "Contacto: ",
        tags$a(href = "mailto:manuel.spinola@una.ac.cr",
               "manuel.spinola@una.ac.cr")
      )
    )
  )
}

mod_acerca_de_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # sin lógica reactiva
  })
}
