#' Application UI
#'
#' @return A Shiny UI object.
#' @noRd
app_ui <- function() {

  tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(),

    bslib::page_navbar(
      title  = div(
        style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
        img(src = "www/hexsticker_StatRoad.png", height = "38px"),
        span("StatRoad", style = "font-weight: 600;")
      ),
      theme  = tema_app,
      lang   = "es",
      footer = div(
        class = "text-center small py-2",
        style = paste0("background:", colores$primario, "; color: white;"),
        "Manuel Spínola · ICOMVIS · Universidad Nacional · Costa Rica"
      ),

      # ── Módulos activos ───────────────────────────────────
      bslib::nav_panel(
        title = "Datos y red vial",
        icon  = bsicons::bs_icon("signpost-split"),
        mod_datos_red_ui("datos_red")
      ),
      bslib::nav_panel(
        title = "Escala de agregación",
        icon  = bsicons::bs_icon("rulers"),
        mod_escala_ui("escala")
      ),
      bslib::nav_panel(
        title = "Puntos críticos (KDE)",
        icon  = bsicons::bs_icon("fire"),
        mod_nkde_ui("nkde")
      ),
      # (siguiente: gistar)

      bslib::nav_spacer(),

      bslib::nav_panel(
        title = "Acerca de",
        icon  = bsicons::bs_icon("info-circle"),
        mod_acerca_de_ui("acerca_de")
      ),

      bslib::nav_item(
        tags$span(class = "text-white-50 small",
                  paste0("StatRoad v", utils::packageVersion("StatRoad")))
      )
    )
  )
}

#' Add external resources to the application
#'
#' Registra la carpeta www/ y el <head> de la página (favicon, etc.).
#' Se llama como hermana de page_navbar() dentro de un tagList(),
#' no como argumento de page_navbar() — ese slot solo acepta
#' nav_panel()/nav_menu().
#'
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path("www", app_sys("app/www"))

  tags$head(
    golem::favicon(ext = "png")
  )
}
