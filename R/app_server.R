#' Application Server
#'
#' @param input,output,session Internal parameters for Shiny.
#' @noRd
app_server <- function(input, output, session) {
  # Resultado del ajuste a la vía: lo usarán los módulos siguientes
  datos_ajustados <- mod_datos_red_server("datos_red")

  # Escala de agregación: su resultado orientará KDE y Gi*
  escala <- mod_escala_server("escala", datos = datos_ajustados)

  # Puntos críticos por KDE en red (usa la escala como ancho de banda sugerido)
  nkde <- mod_nkde_server("nkde", datos = datos_ajustados, escala = escala)

  # Puntos críticos por segmentos (Getis-Ord Gi*)
  gistar <- mod_gistar_server("gistar", datos = datos_ajustados, escala = escala)

  # Relación entre atropellos y estructuras (alcantarillas, puentes…)
  mod_estructuras_server("estructuras", datos = datos_ajustados)

  mod_acerca_de_server("acerca_de")

  session$onSessionEnded(function() {})
}
