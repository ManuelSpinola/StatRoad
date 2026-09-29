#' Application Server
#'
#' @param input,output,session Internal parameters for Shiny.
#' @noRd
app_server <- function(input, output, session) {
  # Resultado del ajuste a la vía: lo usarán los módulos siguientes
  datos_ajustados <- mod_datos_red_server("datos_red")

  mod_acerca_de_server("acerca_de")

  session$onSessionEnded(function() {})
}
