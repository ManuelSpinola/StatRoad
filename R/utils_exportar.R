# ============================================================
# utils_exportar.R — Descarga de resultados en GeoPackage
# StatRoad · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Un solo .gpkg por descarga, con:
#   - una capa por resultado espacial (sf), en el CRS métrico del
#     análisis (CRTM05);
#   - una tabla "parametros" (sin geometría) con todo lo necesario
#     para reproducir el resultado.
# GPKG en vez de Shapefile: un solo archivo, nombres de campo sin
# límite de 10 caracteres, fechas como fecha; abre directo en QGIS.
# ============================================================

# capas:      lista nombrada de objetos sf o data.frame (nombre = nombre
#             de la capa); los data.frame se escriben como tablas sin
#             geometría (p. ej. curvas o resúmenes de pruebas)
# archivo:    ruta de salida (en un downloadHandler, el argumento 'file')
# parametros: lista nombrada de valores escalares (módulo, tolerancia…)
escribir_gpkg <- function(capas, archivo, parametros = list()) {
  stopifnot(is.list(capas), length(capas) > 0, !is.null(names(capas)),
            all(nzchar(names(capas))))
  # En un downloadHandler 'archivo' siempre es una ruta nueva. Si se
  # reutiliza una ruta ya leída en la sesión, GDAL emite advertencias
  # "sqlite3_open failed" inocuas: el archivo resultante es correcto.
  if (file.exists(archivo)) unlink(archivo)

  for (nombre in names(capas)) {
    capa <- capas[[nombre]]
    if (!is.data.frame(capa)) {
      stop("La capa '", nombre, "' no es un objeto sf ni un data.frame.",
           call. = FALSE)
    }
    sf::st_write(capa, archivo, layer = nombre, quiet = TRUE)
  }

  sf::st_write(tabla_parametros(parametros), archivo, layer = "parametros",
               quiet = TRUE)
  invisible(archivo)
}

# Lista nombrada → data.frame (parametro, valor), con app, versión y fecha
tabla_parametros <- function(parametros = list()) {
  version <- tryCatch(as.character(utils::packageVersion("StatRoad")),
                      error = function(e) NA_character_)
  base <- list(app = "StatRoad", version = version,
               fecha = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  todo <- c(base, parametros)
  data.frame(
    parametro = names(todo),
    valor     = vapply(todo, function(v) paste(format(v), collapse = ", "),
                       character(1)),
    row.names = NULL
  )
}
