# ============================================================
# datos_ejemplo.R — Archivos de los datos de ejemplo de StatRoad
# StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# ÚNICO lugar donde se nombran los archivos del ejemplo. Todos los
# módulos los obtienen con archivo_ejemplo(). Para cambiar de
# ejemplo, se cambia solo esta lista.
#
# Ejemplo actual: Ruta 1 (Interamericana Norte) entre Liberia y
# La Cruz, Guanacaste. Red real (OpenStreetMap); estructuras y
# atropellos simulados (data-raw/ejemplo_ruta1_larga.R).
# ============================================================

archivos_ejemplo <- c(
  red         = "red_ruta1_larga.gpkg",
  atropellos  = "atropellos_ruta1_larga.csv",
  estructuras = "estructuras_ruta1_larga.csv",
  verdad      = "hotspots_verdaderos_ruta1_larga.csv"   # hotspots sembrados
)

# Ruta completa de un archivo de ejemplo dentro del paquete
archivo_ejemplo <- function(que = names(archivos_ejemplo)) {
  que <- match.arg(que)
  app_sys("extdata", archivos_ejemplo[[que]])
}
