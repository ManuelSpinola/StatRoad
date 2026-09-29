# ============================================================
# data-raw/red_ruta1_santarosa.R
# Red vial de ejemplo: Ruta 1 (Interamericana Norte) en la zona
# de influencia del PN Santa Rosa, Área de Conservación Guanacaste.
#
# Fuente: OpenStreetMap (© colaboradores de OpenStreetMap, ODbL).
# Salida: inst/extdata/red_ruta1_santarosa.gpkg
#
# Se ejecuta a mano, una sola vez (o cuando se quiera actualizar
# la red). No forma parte del paquete: data-raw/ está en .Rbuildignore.
# ============================================================

library(osmdata)
library(sf)
library(dplyr)

# ── Parámetros ─────────────────────────────────────────────
# Área de búsqueda (WGS84). Aproximada: verificar en un mapa y
# ajustar si se quiere otro tramo.
bbox_tramo <- c(xmin = -85.72, ymin = 10.75, xmax = -85.55, ymax = 10.95)

# CRS métrico de destino (CRTM05).
# 8908 = CR-SIRGAS / CRTM05  |  5367 = CR05 / CRTM05
# Usar el mismo que usan los datos de los colegas de la UNA.
crs_destino <- 8908

# ── 1. Descargar vías principales del área ─────────────────
consulta <- opq(bbox = bbox_tramo, timeout = 120) |>
  add_osm_feature(key = "highway",
                  value = c("trunk", "trunk_link", "primary"))

osm <- osmdata_sf(consulta)

# ── 2. Revisar cómo está etiquetada la ruta en OSM ─────────
# Antes de filtrar, ver qué valores de 'ref' y 'name' existen.
# (osm_lines es data.frame base, no tibble: no usar print(n = ...))
osm$osm_lines |>
  st_drop_geometry() |>
  count(ref, name, highway, sort = TRUE)

# ── 3. Quedarse solo con la Ruta 1 ─────────────────────────
# En OSM la ref puede venir como "1" o "CR-1", a veces combinada
# con otras rutas ("1;2"). Se aceptan esas variantes.
es_ruta1 <- function(ref) {
  !is.na(ref) & grepl("(^|;)\\s*(CR-)?1\\s*($|;)", ref)
}

red <- osm$osm_lines |>
  filter(es_ruta1(ref)) |>
  select(osm_id, name, ref, highway,
         any_of(c("lanes", "maxspeed", "surface"))) |>
  st_transform(crs_destino)

# ── 4. Verificaciones ──────────────────────────────────────
cat("Segmentos:", nrow(red), "\n")
cat("Longitud total (km):",
    round(sum(as.numeric(st_length(red))) / 1000, 1), "\n")
plot(st_geometry(red), main = "Ruta 1 — zona PN Santa Rosa")

# ── 5. Guardar ─────────────────────────────────────────────
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
st_write(red, "inst/extdata/red_ruta1_santarosa.gpkg",
         layer = "red_vial", delete_dsn = TRUE)
