# ============================================================
# data-raw/red_ruta1_larga.R
# Red vial de ejemplo (versión larga): Ruta 1 (Interamericana Norte)
# entre Liberia y La Cruz, Área de Conservación Guanacaste, y los
# cruces de la ruta con ríos y quebradas (donde van las alcantarillas
# y puentes del ejemplo).
# Basado en data-raw/red_ruta1_santarosa.R.
#
# Fuente: OpenStreetMap (© colaboradores de OpenStreetMap, ODbL),
# extracto de Costa Rica de Geofabrik, leído con osmextract. No usa
# el servidor Overpass (inestable), así que es reproducible.
# La primera vez descarga ~100 MB a la carpeta dir_osm; las
# siguientes reutiliza ese archivo.
#
# Salida (para revisión): data-raw/red_ruta1_larga.gpkg
#                         data-raw/cruces_agua_ruta1.gpkg
# ============================================================

library(osmextract)
library(sf)
library(dplyr)

# ── Parámetros ─────────────────────────────────────────────
# Área de búsqueda (WGS84): de Liberia (sur) hacia La Cruz (norte).
# Con esta área la Ruta 1 mide ~66 km continuos.
caja <- st_as_sfc(st_bbox(c(xmin = -85.75, ymin = 10.68,
                            xmax = -85.45, ymax = 11.08), crs = 4326))

# CRS métrico de destino (CRTM05).
# 8908 = CR-SIRGAS / CRTM05  |  5367 = CR05 / CRTM05
crs_destino <- 8908

# Carpeta donde se guarda el extracto de Geofabrik (fuera del proyecto,
# para no subir 100 MB a GitHub)
dir_osm <- file.path(path.expand("~"), "osm_geofabrik")
dir.create(dir_osm, showWarnings = FALSE)

# ── 1. Leer las líneas de OSM del área (carreteras y ríos) ──
lineas <- oe_get(
  "Costa Rica",
  provider           = "geofabrik",
  layer              = "lines",
  extra_tags         = c("ref", "bridge", "tunnel"),
  boundary           = caja,
  boundary_type      = "clipsrc",
  download_directory = dir_osm,
  quiet              = FALSE
)

# ── 2. Revisar cómo está etiquetada la ruta en OSM ─────────
lineas |>
  st_drop_geometry() |>
  filter(highway %in% c("trunk", "trunk_link", "primary")) |>
  count(ref, name, highway, sort = TRUE) |>
  print()

# ── 3. Quedarse solo con la Ruta 1 ─────────────────────────
# En OSM la ref puede venir como "1" o "CR-1", a veces combinada
# con otras rutas ("1;2"). Se aceptan esas variantes.
es_ruta1 <- function(ref) {
  !is.na(ref) & grepl("(^|;)\\s*(CR-)?1\\s*($|;)", ref)
}

red <- lineas |>
  filter(highway %in% c("trunk", "trunk_link", "primary"), es_ruta1(ref)) |>
  select(osm_id, name, ref, highway, any_of("bridge")) |>
  st_cast("LINESTRING") |>
  st_transform(crs_destino)

# ── 4. Verificaciones ──────────────────────────────────────
cat("Segmentos:", nrow(red), "\n")
cat("Longitud total (km):",
    round(sum(as.numeric(st_length(red))) / 1000, 1), "\n")
cat("¿Una sola línea continua?:",
    identical(as.character(st_geometry_type(
      st_line_merge(st_union(st_geometry(red))))), "LINESTRING"), "\n")
plot(st_geometry(red), main = "Ruta 1 — Liberia a La Cruz")

# ── 5. Guardar (en data-raw/, para revisar) ────────────────
st_write(red, "data-raw/red_ruta1_larga.gpkg",
         layer = "red_vial", delete_dsn = TRUE)

# ── 6. Cruces de la ruta con ríos y quebradas ──────────────
# Solo ríos y quebradas (river, stream): ahí están las alcantarillas
# y puentes.
agua <- lineas |>
  filter(waterway %in% c("river", "stream")) |>
  select(osm_id, name, waterway, any_of("tunnel")) |>
  st_transform(crs_destino)

cruces <- st_intersection(agua, st_union(st_geometry(red))) |>
  st_cast("POINT")

cat("Cruces de agua con la Ruta 1:", nrow(cruces), "\n")
print(table(cruces$waterway, useNA = "ifany"))
plot(st_geometry(cruces), add = TRUE, col = "blue", pch = 19)

st_write(cruces, "data-raw/cruces_agua_ruta1.gpkg",
         layer = "cruces_agua", delete_dsn = TRUE)
