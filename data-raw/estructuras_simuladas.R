# ============================================================
# data-raw/estructuras_simuladas.R
# Estructuras de paso (alcantarillas y puentes) SIMULADAS sobre la
# red real de la Ruta 1 (PN Santa Rosa) — datos de ejemplo de StatRoad
#
# Diseño (la "verdad" que el análisis debe recuperar):
#   - 5 alcantarillas agrupadas en H1 (km 3.79 ± 0.4): asociación
#     sembrada con el punto crítico dominado por anfibios.
#   - 9 alcantarillas al azar en el resto de la ruta, FUERA de las
#     ventanas de H1, H2 y H3 (± 0.5 km): H3 (reptiles) queda sin
#     estructuras.
#   - 2 puentes al azar, también fuera de las ventanas de H1–H3.
#   - 1 alcantarilla a ~150 m de la vía (error de digitación), para
#     que el ajuste a la vía la excluya.
#   - Error de georreferenciación de 2–10 m en las demás.
#
# Salida: inst/extdata/estructuras_simuladas_santarosa.csv
#         (id, tipo, diametro_m, x, y — lon/lat WGS84)
# ============================================================

library(sf)
source("R/utils_espacial.R")

set.seed(20261008)

red <- st_read("inst/extdata/red_ruta1_santarosa.gpkg", quiet = TRUE)
rp  <- preparar_red(red, crs = 8908)
L   <- as.numeric(st_length(rp$linea)) / 1000          # ~18.94 km

verdad <- read.csv("inst/extdata/hotspots_verdaderos_santarosa.csv",
                   fileEncoding = "UTF-8")
centros <- verdad$centro_km                             # H1, H2, H3

# ── Posiciones (km a lo largo de la vía) ─────────────────────
km_s1 <- sort(runif(5, centros[1] - 0.4, centros[1] + 0.4))

fuera_de_s <- function(km) all(abs(km - centros) > 0.5)
al_azar_fuera <- function(n) {
  out <- numeric(0)
  while (length(out) < n) {
    k <- runif(1, 0.2, L - 0.2)
    if (fuera_de_s(k)) out <- c(out, k)
  }
  out
}
km_fondo   <- al_azar_fuera(9)
km_puentes <- al_azar_fuera(2)
km_error   <- runif(1, 0.2, L - 0.2)

est <- data.frame(
  tipo   = c(rep("Alcantarilla", 5 + 9), rep("Puente", 2), "Alcantarilla"),
  km     = c(km_s1, km_fondo, km_puentes, km_error),
  origen = c(rep("H1", 5), rep("fondo", 9), rep("fondo", 2), "error"),
  desv_m = c(runif(16, 2, 10), 150)
)

# ── km → punto sobre la línea, más un desplazamiento perpendicular ─
punto_en_km <- function(linea, km, desv_m) {
  Lm <- as.numeric(st_length(linea))
  p  <- st_coordinates(st_cast(st_line_sample(linea, sample = km * 1000 / Lm),
                               "POINT"))[, 1:2]
  # dirección local de la vía con un punto 20 m adelante
  q  <- st_coordinates(st_cast(st_line_sample(
          linea, sample = min(km * 1000 + 20, Lm - 1) / Lm), "POINT"))[, 1:2]
  v  <- (q - p) / sqrt(sum((q - p)^2))
  lado <- sample(c(-1, 1), 1)
  p + lado * desv_m * c(-v[2], v[1])
}

xy <- t(mapply(function(k, d) punto_en_km(rp$linea, k, d), est$km, est$desv_m))
pts <- st_transform(st_as_sf(data.frame(xy), coords = 1:2, crs = 8908), 4326)
ll  <- st_coordinates(pts)

o   <- order(est$km)                       # ids E01… en orden de km
est <- est[o, ]
ll  <- ll[o, , drop = FALSE]
salida <- data.frame(
  id         = sprintf("E%02d", seq_len(nrow(est))),
  tipo       = est$tipo,
  diametro_m = ifelse(est$tipo == "Alcantarilla",
                      round(runif(nrow(est), 0.9, 1.8), 1), NA),
  x          = round(ll[, 1], 6),
  y          = round(ll[, 2], 6)
)

write.csv(salida, "inst/extdata/estructuras_simuladas_santarosa.csv",
          row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Registro de la verdad sembrada (para validar; no lo lee la app)
print(cbind(salida[, c("id", "tipo")], km = round(est$km, 2),
            origen = est$origen))
