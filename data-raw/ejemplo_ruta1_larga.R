# ============================================================
# data-raw/ejemplo_ruta1_larga.R
# Ejemplo grande de StatRoad: estructuras y atropellos SIMULADOS
# sobre la red real de la Ruta 1 entre Liberia y La Cruz (~51 km).
# Basado en data-raw/atropellos_simulados_santarosa.R (mismas
# especies, fechas, estacionalidad, error de GPS y puntos fuera).
#
# Entradas (de data-raw/red_ruta1_larga.R):
#   data-raw/red_ruta1_larga.gpkg     red vial (OSM)
#   data-raw/cruces_agua_ruta1.gpkg   cruces de la ruta con ríos (OSM)
# Salidas:
#   inst/extdata/red_ruta1_larga.gpkg
#   inst/extdata/estructuras_ruta1_larga.csv
#   inst/extdata/atropellos_ruta1_larga.csv
#   inst/extdata/hotspots_verdaderos_ruta1_larga.csv
#
# Lo sembrado (la "verdad" conocida):
#   ESTRUCTURAS
#   - Los 7 cruces reales de OSM: puentes en ríos, alcantarillas en
#     quebradas (fuente = "OSM").
#   - Alcantarillas simuladas cada ~1.2 km en promedio (separadas al
#     menos 400 m), fuera de los sectores de H5 y H6 (fuente = "simulada").
#   - 1 alcantarilla mal digitada a ~150 m de la vía.
#   ATROPELLOS (2024–2025)
#   - Fondo al azar en toda la ruta.
#   - 6 hotspots de ~500 m (sd = 125 m):
#       H1 anfibios — Quebrada Peor es Nada (alcantarilla, OSM)
#       H2 mamíferos — Río Tempisquito (puente, OSM): corredor ribereño
#       H3 anfibios — alcantarilla (simulada), km ~31
#       H4 mixto — alcantarilla (simulada), km ~40
#       H5 mamíferos — borde de bosque, SIN estructuras, km ~26
#       H6 reptiles — zona abierta, SIN estructuras, km ~46
#   - 30 % de los anfibios del fondo cruzan junto a alcantarillas
#     (± 80 m): mortalidad asociada a drenajes en toda la ruta.
#   - Error de GPS de 5–30 m; 2 % de puntos a 100–300 m de la vía.
# Las ubicaciones de los hotspots son ARBITRARIAS.
# ============================================================

library(sf)
library(dplyr)

set.seed(2026)

# ── 1. Red: una sola línea, km 0 en el extremo sur ─────────
red <- st_read("data-raw/red_ruta1_larga.gpkg", quiet = TRUE)

linea <- red |> st_union() |> st_line_merge()
if (st_geometry_type(linea) != "LINESTRING") {
  stop("Los segmentos no se unen en una sola línea continua.")
}
extremos <- st_coordinates(linea)[, 1:2]
if (extremos[1, 2] > extremos[nrow(extremos), 2]) linea <- st_reverse(linea)

L <- as.numeric(st_length(linea))   # metros
cat("Longitud del tramo:", round(L / 1000, 2), "km\n")

# Posición (m) de un punto a lo largo de la línea
pos_en_linea <- function(pts) {
  xy <- st_coordinates(linea)[, 1:2]
  A  <- xy[-nrow(xy), , drop = FALSE]; AB <- xy[-1, , drop = FALSE] - A
  len <- sqrt(rowSums(AB^2)); acum <- c(0, cumsum(len))[seq_len(nrow(A))]
  P <- st_coordinates(pts)[, 1:2, drop = FALSE]
  vapply(seq_len(nrow(P)), function(i) {
    t <- pmin(pmax(((P[i, 1] - A[, 1]) * AB[, 1] + (P[i, 2] - A[, 2]) * AB[, 2]) /
                     pmax(len^2, 1e-12), 0), 1)
    j <- which.min((P[i, 1] - A[, 1] - AB[, 1] * t)^2 + (P[i, 2] - A[, 2] - AB[, 2] * t)^2)
    acum[j] + t[j] * len[j]
  }, numeric(1))
}

# Punto sobre la vía a pos_m, desplazado d_m perpendicularmente
punto_en <- function(frac) {
  st_coordinates(st_line_sample(linea, sample = frac))[1, 1:2]
}
ubicar <- function(pos_m, d_m) {
  frac  <- pmin(pmax(pos_m / L, 0), 1)
  xy    <- t(vapply(frac, punto_en, numeric(2)))
  frac2 <- ifelse(frac + 1 / L <= 1, frac + 1 / L, frac - 1 / L)
  xy2   <- t(vapply(frac2, punto_en, numeric(2)))
  tang  <- (xy2 - xy) / sqrt(rowSums((xy2 - xy)^2))
  lado  <- sample(c(-1, 1), length(pos_m), replace = TRUE)
  xy + cbind(-tang[, 2], tang[, 1]) * d_m * lado
}
a_lonlat <- function(xy) {
  st_as_sf(data.frame(xy), coords = c(1, 2), crs = st_crs(red)) |>
    st_transform(4326) |> st_coordinates()
}

# ── 2. Estructuras ─────────────────────────────────────────
cruces <- st_read("data-raw/cruces_agua_ruta1.gpkg", quiet = TRUE) |>
  st_transform(st_crs(red))
osm <- data.frame(
  pos_m  = pos_en_linea(cruces),
  tipo   = ifelse(cruces$waterway == "river", "Puente", "Alcantarilla"),
  nombre = cruces$name,
  fuente = "OSM"
)

# Hotspots: posición del centro (m)
pos_osm <- function(nombre) osm$pos_m[osm$nombre == nombre]
hotspots <- data.frame(
  hotspot     = paste0("H", 1:6),
  descripcion = c("Anfibios — Quebrada Peor es Nada (alcantarilla)",
                  "Mamíferos — Río Tempisquito (puente)",
                  "Anfibios — alcantarilla",
                  "Mixto — alcantarilla",
                  "Mamíferos — borde de bosque, sin estructuras",
                  "Reptiles — zona abierta, sin estructuras"),
  centro_m    = c(pos_osm("Quebrada Peor es Nada"), pos_osm("Río Tempisquito"),
                  31000, 40000, 26000, 46000),
  sd_m        = 125,
  n           = c(70, 60, 70, 60, 60, 60),
  estructura  = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
)

# Alcantarillas simuladas: las de H3 y H4 fijas; el resto al azar,
# separadas >= 400 m de cualquier otra estructura y fuera de ± 1 km
# de H5 y H6 (sectores sin drenajes)
sim_pos <- hotspots$centro_m[3:4]
sin_estr <- hotspots$centro_m[5:6]
objetivo <- round(L / 1200)                       # ~1 estructura cada 1.2 km
intentos <- 0
while (nrow(osm) + length(sim_pos) < objetivo && intentos < 1e5) {
  intentos <- intentos + 1
  p <- runif(1, 300, L - 300)
  if (all(abs(p - c(osm$pos_m, sim_pos)) >= 400) && all(abs(p - sin_estr) > 1000)) {
    sim_pos <- c(sim_pos, p)
  }
}
sim <- data.frame(pos_m = sim_pos, tipo = "Alcantarilla", nombre = NA_character_,
                  fuente = "simulada")
error <- data.frame(pos_m = runif(1, 1000, L - 1000), tipo = "Alcantarilla",
                    nombre = NA_character_, fuente = "simulada")

est <- rbind(osm, sim, error) |> arrange(pos_m)
d_est <- ifelse(seq_len(nrow(est)) == which(est$pos_m == error$pos_m), 150,
                runif(nrow(est), 2, 10))
xy_est <- a_lonlat(ubicar(est$pos_m, d_est))

estructuras <- data.frame(
  id         = sprintf("E%02d", seq_len(nrow(est))),
  tipo       = est$tipo,
  nombre     = est$nombre,
  fuente     = est$fuente,
  diametro_m = ifelse(est$tipo == "Alcantarilla", round(runif(nrow(est), 0.9, 2.0), 1), NA),
  x          = round(xy_est[, 1], 6),
  y          = round(xy_est[, 2], 6)
)
alc_pos <- est$pos_m[est$tipo == "Alcantarilla" & d_est < 100]   # sin la mal digitada

# ── 3. Catálogo de especies y fechas (como el ejemplo de Santa Rosa) ─
especies <- list(
  "Mamíferos" = c("Didelphis marsupialis", "Nasua narica", "Procyon lotor",
                   "Canis latrans", "Odocoileus virginianus",
                   "Tamandua mexicana", "Dasypus novemcinctus"),
  "Reptiles"   = c("Ctenosaura similis", "Iguana iguana", "Boa imperator",
                   "Crotalus simus", "Drymarchon melanurus"),
  "Anfibios"   = c("Incilius luetkenii", "Rhinella horribilis",
                   "Smilisca baudinii", "Leptodactylus fragilis"),
  "Aves"       = c("Nyctidromus albicollis", "Columbina inca",
                   "Calocitta formosa", "Crotophaga sulcirostris")
)
grupos <- names(especies)
fechas <- seq(as.Date("2024-01-01"), as.Date("2025-12-31"), by = "day")
mes    <- as.integer(format(fechas, "%m"))
peso_anfibios <- ifelse(mes %in% 5:6, 4, ifelse(mes %in% 7:11, 1, 0))

generar <- function(pos_m, prob, origen, grupo = NULL) {
  n <- length(pos_m)
  if (is.null(grupo)) grupo <- sample(grupos, n, replace = TRUE, prob = prob)
  fecha <- as.Date(vapply(grupo, function(g) {
    if (g == "Anfibios") sample(fechas, 1, prob = peso_anfibios) else sample(fechas, 1)
  }, numeric(1)), origin = "1970-01-01")
  especie <- vapply(grupo, function(g) sample(especies[[g]], 1), character(1))
  data.frame(pos_m = pos_m, grupo = grupo, especie = unname(especie),
             fecha = fecha, origen = origen)
}

# ── 4. Atropellos ──────────────────────────────────────────
# Fondo: al azar en toda la ruta (Mamíferos, Reptiles, Anfibios, Aves)
fondo <- generar(runif(1000, 0, L), prob = c(0.35, 0.30, 0.15, 0.20), origen = "fondo")

# 30 % de los anfibios del fondo: junto a alcantarillas (± 80 m)
anf <- which(fondo$grupo == "Anfibios")
dren <- sample(anf, round(0.30 * length(anf)))
fondo$pos_m[dren] <- pmin(pmax(rnorm(length(dren), sample(alc_pos, length(dren),
                                                          replace = TRUE), 80), 1), L - 1)
fondo$origen[dren] <- "drenaje"

prob_hot <- list(
  H1 = c(0.10, 0.10, 0.70, 0.10),   # Mamíferos, Reptiles, Anfibios, Aves
  H2 = c(0.65, 0.10, 0.10, 0.15),
  H3 = c(0.10, 0.10, 0.70, 0.10),
  H4 = c(0.30, 0.25, 0.25, 0.20),
  H5 = c(0.65, 0.10, 0.05, 0.20),
  H6 = c(0.10, 0.65, 0.05, 0.20)
)
eventos_hot <- do.call(rbind, lapply(seq_len(nrow(hotspots)), function(i) {
  h   <- hotspots[i, ]
  pos <- pmin(pmax(rnorm(h$n, h$centro_m, h$sd_m), 1), L - 1)
  generar(pos, prob = prob_hot[[h$hotspot]], origen = h$hotspot)
}))

eventos <- rbind(fondo, eventos_hot) |> arrange(fecha, pos_m)
n <- nrow(eventos)

# Error de GPS y puntos fuera de la vía
dist_m <- runif(n, 5, 30)
fuera  <- sample(n, round(0.02 * n))
dist_m[fuera] <- runif(length(fuera), 100, 300)
xy_atr <- a_lonlat(ubicar(eventos$pos_m, dist_m))

atropellos <- data.frame(
  id      = seq_len(n),
  especie = eventos$especie,
  grupo   = eventos$grupo,
  fecha   = format(eventos$fecha, "%Y-%m-%d"),
  x       = round(xy_atr[, 1], 6),
  y       = round(xy_atr[, 2], 6)
)

# ── 5. La "verdad" sembrada ────────────────────────────────
xy_h <- a_lonlat(ubicar(hotspots$centro_m, rep(0, nrow(hotspots))))
verdad <- data.frame(
  hotspot     = hotspots$hotspot,
  descripcion = hotspots$descripcion,
  centro_km   = round(hotspots$centro_m / 1000, 2),
  ancho_m     = 4 * hotspots$sd_m,
  n_eventos   = hotspots$n,
  con_estructura = hotspots$estructura,
  x           = round(xy_h[, 1], 6),
  y           = round(xy_h[, 2], 6)
)

# ── 6. Verificaciones ──────────────────────────────────────
cat("Estructuras:", nrow(estructuras), "\n"); print(table(estructuras$fuente, estructuras$tipo))
cat("Atropellos:", n, " | fuera de tolerancia (>100 m):", length(fuera), "\n")
print(table(eventos$origen, eventos$grupo))
print(verdad[, 1:6])

# ── 7. Guardar ─────────────────────────────────────────────
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
st_write(red, "inst/extdata/red_ruta1_larga.gpkg", layer = "red_vial",
         delete_dsn = TRUE, quiet = TRUE)
write.csv(estructuras, "inst/extdata/estructuras_ruta1_larga.csv",
          row.names = FALSE, fileEncoding = "UTF-8", na = "")
write.csv(atropellos, "inst/extdata/atropellos_ruta1_larga.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(verdad, "inst/extdata/hotspots_verdaderos_ruta1_larga.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
