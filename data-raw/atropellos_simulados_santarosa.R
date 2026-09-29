# ============================================================
# data-raw/atropellos_simulados_santarosa.R
# Atropellos SIMULADOS sobre la red real de la Ruta 1
# (zona PN Santa Rosa, ACG). Dataset didáctico de StatRoad.
#
# Entrada: inst/extdata/red_ruta1_santarosa.gpkg
#          (generada con data-raw/red_ruta1_santarosa.R)
# Salidas: inst/extdata/atropellos_simulados_santarosa.csv
#          inst/extdata/hotspots_verdaderos_santarosa.csv
#
# Características sembradas a propósito (la "verdad" conocida):
#   - Fondo: eventos al azar en todo el tramo.
#   - 3 hotspots (~1 km cada uno) con composición distinta:
#       H1 dominado por anfibios, concentrados al inicio de lluvias
#       H2 dominado por mamíferos
#       H3 dominado por reptiles
#   - Error de GPS de 5–30 m perpendicular a la vía.
#   - 2 % de puntos a 100–300 m de la vía (fuera de tolerancia).
# Las ubicaciones de los hotspots son ARBITRARIAS: no representan
# sitios reales de la ruta.
# ============================================================

library(sf)
library(dplyr)

set.seed(2026)

# ── 1. Red: unir los segmentos en una sola línea ───────────
red <- st_read("inst/extdata/red_ruta1_santarosa.gpkg", quiet = TRUE)

linea <- red |> st_union() |> st_line_merge()
if (st_geometry_type(linea) != "LINESTRING") {
  stop("Los segmentos no se unen en una sola línea continua ",
       "(hay huecos en la red). Revisar red_ruta1_santarosa.gpkg.")
}

# Orientar la línea de sur a norte: el km 0 queda en el extremo sur
extremos <- st_coordinates(linea)[, 1:2]
if (extremos[1, 2] > extremos[nrow(extremos), 2]) linea <- st_reverse(linea)

L <- as.numeric(st_length(linea))   # metros
cat("Longitud del tramo:", round(L / 1000, 2), "km\n")

# ── 2. Catálogo de especies (bosque seco, Guanacaste) ──────
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

# ── 3. Fechas: 2024–2025, estacionalidad de Guanacaste ─────
fechas <- seq(as.Date("2024-01-01"), as.Date("2025-12-31"), by = "day")
mes    <- as.integer(format(fechas, "%m"))
# Anfibios: solo en lluvias (may–nov), con pico al inicio (may–jun)
peso_anfibios <- ifelse(mes %in% 5:6, 4, ifelse(mes %in% 7:11, 1, 0))

# ── 4. Generador de eventos ────────────────────────────────
# pos_m: posición a lo largo de la vía (m desde el extremo sur)
# prob:  composición por grupo, en el orden de 'grupos'
generar <- function(pos_m, prob, origen) {
  n     <- length(pos_m)
  grupo <- sample(grupos, n, replace = TRUE, prob = prob)
  fecha <- as.Date(vapply(grupo, function(g) {
    if (g == "Anfibios") {
      sample(fechas, 1, prob = peso_anfibios)
    } else {
      sample(fechas, 1)
    }
  }, numeric(1)), origin = "1970-01-01")
  especie <- vapply(grupo, function(g) sample(especies[[g]], 1), character(1))
  data.frame(pos_m = pos_m, grupo = grupo, especie = unname(especie),
             fecha = fecha, origen = origen)
}

# Fondo: al azar en todo el tramo
fondo <- generar(runif(150, 0, L),
                 prob = c(0.35, 0.30, 0.15, 0.20), origen = "fondo")

# Hotspots: posición ~ Normal(centro, 250 m)  →  ~1 km de ancho
hotspots <- data.frame(
  hotspot     = c("H1", "H2", "H3"),
  descripcion = c("Dominado por anfibios (inicio de lluvias)",
                  "Dominado por mamíferos",
                  "Dominado por reptiles"),
  centro_m    = c(0.20, 0.50, 0.80) * L,
  sd_m        = 250,
  n           = c(45, 40, 45)
)
prob_hot <- list(
  H1 = c(0.15, 0.10, 0.65, 0.10),   # Mamíferos, Reptiles, Anfibios, Aves
  H2 = c(0.60, 0.15, 0.10, 0.15),
  H3 = c(0.15, 0.60, 0.10, 0.15)
)

eventos_hot <- do.call(rbind, lapply(seq_len(nrow(hotspots)), function(i) {
  h   <- hotspots[i, ]
  pos <- rnorm(h$n, h$centro_m, h$sd_m)
  pos <- pmin(pmax(pos, 1), L - 1)
  generar(pos, prob = prob_hot[[h$hotspot]], origen = h$hotspot)
}))

eventos <- rbind(fondo, eventos_hot) |> arrange(fecha, pos_m)
n <- nrow(eventos)

# ── 5. Posición sobre la vía + error de GPS ────────────────
punto_en <- function(frac) {
  st_coordinates(st_line_sample(linea, sample = frac))[1, 1:2]
}
frac  <- eventos$pos_m / L
xy    <- t(vapply(frac, punto_en, numeric(2)))
# segundo punto 1 m más adelante (o atrás en el extremo) para la tangente
frac2 <- ifelse(frac + 1 / L <= 1, frac + 1 / L, frac - 1 / L)
xy2   <- t(vapply(frac2, punto_en, numeric(2)))

tang   <- xy2 - xy
tang   <- tang / sqrt(rowSums(tang^2))
normal <- cbind(-tang[, 2], tang[, 1])

dist_m <- runif(n, 5, 30)
fuera  <- sample(n, round(0.02 * n))
dist_m[fuera] <- runif(length(fuera), 100, 300)
lado   <- sample(c(-1, 1), n, replace = TRUE)

xy_obs <- xy + normal * dist_m * lado

# ── 6. Pasar a lon/lat (como vendrían de un GPS) ───────────
pts_ll <- st_as_sf(data.frame(xy_obs), coords = c(1, 2), crs = st_crs(red)) |>
  st_transform(4326) |>
  st_coordinates()

atropellos <- data.frame(
  id      = seq_len(n),
  especie = eventos$especie,
  grupo   = eventos$grupo,
  fecha   = format(eventos$fecha, "%Y-%m-%d"),
  x       = round(pts_ll[, 1], 6),
  y       = round(pts_ll[, 2], 6)
)

# ── 7. La "verdad" sembrada ────────────────────────────────
centros_ll <- t(vapply(hotspots$centro_m / L, punto_en, numeric(2))) |>
  data.frame() |>
  st_as_sf(coords = c(1, 2), crs = st_crs(red)) |>
  st_transform(4326) |>
  st_coordinates()

verdad <- data.frame(
  hotspot     = hotspots$hotspot,
  descripcion = hotspots$descripcion,
  centro_km   = round(hotspots$centro_m / 1000, 2),
  ancho_m     = 4 * hotspots$sd_m,
  n_eventos   = hotspots$n,
  x           = round(centros_ll[, 1], 6),
  y           = round(centros_ll[, 2], 6)
)

# ── 8. Verificaciones ──────────────────────────────────────
cat("Atropellos:", n, " | fuera de tolerancia (>100 m):", length(fuera), "\n")
print(table(eventos$origen, eventos$grupo))
print(verdad)

plot(st_geometry(linea), main = "Atropellos simulados — Ruta 1, Santa Rosa")
plot(st_as_sf(data.frame(xy_obs), coords = c(1, 2), crs = st_crs(red)),
     add = TRUE, pch = 20, cex = 0.6, col = "#FC7D0B")

# ── 9. Guardar ─────────────────────────────────────────────
write.csv(atropellos, "inst/extdata/atropellos_simulados_santarosa.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(verdad, "inst/extdata/hotspots_verdaderos_santarosa.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
