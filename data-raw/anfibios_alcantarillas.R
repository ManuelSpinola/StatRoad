# ============================================================
# data-raw/anfibios_alcantarillas.R
# Agrega a los atropellos SIMULADOS una mortalidad de anfibios
# concentrada junto a cada alcantarilla (drenajes), para que el
# módulo "Estructuras" tenga una asociación detectable.
#
# Orden de ejecución:
#   1. script original de los atropellos (genera los ids 1–280)
#   2. data-raw/estructuras_simuladas.R
#   3. este script
#
# Idempotente: conserva SOLO los registros 1–280 del archivo y les
# agrega los nuevos, así que correrlo de nuevo no los duplica.
# Los 280 registros originales no cambian: los análisis ya
# validados (K, KDE, Gi*) parten de los mismos datos más estos.
#
# Diseño:
#   - 2 anfibios por alcantarilla válida (las 14 que quedan sobre la
#     vía; no la mal digitada ni los puentes), a ± 80 m (desv. est.)
#     de la alcantarilla a lo largo de la vía.
#   - Fechas en la estación lluviosa (mayo–noviembre de 2024–2025).
#   - Error GPS de 5–30 m perpendicular a la vía, como el resto.
# ============================================================

library(sf)
source("R/utils_espacial.R")
source("R/fct_estructuras.R")

set.seed(20261009)

red <- st_read("inst/extdata/red_ruta1_santarosa.gpkg", quiet = TRUE)
rp  <- preparar_red(red, crs = 8908)
Lm  <- as.numeric(st_length(rp$linea))

# ── Estructuras: alcantarillas válidas y su km ──────────────────
e   <- normalizar_estructuras(read.csv(
         "inst/extdata/estructuras_simuladas_santarosa.csv", fileEncoding = "UTF-8"))
est <- ajustar_a_red(st_as_sf(e, coords = c("x", "y"), crs = 4326, remove = FALSE),
                     rp, tolerancia_m = 50)$ajustados
km_alc <- est$km[est$tipo == "Alcantarilla"]

# ── Atropellos originales (ids 1–280) ───────────────────────────
ruta_atr <- "inst/extdata/atropellos_simulados_santarosa.csv"
atr <- read.csv(ruta_atr, fileEncoding = "UTF-8")
atr <- atr[atr$id <= 280, ]

# ── Nuevos anfibios ─────────────────────────────────────────────
km_nuevos <- unlist(lapply(km_alc, function(k) stats::rnorm(2, k, 0.08)))
# ordenados: st_line_sample devuelve los puntos en orden a lo largo de la línea
km_nuevos <- sort(pmin(pmax(km_nuevos, 0.01), Lm / 1000 - 0.01))
n_nuevos  <- length(km_nuevos)

p0 <- st_cast(st_line_sample(rp$linea, sample = km_nuevos * 1000 / Lm), "POINT")
p1 <- st_cast(st_line_sample(rp$linea,
        sample = pmin(km_nuevos * 1000 + 20, Lm - 1) / Lm), "POINT")
xy0 <- st_coordinates(p0)[, 1:2]; xy1 <- st_coordinates(p1)[, 1:2]
v   <- (xy1 - xy0) / sqrt(rowSums((xy1 - xy0)^2))
desv <- stats::runif(n_nuevos, 5, 30) * sample(c(-1, 1), n_nuevos, replace = TRUE)
xy  <- xy0 + desv * cbind(-v[, 2], v[, 1])
ll  <- st_coordinates(st_transform(st_as_sf(data.frame(xy), coords = 1:2,
                                            crs = 8908), 4326))

especies <- c("Rhinella horribilis", "Incilius luetkenii", "Smilisca baudinii",
              "Leptodactylus fragilis")
fechas <- as.Date(c(paste0(2024, "-05-01"), paste0(2025, "-05-01")))
fecha  <- sample(fechas, n_nuevos, replace = TRUE) +
          sample(0:213, n_nuevos, replace = TRUE)        # 1 may – 30 nov

nuevos <- data.frame(
  id = NA_integer_, especie = sample(especies, n_nuevos, replace = TRUE),
  grupo = "Anfibios", fecha = format(fecha, "%Y-%m-%d"),
  x = round(ll[, 1], 6), y = round(ll[, 2], 6)
)

# ids originales intactos; los nuevos continúan desde 281, por fecha
nuevos    <- nuevos[order(fecha), ]
nuevos$id <- 280L + seq_len(n_nuevos)
todo      <- rbind(atr, nuevos)
write.csv(todo, ruta_atr, row.names = FALSE, fileEncoding = "UTF-8")

cat("Atropellos:", nrow(todo), "(280 originales +", n_nuevos, "anfibios nuevos)\n")
