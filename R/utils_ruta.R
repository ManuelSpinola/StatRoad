# ============================================================
# utils_ruta.R — Cálculos sobre una ruta continua
# StatRoad · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Funciones puras (sin Shiny). En una ruta continua la distancia
# por la red entre dos puntos es |km_i - km_j|, así que el KDE en
# red se calcula de forma exacta sobre la posición a lo largo de
# la vía, con corrección de borde en los extremos.
# ============================================================

# ── Lixels: dividir la ruta en tramos de igual longitud ────
# fusionar_resto = TRUE: si el último tramo queda más corto que la
# mitad del largo, se une al anterior (útil cuando los tramos se
# comparan por conteos, como en Gi*).
lixelar_ruta <- function(linea, largo_m, fusionar_resto = FALSE) {
  xy <- sf::st_coordinates(linea)[, 1:2, drop = FALSE]
  # quitar vértices consecutivos repetidos (tramos de largo cero)
  keep <- c(TRUE, rowSums(diff(xy)^2) > 0)
  xy   <- xy[keep, , drop = FALSE]

  seglen <- sqrt(rowSums(diff(xy)^2))
  cum    <- c(0, cumsum(seglen))
  L      <- cum[length(cum)]

  cortes <- unique(c(seq(0, L, by = largo_m), L))
  if (fusionar_resto && length(cortes) > 2 &&
      (L - cortes[length(cortes) - 1]) < largo_m / 2) {
    cortes <- cortes[-(length(cortes) - 1)]
  }

  interp <- function(t) {
    j <- findInterval(t, cum, rightmost.closed = TRUE, all.inside = TRUE)
    f <- (t - cum[j]) / seglen[j]
    xy[j, ] + (xy[j + 1, ] - xy[j, ]) * f
  }

  geoms <- lapply(seq_len(length(cortes) - 1), function(i) {
    a <- cortes[i]
    b <- cortes[i + 1]
    dentro <- which(cum > a & cum < b)
    sf::st_linestring(rbind(interp(a), xy[dentro, , drop = FALSE], interp(b)))
  })

  ini <- cortes[-length(cortes)]
  fin <- cortes[-1]
  sf::st_sf(
    km_ini    = ini / 1000,
    km_fin    = fin / 1000,
    km_centro = (ini + fin) / 2000,
    geometry  = sf::st_sfc(geoms, crs = sf::st_crs(linea))
  )
}

# ── Kernel cuártico (biponderado) ──────────────────────────
# K(u) = 15/16 (1 - u^2)^2 en |u| <= 1; h = radio de influencia.
kernel_cuartico <- function(u) {
  ifelse(abs(u) <= 1, 15 / 16 * (1 - u^2)^2, 0)
}
# Función de distribución del kernel (para la corrección de borde)
fda_cuartico <- function(u) {
  u <- pmin(pmax(u, -1), 1)
  0.5 + 15 / 16 * (u - 2 * u^3 / 3 + u^5 / 5)
}

# ── KDE en la ruta (atropellos por km) ─────────────────────
# x: posiciones de los registros (m); centros: dónde evaluar (m);
# L: longitud de la ruta (m); h: ancho de banda (m).
# Corrección de borde: cada registro reparte una masa de 1 dentro
# de [0, L], aunque esté cerca de un extremo.
kde_ruta <- function(x, centros, L, h) {
  masa <- fda_cuartico((L - x) / h) - fda_cuartico((0 - x) / h)
  u    <- outer(centros, x, "-") / h
  dens <- kernel_cuartico(u) %*% (1 / (h * masa))   # eventos por metro
  as.vector(dens) * 1000                             # eventos por km
}

# ── Umbral por Monte Carlo (KDE+, Bíl et al. 2013) ─────────
# Máximo de la densidad en cada simulación al azar uniforme;
# el umbral es el cuantil 'nivel' de esos máximos (control global).
umbral_kde_ruta <- function(n, centros, L, h, nsim, nivel = 0.95) {
  maximos <- vapply(seq_len(nsim), function(i) {
    max(kde_ruta(stats::runif(n, 0, L), centros, L, h))
  }, numeric(1))
  list(umbral = unname(stats::quantile(maximos, nivel)), maximos = maximos)
}

# ── Agrupar lixels consecutivos sobre el umbral ────────────
# Devuelve una fila por punto crítico, con los registros que caen
# dentro y el grupo dominante.
puntos_criticos_ruta <- function(lixels, sig, registros_km, registros_grupo) {
  if (!any(sig)) return(NULL)
  r   <- rle(sig)
  fin <- cumsum(r$lengths)
  ini <- fin - r$lengths + 1
  tramos <- which(r$values)

  filas <- lapply(seq_along(tramos), function(k) {
    i <- ini[tramos[k]]
    j <- fin[tramos[k]]
    km_a <- lixels$km_ini[i]
    km_b <- lixels$km_fin[j]
    dentro <- registros_km >= km_a & registros_km <= km_b
    tab <- sort(table(registros_grupo[dentro]), decreasing = TRUE)
    data.frame(
      punto_critico   = paste0("PC", k),
      km_inicio       = round(km_a, 2),
      km_fin          = round(km_b, 2),
      longitud_m      = round((km_b - km_a) * 1000),
      registros       = sum(dentro),
      densidad_max    = round(max(lixels$densidad[i:j]), 1),
      grupo_dominante = if (length(tab)) names(tab)[1] else NA_character_,
      pct_dominante   = if (length(tab)) round(100 * tab[[1]] / sum(dentro)) else NA
    )
  })
  do.call(rbind, filas)
}

# ── Getis-Ord Gi* en segmentos de una ruta ─────────────────
# conteos: atropellos por segmento, en orden a lo largo de la ruta.
# largos:  largo de cada segmento (cualquier unidad).
# Vecindad binaria: el segmento más k vecinos a cada lado (Gi* incluye
# al propio segmento). Hipótesis nula: los N registros se distribuyen
# al azar uniforme a lo largo de la ruta (multinomial con probabilidad
# proporcional al largo), la misma nula de K y KDE+.
# Se reporta el z de la simulación: (observado - media) / desviación.
# El z analítico no se usa: su desviación se calcula con todos los
# conteos, incluidos los hotspots, y subestima la señal.
# La permutación condicional (estándar en polígonos) tampoco: con solo
# 2 vecinos por segmento, fijar el conteo propio deja la prueba casi
# sin potencia. Valores p corregidos por FDR.
# bilateral = FALSE (por defecto): prueba unilateral de hotspots, la
# pregunta de gestión. bilateral = TRUE: también coldspots; ojo, un
# coldspot significa "por debajo del promedio de toda la ruta, que
# incluye los puntos críticos", no un tramo sin riesgo.
gistar_ruta <- function(conteos, largos, k = 1, nsim = 999, bilateral = FALSE) {
  n <- length(conteos)
  W <- outer(seq_len(n), seq_len(n), function(i, j) abs(i - j) <= k) * 1
  obs  <- as.vector(W %*% conteos)
  sims <- W %*% stats::rmultinom(nsim, sum(conteos), prob = largos / sum(largos))

  media <- rowMeans(sims)
  desv  <- apply(sims, 1, stats::sd)
  z     <- ifelse(desv > 0, (obs - media) / desv, 0)

  p <- if (bilateral) {
    extremos <- pmin(rowSums(sims >= obs), rowSums(sims <= obs))
    pmin(1, 2 * (extremos + 1) / (nsim + 1))
  } else {
    (rowSums(sims >= obs) + 1) / (nsim + 1)
  }

  data.frame(suma_local = obs, esperado = media, z = z,
             p_sim = p, p_fdr = stats::p.adjust(p, method = "BH"))
}

# Categoría de cada segmento según el signo de z y el p corregido
niveles_gistar <- c("Hotspot 99 %", "Hotspot 95 %", "Hotspot 90 %",
                    "No significativo",
                    "Coldspot 90 %", "Coldspot 95 %", "Coldspot 99 %")

clasificar_gistar <- function(z, p, bilateral = FALSE) {
  nivel <- ifelse(p < 0.01, "99 %", ifelse(p < 0.05, "95 %",
                  ifelse(p < 0.10, "90 %", NA)))
  signo_ok <- if (bilateral) TRUE else z > 0
  cat <- ifelse(is.na(nivel) | is.na(z) | !signo_ok, "No significativo",
                paste(ifelse(z > 0, "Hotspot", "Coldspot"), nivel))
  factor(cat, levels = niveles_gistar)
}

# Colores (Tableau Color Blind): cálidos = hotspot, fríos = coldspot
colores_gistar <- c(
  "Hotspot 99 %"     = "#C85200",
  "Hotspot 95 %"     = "#FC7D0B",
  "Hotspot 90 %"     = "#F1CE63",
  "No significativo" = "#A3ACB9",
  "Coldspot 90 %"    = "#7BC8ED",
  "Coldspot 95 %"    = "#5FA2CE",
  "Coldspot 99 %"    = "#1170AA"
)
