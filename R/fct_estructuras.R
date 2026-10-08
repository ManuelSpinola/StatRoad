# ============================================================
# fct_estructuras.R — Relación entre atropellos y estructuras
# (alcantarillas, puentes, pasos de fauna…) a lo largo de la vía
# StatRoad · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Funciones puras (sin Shiny): se pueden probar desde la consola.
# Requieren una ruta continua: todo se mide en km a lo largo de la
# vía (salida de ajustar_a_red() en utils_espacial.R), así que la
# distancia en red entre dos puntos es |km1 - km2|.
#
# Modelo nulo (las estructuras quedan SIEMPRE fijas):
#   "circular": desplaza todos los atropellos juntos una distancia
#               al azar (módulo L). Conserva la agregación entre
#               atropellos; solo rompe su posición respecto a las
#               estructuras.
#   "uniforme": reubica cada atropello al azar e independientemente.
#               SOLO para la demostración didáctica: con atropellos
#               agregados es anticonservador (error tipo I de 22–84 %
#               en la validación con el ejemplo). No se ofrece como
#               opción de análisis.
#
# Métrica circular: los estadísticos miden distancias sobre el
# anillo de largo L (los extremos de la ruta se "unen"). Así, rotar
# los atropellos equivale a rotar las estructuras y la prueba es
# exacta (Lotwick y Silverman 1982). Con distancias lineales la
# prueba se descalibra a medida que crece r.
# ============================================================

# ── Normalizar la tabla de estructuras ────────────────────────
# Recibe un data.frame SIN geometría. 'tipo' e 'id' son opcionales.
normalizar_estructuras <- function(df) {
  names(df) <- tolower(trimws(names(df)))
  if (!"id" %in% names(df))   df$id   <- sprintf("E%02d", seq_len(nrow(df)))
  if (!"tipo" %in% names(df)) df$tipo <- "Estructura"
  df$id   <- as.character(df$id)
  df$tipo <- as.character(df$tipo)
  df$tipo[is.na(df$tipo) | trimws(df$tipo) == ""] <- "Sin tipo"
  df
}

# ── Distancia de cada atropello a la estructura más cercana ───
# km_ev, km_est: posiciones en km. Con L, distancia sobre el anillo
# (se replican las estructuras en km - L y km + L). Devuelve km.
dist_mas_cercana <- function(km_ev, km_est, L = NULL) {
  if (!is.null(L)) km_est <- c(km_est - L, km_est, km_est + L)
  est <- sort(km_est)
  i   <- findInterval(km_ev, est)                # estructura a la izquierda
  izq <- ifelse(i >= 1, km_ev - est[pmax(i, 1)], Inf)
  der <- ifelse(i < length(est), est[pmin(i + 1, length(est))] - km_ev, Inf)
  pmin(izq, der)
}

# ── Un conjunto de atropellos simulado bajo el modelo nulo ────
simular_km <- function(km_ev, L, nulo = c("circular", "uniforme")) {
  nulo <- match.arg(nulo)
  if (nulo == "circular") {
    (km_ev + stats::runif(1, 0, L)) %% L
  } else {
    stats::runif(length(km_ev), 0, L)
  }
}

# ── 1. Prueba de distancia a la estructura más cercana ────────
# Estadístico: mediana (robusta a atropellos aislados lejos de todo).
prueba_distancia <- function(km_ev, km_est, L, nsim = 999,
                             nulo = c("circular", "uniforme")) {
  nulo <- match.arg(nulo)
  d    <- dist_mas_cercana(km_ev, km_est, L)
  obs  <- stats::median(d)
  sim  <- vapply(seq_len(nsim), function(s) {
    stats::median(dist_mas_cercana(simular_km(km_ev, L, nulo), km_est, L))
  }, numeric(1))
  list(
    distancias  = d,
    obs         = obs,
    sim         = sim,
    nsim        = nsim,
    nulo        = nulo,
    p_atraccion = (1 + sum(sim <= obs)) / (nsim + 1),
    p_repulsion = (1 + sum(sim >= obs)) / (nsim + 1)
  )
}

# ── 2. K cruzada en 1D como razón observado / esperado ────────
# Para cada radio r: atropellos a menos de r de cada estructura
# (sumados sobre estructuras) dividido entre lo esperado si los
# atropellos estuvieran repartidos uniformemente: λ · 2r · n_est.
# Métrica circular: no hace falta corrección de borde (r < L/2).
# Bajo independencia ≈ 1; > 1 = más atropellos cerca de lo esperado;
# < 1 = menos (repulsión, p. ej. estructuras con cercas eficaces).
razon_oe <- function(km_ev, km_est, L, r) {
  stopifnot(all(r < L / 2))
  lambda <- length(km_ev) / L
  ev     <- sort(c(km_ev - L, km_ev, km_ev + L))
  vapply(r, function(rr) {
    obs <- sum(findInterval(km_est + rr, ev) -
               findInterval(km_est - rr, ev, left.open = TRUE))
    obs / (lambda * 2 * rr * length(km_est))
  }, numeric(1))
}

# rmax por defecto 2 km: la escala a la que una estructura puede
# influir en el cruce. Rangos mayores diluyen el efecto (validación:
# potencia 0.67 con rmax = 2 km frente a 0.37 con rmax = L/4).
prueba_k_cruzada <- function(km_ev, km_est, L, rmax = 2, paso = 0.05,
                             nsim = 1999, nulo = c("circular", "uniforme")) {
  nulo <- match.arg(nulo)
  r   <- seq(paso, min(rmax, L / 2 - paso), by = paso)
  obs <- razon_oe(km_ev, km_est, L, r)
  sim <- vapply(seq_len(nsim), function(s) {
    razon_oe(simular_km(km_ev, L, nulo), km_est, L, r)
  }, numeric(length(r)))
  cs  <- GET::create_curve_set(list(r = r, obs = obs, sim_m = sim))
  res <- GET::global_envelope_test(cs, type = "area", alternative = "two.sided")
  df  <- as.data.frame(res)
  list(tabla = data.frame(r = df$r, obs = df$obs, central = df$central,
                          lo = df$lo, hi = df$hi),
       p = attr(res, "p"), nsim = nsim, nulo = nulo, get = res)
}

# ── 3. Modelo por segmentos (binomial negativa) ───────────────
# Cuenta atropellos por segmento y los modela en función de la
# distancia del centro del segmento a la estructura más cercana,
# con el largo del segmento como offset.
segmentar_ruta <- function(L, largo_km) {
  n  <- max(1, floor(L / largo_km))
  br <- c(seq(0, by = largo_km, length.out = n), L)  # el último absorbe el resto
  data.frame(seg = seq_len(n), km_ini = br[-length(br)], km_fin = br[-1])
}

# Geometría de los segmentos (líneas) a partir de la ruta continua,
# para exportarlos. Cada segmento se traza con puntos cada ~20 m.
segmentos_sf <- function(linea, s) {
  Lm <- as.numeric(sf::st_length(linea))
  geoms <- lapply(seq_len(nrow(s)), function(i) {
    k  <- max(2, ceiling((s$km_fin[i] - s$km_ini[i]) * 1000 / 20) + 1)
    fr <- pmin(seq(s$km_ini[i], s$km_fin[i], length.out = k) * 1000 / Lm, 1)
    xy <- sf::st_coordinates(sf::st_cast(sf::st_line_sample(linea, sample = fr),
                                         "POINT"))[, 1:2]
    sf::st_linestring(xy)
  })
  sf::st_sf(s, geometry = sf::st_sfc(geoms, crs = sf::st_crs(linea)))
}

contar_por_segmento <- function(km_ev, s, L) {
  as.vector(table(cut(km_ev, c(s$km_ini, L), include.lowest = TRUE,
                      right = FALSE)))
}

ajustar_glm_seg <- function(s) {
  ajuste <- tryCatch(
    MASS::glm.nb(n ~ dist_est_km + offset(log(largo_km)), data = s),
    error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(ajuste)) return(list(ajuste = ajuste, familia = "Binomial negativa"))
  # p. ej. sin sobredispersión (theta → ∞): Poisson
  list(ajuste = stats::glm(n ~ dist_est_km + offset(log(largo_km)),
                           family = stats::poisson, data = s),
       familia = "Poisson")
}

# Razón de tasas e IC de Wald: DESCRIPTIVOS del tamaño del efecto.
# El valor p NO es el de Wald: los conteos de segmentos vecinos están
# autocorrelacionados y el p de Wald es optimista (error tipo I de
# 13 % frente a 2.5 % nominal en la validación). Se obtiene por
# desplazamiento circular del coeficiente, igual que las otras pruebas.
modelo_segmentos <- function(km_ev, km_est, L, largo_km = 0.5, nsim = 499) {
  s <- segmentar_ruta(L, largo_km)
  s$largo_km    <- s$km_fin - s$km_ini
  s$km_centro   <- (s$km_ini + s$km_fin) / 2
  s$n           <- contar_por_segmento(km_ev, s, L)
  s$dist_est_km <- dist_mas_cercana(s$km_centro, km_est, L)

  m  <- ajustar_glm_seg(s)
  b  <- stats::coef(summary(m$ajuste))["dist_est_km", ]
  ic <- b[1] + c(-1, 1) * stats::qnorm(0.975) * b[2]
  s$ajustado <- stats::fitted(m$ajuste)
  s$residuo  <- stats::residuals(m$ajuste, type = "pearson")

  b_sim <- vapply(seq_len(nsim), function(i) {
    s2 <- s
    s2$n <- contar_por_segmento(simular_km(km_ev, L, "circular"), s, L)
    stats::coef(ajustar_glm_seg(s2)$ajuste)[["dist_est_km"]]
  }, numeric(1))

  list(
    segmentos = s,
    ajuste    = m$ajuste,
    familia   = m$familia,
    coef = data.frame(
      termino     = "Distancia a la estructura (por km)",
      razon_tasas = exp(b[[1]]), ic_inf = exp(ic[1]), ic_sup = exp(ic[2]),
      p_wald      = b[[4]],
      p_circular  = min(1, 2 * min((1 + sum(b_sim <= b[[1]])) / (nsim + 1),
                                   (1 + sum(b_sim >= b[[1]])) / (nsim + 1))),
      row.names = NULL),
    b_sim = b_sim,
    moran = moran_residuos(s$residuo)
  )
}

# I de Moran de los residuos con vecinos = segmentos contiguos.
# Si es significativo, los errores estándar del modelo son optimistas.
moran_residuos <- function(res, nsim = 999) {
  n  <- length(res)
  if (n < 4) return(NULL)
  nb <- lapply(seq_len(n), function(i) as.integer(setdiff(c(i - 1, i + 1), c(0, n + 1))))
  class(nb) <- "nb"
  attr(nb, "region.id") <- as.character(seq_len(n))
  lw <- spdep::nb2listw(nb, style = "W")
  mc <- spdep::moran.mc(res, lw, nsim = nsim)
  data.frame(I = unname(mc$statistic), p = mc$p.value)
}
