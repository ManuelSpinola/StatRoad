# ============================================================
# utils_espacial.R — Lectura de capas y ajuste de registros a la vía
# StatRoad · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Funciones puras (sin Shiny): se pueden probar desde la consola.
# Todos los cálculos se hacen en un CRS métrico (CRTM05).
# ============================================================

# ── CRS disponibles ───────────────────────────────────────
# Métricos (para el análisis)
crs_metricos <- c(
  "CRTM05 — CR-SIRGAS (EPSG:8908)" = 8908,
  "CRTM05 — CR05 (EPSG:5367)"      = 5367
)
# Posibles CRS de las columnas x/y de una tabla
crs_coordenadas <- c(
  "Longitud/latitud — WGS84 (EPSG:4326)" = 4326,
  "CRTM05 — CR-SIRGAS (EPSG:8908)"       = 8908,
  "CRTM05 — CR05 (EPSG:5367)"            = 5367
)

columnas_atropellos <- c("especie", "grupo", "fecha")

# ── Mapa base común a todos los mapas de la app ───────────
# Proveedores sin clave de API (CARTO exige clave desde 2025).
# Si un proveedor cambia, se corrige solo aquí.
mapa_base <- function() {
  leaflet::leaflet() |>
    leaflet::addProviderTiles("Esri.WorldGrayCanvas", group = "Claro") |>
    leaflet::addTiles(group = "OpenStreetMap") |>
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satélite") |>
    leaflet::addLayersControl(
      baseGroups = c("Claro", "OpenStreetMap", "Satélite"),
      options    = leaflet::layersControlOptions(collapsed = TRUE)
    )
}

# ── Evaluar una lectura y convertir errores en validate() ─
# Muestra el mensaje real del error en la salida de Shiny.
validar_lectura <- function(expr) {
  res <- tryCatch(expr, error = function(e) e)
  shiny::validate(shiny::need(
    !inherits(res, "error"),
    if (inherits(res, "error")) conditionMessage(res) else ""
  ))
  res
}

# ── Leer una capa espacial (GPKG, GeoJSON o Shapefile .zip) ─
# tipo = "lineas" (red vial) o "puntos" (atropellos)
leer_capa_espacial <- function(ruta, nombre, tipo = c("lineas", "puntos")) {
  tipo <- match.arg(tipo)
  ext  <- tolower(tools::file_ext(nombre))

  if (ext == "zip") {
    dir_tmp <- tempfile("capa_")
    utils::unzip(ruta, exdir = dir_tmp)
    shp <- list.files(dir_tmp, pattern = "\\.shp$", recursive = TRUE,
                      full.names = TRUE)
    if (length(shp) != 1) {
      stop("El .zip debe contener exactamente un archivo .shp ",
           "(junto con sus .shx, .dbf y .prj).", call. = FALSE)
    }
    ruta <- shp
  } else if (!ext %in% c("gpkg", "geojson", "json")) {
    stop("Formato no reconocido. Usa GPKG, GeoJSON o un Shapefile ",
         "comprimido en .zip.", call. = FALSE)
  }

  capa <- sf::st_read(ruta, quiet = TRUE)

  if (is.na(sf::st_crs(capa))) {
    stop("El archivo no tiene sistema de coordenadas (CRS) definido. ",
         "Si es un Shapefile, revisa que el .zip incluya el archivo .prj.",
         call. = FALSE)
  }

  geom <- unique(as.character(sf::st_geometry_type(capa)))
  if (tipo == "lineas") {
    if (!all(geom %in% c("LINESTRING", "MULTILINESTRING"))) {
      stop("La red vial debe contener líneas. El archivo contiene: ",
           paste(geom, collapse = ", "), ".", call. = FALSE)
    }
  } else {
    if (!all(geom %in% c("POINT", "MULTIPOINT"))) {
      stop("Los atropellos deben ser puntos. El archivo contiene: ",
           paste(geom, collapse = ", "), ".", call. = FALSE)
    }
    if ("MULTIPOINT" %in% geom) capa <- sf::st_cast(capa, "POINT")
  }

  sf::st_zm(capa)
}

# ── Leer una tabla de atropellos (CSV o Excel) ─────────────
leer_tabla_atropellos <- function(ruta, nombre, separador = ",") {
  ext <- tolower(tools::file_ext(nombre))
  df <- if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(ruta)
  } else if (ext %in% c("csv", "txt")) {
    readr::read_delim(ruta, delim = separador, show_col_types = FALSE)
  } else {
    stop("Formato no reconocido. Usa CSV o Excel.", call. = FALSE)
  }
  # data.frame base: los tibbles se comportan distinto al subsetear
  as.data.frame(df)
}

# ── Normalizar atributos: columnas requeridas y fecha ──────
# Recibe un data.frame SIN geometría.
normalizar_atropellos <- function(df) {
  names(df) <- tolower(trimws(names(df)))

  faltan <- setdiff(columnas_atropellos, names(df))
  if (length(faltan) > 0) {
    stop("Faltan columnas obligatorias: ", paste(faltan, collapse = ", "),
         ". Revisa la plantilla de ejemplo.", call. = FALSE)
  }

  if (!"id" %in% names(df)) df$id <- seq_len(nrow(df))

  fecha <- df$fecha
  if (inherits(fecha, "POSIXt")) {
    fecha <- as.Date(fecha)
  } else if (!inherits(fecha, "Date")) {
    fecha <- as.Date(as.character(fecha),
                     tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y"),
                     optional = TRUE)
  }
  if (all(is.na(fecha))) {
    stop("No se pudo interpretar la columna 'fecha'. ",
         "Usa el formato AAAA-MM-DD (por ejemplo, 2025-03-14).", call. = FALSE)
  }

  df$fecha   <- fecha
  df$especie <- as.character(df$especie)
  df$grupo   <- as.character(df$grupo)
  df
}

# ── Tabla con columnas x/y → puntos sf ─────────────────────
tabla_a_sf <- function(df, crs) {
  if (!all(c("x", "y") %in% names(df))) {
    stop("La tabla debe tener columnas 'x' e 'y' con las coordenadas.",
         call. = FALSE)
  }
  df$x <- suppressWarnings(as.numeric(df$x))
  df$y <- suppressWarnings(as.numeric(df$y))
  sin_coord <- is.na(df$x) | is.na(df$y)
  if (any(sin_coord)) {
    stop(sum(sin_coord), " registro(s) sin coordenadas válidas ",
         "(filas: ", paste(utils::head(which(sin_coord), 10), collapse = ", "),
         if (sum(sin_coord) > 10) ", …", ").", call. = FALSE)
  }
  sf::st_as_sf(df, coords = c("x", "y"), crs = crs, remove = FALSE)
}

# ── Preparar la red: CRS métrico, tramos continuos, orientación ─
# La red se separa en TRAMOS: líneas continuas sin cruces. Si la red
# tiene cruces, cada pedazo entre cruces es un tramo. Cada tramo se
# orienta: km 0 en el extremo sur (tramos norte-sur) o en el oeste
# (este-oeste). Los tramos se numeran según la posición de su km 0.
# Compatibilidad: 'linea' y 'continua' significan lo mismo que antes;
# con un solo tramo, tramo 1 = la ruta completa.
preparar_red <- function(red, crs) {
  red   <- sf::st_transform(red, crs)
  union <- sf::st_line_merge(sf::st_union(sf::st_geometry(red)))
  continua <- identical(as.character(sf::st_geometry_type(union)), "LINESTRING")

  partes <- if (continua) union else sf::st_cast(union, "LINESTRING")
  partes <- sf::st_sfc(lapply(seq_along(partes), function(i) {
    orientar_linea(partes[i])[[1]]
  }), crs = sf::st_crs(red))

  # numeración estable: por la posición del km 0 de cada tramo
  inicio <- t(vapply(seq_along(partes), function(i) {
    sf::st_coordinates(partes[i])[1, 1:2]
  }, numeric(2)))
  rango <- apply(inicio, 2, function(v) diff(range(v)))
  orden <- if (rango[2] >= rango[1]) order(inicio[, 2], inicio[, 1]) else
    order(inicio[, 1], inicio[, 2])
  partes <- partes[orden]

  tramos <- sf::st_sf(
    tramo       = seq_along(partes),
    longitud_km = round(as.numeric(sf::st_length(partes)) / 1000, 3),
    geometry    = partes
  )

  list(
    segmentos   = red,
    linea       = if (continua) partes[1] else union,
    continua    = continua,
    tramos      = tramos,
    n_tramos    = nrow(tramos),
    longitud_km = as.numeric(sum(sf::st_length(red))) / 1000
  )
}

# Orienta una línea: km 0 en el extremo sur (si corre norte-sur) o
# en el oeste (si corre este-oeste)
orientar_linea <- function(linea) {
  xy <- sf::st_coordinates(linea)[, 1:2]
  d  <- xy[nrow(xy), ] - xy[1, ]
  invertir <- if (abs(d[2]) >= abs(d[1])) d[2] < 0 else d[1] < 0
  if (invertir) sf::st_reverse(linea) else linea
}

# ── Posición a lo largo de una línea (km desde su inicio) ──
# Proyecta cada punto sobre el segmento más cercano de la línea.
km_en_linea <- function(linea, pts) {
  L  <- sf::st_coordinates(linea)[, 1:2, drop = FALSE]
  A  <- L[-nrow(L), , drop = FALSE]
  AB <- L[-1, , drop = FALSE] - A
  len  <- sqrt(rowSums(AB^2))
  acum <- c(0, cumsum(len))[seq_len(nrow(A))]
  P <- sf::st_coordinates(pts)[, 1:2, drop = FALSE]

  vapply(seq_len(nrow(P)), function(i) {
    AP <- cbind(P[i, 1] - A[, 1], P[i, 2] - A[, 2])
    t  <- pmin(pmax(rowSums(AP * AB) / pmax(len^2, 1e-12), 0), 1)
    proj <- A + AB * t
    d2 <- (P[i, 1] - proj[, 1])^2 + (P[i, 2] - proj[, 2])^2
    j  <- which.min(d2)
    (acum[j] + t[j] * len[j]) / 1000
  }, numeric(1))
}

# ── Ajuste de registros a la vía (snapping) ────────────────
# Mueve cada punto al punto más cercano de la red, calcula la
# distancia original y marca como excluidos los que superan la
# tolerancia. Asigna a cada punto su tramo y su km dentro del tramo
# (con un solo tramo: tramo 1 y el km a lo largo de toda la ruta).
ajustar_a_red <- function(pts, red_prep, tolerancia_m) {
  pts <- sf::st_transform(pts, sf::st_crs(red_prep$segmentos))
  seg <- sf::st_geometry(red_prep$segmentos)

  idx   <- sf::st_nearest_feature(pts, seg)
  conex <- sf::st_nearest_points(sf::st_geometry(pts), seg[idx],
                                 pairwise = TRUE)
  dist  <- as.numeric(sf::st_length(conex))
  # cada conexión es una línea punto original → punto sobre la vía
  sobre_via <- sf::st_cast(conex, "POINT")[seq(2, 2 * length(conex), by = 2)]

  res <- pts
  res$dist_via_m <- round(dist, 1)
  res$dentro     <- dist <= tolerancia_m

  # tramo más cercano y km dentro de ese tramo
  tramos <- sf::st_geometry(red_prep$tramos)
  res$tramo <- if (length(tramos) == 1) 1L else
    as.integer(sf::st_nearest_feature(sobre_via, tramos))
  res$km <- NA_real_
  for (t in unique(res$tramo)) {
    i <- which(res$tramo == t)
    res$km[i] <- round(km_en_linea(tramos[t], sobre_via[i]), 3)
  }
  res <- sf::st_set_geometry(res, sobre_via)

  list(
    original   = pts,
    todos      = res,
    ajustados  = res[res$dentro, ],
    excluidos  = res[!res$dentro, ],
    tolerancia = tolerancia_m
  )
}
