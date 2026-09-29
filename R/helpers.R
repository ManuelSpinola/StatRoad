# ============================================================
# helpers.R — Funciones y objetos compartidos entre módulos
# StatRoad — Atropellos de fauna en redes viales
# Paleta: Tableau Color Blind (coherente con StatSuite)
# ============================================================

# ── Paleta de colores (idéntica a StatSuite) ───────────────
colores <- list(
  fondo       = "#F4F7FB",
  primario    = "#1170AA",
  acento      = "#FC7D0B",
  secundario  = "#5FA2CE",
  texto       = "#57606C",
  exito       = "#5FA2CE",
  advertencia = "#F1CE63",
  peligro     = "#C85200",
  borde       = "#C8D9EC",

  tableau = c(
    "#1170AA", "#FC7D0B", "#A3ACB9", "#57606C",
    "#C85200", "#7BC8ED", "#5FA2CE", "#F1CE63",
    "#9F8B75", "#B85A0D"
  )
)

# ── Tema visual (idéntico a StatSuite) ─────────────────────
tema_app <- bs_theme(
  version      = 5,
  bg           = colores$fondo,
  fg           = colores$texto,
  primary      = colores$primario,
  secondary    = colores$secundario,
  success      = colores$exito,
  danger       = colores$peligro,
  warning      = colores$advertencia,
  base_font    = font_google("Nunito"),
  heading_font = font_google("Nunito", wght = 700),
  bootswatch   = NULL
) |>
  bs_add_rules("
  .navbar { background-color: #1170AA !important; }
  .navbar-brand { color: #ffffff !important; display: flex !important;
                  align-items: center !important;
                  padding-top: 0 !important; padding-bottom: 0 !important; }
  .navbar .nav-link { color: #ffffff !important; }
  .navbar .nav-link.active { border-bottom: 2px solid #FC7D0B; }
  .nav-tabs .nav-link.active,
  .nav-tabs .nav-item .nav-link.active,
  ul.nav.nav-tabs li.nav-item a.nav-link.active {
    background-color: #1170AA !important;
    color: #ffffff !important;
    border-top-color: #1170AA !important;
    border-left-color: #1170AA !important;
    border-right-color: #1170AA !important;
    border-bottom-color: transparent !important;
    font-weight: 600 !important;
  }
  .nav-tabs .nav-link:not(.active):hover {
    background-color: #EEF3FA !important;
    color: #1170AA !important;
  }
  .btn-primary { background-color: #FC7D0B; border-color: #FC7D0B; color: #ffffff; }
  .btn-primary:hover { background-color: #d4680a; border-color: #d4680a; }
  .card > .card-header { background-color: #C8D9EC; color: #1170AA; font-weight: 700;
                         border-bottom: none; }
  .card > .card-header:has(.nav-tabs) { background-color: transparent; color: inherit;
                                        border-bottom: revert; }

  /* Semáforo de calidad */
  .sem-ok   { background: #f0f9f5; border-left: 4px solid #5FA2CE; }
  .sem-warn { background: #fffbf0; border-left: 4px solid #F1CE63; }
  .sem-bad  { background: #fff0f2; border-left: 4px solid #C85200; }

  /* Quiz didáctico */
  .quiz-opt { border: 1px solid #C8D9EC; border-radius: 8px;
              padding: 0.6rem 1rem; margin-bottom: 0.4rem;
              cursor: pointer; background: #ffffff;
              transition: border-color 0.15s; font-size: 0.88rem; }
  .quiz-opt:hover { border-color: #1170AA; }
  .quiz-correct   { border-color: #5FA2CE !important;
                    background: #f0f9f5 !important; }
  .quiz-wrong     { border-color: #C85200 !important;
                    background: #fff0f2 !important; }

  /* Código R */
  .codigo-bloque { background: #1e1e2e; color: #cdd6f4;
                   border-radius: 8px; padding: 1rem;
                   font-family: 'Fira Code', monospace;
                   font-size: 0.82rem; line-height: 1.7;
                   overflow-x: auto; white-space: pre; }
")

# ── Escalas ggplot2 (Tableau Color Blind) ─────────────────
scale_fill_tableau_cb <- function(...) {
  scale_fill_manual(values = colores$tableau, ...)
}
scale_color_tableau_cb <- function(...) {
  scale_color_manual(values = colores$tableau, ...)
}

# ── Encabezado estándar de scripts R ──────────────────────
# Usada por todos los módulos de StatSuite que generan código R.
encabezado_script <- function(app, modulo) {
  paste0(
    "# ============================================\n",
    "# ", app, " · StatSuite\n",
    "# Módulo: ", modulo, "\n",
    "# Generado: ", format(Sys.Date(), "%Y-%m-%d"), "\n",
    "# Manuel Spínola · ICOMVIS · UNA · Costa Rica\n",
    "# ============================================\n\n"
  )
}

# ── Metadatos y citación de la app ────────────────────────
# Fuente única: DESCRIPTION del paquete (Title, Version, Date,
# Authors@R, URL). Ningún dato de citación se escribe a mano.

institucion_statsuite <- paste0(
  "Instituto Internacional en Conservación y Manejo de Vida Silvestre ",
  "(ICOMVIS), Universidad Nacional, Costa Rica"
)

# Lee los metadatos de la app desde DESCRIPTION.
# Date es obligatorio (define el año de la cita); URL es opcional.
metadatos_app <- function(pkg) {
  d <- utils::packageDescription(pkg)
  campo <- function(x) {
    if (is.null(x) || is.na(x)) NA_character_ else gsub("\\s+", " ", trimws(x))
  }

  fecha <- campo(d[["Date"]])
  if (is.na(fecha)) {
    stop("El DESCRIPTION de ", pkg, " no tiene campo Date. ",
         "Defínalo con desc::desc_set(\"Date\", \"AAAA-MM-DD\").",
         call. = FALSE)
  }

  personas <- eval(parse(text = d[["Authors@R"]]))
  es_autor <- vapply(seq_along(personas),
                     function(i) "aut" %in% personas[[i]]$role,
                     logical(1))

  url <- campo(d[["URL"]])
  if (!is.na(url)) url <- trimws(strsplit(url, ",")[[1]][1])

  list(
    nombre  = pkg,
    titulo  = campo(d[["Title"]]),
    version = campo(d[["Version"]]),
    anio    = substr(fecha, 1, 4),
    url     = url,
    autores = personas[es_autor]
  )
}

# Tarjeta "Cómo citar" (APA 7 para software + BibTeX).
# Se usa igual en el módulo "Acerca de" de todas las apps de StatSuite.
tarjeta_cita <- function(pkg, ns) {
  m <- metadatos_app(pkg)
  n <- length(m$autores)

  autores_apa <- vapply(seq_len(n), function(i) {
    p <- m$autores[[i]]
    paste0(p$family, ", ", paste0(substr(p$given, 1, 1), ".", collapse = " "))
  }, character(1))
  autores_apa <- if (n == 1) {
    autores_apa
  } else {
    paste0(paste(autores_apa[-n], collapse = ", "), ", & ", autores_apa[n])
  }

  autores_bib <- paste(vapply(seq_len(n), function(i) {
    p <- m$autores[[i]]
    paste0(p$family, ", ", paste(p$given, collapse = " "))
  }, character(1)), collapse = " and ")

  titulo_completo <- paste0(m$nombre, ": ", m$titulo)

  bibtex <- paste0(
    "@software{", tolower(m$nombre), m$anio, ",\n",
    "  author    = {", autores_bib, "},\n",
    "  title     = {", titulo_completo, "},\n",
    "  year      = {", m$anio, "},\n",
    "  version   = {", m$version, "},\n",
    "  publisher = {", institucion_statsuite, "}",
    if (!is.na(m$url)) paste0(",\n  url       = {", m$url, "}"),
    "\n}"
  )

  card(
    class = "mt-3",
    card_header(bs_icon("quote", class = "me-1"),
                paste("Cómo citar", m$nombre)),
    card_body(
      p(
        id = ns("cita_texto"),
        class = "small mb-2",
        autores_apa, " (", m$anio, "). ",
        tags$em(titulo_completo),
        " (Versión ", m$version, ") [Aplicación web]. ",
        institucion_statsuite, ".",
        if (!is.na(m$url)) paste0(" ", m$url)
      ),
      tags$button(
        type    = "button",
        class   = "btn btn-sm btn-outline-primary",
        onclick = paste0(
          "navigator.clipboard.writeText(",
          "document.getElementById('", ns("cita_texto"), "').innerText);",
          "this.innerText = '✓ Cita copiada';"
        ),
        bs_icon("clipboard", class = "me-1"), "Copiar cita"
      ),
      tags$details(
        class = "mt-3 small",
        tags$summary("BibTeX"),
        tags$pre(class = "codigo-bloque mt-2", bibtex)
      )
    )
  )
}
