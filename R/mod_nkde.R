# ============================================================
# mod_nkde.R — Puntos críticos: KDE en red con inferencia (KDE+)
# StatRoad · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Usa los registros ajustados (mod_datos_red) y la escala sugerida
# (mod_escala). En una ruta continua el KDE en red es exacto sobre
# la posición a lo largo de la vía (utils_ruta.R). Significancia por
# Monte Carlo con umbral global (KDE+, Bíl et al. 2013).
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_nkde_ui <- function(id) {
  ns <- NS(id)

  navset_card_tab(
    id = ns("tabs_nkde"),

    # ── 1. ¿Qué es? ───────────────────────────────────────
    nav_panel(
      fillable = FALSE,
      title = tagList(bs_icon("book", class = "me-1"), "¿Qué es?"),
      card_body(
        withMathJax(),
        div(
          class = "px-1 pb-2",
          style = "max-width: 780px; margin: 0 auto;",
          h5("Densidad de kernel en red y puntos críticos",
             style = paste0("color:", colores$primario, "; font-weight:700;")),
          p(
            "La ", strong("estimación de densidad de kernel en red"), " (KDE en red) ",
            "convierte los registros puntuales en una ", strong("superficie continua"),
            " a lo largo de la vía: cada atropello \"reparte\" su peso en los metros ",
            "cercanos, y la suma de todos da la densidad en cada tramo, expresada en ",
            strong("atropellos por km"), "."
          ),
          helpText(
            "$$\\hat{\\lambda}(s) = \\sum_{i=1}^{n} \\frac{1}{h\\,c_i}\\, K\\!\\left(\\frac{d(s, x_i)}{h}\\right), \\qquad K(u) = \\tfrac{15}{16}\\,(1-u^2)^2,\\; |u| \\le 1$$"
          ),
          tags$ul(
            class = "small",
            tags$li(tags$b("d(s, xᵢ)"), " = distancia por la vía entre el punto ", em("s"),
                    " y el atropello ", em("i")),
            tags$li(tags$b("h"), " = ", strong("ancho de banda"), ": radio de influencia de cada ",
                    "atropello. Es el parámetro clave."),
            tags$li(tags$b("cᵢ"), " = corrección de borde: cerca de los extremos de la ruta ",
                    "el kernel se \"corta\"; se reescala para que cada atropello pese lo mismo.")
          ),
          card(
            fill = FALSE,
            class = "mt-3",
            card_header(bs_icon("arrows-expand", class = "me-1"),
                        "¿Cómo elegir el ancho de banda?"),
            card_body(
              p(class = "small mb-0",
                "Un ", em("h"), " pequeño produce muchos picos angostos (ruido); uno grande ",
                "suaviza tanto que los puntos críticos se funden. La escala de agregación ",
                "detectada con la función g (módulo anterior) es una buena guía: StatRoad ",
                "la propone como valor inicial. Compara varios valores en ",
                strong("\"Exploración\""), ".")
            )
          ),
          card(
            fill = FALSE,
            class = "mt-3",
            card_header(bs_icon("shuffle", class = "me-1"),
                        "¿Cuándo un pico es un punto crítico? (KDE+)"),
            card_body(
              p(class = "small",
                "Un mapa de densidad siempre tiene algún máximo, incluso si los atropellos ",
                "fueran completamente al azar. Para distinguir picos reales se usa el enfoque ",
                strong("KDE+"), " (Bíl et al. 2013):"),
              tags$ol(
                class = "small mb-0",
                tags$li("Se colocan los mismos ", em("n"), " atropellos al azar sobre la ruta muchas veces."),
                tags$li("En cada simulación se registra la ", strong("densidad máxima"),
                        " de toda la ruta."),
                tags$li("El ", strong("umbral"), " es el percentil 95 de esos máximos: el azar ",
                        "solo lo supera en 5 % de los casos ", em("en toda la ruta a la vez"), "."),
                tags$li("Los tramos continuos que superan el umbral son los ",
                        strong("puntos críticos"), ".")
              )
            )
          ),
          div(
            class = "alert alert-warning small mt-3 mb-0",
            bs_icon("exclamation-triangle", class = "me-1"),
            strong("Recuerda: "),
            "la densidad refleja dónde se ", em("registraron"), " atropellos. Solo equivale a ",
            "dónde ", em("ocurren"), " si toda la ruta se recorrió con el mismo esfuerzo."
          )
        )
      )
    ),

    # ── 2. Los datos ──────────────────────────────────────
    nav_panel(
      fillable = FALSE,
      title = tagList(bs_icon("table", class = "me-1"), "Los datos"),
      card_body(
        p(class = "small text-muted mb-3",
          "Este análisis usa los registros ajustados en ", strong("\"Datos y red vial\""),
          " y, si ya la calculaste, la escala de ", strong("\"Escala de agregación\""), "."),
        uiOutput(ns("estado_datos"))
      )
    ),

    # ── 3. Exploración ────────────────────────────────────
    nav_panel(
      fillable = FALSE,
      title = tagList(bs_icon("zoom-in", class = "me-1"), "Exploración"),
      card_body(
        div(class = "alert alert-secondary small mb-3",
            bs_icon("lightbulb", class = "me-1"),
            strong("Efecto del ancho de banda: "),
            "la misma información con tres anchos de banda (la mitad, el elegido y el ",
            "doble). Todavía sin prueba de significancia: solo para ver cómo cambia ",
            "la forma de la densidad."),
        layout_columns(
          col_widths = c(3, 9),
          fill = FALSE,
          div(
            numericInput(ns("h_explora"), "Ancho de banda central (m)",
                         value = 500, min = 50, max = 5000, step = 50),
            uiOutput(ns("sugerencia_h"))
          ),
          plotOutput(ns("plot_anchos"), height = "400px")
        )
      )
    ),

    # ── 4. Configurar análisis ────────────────────────────
    nav_panel(
      fillable = FALSE,
      title = tagList(bs_icon("gear", class = "me-1"), "Configurar análisis"),
      card_body(
        layout_columns(
          col_widths = c(4, 8),
          fill = FALSE,
          card(
            fill = FALSE,
            card_header(bs_icon("sliders", class = "me-1"), "Parámetros"),
            card_body(
              uiOutput(ns("selector_grupo")),
              numericInput(ns("h"), "Ancho de banda h (m)",
                           value = 500, min = 50, max = 5000, step = 50),
              uiOutput(ns("nota_h")),
              numericInput(ns("lixel"), "Largo de los tramos (m)",
                           value = 50, min = 10, max = 500, step = 10),
              p(class = "small text-muted mt-n2 mb-3",
                "La ruta se divide en tramos de este largo (", em("lixels"), ") y la ",
                "densidad se calcula en el centro de cada uno. Debe ser bastante menor ",
                "que el ancho de banda."),
              numericInput(ns("nsim"), "Simulaciones de Monte Carlo",
                           value = 99, min = 19, max = 999, step = 10),
              p(class = "small text-muted mt-n2 mb-3",
                "Para construir el umbral de significancia. 99 es un buen valor; 199 o ",
                "más da un umbral más estable."),
              actionButton(ns("ejecutar"), "Calcular densidad y puntos críticos",
                           class = "btn-primary w-100 mt-2",
                           icon = icon("play"))
            )
          ),
          div(uiOutput(ns("estado_analisis")))
        )
      )
    ),

    # ── 5. Resultados ─────────────────────────────────────
    nav_panel(
      value = "tab_resultados",
      fillable = FALSE,
      title = tagList(bs_icon("graph-up-arrow", class = "me-1"), "Resultados"),
      card_body(
        uiOutput(ns("resumen_resultados")),
        br(),
        navset_pill(
          nav_panel(
            title = "Mapa",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                strong("Cómo leer este mapa: "),
                "cada tramo de la vía se colorea según su densidad de atropellos ",
                "(amarillo = baja, rojo = alta). Los tramos resaltados con borde ancho ",
                "son los ", strong("puntos críticos"), " (superan el umbral). ",
                "Haz clic en un tramo para ver su km y densidad."),
            leaflet::leafletOutput(ns("mapa"), height = "540px")
          ),
          nav_panel(
            title = "Perfil de densidad",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                strong("Cómo leer este gráfico: "),
                "la curva es la densidad a lo largo de la ruta; la línea punteada es el ",
                "umbral de significancia. Las franjas sombreadas son los puntos críticos. ",
                "Las marcas en la base son los atropellos."),
            uiOutput(ns("control_verdad")),
            plotOutput(ns("plot_perfil"), height = "420px")
          ),
          nav_panel(
            title = "Tabla de puntos críticos",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                "Un punto crítico por fila, ordenados a lo largo de la ruta: ubicación ",
                "(km), longitud, registros dentro, densidad máxima (atropellos/km) y ",
                "grupo dominante. Es la base para priorizar medidas de mitigación."),
            div(class = "d-flex flex-wrap gap-2 mb-3",
                downloadButton(ns("descargar_tabla"), "Descargar tabla (.csv)",
                               class = "btn-outline-primary btn-sm"),
                downloadButton(ns("descargar_tramos"),
                               "Descargar tramos con densidad (.gpkg)",
                               class = "btn-outline-primary btn-sm")),
            DTOutput(ns("tabla_pc"))
          )
        )
      )
    ),

    # ── 6. Código R ───────────────────────────────────────
    nav_panel(
      fillable = FALSE,
      title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
      card_body(
        card(
          fill = FALSE,
          card_header(bs_icon("code-slash", class = "me-1"),
                      "Código reproducible"),
          card_body(verbatimTextOutput(ns("codigo_r")))
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────
# datos:  reactive de mod_datos_red_server()
# escala: reactive de mod_escala_server()
mod_nkde_server <- function(id, datos, escala) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hay_datos <- reactive(!is.null(datos()))
    es_ruta   <- reactive(hay_datos() && isTRUE(datos()$red_prep$continua))

    aviso_sin_datos <- function() {
      div(class = "alert alert-warning small py-2 px-3",
          bs_icon("exclamation-triangle", class = "me-1"),
          "Primero ajusta los registros a la vía en ",
          strong("\"Datos y red vial\" → \"Configurar análisis\""), ".")
    }
    aviso_ramales <- function() {
      div(class = "alert alert-warning small py-2 px-3",
          bs_icon("exclamation-triangle", class = "me-1"),
          strong("Red con ramales: "),
          "esta versión de StatRoad calcula el KDE en red para una sola ruta ",
          "continua. El análisis para redes con ramales llegará en la próxima versión.")
    }

    registros <- reactive({
      req(hay_datos())
      datos()$ajuste$ajustados
    })
    longitud_m <- reactive({
      req(es_ruta())
      as.numeric(sf::st_length(datos()$red_prep$linea))
    })

    # Ancho de banda sugerido: límite superior de la agregación (g).
    # Con kernel cuártico de radio h, la desviación estándar es h/√7,
    # comparable a la de los grupos detectados.
    h_sugerido <- reactive({
      e <- escala()
      if (is.null(e) || is.na(e$escala$hasta) || e$escala$hasta <= 0) return(NA)
      e$escala$hasta
    })

    observeEvent(h_sugerido(), {
      if (!is.na(h_sugerido())) {
        updateNumericInput(session, "h", value = h_sugerido())
        updateNumericInput(session, "h_explora", value = h_sugerido())
      }
    })

    nota_sugerencia <- function() {
      if (is.na(h_sugerido())) {
        p(class = "small text-muted mt-n2 mb-3",
          "Radio de influencia de cada atropello. Calcula primero la escala de ",
          "agregación para obtener un valor sugerido.")
      } else {
        p(class = "small text-muted mt-n2 mb-3",
          bs_icon("magic", class = "me-1"),
          "Sugerido: ", strong(h_sugerido(), " m"),
          ", el límite de la agregación detectada con la función g.")
      }
    }
    output$nota_h       <- renderUI(nota_sugerencia())
    output$sugerencia_h <- renderUI(nota_sugerencia())

    output$estado_datos <- renderUI({
      if (!hay_datos()) return(aviso_sin_datos())
      r <- datos()
      tagList(
        tarjetas_resumen(r$ajuste$ajustados, r$red_prep$segmentos),
        if (!es_ruta()) aviso_ramales(),
        div(class = "alert alert-info small py-2 px-3 mt-3 mb-0",
            bs_icon("info-circle", class = "me-1"),
            if (is.na(h_sugerido())) {
              "Aún no se ha calculado la escala de agregación: el ancho de banda no tiene valor sugerido."
            } else {
              paste0("Escala de agregación detectada: hasta ", h_sugerido(),
                     " m. Se propone como ancho de banda.")
            })
      )
    })

    output$selector_grupo <- renderUI({
      opciones <- if (hay_datos()) {
        c("Todos los grupos" = "Todos", sort(unique(registros()$grupo)))
      } else {
        c("Todos los grupos" = "Todos")
      }
      selectInput(ns("grupo"), "Registros a analizar", choices = opciones)
    })

    # ────────────────────────────────────────────────────
    # EXPLORACIÓN: efecto del ancho de banda
    # ────────────────────────────────────────────────────
    output$plot_anchos <- renderPlot({
      validate(need(hay_datos(),
        "Primero ajusta los registros a la vía en \"Datos y red vial\"."))
      validate(need(es_ruta(),
        "La red tiene ramales: el KDE en red para ese caso llegará en la próxima versión."))
      req(input$h_explora > 0)

      L  <- longitud_m()
      x  <- registros()$km * 1000
      cn <- seq(0, L, length.out = 600)
      h  <- input$h_explora
      df <- do.call(rbind, lapply(c(h / 2, h, 2 * h), function(hh) {
        data.frame(km = cn / 1000, densidad = kde_ruta(x, cn, L, hh),
                   h = paste0("h = ", round(hh), " m"))
      }))
      df$h <- factor(df$h, levels = unique(df$h))

      ggplot(df, aes(x = km, y = densidad, color = h)) +
        geom_line(linewidth = 0.9) +
        geom_rug(data = data.frame(km = x / 1000), aes(x = km),
                 inherit.aes = FALSE, alpha = 0.4, color = colores$texto) +
        scale_color_manual(values = colores$tableau[c(6, 1, 2)]) +
        labs(x = "Posición a lo largo de la vía (km)",
             y = "Atropellos por km", color = NULL) +
        theme_light(base_size = 13) +
        theme(legend.position = "bottom")
    })

    # ────────────────────────────────────────────────────
    # ANÁLISIS
    # ────────────────────────────────────────────────────
    resultado <- reactiveVal(NULL)
    observeEvent(datos(), resultado(NULL), ignoreInit = TRUE, ignoreNULL = FALSE)

    observeEvent(input$ejecutar, {
      if (!hay_datos()) {
        showNotification("Primero ajusta los registros a la vía en \"Datos y red vial\".",
                         type = "warning")
        return()
      }
      if (!es_ruta()) {
        showNotification("La red tiene ramales: análisis disponible en la próxima versión.",
                         type = "warning")
        return()
      }
      if (input$lixel >= input$h) {
        showNotification("El largo de los tramos debe ser menor que el ancho de banda.",
                         type = "error")
        return()
      }

      grupo_sel <- if (is.null(input$grupo)) "Todos" else input$grupo
      pts <- registros()
      if (!identical(grupo_sel, "Todos")) pts <- pts[pts$grupo == grupo_sel, ]
      if (nrow(pts) < 10) {
        showNotification("Se necesitan al menos 10 registros.", type = "error")
        return()
      }

      res <- withProgress(message = "Calculando densidad y umbral…",
                          detail = paste(input$nsim, "simulaciones"), value = 0.3, {
        tryCatch({
          L      <- longitud_m()
          x      <- pts$km * 1000
          lixels <- lixelar_ruta(datos()$red_prep$linea, input$lixel)
          centros <- lixels$km_centro * 1000
          lixels$densidad <- kde_ruta(x, centros, L, input$h)
          u <- umbral_kde_ruta(length(x), centros, L, input$h, input$nsim)
          lixels$critico <- lixels$densidad > u$umbral
          tabla <- puntos_criticos_ruta(lixels, lixels$critico, pts$km, pts$grupo)
          list(lixels = lixels, umbral = u$umbral, tabla = tabla, pts = pts,
               grupo = grupo_sel,
               param = list(h = input$h, lixel = input$lixel, nsim = input$nsim))
        }, error = function(e) {
          showNotification(paste("Error en el KDE:", conditionMessage(e)),
                           type = "error", duration = 10)
          NULL
        })
      })
      resultado(res)
    })

    output$estado_analisis <- renderUI({
      r <- resultado()
      if (is.null(r)) {
        if (!hay_datos()) return(aviso_sin_datos())
        if (!es_ruta())   return(aviso_ramales())
        return(div(
          class = "alert alert-secondary small py-3 px-3",
          bs_icon("exclamation-circle", class = "me-1"),
          "Aún no se ha calculado la densidad.", tags$br(), tags$br(),
          "Revisa los parámetros y presiona ",
          strong("\"Calcular densidad y puntos críticos\""), "."
        ))
      }
      n_pc <- if (is.null(r$tabla)) 0 else nrow(r$tabla)
      tagList(
        div(class = "alert alert-info small py-2 px-3 mb-3",
            bs_icon("check-circle-fill", class = "me-1"),
            strong("Análisis completado. "),
            if (n_pc == 0) "Ningún tramo supera el umbral: no hay evidencia de puntos críticos."
            else paste0(n_pc, " punto(s) crítico(s) detectado(s).")),
        actionButton(ns("ir_a_resultados"), "Ver resultados completos →",
                     class = "btn-outline-primary w-100")
      )
    })

    observeEvent(input$ir_a_resultados, {
      bslib::nav_select(id = "tabs_nkde", selected = "tab_resultados", session = session)
    })

    # ────────────────────────────────────────────────────
    # RESULTADOS
    # ────────────────────────────────────────────────────
    output$resumen_resultados <- renderUI({
      r <- resultado()
      if (is.null(r)) {
        return(div(class = "alert alert-secondary small py-2 px-3",
                   bs_icon("exclamation-circle", class = "me-1"),
                   "Primero calcula la densidad en \"Configurar análisis\"."))
      }
      t <- r$tabla
      n_pc  <- if (is.null(t)) 0 else nrow(t)
      km_pc <- if (is.null(t)) 0 else sum(t$longitud_m) / 1000
      pct   <- if (is.null(t)) 0 else round(100 * sum(t$registros) / nrow(r$pts))
      L_km  <- max(r$lixels$km_fin)
      layout_columns(
        col_widths = c(3, 3, 3, 3), fill = FALSE,
        tarjeta_valor(n_pc, "Puntos críticos", colores$peligro),
        tarjeta_valor(paste0(round(km_pc, 1), " km"),
                      paste0("En puntos críticos (", round(100 * km_pc / L_km), " % de la ruta)"),
                      colores$acento),
        tarjeta_valor(paste0(pct, " %"), "De los registros", colores$primario),
        tarjeta_valor(round(r$umbral, 1), "Umbral (atropellos/km)", colores$texto)
      )
    })

    output$mapa <- leaflet::renderLeaflet({
      r <- resultado()
      req(r)
      lx  <- sf::st_transform(r$lixels, 4326)
      pal <- leaflet::colorNumeric("YlOrRd", domain = lx$densidad)
      crit <- lx[lx$critico, ]

      popup_lx <- ~paste0("km ", round(km_ini, 2), "–", round(km_fin, 2),
                          "<br>Densidad: ", round(densidad, 1), " atropellos/km",
                          ifelse(critico, "<br><b>Punto crítico</b>", ""))

      m <- mapa_base()
      if (nrow(crit) > 0) {
        m <- m |> leaflet::addPolylines(data = crit, color = colores$peligro,
                                        weight = 14, opacity = 0.35)
      }
      m |>
        leaflet::addPolylines(data = lx, color = ~pal(densidad), weight = 6,
                              opacity = 1, popup = popup_lx) |>
        leaflet::addLegend(position = "bottomright", pal = pal,
                           values = lx$densidad, title = "Atropellos/km",
                           opacity = 1)
    })

    # Puntos críticos verdaderos: solo con los datos de ejemplo (simulados)
    output$control_verdad <- renderUI({
      req(resultado())
      if (!isTRUE(datos()$ejemplo)) return(NULL)
      checkboxInput(ns("mostrar_verdad"),
                    "Mostrar los puntos críticos sembrados en la simulación",
                    value = FALSE, width = "100%")
    })

    output$plot_perfil <- renderPlot({
      r <- resultado()
      req(r)
      lx <- sf::st_drop_geometry(r$lixels)
      g <- ggplot(lx, aes(x = km_centro, y = densidad))

      if (!is.null(r$tabla)) {
        g <- g + geom_rect(data = r$tabla,
                           aes(xmin = km_inicio, xmax = km_fin, ymin = -Inf, ymax = Inf),
                           inherit.aes = FALSE, fill = colores$peligro, alpha = 0.12)
      }
      g <- g +
        geom_line(color = colores$primario, linewidth = 1) +
        geom_hline(yintercept = r$umbral, linetype = "dashed",
                   color = colores$peligro, linewidth = 0.8) +
        geom_rug(data = sf::st_drop_geometry(r$pts), aes(x = km),
                 inherit.aes = FALSE, alpha = 0.4, color = colores$texto) +
        labs(x = "Posición a lo largo de la vía (km)", y = "Atropellos por km",
             caption = "Línea punteada: umbral (percentil 95 del máximo bajo azar). Franjas: puntos críticos.") +
        theme_light(base_size = 13)

      if (isTRUE(input$mostrar_verdad) && isTRUE(datos()$ejemplo)) {
        verdad <- utils::read.csv(app_sys("extdata", "hotspots_verdaderos_santarosa.csv"),
                                  fileEncoding = "UTF-8")
        g <- g +
          geom_vline(data = verdad, aes(xintercept = centro_km),
                     color = colores$acento, linewidth = 1, linetype = "dotdash") +
          geom_text(data = verdad, aes(x = centro_km, y = Inf, label = hotspot),
                    inherit.aes = FALSE, vjust = 1.5, hjust = -0.2,
                    color = colores$acento, fontface = "bold")
      }
      g
    })

    output$tabla_pc <- renderDT({
      r <- resultado()
      req(r)
      validate(need(!is.null(r$tabla), "Ningún tramo supera el umbral."))
      datatable(r$tabla, rownames = FALSE,
                colnames = c("Punto crítico", "km inicio", "km fin", "Longitud (m)",
                             "Registros", "Densidad máx. (atropellos/km)",
                             "Grupo dominante", "% del grupo dominante"),
                options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
    })

    output$descargar_tabla <- downloadHandler(
      filename = function() "puntos_criticos_StatRoad.csv",
      content  = function(file) {
        r <- resultado()
        utils::write.csv(if (is.null(r$tabla)) data.frame() else r$tabla,
                         file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    output$descargar_tramos <- downloadHandler(
      filename = function() "tramos_densidad_StatRoad.gpkg",
      content  = function(file) {
        sf::st_write(resultado()$lixels, file, layer = "tramos_densidad",
                     driver = "GPKG", quiet = TRUE, delete_dsn = TRUE)
      }
    )

    # ────────────────────────────────────────────────────
    # CÓDIGO R
    # ────────────────────────────────────────────────────
    output$codigo_r <- renderText({
      r <- resultado()
      if (is.null(r)) {
        return("# Calcula la densidad en \"Configurar análisis\" para generar el código.")
      }
      p <- r$param
      paste0(
        encabezado_script("StatRoad", "Puntos críticos — KDE en red (KDE+)"),
        "# 'ajustados' (con su columna km) y 'linea' (la ruta como una sola\n",
        "# línea) vienen del paso de ajuste a la vía.\n",
        if (!identical(r$grupo, "Todos"))
          paste0("ajustados <- ajustados[ajustados$grupo == \"", r$grupo, "\", ]\n"),
        "x <- ajustados$km * 1000                    # posiciones (m)\n",
        "L <- as.numeric(sf::st_length(linea))        # longitud (m)\n",
        "h <- ", p$h, "                                   # ancho de banda (m)\n\n",
        "# Kernel cuártico y su función de distribución (corrección de borde)\n",
        "K <- function(u) ifelse(abs(u) <= 1, 15/16 * (1 - u^2)^2, 0)\n",
        "F <- function(u) { u <- pmin(pmax(u, -1), 1); 0.5 + 15/16 * (u - 2*u^3/3 + u^5/5) }\n\n",
        "kde <- function(x, s, L, h) {\n",
        "  masa <- F((L - x) / h) - F(-x / h)\n",
        "  as.vector(K(outer(s, x, \"-\") / h) %*% (1 / (h * masa))) * 1000  # atropellos/km\n",
        "}\n\n",
        "# Densidad en el centro de tramos de ", p$lixel, " m\n",
        "s <- seq(", p$lixel / 2, ", L, by = ", p$lixel, ")\n",
        "densidad <- kde(x, s, L, h)\n\n",
        "# Umbral KDE+: percentil 95 del máximo bajo azar (", p$nsim, " simulaciones)\n",
        "set.seed(2026)\n",
        "maximos <- replicate(", p$nsim, ", max(kde(runif(length(x), 0, L), s, L, h)))\n",
        "umbral  <- quantile(maximos, 0.95)\n\n",
        "plot(s / 1000, densidad, type = \"l\", lwd = 2, col = \"#1170AA\",\n",
        "     xlab = \"km\", ylab = \"Atropellos por km\")\n",
        "abline(h = umbral, lty = 2, col = \"#C85200\")\n",
        "rug(x / 1000)\n\n",
        "# Tramos sobre el umbral = puntos críticos\n",
        "critico <- densidad > umbral\n",
        "rle(critico)\n"
      )
    })

    resultado
  })
}
