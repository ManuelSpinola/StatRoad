# ============================================================
# mod_gistar.R — Puntos críticos por segmentos: Getis-Ord Gi*
# StatRoad · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Usa los registros ajustados (mod_datos_red) y la escala sugerida
# (mod_escala). La ruta se divide en segmentos de largo fijo; Gi* con
# vecindad binaria (segmento ± k vecinos), significancia por
# simulación de azar uniforme sobre la ruta y corrección FDR (utils_ruta.R).
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_gistar_ui <- function(id) {
  ns <- NS(id)

  navset_card_tab(
    id = ns("tabs_gistar"),

    # ── 1. ¿Qué es? ───────────────────────────────────────
    nav_panel(
      fillable = FALSE,
      title = tagList(bs_icon("book", class = "me-1"), "¿Qué es?"),
      card_body(
        withMathJax(),
        div(
          class = "px-1 pb-2",
          style = "max-width: 780px; margin: 0 auto;",
          h5("Getis-Ord Gi* sobre segmentos de la vía",
             style = paste0("color:", colores$primario, "; font-weight:700;")),
          p(
            "La ruta se divide en ", strong("segmentos de largo fijo"), " (por ejemplo, ",
            "500 m) y se cuentan los atropellos de cada uno. El estadístico ",
            strong("Gi*"), " (Getis & Ord 1992) compara la suma de atropellos de un ",
            "segmento ", strong("y sus vecinos"), " con lo esperable si el promedio ",
            "fuera el mismo en toda la ruta:"
          ),
          helpText(
            "$$G_i^* = \\frac{\\sum_{j} w_{ij}\\,x_j \\; - \\; \\bar{x}\\,W_i}{S\\sqrt{\\dfrac{n\\,W_i - W_i^2}{n-1}}}$$"
          ),
          tags$ul(
            class = "small",
            tags$li(tags$b("xⱼ"), " = atropellos en el segmento ", em("j"),
                    "; ", tags$b("x̄"), " y ", tags$b("S"), " = media y desviación de los conteos"),
            tags$li(tags$b("wᵢⱼ"), " = 1 si ", em("j"), " es el propio segmento ", em("i"),
                    " o uno de sus vecinos; 0 si no. ", tags$b("Wᵢ"), " = número de segmentos en la vecindad"),
            tags$li(tags$b("n"), " = número de segmentos")
          ),
          p(
            "Un Gi* ", strong("positivo y significativo"), " indica un ", strong("hotspot"),
            ": una zona con más atropellos de lo esperado. Es la pregunta de gestión y ",
            "lo que StatRoad prueba por defecto (prueba unilateral). Opcionalmente se pueden ",
            "buscar también ", strong("coldspots"), " (zonas con menos atropellos que el ",
            "promedio de la ruta), con la precaución que se explica al activarlos."
          ),
          card(
            fill = FALSE,
            class = "mt-3",
            card_header(bs_icon("shuffle", class = "me-1"),
                        "Significancia: simulación y FDR"),
            card_body(
              p(class = "small",
                "La significancia se obtiene por ", strong("simulación"), ": los mismos ",
                em("N"), " atropellos se reparten al azar a lo largo de la ruta cientos de ",
                "veces (con probabilidad proporcional al largo de cada segmento) y se compara ",
                "la suma observada de cada vecindad con la de las simulaciones. Es la misma ",
                "hipótesis nula que usan la función K y el KDE+."),
              p(class = "small",
                "El ", strong("z"), " que se reporta es el de la simulación: cuántas ",
                "desviaciones estándar se aleja la suma observada de la esperada por azar. ",
                "No se usa el z analítico de la fórmula porque su desviación ", em("S"),
                " se calcula con todos los conteos, incluidos los de los hotspots, y ",
                "subestima la señal cuando la agregación es fuerte."),
              p(class = "small",
                bs_icon("info-circle", class = "me-1"),
                "¿Por qué no la ", em("permutación condicional"), ", estándar en ",
                "polígonos? Porque fija el conteo del propio segmento y solo sortea el de sus ",
                "vecinos. Con polígonos hay 5 o 6 vecinos; en una ruta hay solo 2, y la ",
                "prueba queda casi sin potencia."),
              p(class = "small mb-0",
                "Como se prueban todos los segmentos a la vez, algunos saldrían ",
                "\"significativos\" por azar. Por eso los valores p se corrigen con la ",
                strong("tasa de falsos descubrimientos (FDR, Benjamini-Hochberg)"), ".")
            )
          ),
          card(
            fill = FALSE,
            class = "mt-3",
            card_header(bs_icon("arrows-angle-contract", class = "me-1"),
                        "Gi* y KDE: ¿en qué se diferencian?"),
            card_body(
              p(class = "small mb-0",
                "Gi* equivale a un kernel ", em("rectangular"), " evaluado en los centros de los ",
                "segmentos, con una prueba por segmento; el KDE+ usa un kernel suave y un ",
                "umbral global. Gi* depende del ", strong("largo del segmento"), " y de la ",
                strong("vecindad"), " elegidos: prueba varios valores en \"Exploración\". ",
                "Su ventaja es que produce tramos con kilometraje, comparables con estudios ",
                "previos y la gestión vial.")
            )
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
            strong("Conteos por segmento: "),
            "cambia el largo del segmento y observa cómo cambia el patrón. ",
            "Segmentos muy cortos dan conteos de 0 y 1 (mucho ruido); muy largos diluyen ",
            "los puntos críticos. Ese es el problema de la unidad de área modificable."),
        layout_columns(
          col_widths = c(3, 9),
          fill = FALSE,
          div(
            numericInput(ns("largo_explora"), "Largo del segmento (m)",
                         value = 500, min = 50, max = 5000, step = 50),
            uiOutput(ns("resumen_conteos"))
          ),
          plotOutput(ns("plot_conteos"), height = "400px")
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
              numericInput(ns("largo"), "Largo del segmento (m)",
                           value = 500, min = 50, max = 5000, step = 50),
              uiOutput(ns("nota_largo")),
              numericInput(ns("k"), "Vecinos a cada lado",
                           value = 1, min = 1, max = 10, step = 1),
              p(class = "small text-muted mt-n2 mb-3",
                "Cuántos segmentos a cada lado forman la vecindad. Con 1, cada segmento ",
                "se evalúa junto con el anterior y el siguiente."),
              numericInput(ns("nsim"), "Simulaciones",
                           value = 999, min = 99, max = 9999, step = 100),
              p(class = "small text-muted mt-n2 mb-3",
                "999 permite valores p de hasta 0.001. Con muchos segmentos, ",
                "9999 da más resolución para la corrección FDR."),
              checkboxInput(ns("coldspots"), "Buscar también coldspots",
                            value = FALSE, width = "100%"),
              conditionalPanel(
                condition = sprintf("input['%s']", ns("coldspots")),
                div(class = "alert alert-warning small py-2 px-3 mb-3",
                    bs_icon("exclamation-triangle", class = "me-1"),
                    strong("Interpretar con cuidado: "),
                    "un coldspot es un tramo con menos atropellos que el promedio de ",
                    strong("toda la ruta"), ", y ese promedio incluye los puntos ",
                    "críticos. Cuando la agregación es fuerte, el resto de la ruta ",
                    "tiende a salir como coldspot aunque ahí se atropelle a la tasa ",
                    "normal. ", strong("No significa que el tramo no requiera mitigación."),
                    " Es útil, por ejemplo, para comparar tramos con pasos de fauna.")
              ),
              actionButton(ns("ejecutar"), "Calcular Gi*",
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
                "cada segmento se colorea según su resultado: tonos ",
                strong("naranja-rojo"), " = hotspots, tonos ", strong("azules"),
                " = coldspots (solo si se activó su búsqueda), gris = no significativo. Los porcentajes son el nivel ",
                "de confianza después de corregir por FDR. Haz clic en un segmento ",
                "para ver sus valores."),
            leaflet::leafletOutput(ns("mapa"), height = "540px")
          ),
          nav_panel(
            title = "Perfil a lo largo de la vía",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                strong("Cómo leer este gráfico: "),
                "cada barra es un segmento y su altura, el z: cuántas desviaciones ",
                "estándar se aleja la suma de su vecindad de lo esperado por azar. Por ",
                "encima de cero, más atropellos de lo esperado; por debajo, menos. ",
                "El color indica la significancia (simulación + FDR)."),
            uiOutput(ns("control_verdad")),
            plotOutput(ns("plot_perfil"), height = "420px")
          ),
          nav_panel(
            title = "Tabla de segmentos",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                "Todos los segmentos con su conteo, suma de la vecindad, suma esperada por ",
                "azar, z, valor p por simulación, valor p corregido (FDR) y categoría. ",
                "Ordena por cualquier columna o filtra con el buscador."),
            div(class = "d-flex flex-wrap gap-2 mb-3",
                downloadButton(ns("descargar_tabla"), "Descargar tabla (.csv)",
                               class = "btn-outline-primary btn-sm"),
                downloadButton(ns("descargar_segmentos"),
                               "Descargar segmentos (.gpkg)",
                               class = "btn-outline-primary btn-sm")),
            DTOutput(ns("tabla_segmentos"))
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
mod_gistar_server <- function(id, datos, escala) {
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
          "esta versión de StatRoad calcula Gi* para una sola ruta continua. ",
          "El análisis para redes con ramales llegará en la próxima versión.")
    }

    registros <- reactive({
      req(hay_datos())
      datos()$ajuste$ajustados
    })

    # Conteo de registros por segmento (posiciones en km)
    contar_segmentos <- function(segmentos, km) {
      cortes <- c(segmentos$km_ini, max(segmentos$km_fin))
      idx <- findInterval(km, cortes, rightmost.closed = TRUE, all.inside = TRUE)
      tabulate(idx, nbins = nrow(segmentos))
    }

    # Largo sugerido: la escala de agregación redondeada a 100 m
    largo_sugerido <- reactive({
      e <- escala()
      if (is.null(e) || is.na(e$escala$hasta) || e$escala$hasta <= 0) return(NA)
      max(100, round(e$escala$hasta / 100) * 100)
    })

    observeEvent(largo_sugerido(), {
      if (!is.na(largo_sugerido())) {
        updateNumericInput(session, "largo", value = largo_sugerido())
        updateNumericInput(session, "largo_explora", value = largo_sugerido())
      }
    })

    output$nota_largo <- renderUI({
      if (is.na(largo_sugerido())) {
        p(class = "small text-muted mt-n2 mb-3",
          "Calcula primero la escala de agregación para obtener un valor sugerido. ",
          "Muchos estudios usan 500 m o 1 km.")
      } else {
        p(class = "small text-muted mt-n2 mb-3",
          bs_icon("magic", class = "me-1"),
          "Sugerido: ", strong(largo_sugerido(), " m"),
          ", según la escala de agregación detectada.")
      }
    })

    output$estado_datos <- renderUI({
      if (!hay_datos()) return(aviso_sin_datos())
      r <- datos()
      tagList(
        tarjetas_resumen(r$ajuste$ajustados, r$red_prep$segmentos),
        if (!es_ruta()) aviso_ramales(),
        div(class = "alert alert-info small py-2 px-3 mt-3 mb-0",
            bs_icon("info-circle", class = "me-1"),
            if (is.na(largo_sugerido())) {
              "Aún no se ha calculado la escala de agregación: el largo de segmento no tiene valor sugerido."
            } else {
              paste0("Largo de segmento sugerido: ", largo_sugerido(),
                     " m, según la escala de agregación.")
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
    # EXPLORACIÓN: conteos por segmento
    # ────────────────────────────────────────────────────
    segmentos_explora <- reactive({
      req(es_ruta(), input$largo_explora >= 50)
      seg <- lixelar_ruta(datos()$red_prep$linea, input$largo_explora,
                          fusionar_resto = TRUE)
      seg$registros <- contar_segmentos(seg, registros()$km)
      sf::st_drop_geometry(seg)
    })

    output$plot_conteos <- renderPlot({
      validate(need(hay_datos(),
        "Primero ajusta los registros a la vía en \"Datos y red vial\"."))
      validate(need(es_ruta(),
        "La red tiene ramales: Gi* para ese caso llegará en la próxima versión."))
      s <- segmentos_explora()
      ggplot(s, aes(xmin = km_ini, xmax = km_fin, ymin = 0, ymax = registros)) +
        geom_rect(fill = colores$primario, color = "white", linewidth = 0.3) +
        geom_hline(yintercept = mean(s$registros), linetype = "dashed",
                   color = colores$acento) +
        labs(x = "Posición a lo largo de la vía (km)",
             y = "Registros por segmento",
             caption = "Línea punteada: promedio por segmento.") +
        theme_light(base_size = 13)
    })

    output$resumen_conteos <- renderUI({
      req(es_ruta())
      s <- segmentos_explora()
      tags$ul(
        class = "small text-muted mt-2",
        tags$li(nrow(s), " segmentos"),
        tags$li("Promedio: ", round(mean(s$registros), 1), " registros por segmento"),
        tags$li(sum(s$registros == 0), " segmentos sin registros")
      )
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

      grupo_sel <- if (is.null(input$grupo)) "Todos" else input$grupo
      pts <- registros()
      if (!identical(grupo_sel, "Todos")) pts <- pts[pts$grupo == grupo_sel, ]
      if (nrow(pts) < 10) {
        showNotification("Se necesitan al menos 10 registros.", type = "error")
        return()
      }

      res <- withProgress(message = "Calculando Gi*…",
                          detail = paste(input$nsim, "simulaciones"), value = 0.3, {
        tryCatch({
          seg <- lixelar_ruta(datos()$red_prep$linea, input$largo,
                              fusionar_resto = TRUE)
          if (nrow(seg) < 5) stop("Con ese largo quedan menos de 5 segmentos: usa un largo menor.")
          if (input$k >= nrow(seg) / 2) stop("La vecindad es demasiado grande para el número de segmentos.")

          seg$segmento  <- seq_len(nrow(seg))
          seg$registros <- contar_segmentos(seg, pts$km)
          bil <- isTRUE(input$coldspots)
          g <- gistar_ruta(seg$registros, largos = seg$km_fin - seg$km_ini,
                           k = input$k, nsim = input$nsim, bilateral = bil)
          seg$suma_vecindad <- g$suma_local
          seg$esperado      <- round(g$esperado, 1)
          seg$z_gi          <- round(g$z, 2)
          seg$p_sim         <- g$p_sim
          seg$p_fdr         <- round(g$p_fdr, 4)
          seg$categoria     <- clasificar_gistar(g$z, g$p_fdr, bilateral = bil)

          list(segmentos = seg, pts = pts, grupo = grupo_sel,
               param = list(largo = input$largo, k = input$k, nsim = input$nsim,
                            bilateral = bil))
        }, error = function(e) {
          showNotification(paste("Error en Gi*:", conditionMessage(e)),
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
          "Aún no se ha calculado Gi*.", tags$br(), tags$br(),
          "Revisa los parámetros y presiona ", strong("\"Calcular Gi*\""), "."
        ))
      }
      cat <- r$segmentos$categoria
      n_hot  <- sum(grepl("^Hotspot", cat))
      n_cold <- sum(grepl("^Coldspot", cat))
      tagList(
        div(class = "alert alert-info small py-2 px-3 mb-3",
            bs_icon("check-circle-fill", class = "me-1"),
            strong("Análisis completado. "),
            n_hot, " segmento(s) hotspot",
            if (isTRUE(r$param$bilateral)) paste0(" y ", n_cold, " coldspot"),
            " (FDR < 0.10)."),
        actionButton(ns("ir_a_resultados"), "Ver resultados completos →",
                     class = "btn-outline-primary w-100")
      )
    })

    observeEvent(input$ir_a_resultados, {
      bslib::nav_select(id = "tabs_gistar", selected = "tab_resultados", session = session)
    })

    # ────────────────────────────────────────────────────
    # RESULTADOS
    # ────────────────────────────────────────────────────
    output$resumen_resultados <- renderUI({
      r <- resultado()
      if (is.null(r)) {
        return(div(class = "alert alert-secondary small py-2 px-3",
                   bs_icon("exclamation-circle", class = "me-1"),
                   "Primero calcula Gi* en \"Configurar análisis\"."))
      }
      s <- r$segmentos
      hot <- grepl("^Hotspot", s$categoria)
      pct <- round(100 * sum(s$registros[hot]) / nrow(r$pts))
      layout_columns(
        col_widths = c(3, 3, 3, 3), fill = FALSE,
        tarjeta_valor(nrow(s), paste0("Segmentos de ", r$param$largo, " m"), colores$primario),
        tarjeta_valor(sum(hot), "Segmentos hotspot", colores$peligro),
        tarjeta_valor(if (isTRUE(r$param$bilateral))
                        sum(grepl("^Coldspot", s$categoria)) else "—",
                      if (isTRUE(r$param$bilateral)) "Segmentos coldspot"
                      else "Coldspots (no evaluados)",
                      colores$secundario),
        tarjeta_valor(paste0(pct, " %"), "De los registros en hotspots", colores$acento)
      )
    })

    output$mapa <- leaflet::renderLeaflet({
      r <- resultado()
      req(r)
      s <- sf::st_transform(r$segmentos, 4326)
      pal <- leaflet::colorFactor(unname(colores_gistar), levels = niveles_gistar,
                                  ordered = TRUE)
      presentes <- niveles_gistar[niveles_gistar %in% as.character(s$categoria)]

      mapa_base() |>
        leaflet::addPolylines(
          data = s, color = ~pal(categoria), weight = 7, opacity = 1,
          popup = ~paste0("<b>Segmento ", segmento, "</b> (km ",
                          round(km_ini, 2), "–", round(km_fin, 2), ")",
                          "<br>Registros: ", registros,
                          "<br>Suma de la vecindad: ", suma_vecindad,
                          " (esperado por azar: ", esperado, ")",
                          "<br>z: ", z_gi,
                          "<br>p (FDR): ", p_fdr,
                          "<br><b>", categoria, "</b>")
        ) |>
        leaflet::addLegend(position = "bottomright",
                           colors = unname(colores_gistar[presentes]),
                           labels = presentes, title = "Gi*", opacity = 1)
    })

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
      s <- sf::st_drop_geometry(r$segmentos)
      g <- ggplot(s, aes(xmin = km_ini, xmax = km_fin, ymin = 0, ymax = z_gi,
                         fill = categoria)) +
        geom_rect(color = "white", linewidth = 0.3) +
        geom_hline(yintercept = 0, color = colores$texto) +
        scale_fill_manual(values = colores_gistar, drop = TRUE, name = NULL) +
        labs(x = "Posición a lo largo de la vía (km)",
             y = "z (respecto al azar uniforme)") +
        theme_light(base_size = 13) +
        theme(legend.position = "bottom")

      if (isTRUE(input$mostrar_verdad) && isTRUE(datos()$ejemplo)) {
        verdad <- utils::read.csv(app_sys("extdata", "hotspots_verdaderos_santarosa.csv"),
                                  fileEncoding = "UTF-8")
        g <- g +
          geom_vline(data = verdad, aes(xintercept = centro_km),
                     inherit.aes = FALSE, color = colores$texto,
                     linewidth = 0.9, linetype = "dotdash") +
          geom_text(data = verdad, aes(x = centro_km, y = Inf, label = hotspot),
                    inherit.aes = FALSE, vjust = 1.5, hjust = -0.2,
                    color = colores$texto, fontface = "bold")
      }
      g
    })

    tabla_segmentos <- reactive({
      r <- resultado()
      req(r)
      s <- sf::st_drop_geometry(r$segmentos)
      s$km_ini <- round(s$km_ini, 2)
      s$km_fin <- round(s$km_fin, 2)
      s[, c("segmento", "km_ini", "km_fin", "registros", "suma_vecindad",
            "esperado", "z_gi", "p_sim", "p_fdr", "categoria")]
    })

    output$tabla_segmentos <- renderDT({
      datatable(tabla_segmentos(), rownames = FALSE,
                colnames = c("Segmento", "km inicio", "km fin", "Registros",
                             "Suma vecindad", "Esperado (azar)", "z", "p (simulación)",
                             "p (FDR)", "Categoría"),
                options = list(scrollX = TRUE, pageLength = 15))
    })

    output$descargar_tabla <- downloadHandler(
      filename = function() "gistar_segmentos_StatRoad.csv",
      content  = function(file) {
        utils::write.csv(tabla_segmentos(), file, row.names = FALSE,
                         fileEncoding = "UTF-8")
      }
    )

    output$descargar_segmentos <- downloadHandler(
      filename = function() "gistar_segmentos_StatRoad.gpkg",
      content  = function(file) {
        s <- resultado()$segmentos
        s$categoria <- as.character(s$categoria)
        sf::st_write(s, file, layer = "gistar", driver = "GPKG",
                     quiet = TRUE, delete_dsn = TRUE)
      }
    )

    # ────────────────────────────────────────────────────
    # CÓDIGO R
    # ────────────────────────────────────────────────────
    output$codigo_r <- renderText({
      r <- resultado()
      if (is.null(r)) {
        return("# Calcula Gi* en \"Configurar análisis\" para generar el código.")
      }
      p <- r$param
      paste0(
        encabezado_script("StatRoad", "Puntos críticos — Getis-Ord Gi*"),
        "# 'ajustados' (con su columna km) y 'linea' (la ruta como una sola\n",
        "# línea) vienen del paso de ajuste a la vía.\n",
        if (!identical(r$grupo, "Todos"))
          paste0("ajustados <- ajustados[ajustados$grupo == \"", r$grupo, "\", ]\n"),
        "L <- as.numeric(sf::st_length(linea))\n\n",
        "# Segmentos de ", p$largo, " m (el resto final corto se une al anterior)\n",
        "cortes <- unique(c(seq(0, L, by = ", p$largo, "), L))\n",
        "if ((L - cortes[length(cortes) - 1]) < ", p$largo, " / 2)\n",
        "  cortes <- cortes[-(length(cortes) - 1)]\n",
        "conteos <- tabulate(findInterval(ajustados$km * 1000, cortes,\n",
        "                                 rightmost.closed = TRUE, all.inside = TRUE),\n",
        "                    nbins = length(cortes) - 1)\n\n",
        "# Gi* con vecindad binaria (segmento ± ", p$k, ").\n",
        "# Nula: los N registros al azar uniforme a lo largo de la ruta.\n",
        "largos <- diff(cortes)\n",
        "n <- length(conteos)\n",
        "W <- outer(1:n, 1:n, function(i, j) abs(i - j) <= ", p$k, ") * 1\n",
        "obs <- as.vector(W %*% conteos)                 # suma de cada vecindad\n\n",
        "set.seed(2026)\n",
        "sims <- W %*% rmultinom(", p$nsim, ", sum(conteos), prob = largos / sum(largos))\n",
        "z <- (obs - rowMeans(sims)) / apply(sims, 1, sd)\n",
        if (isTRUE(p$bilateral)) paste0(
          "# Prueba bilateral (hotspots y coldspots)\n",
          "p <- pmin(1, 2 * (pmin(rowSums(sims >= obs), rowSums(sims <= obs)) + 1) / (",
          p$nsim, " + 1))\n\n"
        ) else paste0(
          "# Prueba unilateral de hotspots\n",
          "p <- (rowSums(sims >= obs) + 1) / (", p$nsim, " + 1)\n\n"
        ),
        "g <- data.frame(km_ini = head(cortes, -1) / 1000, registros = conteos,\n",
        "                suma_vecindad = obs, z = round(z, 2), p = p,\n",
        "                p_fdr = p.adjust(p, method = \"BH\"))   # corrección FDR\n",
        "subset(g, p_fdr < 0.10)   # segmentos significativos\n"
      )
    })

    resultado
  })
}
