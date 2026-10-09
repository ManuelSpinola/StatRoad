# ============================================================
# mod_escala.R — Escala de agregación: K y g de Ripley en red
# StatRoad · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Usa los registros ajustados a la vía por mod_datos_red.
# Funciones K y g (correlación de pares) con envolventes de Monte
# Carlo. Ruta continua: cálculo exacto con distancias por la vía y
# g como razón observado / esperado. Red con ramales:
# spNetwork::kfunctions() (Okabe & Yamada 2001). Define la escala a
# la que se agrupan los atropellos, que guía el ancho de banda del
# KDE en red y el largo de los segmentos del Gi*.
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_escala_ui <- function(id) {
  ns <- NS(id)

  navset_card_tab(
    id = ns("tabs_escala"),

    # ── 1. ¿Qué es? ───────────────────────────────────────
    nav_panel(
      fillable = FALSE,
      title = tagList(bs_icon("book", class = "me-1"), "¿Qué es?"),
      card_body(
        withMathJax(),
        div(
          class = "px-1 pb-2",
          style = "max-width: 780px; margin: 0 auto;",
          h5("¿A qué distancia se agrupan los atropellos?",
             style = paste0("color:", colores$primario, "; font-weight:700;")),

          # 1. La pregunta
          p(
            "Antes de buscar puntos críticos hay que responder una pregunta ",
            "previa: ", strong("¿los atropellos están agregados o repartidos al ",
            "azar a lo largo de la vía? Y si están agregados, ¿a qué distancia?"),
            " Si están al azar, no hay puntos críticos que buscar. Si están ",
            "agregados, la distancia a la que se agregan indica el tamaño típico ",
            "de un punto crítico y orienta el ancho de banda del KDE y el largo ",
            "de los segmentos del Gi*."
          ),

          # 2. El radio en una vía
          h6(class = "mt-4 fw-bold", "1. Un radio medido por la vía"),
          p(
            "Alrededor de cada atropello se mide un radio ", em("r", .noWS = "after"), ", siempre ",
            strong("a lo largo de la vía"), " y no en línea recta. Como la vía es ",
            "una línea, el radio se mide ", strong("hacia un lado y hacia el otro"),
            " del atropello."
          ),

          # 3. K y g
          h6(class = "mt-4 fw-bold", "2. Dos formas de contar: K y g"),
          tags$ul(
            tags$li(strong("K(r)"), " (función K de Ripley): cuántos atropellos hay, ",
                    "en promedio, ", strong("dentro"), " del radio, de 0 a ", em("r", .noWS = "after"),
                    ". Es acumulativa: arrastra lo que pasa a distancias cortas."),
            tags$li(strong("g(r)"), " (función de correlación de pares): cuántos hay ",
                    "solo en un ", strong("anillo"), " alrededor de ", em("r", .noWS = "after"),
                    ". Por eso indica ", strong("a qué distancia"),
                    " se agregan los atropellos.")
          ),

          # 4. La ventana móvil
          h6(class = "mt-4 fw-bold", "3. g es una ventana móvil"),
          p(
            "El anillo funciona como una ", strong("ventana de tamaño fijo"),
            " (el ancho del anillo, por ejemplo 200 m) que se aleja del atropello ",
            "paso a paso (el intervalo entre radios, por ejemplo 50 m) hasta el ",
            "radio máximo (por ejemplo 3000 m). En cada posición se cuentan los ",
            "atropellos que caen dentro de la ventana: cada posición es un punto ",
            "de la curva. La ventana nunca cambia de tamaño; lo que cambia es ",
            strong("a qué distancia del atropello"), " está."
          ),
          plotOutput(ns("fig_ventana"), height = "330px"),
          p(class = "small text-muted mt-2",
            "Es la misma idea de las ventanas móviles de la ecología del paisaje, ",
            "con una diferencia: aquí la ventana no recorre un mapa, sino las ",
            "distancias entre atropellos. El resultado no es un mapa, sino una ",
            "curva que dice a qué distancia se agregan."),

          # 5. Comparación con el azar
          h6(class = "mt-4 fw-bold", "4. ¿Con qué se compara?"),
          p(
            "Se colocan muchas veces atropellos ", strong("al azar sobre la misma vía"),
            " (con el mismo número de registros) y se calculan K y g para cada ",
            "simulación. La ", strong("banda gris"), " del gráfico es el rango que ",
            "produce el azar. En una ruta continua, g se muestra como ",
            strong("razón observado / esperado", .noWS = "after"), ": 1 es lo que se espera por azar, ",
            "2 es el doble de pares de lo esperado."
          ),
          tags$ul(
            tags$li(strong("Curva por encima de la banda:"), " a esa distancia hay ",
                    "más atropellos juntos de lo esperado (", strong("agregación", .noWS = "outside"), ")."),
            tags$li(strong("Dentro de la banda:"), " no se distingue del azar."),
            tags$li(strong("Por debajo de la banda:"), " hay menos de lo esperado ",
                    "(atropellos más espaciados); es raro en atropellos.")
          ),

          # 6. Lo que no dice
          div(
            class = "alert alert-info small mt-3",
            bs_icon("info-circle", class = "me-1"),
            strong("Lo que g no dice: "),
            "g muestra si hay agregación y a qué escala, pero ", strong("no por qué", .noWS = "after"),
            ". Los atropellos pueden agregarse porque un evento atrae a otros ",
            "(contagio) o, mucho más a menudo, porque el ambiente no es igual en ",
            "toda la vía: se concentran donde cruzan los animales (una quebrada, ",
            "un parche de bosque, una alcantarilla). K y g no distinguen esas dos ",
            "causas. Dónde están los grupos lo muestran los ", strong("puntos críticos"),
            " (KDE y Gi*); qué los explica se explora en ", strong("Estructuras", .noWS = "after"), "."
          ),

          div(
            class = "alert alert-warning small mt-3 mb-0",
            bs_icon("exclamation-triangle", class = "me-1"),
            strong("¿Por qué una banda global? "),
            "La curva se evalúa en muchos radios a la vez (61 con los valores ",
            "por defecto). Si la banda se construyera radio por radio, con 95 % en ",
            "cada uno, casi siempre algún punto saldría de ella por puro azar, ",
            "como quien lanza una moneda 61 veces y siempre encuentra algo raro. ",
            "Por eso en una ruta continua la banda es ", strong("global", .noWS = "after"),
            ": cubre toda la curva a la vez, con 5 % de error en total (prueba de ",
            "envolvente global, paquete GET). En redes con ramales (spNetwork) la ",
            "banda es puntual: ahí, confía solo en patrones que se mantienen a lo ",
            "largo de varios radios seguidos."
          ),

          # 7. Fórmulas (para quien las quiera)
          card(
            fill = FALSE,
            class = "mt-3",
            card_header(bs_icon("calculator", class = "me-1"), "Las fórmulas"),
            card_body(
              class = "small",
              helpText(
                "$$\\hat{K}(r) = \\frac{L_T}{n(n-1)} \\sum_{i=1}^{n} \\sum_{j \\neq i} \\mathbf{1}\\left(d_{ij} \\le r\\right)$$"
              ),
              helpText(
                "$$\\hat{g}(r) = \\frac{L_T}{n(n-1)} \\sum_{i=1}^{n} \\sum_{j \\neq i} \\mathbf{1}\\left(r - \\tfrac{w}{2} < d_{ij} \\le r + \\tfrac{w}{2}\\right)$$"
              ),
              tags$ul(
                tags$li(tags$b("Lₜ"), " = longitud total de la vía"),
                tags$li(tags$b("n"), " = número de atropellos"),
                tags$li(tags$b("dᵢⱼ"), " = distancia por la vía entre los atropellos ",
                        em("i"), " y ", em("j")),
                tags$li(tags$b("w"), " = ancho del anillo")
              ),
              p("En una ruta continua de largo L, la distancia entre dos atropellos ",
                "colocados al azar cumple P(d ≤ t) = 1 − (1 − t/L)². Con eso se ",
                "calcula exactamente el valor esperado de g por azar, y la curva se ",
                "muestra como ĝ(r) / E[g(r)]. Esta razón corrige también los ",
                "primeros radios, donde el anillo se recorta en 0.")
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
          "Este análisis usa los registros ya ajustados a la vía en el módulo ",
          strong("\"Datos y red vial\""), ". Para cambiar de datos, hazlo allí y ",
          "vuelve a presionar \"Ajustar registros a la vía\"."),
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
            strong("Perfil de atropellos a lo largo de la vía: "),
            "cada barra cuenta los registros en un tramo de la ruta (km 0 en el ",
            "extremo sur, o en el oeste). Los picos sugieren agregación, pero a ",
            "ojo es fácil ver patrones donde no los hay: la función K lo pone a prueba."),
        layout_columns(
          col_widths = c(3, 9),
          fill = FALSE,
          div(
            sliderInput(ns("ancho_barra"), "Ancho de cada barra (m)",
                        min = 100, max = 2000, value = 250, step = 50)
          ),
          plotOutput(ns("plot_perfil"), height = "380px")
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
              div(
                class = "small text-muted bg-light border rounded p-2 mb-3",
                p(class = "mb-1",
                  strong("¿Qué se calcula?"), " Alrededor de cada atropello se mide ",
                  "un radio ", em("r"), " por la vía, hacia un lado y hacia el otro."),
                tags$ul(
                  class = "mb-1 ps-3",
                  tags$li(strong("K(r):"), " cuántos atropellos hay, en promedio, ",
                          strong("dentro"), " de ese radio (de 0 a ", em("r", .noWS = "after"),
                          "). Es acumulativa."),
                  tags$li(strong("g(r):"), " cuántos hay solo en un ", strong("anillo"),
                          " alrededor de ", em("r", .noWS = "after"), ". Indica ", strong("a qué distancia"),
                          " se agrupan los atropellos.")
                ),
                p(class = "mb-0",
                  "Ambas se comparan con atropellos colocados al azar sobre la misma ",
                  "vía. La explicación completa la puedes leer en \"¿Qué es?\".")
              ),
              uiOutput(ns("selector_grupo")),
              p(class = "small text-muted mt-n2 mb-3",
                "Puedes analizar todos los registros o un solo grupo: grupos ",
                "distintos pueden agregarse a escalas distintas."),
              numericInput(ns("ancho_g"), "Ancho del anillo (m)",
                           value = 200, min = 20, max = 2000, step = 10),
              p(class = "small text-muted mt-n2 mb-3",
                "Cada punto de la curva g cuenta los atropellos que están a una ",
                "distancia ", em("r"), " ± la mitad de este ancho (con 200 m: entre ",
                em("r"), " − 100 y ", em("r"), " + 100 m), hacia un lado y hacia el ",
                "otro de cada atropello. Funciona como el ancho de las barras de un ",
                "histograma: ", strong("muy angosto"), " = caen pocos pares en cada ",
                "ventana y la curva sale irregular; ", strong("muy ancho"), " = cada ",
                "ventana abarca casi todo y se pierde el detalle de a qué distancia ",
                "se agrupan los atropellos. Entre 100 y 300 m suele funcionar bien."),
              numericInput(ns("dist_max"), "Radio máximo (m)",
                           value = 3000, min = 200, max = 20000, step = 100),
              p(class = "small text-muted mt-n2 mb-3",
                em("r"), " es el radio alrededor de cada atropello, medido por la ",
                "vía hacia un lado y hacia el otro. Este es el mayor radio que se ",
                "evalúa: el final del eje horizontal del gráfico. Regla práctica: ",
                "no más de un tercio de la longitud de la red."),
              numericInput(ns("paso"), "Intervalo entre radios (m)",
                           value = 50, min = 10, max = 500, step = 10),
              p(class = "small text-muted mt-n2 mb-3",
                "Cada cuántos metros se calcula un punto de la curva: con 50 m, ",
                em("r"), " = 0, 50, 100… hasta el radio máximo. Más pequeño = ",
                "curva más detallada pero más lenta."),
              numericInput(ns("nsim"), "Simulaciones de Monte Carlo",
                           value = 1999, min = 199, max = 9999, step = 100),
              p(class = "small text-muted mt-n2 mb-3",
                "Cuántas veces se colocan los atropellos al azar sobre la ruta para ",
                "construir la banda. La banda es global (cubre toda la curva a la ",
                "vez) y necesita muchas simulaciones: se recomiendan 1999 o más. ",
                "Con ~1300 registros tardan unos 20 segundos."),
              actionButton(ns("ejecutar"), "Calcular K y g",
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
            title = "Función g (escala)",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                strong("Cómo leer este gráfico: "),
                "la línea es la función g observada y la banda gris, el rango del ",
                "azar. En una ruta continua, g se muestra como ", strong("razón ",
                "observado / esperado", .noWS = "after"), ": 1 (línea discontinua) es lo que se ",
                "espera por azar, 2 es el doble de pares de lo esperado y 0,5 la ",
                "mitad. Las distancias donde la línea queda ", strong("por encima"),
                " de la banda (marcadas en naranja) son las distancias a las que los ",
                "atropellos están más juntos de lo esperable. El tramo donde ",
                "termina esa zona indica el ", strong("tamaño típico de los grupos", .noWS = "after"), ".",
                tags$br(), tags$br(),
                bs_icon("info-circle", class = "me-1"),
                "Si después de esa zona la línea queda ", strong("por debajo"),
                " de la banda, no significa que los atropellos estén regularmente ",
                "espaciados: es un efecto de la propia agregación. Si muchos registros ",
                "se concentran en pocos tramos, entre esos tramos quedan menos registros ",
                "que el promedio, y a esas distancias aparecen menos pares de los esperados."),
            plotOutput(ns("plot_g"), height = "420px")
          ),
          nav_panel(
            title = "Función K (acumulada)",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                strong("Cómo leer este gráfico: "),
                "K acumula todos los pares de atropellos hasta cada distancia. Si queda ",
                "por encima de la banda, hay agregación hasta esa distancia. Como es ",
                "acumulada, una agregación a poca distancia se \"arrastra\" hacia ",
                "distancias mayores: para ubicar la escala, usa la función g."),
            plotOutput(ns("plot_k"), height = "420px")
          ),
          nav_panel(
            title = "Tabla",
            DTOutput(ns("tabla_valores"))
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
# datos: reactive devuelto por mod_datos_red_server()
mod_escala_server <- function(id, datos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ────────────────────────────────────────────────────
    # DATOS (vienen del módulo "Datos y red vial")
    # ────────────────────────────────────────────────────
    hay_datos <- reactive(!is.null(datos()))

    aviso_sin_datos <- function() {
      div(class = "alert alert-warning small py-2 px-3",
          bs_icon("exclamation-triangle", class = "me-1"),
          "Primero ajusta los registros a la vía en ",
          strong("\"Datos y red vial\" → \"Configurar análisis\""), ".")
    }

    registros <- reactive({
      req(hay_datos())
      datos()$ajuste$ajustados
    })

    output$estado_datos <- renderUI({
      if (!hay_datos()) return(aviso_sin_datos())
      r <- datos()
      tagList(
        tarjetas_resumen(r$ajuste$ajustados, r$red_prep$segmentos),
        div(class = "alert alert-info small py-2 px-3 mt-3 mb-0",
            bs_icon("check-circle-fill", class = "me-1"),
            nrow(r$ajuste$ajustados), " registros ajustados (tolerancia ",
            r$ajuste$tolerancia, " m; ", nrow(r$ajuste$excluidos),
            " excluidos) sobre ", round(r$red_prep$longitud_km, 1), " km de red.")
      )
    })

    output$selector_grupo <- renderUI({
      if (!hay_datos()) {
        return(selectInput(ns("grupo"), "Registros a analizar", choices = "Todos"))
      }
      grupos <- sort(unique(registros()$grupo))
      selectInput(ns("grupo"), "Registros a analizar",
                  choices = c("Todos los grupos" = "Todos", grupos))
    })

    # ────────────────────────────────────────────────────
    # EXPLORACIÓN: perfil a lo largo de la vía
    # ────────────────────────────────────────────────────
    output$plot_perfil <- renderPlot({
      validate(need(hay_datos(),
        "Primero ajusta los registros a la vía en \"Datos y red vial\"."))
      df <- sf::st_drop_geometry(registros())
      validate(need(!all(is.na(df$km)),
        "La red no es una sola ruta continua: no hay posición en km para dibujar el perfil."))
      ggplot(df, aes(x = km, fill = grupo)) +
        geom_histogram(binwidth = input$ancho_barra / 1000, boundary = 0,
                       color = "white", linewidth = 0.2) +
        scale_fill_tableau_cb() +
        labs(x = "Posición a lo largo de la vía (km)", y = "Registros",
             fill = "Grupo") +
        theme_light(base_size = 13) +
        theme(legend.position = "bottom")
    })

    # ────────────────────────────────────────────────────
    # ANÁLISIS
    # ────────────────────────────────────────────────────
    resultado <- reactiveVal(NULL)

    # Si cambian los datos de entrada, el resultado anterior deja de valer
    observeEvent(datos(), resultado(NULL), ignoreInit = TRUE, ignoreNULL = FALSE)

    observeEvent(input$ejecutar, {
      if (!hay_datos()) {
        showNotification("Primero ajusta los registros a la vía en \"Datos y red vial\".",
                         type = "warning")
        return()
      }
      grupo_sel <- if (is.null(input$grupo)) "Todos" else input$grupo
      pts <- registros()
      if (!identical(grupo_sel, "Todos")) pts <- pts[pts$grupo == grupo_sel, ]

      if (nrow(pts) < 10) {
        showNotification("Se necesitan al menos 10 registros para calcular K.",
                         type = "error")
        return()
      }
      if (input$dist_max <= input$paso) {
        showNotification("El radio máximo debe ser mayor que el intervalo entre radios.",
                         type = "error")
        return()
      }

      # spNetwork es mucho más lento: en redes con ramales se limita a 199
      # simulaciones (su envolvente es puntual de todos modos).
      nsim_red <- min(input$nsim, 199)
      nsim_uso <- if (datos()$red_prep$continua) input$nsim else nsim_red

      res <- withProgress(message = "Calculando K y g en red…",
                          detail = paste(nsim_uso, "simulaciones"), value = 0.3, {
        tryCatch({
          rp <- datos()$red_prep
          if (rp$continua) {
            calcular_k_ruta(
              x        = pts$km * 1000,
              L        = as.numeric(sf::st_length(rp$linea)),
              dist_max = input$dist_max,
              paso     = input$paso,
              ancho_g  = input$ancho_g,
              nsim     = input$nsim
            )
          } else {
            calcular_k_red(
              lineas   = rp$segmentos,
              puntos   = pts,
              dist_max = input$dist_max,
              paso     = input$paso,
              ancho_g  = input$ancho_g,
              nsim     = nsim_red
            )
          }
        }, error = function(e) {
          showNotification(paste("Error al calcular K:", conditionMessage(e)),
                           type = "error", duration = 10)
          NULL
        })
      })
      if (is.null(res)) return()

      res$grupo <- grupo_sel
      res$n     <- nrow(pts)
      res$param <- list(dist_max = input$dist_max, paso = input$paso,
                        ancho_g = input$ancho_g, nsim = nsim_uso)
      resultado(res)
    })

    output$estado_analisis <- renderUI({
      r <- resultado()
      if (is.null(r)) {
        if (!hay_datos()) return(aviso_sin_datos())
        return(div(
          class = "alert alert-secondary small py-3 px-3",
          bs_icon("exclamation-circle", class = "me-1"),
          "Aún no se ha calculado K.", tags$br(), tags$br(),
          "Revisa los parámetros y presiona ", strong("\"Calcular K y g\""),
          ". El tiempo depende del número de registros y de simulaciones."
        ))
      }
      tagList(
        interpretacion_escala(r),
        actionButton(ns("ir_a_resultados"), "Ver resultados completos →",
                     class = "btn-outline-primary w-100")
      )
    })

    observeEvent(input$ir_a_resultados, {
      bslib::nav_select(id = "tabs_escala", selected = "tab_resultados",
                        session = session)
    })

    # Figura didáctica de "¿Qué es?" (valores por defecto: 200 m y 3000 m)
    output$fig_ventana <- renderPlot(figura_ventana(), res = 96)

    # ────────────────────────────────────────────────────
    # RESULTADOS
    # ────────────────────────────────────────────────────
    output$resumen_resultados <- renderUI({
      r <- resultado()
      if (is.null(r)) {
        return(div(class = "alert alert-secondary small py-2 px-3",
                   bs_icon("exclamation-circle", class = "me-1"),
                   "Primero calcula K y g en \"Configurar análisis\"."))
      }
      esc <- r$escala
      tagList(
        if (identical(r$metodo, "spNetwork")) {
          div(class = "alert alert-warning small py-2 px-3 mb-3",
              bs_icon("exclamation-triangle", class = "me-1"),
              strong("Red con ramales: "),
              "K y g se calcularon con spNetwork (", r$param$nsim,
              " simulaciones). Su banda es puntual (radio por radio), así que ",
              "algún punto aislado puede salir de ella por azar; además, sus ",
              "simulaciones no son independientes del patrón observado. ",
              "Interpreta con cautela y confía solo en patrones que se mantienen ",
              "a lo largo de varios radios seguidos.")
        } else {
          div(class = "alert alert-info small py-2 px-3 mb-3",
              bs_icon("info-circle", class = "me-1"),
              strong("Ruta continua: "),
              "cálculo exacto con distancias a lo largo de la vía, ",
              r$param$nsim, " simulaciones al azar uniforme sobre toda la ruta ",
              "y envolvente global.")
        },
      layout_columns(
        col_widths = c(3, 3, 3, 3), fill = FALSE,
        tarjeta_valor(r$n, if (identical(r$grupo, "Todos")) "Registros" else
          paste("Registros —", r$grupo), colores$primario),
        tarjeta_valor(if (is.na(esc$desde)) "—" else paste(esc$desde, "m"),
                      "Agregación desde (g)", colores$secundario),
        tarjeta_valor(if (is.na(esc$hasta)) "—" else paste(esc$hasta, "m"),
                      "Agregación hasta (g)", colores$acento),
        tarjeta_valor(r$param$nsim, "Simulaciones", colores$texto)
      ),
      div(class = "mt-3", interpretacion_escala(r))
      )
    })

    output$plot_g <- renderPlot({
      r <- resultado()
      req(r)
      grafico_funcion(r$valores, "g", global = identical(r$metodo, "ruta"))
    })

    output$plot_k <- renderPlot({
      r <- resultado()
      req(r)
      grafico_funcion(r$valores, "k", global = identical(r$metodo, "ruta"))
    })

    output$tabla_valores <- renderDT({
      r <- resultado()
      req(r)
      v <- r$valores
      v[] <- lapply(v, function(x) if (is.numeric(x)) round(x, 3) else x)
      datatable(v, rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 15))
    })

    # ────────────────────────────────────────────────────
    # CÓDIGO R
    # ────────────────────────────────────────────────────
    output$codigo_r <- renderText({
      r <- resultado()
      if (is.null(r)) {
        return("# Calcula K y g en \"Configurar análisis\" para generar el código.")
      }
      p <- r$param
      filtro <- if (!identical(r$grupo, "Todos"))
        paste0("ajustados <- ajustados[ajustados$grupo == \"", r$grupo, "\", ]\n")

      if (identical(r$metodo, "ruta")) {
        return(paste0(
          encabezado_script("StatRoad", "Escala de agregación — K y g en una ruta"),
          "# 'ajustados' (con su columna km) y 'linea' (la ruta como una sola\n",
          "# línea) vienen del paso de ajuste a la vía.\n",
          filtro,
          "x <- ajustados$km * 1000                       # posiciones (m)\n",
          "L <- as.numeric(sf::st_length(linea))           # longitud (m)\n\n",
          "# K y g exactos: distancia por la vía = |x_i - x_j|. Se cuentan\n",
          "# los pares con las posiciones ordenadas (sin matriz de distancias)\n",
          "k_g_ruta <- function(x, L, r, w) {\n",
          "  n  <- length(x)\n",
          "  xs <- sort(x)\n",
          "  f  <- 2 * L / (n * (n - 1))\n",
          "  hasta <- function(t) vapply(t, function(u) {\n",
          "    if (u < 0) return(0)\n",
          "    (sum(findInterval(xs + u, xs)) -\n",
          "       sum(findInterval(xs - u, xs, left.open = TRUE)) - n) / 2\n",
          "  }, numeric(1))\n",
          "  inf <- ifelse(r - w / 2 <= 0, -1, r - w / 2)\n",
          "  list(k = f * hasta(r), g = f * (hasta(r + w / 2) - hasta(inf)))\n",
          "}\n\n",
          "r   <- seq(0, ", p$dist_max, ", by = ", p$paso, ")\n",
          "obs <- k_g_ruta(x, L, r, w = ", p$ancho_g, ")\n\n",
          "# ", p$nsim, " simulaciones al azar uniforme sobre la ruta\n",
          "set.seed(2026)\n",
          "sims <- replicate(", p$nsim, ", k_g_ruta(runif(length(x), 0, L), L, r, w = ",
          p$ancho_g, ")$g)\n\n",
          "# Envolvente global (una sola banda para toda la curva)\n",
          "cs   <- GET::create_curve_set(list(r = r, obs = obs$g, sim_m = sims))\n",
          "prueba <- GET::global_envelope_test(cs, type = \"area\",\n",
          "                                    alternative = \"two.sided\")\n",
          "attr(prueba, \"p\")                                # valor p\n",
          "env   <- as.data.frame(prueba)\n",
          "banda <- rbind(env$lo, env$hi)\n\n",
          "# Valor esperado de g bajo azar (exacto): con dos puntos al azar en\n",
          "# [0, L], P(d <= t) = 1 - (1 - t/L)^2\n",
          "acum <- function(t) 1 - (1 - pmin(pmax(t, 0), L) / L)^2\n",
          "esp  <- L * (acum(r + ", p$ancho_g, " / 2) - acum(r - ", p$ancho_g, " / 2))\n\n",
          "# g como razón observado / esperado (1 = azar)\n",
          "oe <- obs$g / esp\n",
          "banda_oe <- sweep(banda, 2, esp, \"/\")\n\n",
          "plot(r, oe, type = \"n\", ylim = range(c(oe, banda_oe)),\n",
          "     xlab = \"Radio r, medido por la vía (m)\",\n",
          "     ylab = \"g(r): observado / esperado\")\n",
          "polygon(c(r, rev(r)), c(banda_oe[1, ], rev(banda_oe[2, ])),\n",
          "        col = adjustcolor(\"grey\", 0.5), border = NA)\n",
          "abline(h = 1, lty = 2)\n",
          "lines(r, oe, lwd = 2, col = \"#1170AA\")\n"
        ))
      }

      paste0(
        encabezado_script("StatRoad", "Escala de agregación — K y g en red"),
        "library(sf)\nlibrary(spNetwork)\n\n",
        "# 'red' y 'ajustados' vienen del paso de ajuste a la vía\n",
        "# (ver el código del módulo \"Datos y red vial\").\n",
        "red <- st_cast(red, \"LINESTRING\")\n",
        filtro,
        "\nset.seed(2026)\n",
        "k <- kfunctions(\n",
        "  lines  = red,\n",
        "  points = ajustados,\n",
        "  start  = 0,\n",
        "  end    = ", p$dist_max, ",\n",
        "  step   = ", p$paso, ",\n",
        "  width  = ", p$ancho_g, ",\n",
        "  nsim   = ", p$nsim, ",\n",
        "  conf_int = 0.05,\n",
        "  verbose  = FALSE\n",
        ")\n\n",
        "k$plotg   # función g con envolvente\n",
        "k$plotk   # función K con envolvente\n",
        "head(k$values)\n"
      )
    })

    # Resultado disponible para los módulos siguientes (escala sugerida)
    resultado
  })
}

# ── Cálculo ───────────────────────────────────────────────

# K y g en una ruta continua (método exacto).
# x: posiciones en metros a lo largo de la ruta; L: longitud (m).
# Distancia por la vía = |x_i - x_j|. Misma normalización que
# spNetwork: K(r) = L/(n(n-1)) · #pares ordenados con d <= r;
# g(r) igual, contando pares con d en el anillo (r - w/2, r + w/2].
# Cuenta los pares con las posiciones ordenadas (findInterval), sin
# construir la matriz de distancias: mismos valores, mucho más rápido.
k_g_ruta <- function(x, L, r, w) {
  n  <- length(x)
  xs <- sort(x)
  f  <- 2 * L / (n * (n - 1))
  # nº de pares (no ordenados, i != j) con |x_i - x_j| <= t
  hasta <- function(t) vapply(t, function(u) {
    if (u < 0) return(0)
    (sum(findInterval(xs + u, xs)) -
       sum(findInterval(xs - u, xs, left.open = TRUE)) - n) / 2
  }, numeric(1))
  inf <- ifelse(r - w / 2 <= 0, -1, r - w / 2)
  list(
    k = f * hasta(r),
    g = f * (hasta(r + w / 2) - hasta(inf))
  )
}

# Valor esperado de g(r) bajo azar uniforme en [0, L] (exacto).
# Con dos puntos al azar en [0, L], la distancia d = |x_i - x_j| tiene
# P(d <= t) = 1 - (1 - t/L)^2. Con la normalización de k_g_ruta,
# E[g(r)] = L · P(a < d <= b), con a = max(r - w/2, 0) y b = r + w/2.
# Incluye el efecto de borde de la ruta y el recorte del anillo en 0.
esperado_g_ruta <- function(L, r, w) {
  acum <- function(t) 1 - (1 - pmin(pmax(t, 0), L) / L)^2
  L * (acum(r + w / 2) - acum(r - w / 2))
}

# Envolvente global de Monte Carlo (GET, tipo "area", bilateral):
# una sola banda para toda la curva, con 5 % de error en total. Así un
# punto aislado fuera de la banda por azar no se toma como agregación.
envolvente_global <- function(r, obs, sim) {
  cs  <- GET::create_curve_set(list(r = r, obs = obs, sim_m = sim))
  res <- GET::global_envelope_test(cs, type = "area", alternative = "two.sided")
  df  <- as.data.frame(res)
  list(lo = df$lo, hi = df$hi, p = attr(res, "p"))
}

# Funciones observadas + envolventes globales (azar uniforme en [0, L])
calcular_k_ruta <- function(x, L, dist_max, paso, ancho_g, nsim) {
  r   <- seq(0, dist_max, by = paso)
  obs <- k_g_ruta(x, L, r, ancho_g)
  n   <- length(x)
  sims <- lapply(seq_len(nsim), function(i) {
    k_g_ruta(stats::runif(n, 0, L), L, r, ancho_g)
  })
  sim_k <- vapply(sims, `[[`, numeric(length(r)), "k")
  sim_g <- vapply(sims, `[[`, numeric(length(r)), "g")
  ek <- envolvente_global(r, obs$k, sim_k)
  eg <- envolvente_global(r, obs$g, sim_g)

  valores <- data.frame(
    distances = r,
    obs_k = obs$k, lower_k = ek$lo, upper_k = ek$hi,
    obs_g = obs$g, lower_g = eg$lo, upper_g = eg$hi
  )
  # g como razón observado / esperado (1 = azar)
  esp <- esperado_g_ruta(L, r, ancho_g)
  valores$esperado_g <- esp
  valores$oe_obs_g   <- valores$obs_g   / esp
  valores$oe_lower_g <- valores$lower_g / esp
  valores$oe_upper_g <- valores$upper_g / esp
  list(valores = valores, escala = resumir_escala(valores, p = eg$p),
       metodo = "ruta", p_g = eg$p, p_k = ek$p)
}

# Redes con ramales: spNetwork::kfunctions(). Sus envolventes pueden
# ser demasiado estrechas (las simulaciones no son independientes del
# patrón observado); se advierte al usuario.
calcular_k_red <- function(lineas, puntos, dist_max, paso, ancho_g, nsim) {
  lineas <- sf::st_cast(sf::st_zm(lineas), "LINESTRING", warn = FALSE)
  puntos <- sf::st_zm(puntos)

  k <- spNetwork::kfunctions(
    lines    = lineas,
    points   = puntos,
    start    = 0,
    end      = dist_max,
    step     = paso,
    width    = ancho_g,
    nsim     = nsim,
    conf_int = 0.05,
    verbose  = FALSE
  )

  valores  <- as.data.frame(k$values)
  esperadas <- c("distances", "obs_k", "lower_k", "upper_k",
                 "obs_g", "lower_g", "upper_g")
  faltan <- setdiff(esperadas, names(valores))
  if (length(faltan) > 0) {
    stop("spNetwork devolvió columnas inesperadas (faltan: ",
         paste(faltan, collapse = ", "), "). Revisar la versión de spNetwork.",
         call. = FALSE)
  }

  list(valores = valores, escala = resumir_escala(valores), metodo = "spNetwork")
}

# Primer tramo continuo de distancias con g por encima de la envolvente.
# p: valor p de la prueba de envolvente global (ruta continua). Si no
# es significativa, no se informa agregación. En redes con ramales
# (spNetwork, envolvente puntual) p es NA.
resumir_escala <- function(v, p = NA) {
  if (!is.na(p) && p >= 0.05) return(list(desde = NA, hasta = NA))
  sig <- v$obs_g > v$upper_g
  sig[is.na(sig)] <- FALSE
  if (!any(sig)) return(list(desde = NA, hasta = NA))
  ini <- which(sig)[1]
  fin <- ini
  while (fin < length(sig) && sig[fin + 1]) fin <- fin + 1
  list(desde = v$distances[ini], hasta = v$distances[fin])
}

# Interpretación del resultado: lo técnico y lo que significa en
# términos de colisiones. Se muestra en "Configurar análisis" y en
# "Resultados".
interpretacion_escala <- function(r) {
  esc <- r$escala
  v   <- r$valores
  fmt <- function(x) format(round(x, 1), decimal.mark = ",", nsmall = 1)
  r_max <- max(v$distances)

  txt_p <- if (!is.null(r$p_g) && !is.na(r$p_g))
    paste0(" Prueba de envolvente global: ",
           if (r$p_g < 0.001) "p < 0,001" else
             paste0("p = ", format(round(r$p_g, 3), decimal.mark = ",")),
           ".") else ""

  # Sin agregación
  if (is.na(esc$desde)) {
    return(div(
      class = "alert alert-secondary small py-2 px-3 mb-3",
      p(class = "mb-2", bs_icon("dash-circle", class = "me-1"),
        strong("Sin evidencia de una distribución agregada.")),
      tags$ul(
        class = "mb-0 ps-3",
        tags$li(strong("Qué se encontró: "), "la función g no sale de la banda ",
                "del azar entre 0 y ", r_max, " m.", txt_p),
        tags$li(strong("En palabras simples: "), "los atropellos se distribuyen ",
                "a lo largo de la vía como lo haría el azar; no se detectan grupos."),
        tags$li(strong("Qué implica: "), "el KDE y el Gi* pueden mostrar picos, ",
                "pero probablemente sean producto del azar. Con pocos registros la ",
                "prueba puede no detectar grupos aunque existan, y grupos más ",
                "grandes que el radio máximo no se ven en este rango.")
      )
    ))
  }

  # Con agregación
  rango <- v$distances >= esc$desde & v$distances <= esc$hasta
  maximo <- if ("oe_obs_g" %in% names(v)) max(v$oe_obs_g[rango]) else NA
  tecnico <- paste0("la función g queda por encima de la banda del azar entre ",
                    esc$desde, " y ", esc$hasta, " m",
                    if (!is.na(maximo))
                      paste0(" (hasta ", fmt(maximo), " veces lo esperado)"),
                    ".", txt_p)
  simple <- if (esc$desde == 0) {
    paste0("los atropellos no están repartidos al azar: ocurren en grupos de ",
           "hasta unos ", esc$hasta, " m de carretera. A más de ", esc$hasta,
           " m, la posición de un atropello ya no tiene relación con la de otro.")
  } else {
    paste0("los atropellos no están repartidos al azar: hay más pares de ",
           "atropellos de lo esperado separados por entre ", esc$desde, " y ",
           esc$hasta, " m de carretera.")
  }

  div(
    class = "alert alert-info small py-2 px-3 mb-3",
    p(class = "mb-2", bs_icon("check-circle-fill", class = "me-1"),
      strong("Distribución agregada de colisiones.")),
    tags$ul(
      class = "mb-0 ps-3",
      tags$li(strong("Qué se encontró: "), tecnico),
      tags$li(strong("En palabras simples: "), simple),
      tags$li(strong("Lo que todavía no sabemos: "), "cuántos grupos hay ni ",
              "dónde están. Eso lo responden los ", strong("Puntos críticos"),
              " (KDE y Gi*), que usan estos ", esc$hasta, " m como ancho de banda ",
              "y largo de segmento sugeridos."),
      tags$li(strong("Para manejo: "), esc$hasta, " m es una referencia para el ",
              "largo de las intervenciones (cercas, señalización, reductores de ",
              "velocidad) y de los tramos de monitoreo.")
    )
  )
}

# Gráfico de K o g con envolvente; resalta distancias con agregación.
# En una ruta continua, g se muestra como razón observado / esperado
# (1 = azar). En redes con ramales (spNetwork) no se dispone del
# valor esperado y g se muestra en su escala original.
grafico_funcion <- function(v, tipo = c("g", "k"), global = TRUE) {
  tipo <- match.arg(tipo)
  razon <- tipo == "g" && "oe_obs_g" %in% names(v)
  pre <- if (razon) "oe_" else ""
  df <- data.frame(
    d   = v$distances,
    obs = v[[paste0(pre, "obs_", tipo)]],
    lo  = v[[paste0(pre, "lower_", tipo)]],
    hi  = v[[paste0(pre, "upper_", tipo)]]
  )
  df$agregado <- df$obs > df$hi
  etiqueta <- if (razon) "g(r): observado / esperado" else
    if (tipo == "g") "g(r)" else "K(r)"

  ggplot(df, aes(x = d)) +
    {if (razon) geom_hline(yintercept = 1, linetype = "dashed",
                           color = colores$texto)} +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = colores$tableau[3], alpha = 0.45) +
    geom_line(aes(y = obs), color = colores$primario, linewidth = 1) +
    geom_point(data = df[df$agregado, ], aes(y = obs),
               color = colores$acento, size = 1.8) +
    labs(x = "Radio r, medido por la vía (m)", y = etiqueta,
         caption = paste0(
           "Banda gris: envolvente ", if (global) "global " else "puntual ",
           "de Monte Carlo (95 %). Puntos naranja: agregación.",
           if (razon) "\nLínea discontinua: 1 = lo esperado por azar.")) +
    theme_light(base_size = 13)
}

# Figura didáctica: la ventana (anillo) de ancho fijo que se aleja del
# atropello hasta el radio máximo. Se muestra en "¿Qué es?".
figura_ventana <- function(ancho = 200, r_max = 3000,
                           radios = c(500, 1500, 3000)) {
  media <- ancho / 2
  etq <- paste0("r = ", radios, " m  →  ventana de ", radios - media,
                " a ", radios + media, " m (hacia cada lado)")
  d <- data.frame(panel = factor(etq, levels = etq), r = radios)
  ventanas <- rbind(
    data.frame(panel = d$panel, xmin = d$r - media,  xmax = d$r + media),
    data.frame(panel = d$panel, xmin = -d$r - media, xmax = -d$r + media)
  )
  lim <- r_max + media

  ggplot() +
    annotate("rect", xmin = -lim, xmax = lim, ymin = -0.06, ymax = 0.06,
             fill = "grey80") +
    geom_rect(data = ventanas, aes(xmin = xmin, xmax = xmax),
              ymin = -0.35, ymax = 0.35, fill = colores$acento, alpha = 0.85) +
    annotate("point", x = 0, y = 0, size = 4, color = colores$primario) +
    annotate("text", x = 0, y = 0.62, label = "atropello", size = 3.6,
             color = colores$primario) +
    geom_vline(xintercept = c(-r_max, r_max), linetype = "dashed",
               color = "grey40") +
    annotate("text", x = r_max, y = 0.62, label = "radio máximo", size = 3.3,
             color = "grey40", hjust = 1.05) +
    geom_segment(data = d, aes(x = 0, xend = r), y = -0.6, yend = -0.6,
                 arrow = grid::arrow(length = grid::unit(2, "mm")),
                 color = colores$primario) +
    geom_text(data = d, aes(x = r / 2, label = paste0("r = ", r, " m")),
              y = -0.85, size = 3.6, color = colores$primario) +
    facet_wrap(~panel, ncol = 1) +
    scale_x_continuous("Distancia por la vía desde el atropello (m)",
                       breaks = seq(-r_max, r_max, by = 1000), labels = abs) +
    coord_cartesian(ylim = c(-1, 0.8)) +
    theme_minimal(base_size = 12) +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"))
}
