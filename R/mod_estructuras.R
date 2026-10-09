# ============================================================
# mod_estructuras.R — Atropellos y estructuras de la vía
# (alcantarillas, puentes, pasos de fauna…)
# StatRoad · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Pregunta: ¿los atropellos ocurren más cerca de las estructuras de
# lo que esperaríamos por azar? Tres análisis sobre la posición en
# km a lo largo de la vía (requiere una ruta continua):
#   1. Distancia a la estructura más cercana + Monte Carlo
#   2. K cruzada en 1D (razón observado/esperado) + envolvente global
#   3. Modelo por segmentos (binomial negativa)
# Modelo nulo: desplazamiento circular de los atropellos (las
# estructuras quedan fijas). Funciones en fct_estructuras.R.
#
# Datos de ejemplo: 43 estructuras de la Ruta 1 (data-raw/, datos_ejemplo.R).
# Recibe el resultado de mod_datos_red_server().
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_estructuras_ui <- function(id) {
  ns <- NS(id)

  navset_card_tab(
    id = ns("tabs_est"),

    # ── 1. ¿Qué es? ───────────────────────────────────────
    nav_panel(
      fillable = FALSE,
      title = tagList(bs_icon("book", class = "me-1"), "¿Qué es?"),
      card_body(
        withMathJax(),
        div(
          class = "px-1 pb-2",
          style = "max-width: 780px; margin: 0 auto;",
          h5("Atropellos y estructuras de la vía",
             style = paste0("color:", colores$primario, "; font-weight:700;")),
          p(
            "Las carreteras tienen ", strong("estructuras"), " — alcantarillas, ",
            "puentes, pasos de fauna, cercas — que pueden influir en dónde cruzan ",
            "los animales. Este módulo responde una pregunta concreta: ",
            strong("¿los atropellos ocurren más cerca de las estructuras de lo ",
                   "que esperaríamos por azar?")
          ),
          div(
            class = "alert alert-warning small mt-3",
            bs_icon("exclamation-triangle", class = "me-1"),
            strong("Asociación no es efecto. "),
            "\"¿Las estructuras están donde cruza la fauna?\" y \"¿las estructuras ",
            "reducen los atropellos?\" son preguntas distintas. Las alcantarillas ",
            "suelen estar en quebradas y drenajes, que son a la vez corredores de ",
            "fauna: una asociación puede reflejar el hábitat y no la estructura. ",
            "Saber si una estructura ", em("funciona"), " requiere otro diseño ",
            "(antes/después, cámaras trampa, dimensiones, cercas)."
          ),

          h6(class = "mt-4", style = "font-weight:700;", "Paso a paso"),
          card(
            fill = FALSE, class = "mt-2",
            card_header(bs_icon("rulers", class = "me-1"),
                        "1. La ruta como una regla"),
            card_body(p(class = "small mb-0",
              "Cada atropello y cada estructura se ajustan a la vía y reciben su ",
              strong("posición en km"), ". Como la ruta es una sola línea, la ",
              "distancia en red entre un atropello ", em("i"), " y una estructura ",
              em("j"), " es simplemente \\(|\\,km_i - km_j\\,|\\)."))
          ),
          card(
            fill = FALSE, class = "mt-2",
            card_header(bs_icon("arrows-collapse", class = "me-1"),
                        "2. Un número que resume la relación"),
            card_body(p(class = "small mb-0",
              "Para cada atropello se mide la distancia a la estructura más ",
              "cercana, \\(d_i = \\min_j |\\,km_i - km_j\\,|\\), y se resume con la ",
              strong("mediana"), ". Ese número solo no dice nada: 400 m puede ser ",
              "\"cerca\" o \"lejos\" según el largo de la ruta, cuántas estructuras ",
              "hay y cómo se agrupan los atropellos. Hace falta una referencia."))
          ),
          card(
            fill = FALSE, class = "mt-2",
            card_header(bs_icon("shuffle", class = "me-1"),
                        "3. El azar como referencia: el desplazamiento circular"),
            card_body(
              p(class = "small",
                "Se construye un mundo donde las estructuras ", em("no importan"),
                " y se mide la misma mediana ahí. En la figura, la fila de arriba ",
                "son los datos; las estructuras (triángulos) nunca se mueven."),
              plotOutput(ns("plot_idea"), height = "300px"),
              tags$ul(
                class = "small mt-2",
                tags$li(strong("Desplazamiento circular (el que usa StatRoad):"),
                        " todos los atropellos se mueven juntos la misma distancia ",
                        "al azar, como si la ruta fuera un anillo. Los puntos ",
                        "críticos siguen existiendo con su forma; solo pierden su ",
                        "posición respecto a las estructuras."),
                tags$li(strong("Reubicación uniforme (NO se usa):"),
                        " cada atropello cae en un lugar independiente y los puntos ",
                        "críticos desaparecen. Ese mundo no es comparable con los ",
                        "datos: atropellos agrupados parecen \"anormalmente cerca\" ",
                        "de cualquier cosa que esté junto a un grupo.")
              ),
              p(class = "small",
                "Se repite muchas veces (por ejemplo 999) y el valor p es la ",
                "proporción de repeticiones con una mediana tan extrema como la ",
                "observada: \\(p = (1 + \\#\\{D^* \\le D\\}) / (n_{sim} + 1)\\)."),
              div(
                class = "alert alert-secondary small mb-0",
                bs_icon("clipboard-data", class = "me-1"),
                strong("Por qué importa: "),
                "al colocar estructuras completamente al azar sobre los atropellos ",
                "de ejemplo, la reubicación uniforme declaró una asociación en el ",
                strong("32 %"), " (distancia) y el ", strong("84 %"),
                " (K cruzada) de los casos. Con el desplazamiento circular, en el ",
                "3–7 %, lo esperado para un nivel de 5 %."
              )
            )
          ),
          card(
            fill = FALSE, class = "mt-2",
            card_header(bs_icon("graph-up", class = "me-1"),
                        "4. ¿A qué escala? La K cruzada"),
            card_body(p(class = "small mb-0",
              "La prueba anterior da un solo número. La ", strong("K cruzada"),
              " mira cada radio ", em("r"), " (50 m, 100 m… 2 km) y compara ",
              "cuántos atropellos hay a menos de ", em("r"), " de las estructuras ",
              "con lo esperado si estuvieran repartidos al azar: ",
              "\\(O/E(r) = \\dfrac{\\text{atropellos a} \\le r}{\\lambda \\cdot 2r \\cdot n_{est}}\\). ",
              "Un valor de 1 es lo esperado; 2 significa el doble de atropellos ",
              "de lo esperado. La banda gris es una ", strong("envolvente global"),
              ": si la curva sale de ella en cualquier punto, la diferencia es ",
              "significativa al 5 % sin corregir por comparaciones múltiples."))
          ),
          card(
            fill = FALSE, class = "mt-2",
            card_header(bs_icon("bar-chart-steps", class = "me-1"),
                        "5. ¿Cuánto? El modelo por segmentos"),
            card_body(p(class = "small mb-0",
              "La ruta se divide en segmentos (por ejemplo de 500 m), se cuentan ",
              "los atropellos de cada uno y se ajusta una ", strong("binomial negativa"),
              " con la distancia a la estructura más cercana como predictor. ",
              "La ", strong("razón de tasas"), " es el tamaño del efecto: 0.6 ",
              "significa que por cada km de alejamiento de una estructura los ",
              "atropellos bajan un 40 %. El valor p ", em("no"), " es el de Wald: ",
              "los segmentos vecinos se parecen entre sí y el p de Wald resulta ",
              "optimista, así que se calcula con el mismo desplazamiento circular."))
          ),
          card(
            fill = FALSE, class = "mt-2",
            card_header(bs_icon("fire", class = "me-1"),
                        "6. ¿Y los puntos críticos?"),
            card_body(
              p(class = "small",
                "Con los puntos críticos que encontró el ", strong("KDE"), ", la ",
                "pestaña \"Hotspots\" muestra qué estructuras tiene cada uno y a qué ",
                "distancia queda la más cercana. Es información útil para gestión: ",
                "dónde hay una estructura que se podría adecuar para la fauna."),
              p(class = "small mb-0",
                bs_icon("exclamation-triangle", class = "me-1"),
                strong("Precaución: "),
                "la prueba \"¿hay más estructuras dentro de los puntos críticos de lo ",
                "esperado?\" tiene poca potencia. Si las estructuras son frecuentes ",
                "(una por km, por ejemplo), muchos puntos críticos tienen una cerca ",
                "por pura casualidad; y con pocos puntos críticos hay muy poca ",
                "información. Para probar la asociación, las pruebas con los ",
                "atropellos individuales son mucho más potentes."))
          ),
          div(
            class = "alert alert-info small mt-3 mb-0",
            bs_icon("info-circle", class = "me-1"),
            strong("Analiza por grupo. "),
            "Si solo un grupo (p. ej. anfibios) usa los drenajes, al mezclarlo con ",
            "los demás el efecto se diluye. En los datos de ejemplo, \"Todos\" no ",
            "muestra asociación, pero los anfibios sí."
          )
        )
      )
    ),

    # ── 2. Los datos ──────────────────────────────────────
    nav_panel(
      fillable = FALSE,
      title = tagList(bs_icon("table", class = "me-1"), "Los datos"),
      card_body(
        navset_pill(
          nav_panel(
            fillable = FALSE,
            title = tagList(bs_icon("collection", class = "me-1"),
                            "Estructuras de ejemplo"),
            br(),
            layout_columns(
              col_widths = c(4, 8), fill = FALSE,
              div(
                div(
                  class = "alert alert-info small py-2 px-3 mb-3",
                  bs_icon("info-circle-fill", class = "me-1"),
                  strong("43 estructuras"), " sobre la Ruta 1, entre Liberia y La Cruz: ",
                  "4 puentes y 3 alcantarillas en ", strong("cruces reales"),
                  " con ríos y quebradas (OpenStreetMap), y 36 alcantarillas simuladas.",
                  tags$br(), tags$br(),
                  "Lo sembrado: 4 de los 6 puntos críticos (H1–H4) están junto a una ",
                  "estructura; H5 y H6, no. Además, parte de los anfibios se atropella ",
                  "junto a las alcantarillas, como si cruzaran por los drenajes. Una ",
                  "alcantarilla está a ~150 m de la vía (error de digitación) para ",
                  "que el ajuste la excluya."
                ),
                downloadButton(ns("descargar_est_ejemplo"),
                               "Descargar estructuras de ejemplo (.csv)",
                               class = "btn-outline-primary btn-sm w-100")
              ),
              card(
                fill = FALSE,
                card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                card_body(style = "overflow: auto;", DTOutput(ns("tabla_ejemplo")))
              )
            )
          ),
          nav_panel(
            fillable = FALSE,
            title = tagList(bs_icon("folder2-open", class = "me-1"),
                            "Mis estructuras"),
            br(),
            layout_columns(
              col_widths = c(5, 7), fill = FALSE,
              card(
                fill = FALSE,
                card_header(bs_icon("bricks", class = "me-1"), "Capa de estructuras"),
                card_body(
                  p(class = "small mb-2",
                    strong("Tabla"), " (CSV o Excel) con columnas ", code("x"), " y ",
                    code("y"), " (", code("id"), " y ", code("tipo"), " opcionales), o ",
                    strong("capa de puntos"), " (GPKG, GeoJSON o Shapefile en .zip)."),
                  fileInput(ns("archivo_est"), label = NULL,
                            accept = c(".csv", ".xlsx", ".xls", ".gpkg",
                                       ".geojson", ".json", ".zip"),
                            buttonLabel = "Buscar…",
                            placeholder = "CSV, Excel, GPKG…"),
                  p(class = "small text-muted mb-2",
                    bs_icon("info-circle", class = "me-1"),
                    "Solo para tablas (CSV o Excel):"),
                  selectInput(ns("crs_coords"), "Las coordenadas x/y están en:",
                              choices = crs_coordenadas, selected = "4326"),
                  conditionalPanel(
                    condition = "input.crs_coords == 'otro'", ns = ns,
                    textInput(ns("crs_coords_otro"), "Código EPSG de las coordenadas",
                              placeholder = "por ejemplo, 32718"),
                    p(class = "small text-muted mt-n2",
                      "Puedes buscar el código de tu sistema en ",
                      tags$a("epsg.io", href = "https://epsg.io", target = "_blank"), ".")
                  ),
                  selectInput(ns("separador"), "Separador (solo CSV):",
                              choices = c("Coma (,)" = ",", "Punto y coma (;)" = ";",
                                          "Tabulador" = "\t"),
                              selected = ","),
                  downloadButton(ns("descargar_plantilla"),
                                 "Descargar plantilla (.csv)",
                                 class = "btn-outline-primary btn-sm w-100")
                )
              ),
              div(
                card(
                  fill = FALSE,
                  card_header(bs_icon("table", class = "me-1"),
                              "Así se debe ver tu tabla de estructuras"),
                  card_body(
                    tags$table(
                      class = "table table-sm table-bordered mb-2",
                      tags$thead(tags$tr(tags$th("id"), tags$th("tipo"),
                                         tags$th("x"), tags$th("y"))),
                      tags$tbody(
                        tags$tr(tags$td("E01"), tags$td("Alcantarilla"),
                                tags$td("-85.5441"), tags$td("10.8187")),
                        tags$tr(tags$td("E02"), tags$td("Puente"),
                                tags$td("-85.6012"), tags$td("10.8836")),
                        tags$tr(tags$td(em("...")), tags$td(em("...")),
                                tags$td(em("...")), tags$td(em("...")))
                      )
                    ),
                    tags$ul(
                      class = "small text-muted mb-0",
                      tags$li(code("tipo"), " permite analizar cada tipo por separado ",
                              "(Alcantarilla, Puente, Paso de fauna…)."),
                      tags$li("Puedes agregar otras columnas (diámetro, largo…); ",
                              "se conservan en las descargas."),
                      tags$li("Usa la misma red vial y los mismos atropellos del ",
                              "módulo \"Datos y red vial\".")
                    )
                  )
                ),
                br(),
                card(
                  fill = FALSE,
                  card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                  card_body(style = "overflow: auto;",
                            uiOutput(ns("estado_propias")),
                            DTOutput(ns("tabla_propias")))
                )
              )
            )
          )
        )
      )
    ),

    # ── 3. Configurar análisis ────────────────────────────
    nav_panel(
      fillable = FALSE,
      title = tagList(bs_icon("gear", class = "me-1"), "Configurar análisis"),
      card_body(
        layout_columns(
          col_widths = c(4, 8), fill = FALSE,
          card(
            fill = FALSE,
            card_header(bs_icon("sliders", class = "me-1"), "Parámetros"),
            card_body(
              uiOutput(ns("ui_grupo")),
              uiOutput(ns("ui_tipos")),
              sliderInput(ns("tolerancia"), "Tolerancia de las estructuras (m)",
                          min = 10, max = 300, value = 50, step = 5),
              p(class = "small text-muted mt-n2 mb-3",
                "Estructuras más lejos de la vía se excluyen y se listan aparte."),
              sliderInput(ns("rmax"), "Radio máximo de la K cruzada (km)",
                          min = 0.5, max = 5, value = 2, step = 0.25),
              p(class = "small text-muted mt-n2 mb-3",
                "La escala a la que una estructura podría influir. Rangos muy ",
                "grandes diluyen un efecto local y restan potencia."),
              sliderInput(ns("largo_seg"), "Largo de los segmentos (km)",
                          min = 0.2, max = 2, value = 0.5, step = 0.1),
              selectInput(ns("nsim"), "Número de simulaciones",
                          choices = c(499, 999, 1999), selected = 999),
              numericInput(ns("semilla"), "Semilla", value = 2026, min = 1, step = 1),
              p(class = "small text-muted mt-n2 mb-3",
                "Con la misma semilla se obtienen exactamente los mismos resultados."),
              actionButton(ns("analizar"), "Analizar", class = "btn-primary w-100 mt-2",
                           icon = icon("play"))
            )
          ),
          div(uiOutput(ns("estado_analisis")))
        )
      )
    ),

    # ── 4. Resultados ─────────────────────────────────────
    nav_panel(
      value = "tab_resultados",
      fillable = FALSE,
      title = tagList(bs_icon("graph-up-arrow", class = "me-1"), "Resultados"),
      card_body(
        uiOutput(ns("cards_resultados")),
        div(class = "d-flex flex-wrap gap-2 my-3",
            downloadButton(ns("descargar_gpkg"), "Descargar resultados (.gpkg)",
                           class = "btn-outline-primary btn-sm"),
            downloadButton(ns("descargar_resumen"), "Descargar resumen de pruebas (.csv)",
                           class = "btn-outline-primary btn-sm")),
        navset_pill(
          nav_panel(
            title = "Mapa",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                strong("Cómo leer este mapa: "),
                "los ", strong("círculos grandes con borde blanco"), " son las ",
                "estructuras ajustadas a la vía (color según el tipo); en ",
                strong("rojo"), ", las excluidas, en su posición original. Los ",
                "puntos naranjas pequeños son los atropellos analizados. Haz clic ",
                "en cualquiera para ver sus datos (en los atropellos, también su ",
                "distancia a la estructura más cercana)."),
            leaflet::leafletOutput(ns("mapa"), height = "520px")
          ),
          nav_panel(
            title = "Distancia",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                strong("Cómo leer este gráfico: "),
                "el histograma es la mediana de la distancia a la estructura más ",
                "cercana en cada repetición del azar (desplazamiento circular). La ",
                "línea es la mediana observada. Si queda en la cola ",
                strong("izquierda"), ", los atropellos están más cerca de las ",
                "estructuras de lo esperado; en la ", strong("derecha"), ", más lejos."),
            uiOutput(ns("frase_distancia")),
            plotOutput(ns("plot_distancia"), height = "380px")
          ),
          nav_panel(
            title = "Escala (K cruzada)",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                strong("Cómo leer este gráfico: "),
                "la línea es cuántas veces más (o menos) atropellos de lo esperado ",
                "hay a menos de ", em("r"), " km de las estructuras. La banda gris ",
                "es la envolvente global del azar. Donde la curva sale ",
                strong("por encima"), " hay atracción a esa escala; ",
                strong("por debajo"), ", repulsión. Con radios pequeños la curva ",
                "es ruidosa porque cuenta pocos atropellos."),
            uiOutput(ns("frase_k")),
            plotOutput(ns("plot_k"), height = "400px")
          ),
          nav_panel(
            title = "Modelo por segmentos",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                strong("Cómo leer este gráfico: "),
                "las barras son los atropellos por segmento a lo largo de la vía; ",
                "la línea, lo que predice el modelo según la distancia a la ",
                "estructura más cercana; las marcas inferiores, las estructuras. ",
                "Si el modelo capta algo, la línea sube donde hay estructuras."),
            uiOutput(ns("control_verdad")),
            plotOutput(ns("plot_segmentos"), height = "380px"),
            br(),
            DTOutput(ns("tabla_modelo")),
            uiOutput(ns("nota_moran"))
          ),
          nav_panel(
            title = "Hotspots",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                strong("Cómo leer esta pestaña: "),
                "las franjas son los puntos críticos que encontró el KDE y las ",
                "marcas inferiores, las estructuras. La tabla dice qué estructuras ",
                "tiene cada punto crítico y a qué distancia queda la más cercana. ",
                "Abajo, la prueba compara cuántas estructuras caen dentro de los ",
                "puntos críticos con lo que daría el azar."),
            uiOutput(ns("estado_hotspots")),
            plotOutput(ns("plot_hotspots"), height = "260px"),
            br(),
            DTOutput(ns("tabla_hotspots")),
            uiOutput(ns("prueba_hotspots"))
          ),
          nav_panel(
            title = "Estructuras",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                "Estructuras con su posición en km, distancia original a la vía y ",
                "número de atropellos analizados a menos de 200 m. Las excluidas ",
                "(más lejos de la vía que la tolerancia) aparecen al final."),
            DTOutput(ns("tabla_estructuras"))
          )
        )
      )
    ),

    # ── 5. Código R ───────────────────────────────────────
    nav_panel(
      fillable = FALSE,
      title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
      card_body(
        card(
          fill = FALSE,
          card_header(bs_icon("code-slash", class = "me-1"), "Código reproducible"),
          card_body(verbatimTextOutput(ns("codigo_r")))
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────
# datos: reactive devuelto por mod_datos_red_server()
# nkde:  reactive devuelto por mod_nkde_server() (puntos críticos del KDE)
mod_estructuras_server <- function(id, datos, nkde = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ruta_est_ejemplo <- archivo_ejemplo("estructuras")

    output$plot_idea <- renderPlot(figura_idea_nulo())

    # ────────────────────────────────────────────────────
    # DATOS
    # ────────────────────────────────────────────────────
    est_ejemplo <- reactive({
      df <- utils::read.csv(ruta_est_ejemplo, fileEncoding = "UTF-8")
      tabla_a_sf(normalizar_estructuras(df), crs = 4326)
    })

    output$tabla_ejemplo <- renderDT({
      datatable(sf::st_drop_geometry(est_ejemplo()), rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 10))
    })

    output$descargar_est_ejemplo <- downloadHandler(
      filename = function() basename(ruta_est_ejemplo),
      content  = function(file) file.copy(ruta_est_ejemplo, file)
    )
    output$descargar_plantilla <- downloadHandler(
      filename = function() "plantilla_estructuras_StatRoad.csv",
      content  = function(file) {
        df <- utils::read.csv(ruta_est_ejemplo, fileEncoding = "UTF-8")
        utils::write.csv(utils::head(df[, c("id", "tipo", "x", "y")], 5), file,
                         row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    # El código EPSG escrito a mano se lee con una pausa, para no
    # releer el archivo con cada tecla.
    crs_coords_otro <- debounce(reactive(input$crs_coords_otro), 800)

    est_propias <- reactive({
      req(input$archivo_est)
      nombre <- input$archivo_est$name
      ruta   <- input$archivo_est$datapath
      ext    <- tolower(tools::file_ext(nombre))
      validar_lectura({
        if (ext %in% c("csv", "txt", "xlsx", "xls")) {
          df <- leer_tabla_atropellos(ruta, nombre, input$separador)
          tabla_a_sf(normalizar_estructuras(df),
                     crs = resolver_crs(input$crs_coords, crs_coords_otro(),
                                        metrico = FALSE))
        } else {
          capa <- leer_capa_espacial(ruta, nombre, tipo = "puntos")
          df   <- normalizar_estructuras(sf::st_drop_geometry(capa))
          sf::st_sf(df, geometry = sf::st_geometry(capa))
        }
      })
    })

    output$estado_propias <- renderUI({
      if (is.null(input$archivo_est)) {
        return(p(class = "small text-muted",
                 "Sube tu archivo para ver la vista previa. Mientras no subas nada, ",
                 "se usan las estructuras de ejemplo (solo con los atropellos de ejemplo)."))
      }
      e <- est_propias()
      p(class = "small", strong(nrow(e)), " estructuras leídas; tipos: ",
        paste(sort(unique(e$tipo)), collapse = ", "), ".")
    })

    output$tabla_propias <- renderDT({
      req(input$archivo_est)
      datatable(sf::st_drop_geometry(est_propias()), rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 10))
    })

    # Estructuras activas: las propias, o las de ejemplo SOLO si los
    # atropellos también son los de ejemplo (si no, no tendría sentido)
    est_activas <- reactive({
      if (!is.null(input$archivo_est)) return(est_propias())
      d <- datos()
      validate(need(is.null(d) || isTRUE(d$ejemplo),
                    paste("Estás usando tus propios atropellos: sube también tus",
                          "estructuras en \"Los datos\" → \"Mis estructuras\".")))
      est_ejemplo()
    })

    # ────────────────────────────────────────────────────
    # CONFIGURACIÓN
    # ────────────────────────────────────────────────────
    output$ui_grupo <- renderUI({
      d <- datos()
      grupos <- if (is.null(d)) character(0) else sort(unique(d$ajuste$ajustados$grupo))
      sel <- if ("Anfibios" %in% grupos && isTRUE(d$ejemplo)) "Anfibios" else "Todos"
      selectInput(ns("grupo"), "Grupo de atropellos",
                  choices = c("Todos", grupos), selected = sel)
    })

    output$ui_tipos <- renderUI({
      tipos <- sort(unique(est_activas()$tipo))
      checkboxGroupInput(ns("tipos"), "Tipos de estructura", choices = tipos,
                         selected = tipos)
    })

    resultado <- reactiveVal(NULL)
    observeEvent(list(datos(), input$archivo_est), resultado(NULL), ignoreInit = TRUE)

    observeEvent(input$analizar, {
      d <- datos()
      if (is.null(d)) {
        showNotification("Primero ajusta los atropellos en \"Datos y red vial\".",
                         type = "warning", duration = 8)
        return()
      }
      if (!isTRUE(d$red_prep$continua)) {
        showNotification(paste("La red no es una sola ruta continua: este análisis",
                               "necesita la posición en km."),
                         type = "error", duration = 10)
        return()
      }
      res <- tryCatch(
        withProgress(message = "Analizando…", value = 0, {
          analizar_estructuras(
            d, est_activas(), grupo = input$grupo, tipos = input$tipos,
            tolerancia = input$tolerancia, rmax = input$rmax,
            largo_seg = input$largo_seg, nsim = as.numeric(input$nsim),
            semilla = input$semilla,
            progreso = function(v, txt) setProgress(v, detail = txt)
          )
        }),
        error = function(e) {
          showNotification(paste("No se pudo analizar:", conditionMessage(e)),
                           type = "error", duration = 10)
          NULL
        })
      if (!is.null(res)) {
        res$ejemplo <- isTRUE(d$ejemplo) && is.null(input$archivo_est)
        res$archivo_est <- if (is.null(input$archivo_est)) NULL else input$archivo_est$name
      }
      resultado(res)
    })

    output$estado_analisis <- renderUI({
      r <- resultado()
      if (is.null(r)) {
        return(div(
          class = "alert alert-secondary small py-3 px-3",
          bs_icon("exclamation-circle", class = "me-1"),
          "Aún no se ha corrido el análisis.", tags$br(), tags$br(),
          "Requiere los atropellos ajustados en ", strong("\"Datos y red vial\""),
          ". Elige el grupo y los tipos de estructura y presiona ",
          strong("\"Analizar\""), ". Con 999 simulaciones tarda unos segundos."
        ))
      }
      tagList(
        div(class = "alert alert-info small py-2 px-3 mb-3",
            bs_icon("check-circle-fill", class = "me-1"),
            strong("Análisis completado. "),
            r$n_ev, " atropellos (", r$grupo, ") y ", r$n_est,
            " estructuras sobre la vía; ", nrow(r$est$excluidos), " excluidas."),
        if (r$n_ev < 20) {
          div(class = "alert alert-warning small py-2 px-3 mb-3",
              bs_icon("exclamation-triangle", class = "me-1"),
              "Hay pocos atropellos: las pruebas tienen poca potencia y un ",
              "resultado no significativo dice poco.")
        },
        actionButton(ns("ir_a_resultados"), "Ver resultados completos →",
                     class = "btn-outline-primary w-100")
      )
    })

    observeEvent(input$ir_a_resultados, {
      bslib::nav_select(id = "tabs_est", selected = "tab_resultados", session = session)
    })

    # ────────────────────────────────────────────────────
    # RESULTADOS
    # ────────────────────────────────────────────────────
    output$cards_resultados <- renderUI({
      r <- resultado()
      if (is.null(r)) {
        return(div(class = "alert alert-secondary small py-2 px-3",
                   bs_icon("exclamation-circle", class = "me-1"),
                   "Primero corre el análisis en \"Configurar análisis\"."))
      }
      layout_columns(
        col_widths = c(3, 3, 3, 3), fill = FALSE,
        tarjeta_valor(r$n_ev, paste0("Atropellos (", r$grupo, ")"), colores$primario),
        tarjeta_valor(r$n_est, "Estructuras sobre la vía", colores$secundario),
        tarjeta_valor(paste0(round(r$distancia$obs * 1000), " m"),
                      paste0("Distancia mediana (azar: ",
                             round(stats::median(r$distancia$sim) * 1000), " m)"),
                      colores$acento),
        tarjeta_valor(formatear_p(r$distancia$p),
                      "Valor p (distancia, dos colas)", colores$texto)
      )
    })

    output$mapa <- leaflet::renderLeaflet({
      r <- resultado()
      req(r)
      red <- sf::st_transform(datos()$red_prep$segmentos, 4326)
      ev  <- sf::st_transform(r$eventos, 4326)
      est <- sf::st_transform(r$est$ajustados, 4326)
      exc <- sf::st_transform(r$est$original[!r$est$todos$dentro, ], 4326)
      exc$dist_via_m <- r$est$excluidos$dist_via_m

      # Colores oscuros que contrastan con la vía (azul), los atropellos
      # (naranja) y las excluidas (rojo); con más tipos, se agrega Dark2
      tipos <- sort(unique(r$est$todos$tipo))
      base  <- c("#5B2C6F", "#117A65", "#7E5109", "#212F3D",
                 "#1B9E77", "#7570B3", "#E7298A", "#66A61E", "#A6761D", "#666666")
      pal_t <- leaflet::colorFactor(base[seq_along(tipos)], domain = tipos)

      m <- mapa_base() |>
        leaflet::addPolylines(data = red, color = colores$primario, weight = 4,
                              opacity = 0.8) |>
        leaflet::addCircleMarkers(
          data = ev, radius = 4, stroke = FALSE, fillOpacity = 0.85,
          fillColor = colores$acento,
          popup = ~paste0("<b><i>", especie, "</i></b><br>", grupo, "<br>", fecha,
                          "<br>km ", km, "<br>A ", round(dist_estructura_km * 1000),
                          " m de la estructura más cercana")) |>
        leaflet::addCircleMarkers(
          data = est, radius = 8, weight = 2, color = "white", fillOpacity = 1,
          fillColor = ~pal_t(tipo),
          popup = ~paste0("<b>", id, "</b> — ", tipo, "<br>km ", km,
                          "<br>Distancia a la vía: ", dist_via_m, " m"))
      if (nrow(exc) > 0) {
        m <- m |> leaflet::addCircleMarkers(
          data = exc, radius = 8, weight = 2, color = "white", fillOpacity = 1,
          fillColor = colores$peligro,
          popup = ~paste0("<b>", id, "</b> — ", tipo, " (excluida: a ",
                          round(dist_via_m), " m de la vía)"))
      }
      m |>
        leaflet::addLegend(
          position = "bottomright", opacity = 1,
          colors   = c(colores$primario, pal_t(tipos),
                       if (nrow(exc) > 0) colores$peligro, colores$acento),
          labels   = c("Red vial", tipos,
                       if (nrow(exc) > 0) "Estructura excluida", "Atropellos analizados"))
    })

    output$frase_distancia <- renderUI({
      r <- resultado(); req(r)
      frase_resultado(interpretar_distancia(r$distancia))
    })

    output$plot_distancia <- renderPlot({
      r <- resultado(); req(r)
      pd <- r$distancia
      ggplot(data.frame(sim = pd$sim * 1000), aes(x = sim)) +
        geom_histogram(bins = 40, fill = "grey75", color = "white") +
        geom_vline(xintercept = pd$obs * 1000, color = colores$peligro,
                   linewidth = 1.2) +
        annotate("label", x = pd$obs * 1000, y = Inf, vjust = 1.3,
                 label = paste0("Observada: ", round(pd$obs * 1000), " m"),
                 color = colores$peligro, size = 4.2) +
        labs(x = "Mediana de la distancia a la estructura más cercana (m)",
             y = "Repeticiones del azar",
             caption = paste0(pd$nsim, " desplazamientos circulares")) +
        theme_light(base_size = 13)
    })

    output$frase_k <- renderUI({
      r <- resultado(); req(r)
      frase_resultado(interpretar_k(r$k))
    })

    output$plot_k <- renderPlot({
      r <- resultado(); req(r)
      t <- r$k$tabla
      ggplot(t, aes(x = r)) +
        geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80") +
        geom_line(aes(y = central), linetype = "dashed", color = "grey40") +
        geom_hline(yintercept = 1, color = "grey60") +
        geom_line(aes(y = obs), color = colores$primario, linewidth = 1.1) +
        labs(x = "Radio r (km)", y = "Atropellos observados / esperados",
             caption = paste0("Banda: envolvente global al 95 % (",
                              r$k$nsim, " desplazamientos circulares)")) +
        theme_light(base_size = 13)
    })

    output$control_verdad <- renderUI({
      r <- resultado()
      req(r)
      if (!isTRUE(r$ejemplo)) return(NULL)
      checkboxInput(ns("mostrar_verdad"),
                    "Mostrar los puntos críticos sembrados en la simulación (H1–H6)",
                    value = FALSE, width = "100%")
    })

    output$plot_segmentos <- renderPlot({
      r <- resultado(); req(r)
      s <- r$modelo$segmentos
      # escalones: un punto extra en km_fin cierra el último segmento
      esc <- data.frame(km = c(s$km_ini, utils::tail(s$km_fin, 1)),
                        ajustado = c(s$ajustado, utils::tail(s$ajustado, 1)))
      g <- ggplot(s) +
        geom_rect(aes(xmin = km_ini, xmax = km_fin, ymin = 0, ymax = n),
                  fill = colores$acento, alpha = 0.6, color = "white") +
        geom_step(data = esc, aes(x = km, y = ajustado), direction = "hv",
                  color = colores$primario, linewidth = 1.1) +
        geom_rug(data = sf::st_drop_geometry(r$est$ajustados), aes(x = km),
                 sides = "b", length = grid::unit(0.05, "npc"),
                 color = colores$texto, linewidth = 1) +
        labs(x = "Posición a lo largo de la vía (km)", y = "Atropellos por segmento") +
        theme_light(base_size = 13)

      if (isTRUE(input$mostrar_verdad) && isTRUE(r$ejemplo)) {
        verdad <- utils::read.csv(archivo_ejemplo("verdad"),
                                  fileEncoding = "UTF-8")
        g <- g +
          geom_vline(data = verdad, aes(xintercept = centro_km), color = colores$peligro,
                     linewidth = 0.9, linetype = "dotdash") +
          geom_text(data = verdad, aes(x = centro_km, y = Inf, label = hotspot),
                    vjust = 1.5, hjust = -0.2, color = colores$peligro,
                    fontface = "bold")
      }
      g
    })

    output$tabla_modelo <- renderDT({
      r <- resultado(); req(r)
      c <- r$modelo$coef
      df <- data.frame(
        `Término` = c$termino,
        `Razón de tasas` = round(c$razon_tasas, 3),
        `IC 95 % (Wald)` = paste0(round(c$ic_inf, 3), " – ", round(c$ic_sup, 3)),
        `p (circular)` = formatear_p(c$p_circular),
        `p (Wald, optimista)` = formatear_p(c$p_wald),
        Modelo = r$modelo$familia,
        check.names = FALSE
      )
      datatable(df, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    output$nota_moran <- renderUI({
      r <- resultado(); req(r)
      mo <- r$modelo$moran
      if (is.null(mo)) return(NULL)
      div(class = "alert alert-secondary small mt-3 mb-0",
          bs_icon("info-circle", class = "me-1"),
          strong("Autocorrelación de los residuos: "),
          "I de Moran = ", round(mo$I, 2), " (p = ", formatear_p(mo$p), "). ",
          if (mo$p <= 0.05) {
            paste("Los segmentos vecinos se parecen más de lo que el modelo explica:",
                  "por eso el p de Wald es optimista y se reporta el p circular.")
          } else {
            "Sin autocorrelación detectable en los residuos."
          })
    })

    output$tabla_estructuras <- renderDT({
      r <- resultado(); req(r)
      df <- sf::st_drop_geometry(r$est$todos)
      df$atrop_200m <- ifelse(df$dentro,
                              atropellos_cerca(df$km, r$eventos$km, r$L, 0.2), NA)
      df <- df[order(!df$dentro, df$km), setdiff(names(df), c("x", "y"))]
      datatable(df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    # ────────────────────────────────────────────────────
    # HOTSPOTS (puntos críticos del KDE) RESPECTO A ESTRUCTURAS
    # ────────────────────────────────────────────────────
    kde_actual <- reactive({
      if (is.null(nkde)) return(NULL)
      k <- nkde()
      if (is.null(k)) NULL else k
    })

    hotspots <- reactive({
      r <- resultado()
      k <- kde_actual()
      if (is.null(r) || is.null(k) || is.null(k$tabla) || nrow(k$tabla) == 0) return(NULL)
      set.seed(r$semilla)
      e <- r$est$ajustados
      h <- hotspots_estructuras(k$tabla, e$km, e$id, e$tipo, r$L, nsim = 999)
      h$grupo_kde <- k$grupo
      h$h_kde     <- k$param$h
      h
    })

    output$estado_hotspots <- renderUI({
      req(resultado())
      k <- kde_actual()
      if (is.null(k)) {
        return(div(class = "alert alert-warning small py-2 px-3",
                   bs_icon("exclamation-triangle", class = "me-1"),
                   "Primero calcula los puntos críticos en ",
                   strong("\"Puntos críticos (KDE)\""), "."))
      }
      if (is.null(k$tabla) || nrow(k$tabla) == 0) {
        return(div(class = "alert alert-secondary small py-2 px-3",
                   bs_icon("dash-circle", class = "me-1"),
                   "El KDE no encontró puntos críticos: no hay nada que comparar."))
      }
      h <- hotspots()
      div(class = "alert alert-info small py-2 px-3 mb-3",
          bs_icon("info-circle", class = "me-1"),
          h$n_hotspots, " puntos críticos del KDE (registros: ", h$grupo_kde,
          "; ancho de banda ", h$h_kde, " m) y ", resultado()$n_est,
          " estructuras de los tipos elegidos.")
    })

    output$plot_hotspots <- renderPlot({
      h <- hotspots(); req(h)
      r <- resultado()
      t <- h$tabla
      ggplot() +
        geom_rect(data = t, aes(xmin = km_inicio, xmax = km_fin, ymin = 0, ymax = 1),
                  fill = colores$peligro, alpha = 0.35) +
        geom_text(data = t, aes(x = (km_inicio + km_fin) / 2, y = 1.12, label = hotspot),
                  color = colores$peligro, fontface = "bold", size = 4) +
        geom_segment(data = sf::st_drop_geometry(r$est$ajustados),
                     aes(x = km, xend = km, y = 0, yend = 0.35),
                     color = colores$texto, linewidth = 0.8) +
        scale_x_continuous("Posición a lo largo de la vía (km)", limits = c(0, r$L)) +
        scale_y_continuous(NULL, breaks = NULL, limits = c(0, 1.2)) +
        labs(caption = "Franjas: puntos críticos (KDE). Marcas: estructuras.") +
        theme_light(base_size = 13)
    })

    output$tabla_hotspots <- renderDT({
      h <- hotspots(); req(h)
      t <- h$tabla
      cols <- c("hotspot", "km_inicio", "km_fin",
                intersect("grupo_dominante", names(t)),
                "n_estructuras", "estructuras", "mas_cercana", "distancia_m")
      nombres <- c(hotspot = "Punto crítico", km_inicio = "km inicio", km_fin = "km fin",
                   grupo_dominante = "Grupo dominante", n_estructuras = "Estructuras dentro",
                   estructuras = "Cuáles", mas_cercana = "Más cercana",
                   distancia_m = "Distancia (m)")
      datatable(t[, cols], rownames = FALSE, colnames = unname(nombres[cols]),
                options = list(dom = "t", scrollX = TRUE, pageLength = 50))
    })

    output$prueba_hotspots <- renderUI({
      h <- hotspots(); req(h)
      sig <- h$p <= 0.05
      tagList(
        div(class = paste0("alert alert-", if (sig) "info" else "secondary",
                           " small py-2 px-3 mt-3 mb-2"),
            bs_icon(if (sig) "check-circle-fill" else "dash-circle", class = "me-1"),
            strong("Prueba: "), h$obs, " estructuras dentro de los puntos críticos; ",
            "por azar se esperarían ", round(h$esperado, 1), " (p = ",
            formatear_p(h$p), "). ", h$obs_con, " de ", h$n_hotspots,
            " puntos críticos tienen al menos una estructura (por azar: ",
            round(h$esp_con, 1), ").",
            if (sig) " Hay más estructuras en los puntos críticos de lo esperado." else
              " No hay evidencia de que haya más estructuras en los puntos críticos de lo esperado."),
        div(class = "alert alert-warning small py-2 px-3 mb-0",
            bs_icon("exclamation-triangle", class = "me-1"),
            "Con una estructura cada ", round(h$km_por_est, 1), " km y puntos críticos ",
            "que cubren el ", round(100 * h$fraccion), " % de la ruta, por azar ya se ",
            "esperan ", round(h$esperado, 1), " estructuras dentro. ",
            if (!sig) "Un resultado no significativo no descarta la asociación: con ",
            if (!sig) paste0(h$n_hotspots, " puntos críticos la prueba tiene poca potencia. "),
            "Para probar la asociación, usa las pruebas con atropellos individuales ",
            "(pestañas Distancia, Escala y Modelo).")
      )
    })

    # ────────────────────────────────────────────────────
    # DESCARGAS
    # ────────────────────────────────────────────────────
    hotspots_o_null <- function() tryCatch(hotspots(), error = function(e) NULL)

    output$descargar_gpkg <- downloadHandler(
      filename = function() "estructuras_atropellos_StatRoad.gpkg",
      content  = function(file) {
        r <- resultado()
        validate(need(r, "Primero corre el análisis."))
        escribir_gpkg(capas_estructuras(r, datos()$red_prep$linea, hotspots_o_null()),
                      file, parametros_estructuras(r))
      }
    )

    output$descargar_resumen <- downloadHandler(
      filename = function() "estructuras_resumen_pruebas_StatRoad.csv",
      content  = function(file) {
        r <- resultado()
        validate(need(r, "Primero corre el análisis."))
        utils::write.csv(resumen_pruebas(r, hotspots_o_null()), file,
                         row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    # ────────────────────────────────────────────────────
    # CÓDIGO R REPRODUCIBLE
    # ────────────────────────────────────────────────────
    output$codigo_r <- renderText({
      r <- resultado()
      if (is.null(r)) {
        return("# Corre el análisis en \"Configurar análisis\" para generar el código.")
      }
      propio <- !is.null(r$archivo_est)
      codigo_estructuras(r,
                         nombre_est = if (propio) r$archivo_est else
                           basename(archivo_ejemplo("estructuras")),
                         crs_coords = if (propio) tryCatch(
                           resolver_crs(input$crs_coords, input$crs_coords_otro,
                                        metrico = FALSE),
                           error = function(e) NA) else 4326)
    })
  })
}

# ── Cálculo completo (sin Shiny) ─────────────────────────
analizar_estructuras <- function(d, est_sf, grupo = "Todos", tipos = NULL,
                                 tolerancia = 50, rmax = 2, largo_seg = 0.5,
                                 nsim = 999, semilla = 2026,
                                 progreso = function(v, txt) NULL) {
  set.seed(semilla)
  L  <- as.numeric(sf::st_length(d$red_prep$linea)) / 1000
  ev <- d$ajuste$ajustados
  if (!identical(grupo, "Todos")) ev <- ev[ev$grupo == grupo, ]
  if (nrow(ev) < 2) stop("Hay menos de 2 atropellos en el grupo elegido.", call. = FALSE)

  progreso(0.05, "ajustando estructuras a la vía")
  est <- ajustar_a_red(est_sf, d$red_prep, tolerancia)
  if (!is.null(tipos)) {
    keep <- est$todos$tipo %in% tipos
    est$original <- est$original[keep, ]
    est$todos    <- est$todos[keep, ]
    est$ajustados <- est$todos[est$todos$dentro, ]
    est$excluidos <- est$todos[!est$todos$dentro, ]
  }
  if (nrow(est$ajustados) == 0) {
    stop("Ninguna estructura de los tipos elegidos quedó sobre la vía.", call. = FALSE)
  }
  km_est <- est$ajustados$km
  ev$dist_estructura_km <- round(dist_mas_cercana(ev$km, km_est, L), 3)

  progreso(0.15, "distancia a la estructura más cercana")
  pd <- prueba_distancia(ev$km, km_est, L, nsim = nsim)
  pd$p <- min(1, 2 * min(pd$p_atraccion, pd$p_repulsion))

  progreso(0.40, "K cruzada")
  pk <- prueba_k_cruzada(ev$km, km_est, L, rmax = min(rmax, L / 2 - 0.1), nsim = nsim)

  progreso(0.70, "modelo por segmentos")
  pm <- modelo_segmentos(ev$km, km_est, L, largo_km = largo_seg,
                         nsim = min(nsim, 499))

  progreso(1, "listo")
  list(L = L, grupo = grupo, tipos = tipos, tolerancia = tolerancia, rmax = rmax,
       largo_seg = largo_seg, nsim = nsim, semilla = semilla, crs = d$crs,
       eventos = ev, n_ev = nrow(ev), est = est, n_est = length(km_est),
       distancia = pd, k = pk, modelo = pm)
}

# Atropellos (km_ev) a menos de r km de cada estructura (métrica circular)
atropellos_cerca <- function(km_est, km_ev, L, r) {
  vapply(km_est, function(k) {
    d <- abs(km_ev - k)
    sum(pmin(d, L - d) <= r)
  }, numeric(1))
}

# ── Interpretación automática ────────────────────────────
formatear_p <- function(p) {
  if (is.null(p) || is.na(p)) return("—")
  if (p < 0.001) "< 0.001" else format(round(p, 3), nsmall = 3)
}

interpretar_distancia <- function(pd) {
  obs <- round(pd$obs * 1000); esp <- round(stats::median(pd$sim) * 1000)
  if (pd$p_atraccion <= 0.025) {
    list(tipo = "info", texto = paste0(
      "Los atropellos están más cerca de las estructuras de lo esperado por azar: ",
      "mediana de ", obs, " m frente a ", esp, " m (p = ", formatear_p(pd$p), ")."))
  } else if (pd$p_repulsion <= 0.025) {
    list(tipo = "info", texto = paste0(
      "Los atropellos están más lejos de las estructuras de lo esperado por azar: ",
      "mediana de ", obs, " m frente a ", esp, " m (p = ", formatear_p(pd$p), ")."))
  } else {
    list(tipo = "secondary", texto = paste0(
      "No hay evidencia de que los atropellos estén más cerca o más lejos de las ",
      "estructuras de lo esperado por azar: mediana de ", obs, " m frente a ", esp,
      " m (p = ", formatear_p(pd$p), ")."))
  }
}

interpretar_k <- function(pk) {
  t <- pk$tabla
  # tramos contiguos de r donde la curva sale de la envolvente
  tramos <- function(fuera) {
    rl  <- rle(fuera); fin <- cumsum(rl$lengths); ini <- fin - rl$lengths + 1
    txt <- mapply(function(a, b) if (a == b) paste(t$r[a], "km") else
      paste0(t$r[a], "–", t$r[b], " km"), ini[rl$values], fin[rl$values])
    paste(txt, collapse = ", ")
  }
  arriba <- t$obs > t$hi; abajo <- t$obs < t$lo
  if (pk$p > 0.05) {
    return(list(tipo = "secondary", texto = paste0(
      "La curva no sale de la envolvente global: no hay evidencia de asociación a ",
      "ninguna escala (p = ", formatear_p(pk$p), ").")))
  }
  partes <- c(
    if (any(arriba)) paste0("más atropellos de lo esperado cerca de las estructuras para r = ",
                            tramos(arriba)),
    if (any(abajo)) paste0("menos de lo esperado para r = ", tramos(abajo)))
  if (length(partes) == 0) partes <- "la diferencia se reparte a lo largo de todas las escalas"
  list(tipo = "info", texto = paste0(
    "Asociación significativa (p = ", formatear_p(pk$p), "): ",
    paste(partes, collapse = "; "), "."))
}

frase_resultado <- function(x) {
  div(class = paste0("alert alert-", x$tipo, " small py-2 px-3 mb-3"),
      bs_icon(if (x$tipo == "info") "check-circle-fill" else "dash-circle",
              class = "me-1"),
      x$texto)
}

# ── Figura didáctica: los dos modelos nulos ──────────────
figura_idea_nulo <- function() {
  set.seed(4)
  L   <- 10
  est <- c(2, 2.4, 6.5)
  obs <- c(stats::rnorm(9, 2.2, 0.25), stats::rnorm(6, 6.6, 0.2), stats::runif(5, 0, L))
  obs <- pmin(pmax(obs, 0.05), L - 0.05)
  filas <- c("Datos observados", "Desplazamiento circular", "Reubicación uniforme")
  ev <- rbind(
    data.frame(fila = filas[1], km = obs),
    data.frame(fila = filas[2], km = (obs + 3.7) %% L),
    data.frame(fila = filas[3], km = stats::runif(length(obs), 0, L)))
  ev$fila <- factor(ev$fila, levels = rev(filas))
  es <- expand.grid(km = est, fila = factor(filas, levels = rev(filas)))
  flecha <- data.frame(fila = factor(filas[2], levels = rev(filas)))

  ggplot() +
    geom_segment(data = data.frame(fila = factor(filas, levels = rev(filas))),
                 aes(x = 0, xend = L, y = fila, yend = fila),
                 color = "grey70", linewidth = 3, lineend = "round") +
    geom_point(data = es, aes(x = km, y = fila), shape = 17, size = 5,
               color = colores$texto, position = position_nudge(y = 0.22)) +
    geom_point(data = ev, aes(x = km, y = fila), color = colores$acento,
               size = 3, alpha = 0.85) +
    geom_segment(data = flecha, aes(x = 2.2, xend = 5.9, y = fila, yend = fila),
                 position = position_nudge(y = -0.3), color = colores$primario,
                 arrow = grid::arrow(length = grid::unit(0.2, "cm"))) +
    scale_x_continuous("Posición a lo largo de la vía (km)", limits = c(0, L)) +
    labs(y = NULL, caption = "Triángulos: estructuras (fijas). Puntos: atropellos.") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          axis.text.y = element_text(face = "bold"))
}

# ── Exportación ──────────────────────────────────────────
capas_estructuras <- function(r, linea, hot = NULL) {
  limpiar <- function(x) {
    nm <- names(x)
    nm[nm == "x"] <- "x_original"; nm[nm == "y"] <- "y_original"
    names(x) <- nm
    x[, setdiff(names(x), "dentro")]
  }
  est <- r$est$ajustados
  est$atrop_200m <- atropellos_cerca(est$km, r$eventos$km, r$L, 0.2)
  exc <- r$est$original[!r$est$todos$dentro, ]
  exc$dist_via_m <- r$est$excluidos$dist_via_m

  capas <- list(
    estructuras_ajustadas = limpiar(est),
    atropellos_analizados = limpiar(r$eventos),
    segmentos_modelo      = segmentos_sf(linea, r$modelo$segmentos),
    curva_k               = r$k$tabla,
    distribucion_nula     = data.frame(mediana_sim_km = r$distancia$sim),
    resumen_pruebas       = resumen_pruebas(r, hot)
  )
  if (nrow(exc) > 0) capas$estructuras_excluidas <- limpiar(exc)
  if (!is.null(hot)) capas$hotspots_estructuras <- hot$tabla
  capas
}

parametros_estructuras <- function(r) {
  list(modulo = "Estructuras y atropellos", crs_epsg = r$crs,
       grupo = r$grupo, tipos = paste(r$tipos, collapse = "; "),
       tolerancia_estructuras_m = r$tolerancia, modelo_nulo = "desplazamiento circular",
       nsim = r$nsim, nsim_glm = min(r$nsim, 499), rmax_km = r$rmax,
       largo_segmento_km = r$largo_seg, semilla = r$semilla,
       longitud_ruta_km = round(r$L, 3))
}

resumen_pruebas <- function(r, hot = NULL) {
  c <- r$modelo$coef
  res <- data.frame(
    prueba = c("Distancia a la estructura más cercana",
               "K cruzada (envolvente global)",
               "Modelo por segmentos (razón de tasas por km)"),
    estadistico = c(paste0("mediana = ", round(r$distancia$obs, 3), " km (azar: ",
                           round(stats::median(r$distancia$sim), 3), " km)"),
                    paste0("rmax = ", r$rmax, " km"),
                    paste0("RR = ", round(c$razon_tasas, 3), " [",
                           round(c$ic_inf, 3), ", ", round(c$ic_sup, 3), "]")),
    p = c(r$distancia$p, r$k$p, c$p_circular),
    grupo = r$grupo, n_atropellos = r$n_ev, n_estructuras = r$n_est
  )
  if (!is.null(hot)) {
    res <- rbind(res, data.frame(
      prueba = "Estructuras dentro de los puntos críticos del KDE",
      estadistico = paste0(hot$obs, " dentro (azar: ", round(hot$esperado, 1),
                           "); ", hot$n_hotspots, " puntos críticos"),
      p = hot$p, grupo = paste0(r$grupo, " (KDE: ", hot$grupo_kde, ")"),
      n_atropellos = r$n_ev, n_estructuras = r$n_est))
  }
  res
}

# ── Generador del código R reproducible ──────────────────
codigo_estructuras <- function(r, nombre_est, crs_coords = 4326) {
  paste0(
    encabezado_script("StatRoad", "Atropellos y estructuras de la vía"),
    "# Requiere: 'red' (líneas) y 'atropellos' ya ajustados a la vía con su\n",
    "# columna km (ver el código del módulo \"Datos y red vial\").\n",
    "library(sf); library(MASS); library(GET)\n",
    "set.seed(", r$semilla, ")\n\n",
    "# ── 1. Posición en km a lo largo de la ruta ──\n",
    "linea <- st_line_merge(st_union(st_geometry(red)))\n",
    "# km 0 en el extremo sur (vías norte-sur) u oeste (este-oeste), como en StatRoad\n",
    "xy <- st_coordinates(linea)[, 1:2]; dd <- xy[nrow(xy), ] - xy[1, ]\n",
    "if (if (abs(dd[2]) >= abs(dd[1])) dd[2] < 0 else dd[1] < 0) linea <- st_reverse(linea)\n",
    "km_en_linea <- function(linea, pts) {\n",
    "  L <- st_coordinates(linea)[, 1:2]; A <- L[-nrow(L), , drop = FALSE]\n",
    "  AB <- L[-1, , drop = FALSE] - A; len <- sqrt(rowSums(AB^2))\n",
    "  acum <- c(0, cumsum(len))[seq_len(nrow(A))]; P <- st_coordinates(pts)[, 1:2]\n",
    "  sapply(seq_len(nrow(P)), function(i) {\n",
    "    t <- pmin(pmax(((P[i,1]-A[,1])*AB[,1] + (P[i,2]-A[,2])*AB[,2]) / len^2, 0), 1)\n",
    "    j <- which.min((P[i,1]-A[,1]-AB[,1]*t)^2 + (P[i,2]-A[,2]-AB[,2]*t)^2)\n",
    "    (acum[j] + t[j] * len[j]) / 1000 })\n",
    "}\n",
    "\n",
    "# ── 2. Estructuras: leer, ajustar a la vía (", r$tolerancia, " m) ──\n",
    "est <- st_as_sf(read.csv(\"", nombre_est, "\"), coords = c(\"x\", \"y\"), crs = ",
    crs_coords, ")\n",
    "est <- st_transform(est, st_crs(red))\n",
    if (!is.null(r$tipos)) paste0("est <- est[est$tipo %in% c(\"",
                                  paste(r$tipos, collapse = "\", \""), "\"), ]\n"),
    "conex <- st_nearest_points(st_geometry(est),\n",
    "  st_geometry(red)[st_nearest_feature(est, red)], pairwise = TRUE)\n",
    "est <- est[as.numeric(st_length(conex)) <= ", r$tolerancia, ", ]\n",
    "km_est <- km_en_linea(linea, est)\n",
    if (!identical(r$grupo, "Todos")) paste0(
      "atropellos <- atropellos[atropellos$grupo == \"", r$grupo, "\", ]\n"),
    "km_ev <- atropellos$km\n",
    "L <- as.numeric(st_length(linea)) / 1000\n\n",
    "# Distancias sobre el anillo de largo L: hace exacta la prueba circular\n",
    "dist_min <- function(km, est) sapply(km, function(k) {\n",
    "  d <- abs(k - est); min(pmin(d, L - d)) })\n",
    "desplazar <- function(km) (km + runif(1, 0, L)) %% L\n\n",
    "# ── 3. Distancia a la estructura más cercana ──\n",
    "obs <- median(dist_min(km_ev, km_est))\n",
    "sim <- replicate(", r$nsim, ", median(dist_min(desplazar(km_ev), km_est)))\n",
    "p <- 2 * min(mean(c(sim, obs) <= obs), mean(c(sim, obs) >= obs))\n",
    "c(mediana_obs = obs, mediana_azar = median(sim), p = min(1, p))\n\n",
    "# ── 4. K cruzada 1D (observado / esperado) + envolvente global ──\n",
    "r <- seq(0.05, ", min(r$rmax, round(r$L / 2 - 0.1, 2)), ", by = 0.05)\n",
    "oe <- function(km) sapply(r, function(rr) {\n",
    "  n <- sum(sapply(km_est, function(k) { d <- abs(km - k); sum(pmin(d, L - d) <= rr) }))\n",
    "  n / (length(km) / L * 2 * rr * length(km_est)) })\n",
    "cs <- create_curve_set(list(r = r, obs = oe(km_ev),\n",
    "  sim_m = replicate(", r$nsim, ", oe(desplazar(km_ev)))))\n",
    "res_k <- global_envelope_test(cs, type = \"area\")\n",
    "plot(res_k); attr(res_k, \"p\")\n\n",
    "# ── 5. Modelo por segmentos de ", r$largo_seg, " km ──\n",
    "br <- c(seq(0, by = ", r$largo_seg, ", length.out = max(1, floor(L / ",
    r$largo_seg, "))), L)\n",
    "seg <- data.frame(km_ini = head(br, -1), km_fin = br[-1])\n",
    "seg$largo <- seg$km_fin - seg$km_ini\n",
    "seg$dist  <- dist_min((seg$km_ini + seg$km_fin) / 2, km_est)\n",
    "contar <- function(km) as.vector(table(cut(km, br, include.lowest = TRUE, right = FALSE)))\n",
    "seg$n <- contar(km_ev)\n",
    "m <- glm.nb(n ~ dist + offset(log(largo)), data = seg)\n",
    "exp(cbind(RR = coef(m), confint.default(m)))[\"dist\", ]  # descriptivo\n",
    "# p por desplazamiento circular (el p de Wald es optimista)\n",
    "b_sim <- replicate(", min(r$nsim, 499), ", { s2 <- seg; s2$n <- contar(desplazar(km_ev))\n",
    "  coef(glm.nb(n ~ dist + offset(log(largo)), data = s2))[\"dist\"] })\n",
    "b <- coef(m)[\"dist\"]\n",
    "min(1, 2 * min(mean(c(b_sim, b) <= b), mean(c(b_sim, b) >= b)))\n"
  )
}
