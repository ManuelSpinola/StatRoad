# ============================================================
# mod_datos_red.R — Datos, red vial y ajuste de registros a la vía
# StatRoad · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Datos de ejemplo: red real de la Ruta 1 (zona PN Santa Rosa, OSM)
# con atropellos SIMULADOS (ver data-raw/). Mis datos: red vial
# (GPKG, GeoJSON, Shapefile .zip) + atropellos (CSV, Excel o capa
# de puntos). Funciones de lectura y ajuste en utils_espacial.R.
#
# Devuelve un reactive con el resultado del ajuste, para que lo
# usen los demás módulos (exploración, escala, hotspots).
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_datos_red_ui <- function(id) {
  ns <- NS(id)

  navset_card_tab(
    id = ns("tabs_datos"),

    # ── 1. ¿Qué es? ───────────────────────────────────────
    nav_panel(
      fillable = FALSE,
      title = tagList(bs_icon("book", class = "me-1"), "¿Qué es?"),
      card_body(
        div(
          class = "px-1 pb-2",
          style = "max-width: 780px; margin: 0 auto;",
          h5("Datos de atropellos sobre una red vial",
             style = paste0("color:", colores$primario, "; font-weight:700;")),
          p(
            "Todo análisis de atropellos de fauna combina dos capas: la ",
            strong("red vial"), " (las líneas de las carreteras monitoreadas) y los ",
            strong("registros de atropello"), " (puntos con especie, grupo y fecha). ",
            "Los métodos de StatRoad — K de Ripley en red, KDE en red, Getis-Ord Gi* — ",
            "trabajan ", em("sobre la red"), ": las distancias se miden a lo largo ",
            "de la carretera, no en línea recta."
          ),
          card(
            fill = FALSE,
            class = "mt-3",
            card_header(bs_icon("pin-map", class = "me-1"),
                        "Ajuste de registros a la vía"),
            card_body(
              p(class = "small",
                "Los registros casi nunca caen exactamente sobre la línea de la ",
                "carretera: el GPS tiene error y el animal puede quedar en el ",
                "espaldón o la cuneta. Antes de analizar, cada registro se ",
                strong("mueve al punto más cercano de la vía"),
                " (en inglés, ", em("snapping"), ")."),
              tags$ul(
                class = "small",
                tags$li(strong("Distancia a la vía:"), " cuánto se movió cada registro."),
                tags$li(strong("Tolerancia:"), " distancia máxima aceptada. Los registros ",
                        "más lejanos se ", strong("excluyen"), " y se listan aparte: suelen ser ",
                        "errores de digitación, de GPS o registros de otra vía."),
                tags$li(strong("Posición en km:"), " si la red es una sola ruta continua, ",
                        "cada registro recibe su kilómetro a lo largo de la vía ",
                        "(km 0 en el extremo sur, o en el oeste si la vía corre este-oeste).")
              ),
              p(class = "small mb-0",
                bs_icon("info-circle", class = "me-1"),
                "Una tolerancia de 30–50 m suele cubrir el error de un GPS de mano ",
                "y el ancho de la vía con sus espaldones. Revisa el histograma de ",
                "distancias en ", strong("\"Resultados\""), " antes de decidir.")
            )
          ),
          card(
            fill = FALSE,
            class = "mt-3",
            card_header(bs_icon("globe-americas", class = "me-1"),
                        "Sistema de coordenadas"),
            card_body(
              p(class = "small mb-0",
                "Las distancias solo tienen sentido en un sistema ", strong("métrico"),
                ". StatRoad reproyecta todo a ", strong("CRTM05"),
                ", la proyección oficial de Costa Rica. Los mapas se muestran en ",
                "longitud/latitud solo para dibujarlos; los cálculos siempre se hacen en metros.")
            )
          ),
          div(
            class = "alert alert-warning small mt-3 mb-0",
            bs_icon("exclamation-triangle", class = "me-1"),
            strong("Importante: el esfuerzo de muestreo. "),
            "Los métodos suponen que toda la red se recorrió con el mismo esfuerzo ",
            "(muestreo sistemático). Con registros oportunistas — reportados al pasar — ",
            "un \"punto crítico\" puede reflejar dónde transita más gente, no dónde ",
            "mueren más animales. Incluye en la red solo los tramos monitoreados."
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
                            "Datos de ejemplo"),
            br(),
            layout_columns(
              col_widths = c(4, 8),
              fill = FALSE,
              div(
                div(
                  class = "alert alert-info small py-2 px-3 mb-3",
                  bs_icon("info-circle-fill", class = "me-1"),
                  strong("Ruta 1 (Interamericana Norte), zona del PN Santa Rosa, ",
                         "Área de Conservación Guanacaste."),
                  tags$br(), tags$br(),
                  strong("Red vial real"), " (~19 km) descargada de OpenStreetMap ",
                  "(© colaboradores de OpenStreetMap, ODbL).",
                  tags$br(), tags$br(),
                  strong("Atropellos simulados"), " (280 registros, 2024–2025) con ",
                  "características conocidas: 3 puntos críticos de ~1 km (dominados por ",
                  "anfibios, mamíferos y reptiles), un pulso de anfibios al inicio de ",
                  "las lluvias, error de GPS de 5–30 m y 6 registros a más de 100 m de ",
                  "la vía. Las ubicaciones de los puntos críticos son arbitrarias."
                ),
                downloadButton(ns("descargar_red_ejemplo"),
                               "Descargar red de ejemplo (.gpkg)",
                               class = "btn-outline-primary btn-sm w-100 mb-2"),
                downloadButton(ns("descargar_atrop_ejemplo"),
                               "Descargar atropellos de ejemplo (.csv)",
                               class = "btn-outline-primary btn-sm w-100")
              ),
              card(
                fill = FALSE,
                card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                card_body(
                  style = "overflow: auto;",
                  uiOutput(ns("cards_ejemplo")),
                  br(),
                  DTOutput(ns("tabla_ejemplo"))
                )
              )
            )
          ),

          nav_panel(
            fillable = FALSE,
            title = tagList(bs_icon("folder2-open", class = "me-1"),
                            "Mis datos"),
            br(),
            layout_columns(
              col_widths = c(5, 7),
              fill = FALSE,
              div(
                card(
                  fill = FALSE,
                  card_header(bs_icon("sign-turn-right", class = "me-1"),
                              "1. Red vial"),
                  card_body(
                    p(class = "small mb-2",
                      "Líneas de los tramos monitoreados, con su sistema de ",
                      "coordenadas definido. Formatos: ", strong("GPKG"), ", ",
                      strong("GeoJSON"), " o ", strong("Shapefile comprimido en .zip"),
                      " (con sus archivos .shp, .shx, .dbf y .prj)."),
                    fileInput(
                      ns("archivo_red"),
                      label       = NULL,
                      accept      = c(".gpkg", ".geojson", ".json", ".zip"),
                      buttonLabel = "Buscar…",
                      placeholder = "GPKG, GeoJSON o .zip"
                    )
                  )
                ),
                card(
                  fill = FALSE,
                  class = "mt-3",
                  card_header(bs_icon("geo-alt", class = "me-1"),
                              "2. Registros de atropello"),
                  card_body(
                    p(class = "small mb-2",
                      strong("Tabla"), " (CSV o Excel) con columnas ",
                      code("especie"), ", ", code("grupo"), ", ", code("fecha"),
                      ", ", code("x"), ", ", code("y"), " (", code("id"),
                      " opcional), o ", strong("capa de puntos"),
                      " (GPKG, GeoJSON o .zip) con las mismas columnas de atributos."),
                    fileInput(
                      ns("archivo_atrop"),
                      label       = NULL,
                      accept      = c(".csv", ".xlsx", ".xls",
                                      ".gpkg", ".geojson", ".json", ".zip"),
                      buttonLabel = "Buscar…",
                      placeholder = "CSV, Excel, GPKG…"
                    ),
                    p(class = "small text-muted mb-2",
                      bs_icon("info-circle", class = "me-1"),
                      "Solo para tablas (CSV o Excel):"),
                    selectInput(ns("crs_coords"), "Las coordenadas x/y están en:",
                                choices = crs_coordenadas, selected = 4326),
                    selectInput(ns("separador"), "Separador (solo CSV):",
                                choices = c("Coma (,)" = ",",
                                            "Punto y coma (;)" = ";",
                                            "Tabulador" = "\t"),
                                selected = ","),
                    downloadButton(ns("descargar_plantilla"),
                                   "Descargar plantilla (.csv)",
                                   class = "btn-outline-primary btn-sm w-100")
                  )
                )
              ),
              div(
                card(
                  fill = FALSE,
                  card_header(bs_icon("table", class = "me-1"),
                              "Así se debe ver tu tabla de atropellos"),
                  card_body(
                    tags$table(
                      class = "table table-sm table-bordered mb-2",
                      tags$thead(tags$tr(
                        tags$th("id"), tags$th("especie"), tags$th("grupo"),
                        tags$th("fecha"), tags$th("x"), tags$th("y")
                      )),
                      tags$tbody(
                        tags$tr(tags$td("1"), tags$td(em("Ctenosaura similis")),
                                tags$td("Reptiles"), tags$td("2025-03-14"),
                                tags$td("-85.6051"), tags$td("10.9208")),
                        tags$tr(tags$td("2"), tags$td(em("Incilius luetkenii")),
                                tags$td("Anfibios"), tags$td("2025-05-22"),
                                tags$td("-85.5644"), tags$td("10.8427")),
                        tags$tr(tags$td(em("...")), tags$td(em("...")), tags$td(em("...")),
                                tags$td(em("...")), tags$td(em("...")), tags$td(em("...")))
                      )
                    ),
                    tags$ul(
                      class = "small text-muted mb-0",
                      tags$li(code("fecha"), " en formato AAAA-MM-DD."),
                      tags$li(code("x"), " = longitud y ", code("y"),
                              " = latitud si usas WGS84; en CRTM05, este y norte en metros."),
                      tags$li(code("grupo"), " = categoría que quieras comparar ",
                              "(Mamíferos, Reptiles, Anfibios, Aves…).")
                    )
                  )
                ),
                br(),
                card(
                  fill = FALSE,
                  card_header(bs_icon("eye", class = "me-1"),
                              "Vista previa de tus datos"),
                  card_body(
                    style = "overflow: auto;",
                    uiOutput(ns("estado_propios")),
                    DTOutput(ns("tabla_propios"))
                  )
                )
              )
            )
          )
        )
      )
    ),

    # ── 3. Exploración ────────────────────────────────────
    nav_panel(
      fillable = FALSE,
      title = tagList(bs_icon("zoom-in", class = "me-1"), "Exploración"),
      card_body(
        uiOutput(ns("origen_datos")),
        uiOutput(ns("cards_exploracion")),
        br(),
        card(
          fill = FALSE,
          card_header(bs_icon("map", class = "me-1"),
                      "Red vial y registros originales"),
          card_body(leaflet::leafletOutput(ns("mapa_exploracion"), height = "480px"))
        ),
        layout_columns(
          col_widths = c(5, 7),
          fill = FALSE,
          card(
            fill = FALSE,
            card_header(bs_icon("bar-chart", class = "me-1"), "Registros por grupo"),
            card_body(plotOutput(ns("plot_grupos"), height = "320px"))
          ),
          card(
            fill = FALSE,
            card_header(bs_icon("calendar3", class = "me-1"), "Registros por mes"),
            card_body(plotOutput(ns("plot_meses"), height = "320px"))
          )
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
              selectInput(ns("crs_metrico"), "Sistema de coordenadas métrico",
                          choices = crs_metricos, selected = 8908),
              p(class = "small text-muted mt-n2 mb-3",
                "Todo se reproyecta a este sistema antes de medir distancias. ",
                "Usa el mismo que usan tus datos o tu institución."),
              sliderInput(ns("tolerancia"), "Tolerancia (m)",
                          min = 10, max = 300, value = 50, step = 5),
              p(class = "small text-muted mt-n2 mb-3",
                "Distancia máxima entre un registro y la vía. Los registros más ",
                "lejanos se excluyen del análisis y se listan en \"Resultados\"."),
              actionButton(ns("ajustar"), "Ajustar registros a la vía",
                           class = "btn-primary w-100 mt-2",
                           icon = icon("play"))
            )
          ),
          div(uiOutput(ns("estado_ajuste")))
        )
      )
    ),

    # ── 5. Resultados ─────────────────────────────────────
    nav_panel(
      value = "tab_resultados",
      fillable = FALSE,
      title = tagList(bs_icon("graph-up-arrow", class = "me-1"), "Resultados"),
      card_body(
        uiOutput(ns("cards_resultados")),
        br(),
        navset_pill(
          nav_panel(
            title = "Mapa",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                strong("Cómo leer este mapa: "),
                "los puntos ", strong("naranja"), " son los registros ajustados a la ",
                "vía (ya sobre la línea); los ", strong("rojos"),
                " son los excluidos, en su posición original. Haz clic en un ",
                "punto para ver su especie, fecha y distancia a la vía."),
            leaflet::leafletOutput(ns("mapa_resultados"), height = "520px")
          ),
          nav_panel(
            title = "Distancias a la vía",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                strong("Cómo leer este gráfico: "),
                "cada barra cuenta registros según cuánto se movieron para ",
                "quedar sobre la vía. La línea punteada es la tolerancia. ",
                "Lo esperable es un grupo grande cerca de cero (error normal de GPS) ",
                "y unos pocos registros aislados lejos: esos son los candidatos a ",
                "error. Si la línea corta por la mitad del grupo principal, la ",
                "tolerancia es demasiado estricta."),
            plotOutput(ns("plot_distancias"), height = "400px")
          ),
          nav_panel(
            title = "Registros excluidos",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                "Registros que quedaron más lejos de la vía que la tolerancia. ",
                "Revisa sus coordenadas en tus datos originales: si son errores ",
                "de digitación, corrígelos y vuelve a subir el archivo."),
            DTOutput(ns("tabla_excluidos"))
          ),
          nav_panel(
            title = "Registros ajustados",
            div(class = "alert alert-secondary small mt-3 mb-3",
                bs_icon("lightbulb", class = "me-1"),
                "Registros que se usarán en los análisis siguientes, con su ",
                "distancia original a la vía y su posición en km."),
            downloadButton(ns("descargar_ajustados"),
                           "Descargar registros ajustados (.csv)",
                           class = "btn-outline-primary btn-sm mb-3"),
            DTOutput(ns("tabla_ajustados"))
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
mod_datos_red_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ruta_red_ejemplo   <- app_sys("extdata", "red_ruta1_santarosa.gpkg")
    ruta_atrop_ejemplo <- app_sys("extdata", "atropellos_simulados_santarosa.csv")

    # ────────────────────────────────────────────────────
    # DATOS DE EJEMPLO
    # ────────────────────────────────────────────────────
    red_ejemplo <- reactive({
      sf::st_read(ruta_red_ejemplo, quiet = TRUE)
    })

    atrop_ejemplo <- reactive({
      df <- utils::read.csv(ruta_atrop_ejemplo, fileEncoding = "UTF-8")
      tabla_a_sf(normalizar_atropellos(df), crs = 4326)
    })

    output$cards_ejemplo <- renderUI({
      tarjetas_resumen(atrop_ejemplo(), red_ejemplo())
    })

    output$tabla_ejemplo <- renderDT({
      datatable(sf::st_drop_geometry(atrop_ejemplo()),
                rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 10))
    })

    output$descargar_red_ejemplo <- downloadHandler(
      filename = function() "red_ruta1_santarosa.gpkg",
      content  = function(file) file.copy(ruta_red_ejemplo, file)
    )
    output$descargar_atrop_ejemplo <- downloadHandler(
      filename = function() "atropellos_simulados_santarosa.csv",
      content  = function(file) file.copy(ruta_atrop_ejemplo, file)
    )
    output$descargar_plantilla <- downloadHandler(
      filename = function() "plantilla_atropellos_StatRoad.csv",
      content  = function(file) {
        df <- utils::read.csv(ruta_atrop_ejemplo, fileEncoding = "UTF-8")
        utils::write.csv(utils::head(df, 10), file, row.names = FALSE,
                         fileEncoding = "UTF-8")
      }
    )

    # ────────────────────────────────────────────────────
    # MIS DATOS
    # ────────────────────────────────────────────────────
    red_propia <- reactive({
      req(input$archivo_red)
      validar_lectura(
        leer_capa_espacial(input$archivo_red$datapath,
                           input$archivo_red$name, tipo = "lineas")
      )
    })

    atrop_propios <- reactive({
      req(input$archivo_atrop)
      nombre <- input$archivo_atrop$name
      ruta   <- input$archivo_atrop$datapath
      ext    <- tolower(tools::file_ext(nombre))

      validar_lectura({
        if (ext %in% c("csv", "txt", "xlsx", "xls")) {
          df <- leer_tabla_atropellos(ruta, nombre, input$separador)
          tabla_a_sf(normalizar_atropellos(df),
                     crs = as.numeric(input$crs_coords))
        } else {
          capa <- leer_capa_espacial(ruta, nombre, tipo = "puntos")
          df   <- normalizar_atropellos(sf::st_drop_geometry(capa))
          sf::st_sf(df, geometry = sf::st_geometry(capa))
        }
      })
    })

    output$estado_propios <- renderUI({
      faltan <- c(
        if (is.null(input$archivo_red))   "la red vial",
        if (is.null(input$archivo_atrop)) "los registros de atropello"
      )
      if (length(faltan) == 2) {
        return(p(class = "small text-muted",
                 "Sube tus archivos para ver la vista previa. Mientras no ",
                 "subas nada, la app usa los datos de ejemplo."))
      }
      if (length(faltan) == 1) {
        return(div(class = "alert alert-warning small py-2 px-3",
                   bs_icon("exclamation-triangle", class = "me-1"),
                   "Falta subir ", strong(faltan), "."))
      }
      tagList(
        tarjetas_resumen(atrop_propios(), red_propia()),
        br()
      )
    })

    output$tabla_propios <- renderDT({
      req(input$archivo_atrop)
      datatable(sf::st_drop_geometry(atrop_propios()),
                rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 10))
    })

    # ────────────────────────────────────────────────────
    # DATOS ACTIVOS: ejemplo o propios
    # ────────────────────────────────────────────────────
    usa_propios <- reactive({
      !is.null(input$archivo_red) || !is.null(input$archivo_atrop)
    })

    red_activa <- reactive({
      if (!usa_propios()) return(red_ejemplo())
      validate(need(input$archivo_red,
                    "Sube también la red vial en \"Los datos\" → \"Mis datos\"."))
      red_propia()
    })

    atrop_activos <- reactive({
      if (!usa_propios()) return(atrop_ejemplo())
      validate(need(input$archivo_atrop,
                    "Sube también los registros de atropello en \"Los datos\" → \"Mis datos\"."))
      atrop_propios()
    })

    output$origen_datos <- renderUI({
      div(class = "alert alert-secondary small py-2 px-3 mb-3",
          bs_icon("info-circle", class = "me-1"),
          strong("Datos en uso: "),
          if (usa_propios()) "tus datos" else
            "datos de ejemplo (Ruta 1, PN Santa Rosa; atropellos simulados)")
    })

    # ────────────────────────────────────────────────────
    # EXPLORACIÓN
    # ────────────────────────────────────────────────────
    output$cards_exploracion <- renderUI({
      tarjetas_resumen(atrop_activos(), red_activa())
    })

    output$mapa_exploracion <- leaflet::renderLeaflet({
      red   <- sf::st_transform(red_activa(), 4326)
      atrop <- sf::st_transform(atrop_activos(), 4326)

      mapa_base() |>
        leaflet::addPolylines(data = red, color = colores$primario,
                              weight = 4, opacity = 0.9) |>
        leaflet::addCircleMarkers(
          data = atrop, radius = 4, stroke = FALSE,
          fillColor = colores$acento, fillOpacity = 0.8,
          popup = ~paste0("<b><i>", especie, "</i></b><br>",
                          grupo, "<br>", fecha)
        )
    })

    output$plot_grupos <- renderPlot({
      df <- sf::st_drop_geometry(atrop_activos())
      ggplot(df, aes(x = ordenar_por_frecuencia(grupo), fill = grupo)) +
        geom_bar(show.legend = FALSE) +
        scale_fill_tableau_cb() +
        labs(x = NULL, y = "Registros") +
        coord_flip() +
        theme_light(base_size = 13)
    })

    output$plot_meses <- renderPlot({
      df <- sf::st_drop_geometry(atrop_activos())
      df$mes <- as.Date(format(df$fecha, "%Y-%m-01"))
      ggplot(df, aes(x = mes, fill = grupo)) +
        geom_bar() +
        scale_fill_tableau_cb() +
        scale_x_date(date_labels = "%b %Y") +
        labs(x = NULL, y = "Registros", fill = "Grupo") +
        theme_light(base_size = 13) +
        theme(legend.position = "bottom")
    })

    # ────────────────────────────────────────────────────
    # AJUSTE A LA VÍA
    # ────────────────────────────────────────────────────
    resultado <- reactiveVal(NULL)

    # Si cambian los datos, el ajuste anterior deja de ser válido
    observeEvent(list(input$archivo_red, input$archivo_atrop), {
      resultado(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$ajustar, {
      res <- tryCatch({
        red_prep <- preparar_red(red_activa(), crs = as.numeric(input$crs_metrico))
        ajuste   <- ajustar_a_red(atrop_activos(), red_prep, input$tolerancia)
        list(red_prep = red_prep, ajuste = ajuste,
             crs = as.numeric(input$crs_metrico))
      }, error = function(e) {
        showNotification(paste("No se pudo ajustar:", conditionMessage(e)),
                         type = "error", duration = 8)
        NULL
      })
      resultado(res)
    })

    output$estado_ajuste <- renderUI({
      r <- resultado()
      if (is.null(r)) {
        return(div(
          class = "alert alert-secondary small py-3 px-3",
          bs_icon("exclamation-circle", class = "me-1"),
          "Aún no se han ajustado los registros.", tags$br(), tags$br(),
          "Revisa los parámetros a la izquierda y presiona ",
          strong("\"Ajustar registros a la vía\""), ". Los módulos siguientes ",
          "usan los registros ajustados."
        ))
      }
      a <- r$ajuste
      tagList(
        div(class = "alert alert-info small py-2 px-3 mb-3",
            bs_icon("check-circle-fill", class = "me-1"),
            strong("Ajuste completado. "),
            nrow(a$ajustados), " registros sobre la vía; ",
            nrow(a$excluidos), " excluidos (más de ", a$tolerancia, " m)."),
        if (!r$red_prep$continua) {
          div(class = "alert alert-warning small py-2 px-3 mb-3",
              bs_icon("exclamation-triangle", class = "me-1"),
              "La red no forma una sola ruta continua (tiene ramales o huecos), ",
              "así que no se calculó la posición en km.")
        },
        actionButton(ns("ir_a_resultados"), "Ver resultados completos →",
                     class = "btn-outline-primary w-100")
      )
    })

    observeEvent(input$ir_a_resultados, {
      bslib::nav_select(id = "tabs_datos", selected = "tab_resultados",
                        session = session)
    })

    # ────────────────────────────────────────────────────
    # RESULTADOS
    # ────────────────────────────────────────────────────
    output$cards_resultados <- renderUI({
      r <- resultado()
      if (is.null(r)) {
        return(div(class = "alert alert-secondary small py-2 px-3",
                   bs_icon("exclamation-circle", class = "me-1"),
                   "Primero ajusta los registros en \"Configurar análisis\"."))
      }
      a <- r$ajuste
      n_total <- nrow(a$todos)
      layout_columns(
        col_widths = c(3, 3, 3, 3), fill = FALSE,
        tarjeta_valor(n_total, "Registros", colores$primario),
        tarjeta_valor(nrow(a$ajustados), "Sobre la vía", colores$secundario),
        tarjeta_valor(nrow(a$excluidos),
                      paste0("Excluidos (> ", a$tolerancia, " m de la vía)"),
                      colores$peligro),
        tarjeta_valor(paste0(round(stats::median(a$todos$dist_via_m), 1), " m"),
                      "Distancia mediana a la vía", colores$acento)
      )
    })

    output$mapa_resultados <- leaflet::renderLeaflet({
      r <- resultado()
      req(r)
      a   <- r$ajuste
      red <- sf::st_transform(r$red_prep$segmentos, 4326)
      aj  <- sf::st_transform(a$ajustados, 4326)
      exc <- sf::st_transform(a$original[!a$todos$dentro, ], 4326)
      exc$dist_via_m <- a$excluidos$dist_via_m

      popup_reg <- ~paste0("<b><i>", especie, "</i></b><br>", grupo, "<br>",
                           fecha, "<br>Distancia a la vía: ", dist_via_m, " m")

      mapa <- mapa_base() |>
        leaflet::addPolylines(data = red, color = colores$primario,
                              weight = 4, opacity = 0.9) |>
        leaflet::addCircleMarkers(data = aj, radius = 4, stroke = FALSE,
                                  fillColor = colores$acento, fillOpacity = 0.85,
                                  popup = popup_reg)
      if (nrow(exc) > 0) {
        mapa <- mapa |>
          leaflet::addCircleMarkers(data = exc, radius = 6, color = "white",
                                    weight = 1, fillColor = colores$peligro,
                                    fillOpacity = 1, popup = popup_reg)
      }
      mapa |>
        leaflet::addLegend(
          position = "bottomright",
          colors   = c(colores$primario, colores$acento, colores$peligro),
          labels   = c("Red vial", "Registros ajustados", "Registros excluidos"),
          opacity  = 1
        )
    })

    output$plot_distancias <- renderPlot({
      r <- resultado()
      req(r)
      df <- sf::st_drop_geometry(r$ajuste$todos)
      ggplot(df, aes(x = dist_via_m, fill = dentro)) +
        geom_histogram(binwidth = 5, boundary = 0, color = "white") +
        geom_vline(xintercept = r$ajuste$tolerancia, linetype = "dashed",
                   color = colores$peligro, linewidth = 0.9) +
        scale_fill_manual(values = c(`TRUE` = colores$acento,
                                     `FALSE` = colores$peligro),
                          labels = c(`TRUE` = "Ajustado", `FALSE` = "Excluido"),
                          name = NULL) +
        labs(x = "Distancia a la vía (m)", y = "Registros") +
        theme_light(base_size = 13) +
        theme(legend.position = "bottom")
    })

    output$tabla_excluidos <- renderDT({
      r <- resultado()
      req(r)
      exc <- r$ajuste$original[!r$ajuste$todos$dentro, ]
      df  <- sf::st_drop_geometry(exc)
      df$dist_via_m <- r$ajuste$excluidos$dist_via_m
      datatable(df[order(-df$dist_via_m), ], rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 10))
    })

    tabla_ajustados <- reactive({
      r <- resultado()
      req(r)
      aj <- r$ajuste$ajustados
      ll <- sf::st_coordinates(sf::st_transform(aj, 4326))
      df <- sf::st_drop_geometry(aj)
      df <- df[, setdiff(names(df), c("x", "y", "dentro")), drop = FALSE]
      df$lon_via <- round(ll[, 1], 6)
      df$lat_via <- round(ll[, 2], 6)
      df[order(df$km, df$fecha), ]
    })

    output$tabla_ajustados <- renderDT({
      datatable(tabla_ajustados(), rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 10))
    })

    output$descargar_ajustados <- downloadHandler(
      filename = function() "atropellos_ajustados_StatRoad.csv",
      content  = function(file) {
        utils::write.csv(tabla_ajustados(), file, row.names = FALSE,
                         fileEncoding = "UTF-8")
      }
    )

    # ────────────────────────────────────────────────────
    # CÓDIGO R REPRODUCIBLE
    # ────────────────────────────────────────────────────
    output$codigo_r <- renderText({
      r <- resultado()
      if (is.null(r)) {
        return("# Ajusta los registros en \"Configurar análisis\" para generar el código.")
      }
      codigo_ajuste(
        propios    = usa_propios(),
        nombre_red = if (usa_propios()) input$archivo_red$name else "red_ruta1_santarosa.gpkg",
        nombre_atr = if (usa_propios()) input$archivo_atrop$name else "atropellos_simulados_santarosa.csv",
        crs_coords = if (usa_propios()) input$crs_coords else 4326,
        separador  = input$separador,
        crs        = r$crs,
        tolerancia = r$ajuste$tolerancia
      )
    })

    # Resultado disponible para los demás módulos
    resultado
  })
}

# ── Auxiliares de UI ─────────────────────────────────────

# Tarjeta con un valor destacado
tarjeta_valor <- function(valor, etiqueta, color) {
  card(class = "text-center",
       card_body(class = "p-2",
                 h3(style = paste0("color:", color, "; font-weight:700;"), valor),
                 p(class = "small text-muted mb-0", etiqueta)))
}

# Tarjetas de resumen de un conjunto de datos
tarjetas_resumen <- function(atrop, red) {
  df <- sf::st_drop_geometry(atrop)
  rango <- range(df$fecha, na.rm = TRUE)
  # st_length mide en metros en cualquier CRS (geodésico si es lon/lat)
  long_km <- as.numeric(sum(sf::st_length(red))) / 1000
  layout_columns(
    col_widths = c(3, 3, 3, 3), fill = FALSE,
    tarjeta_valor(nrow(df), "Registros", colores$primario),
    tarjeta_valor(length(unique(df$especie)), "Especies", colores$acento),
    tarjeta_valor(paste(round(long_km, 1), "km"), "Longitud de la red",
                  colores$secundario),
    tarjeta_valor(paste(format(rango, "%m/%Y"), collapse = " – "),
                  "Periodo", colores$texto)
  )
}

# Ordena un factor por frecuencia (para gráficos de barras)
ordenar_por_frecuencia <- function(x) {
  factor(x, levels = names(sort(table(x))))
}

# ── Generador del código R reproducible ──────────────────
codigo_ajuste <- function(propios, nombre_red, nombre_atr, crs_coords,
                          separador, crs, tolerancia) {
  ext_atr <- tolower(tools::file_ext(nombre_atr))
  es_tabla <- ext_atr %in% c("csv", "txt", "xlsx", "xls")

  lectura_atr <- if (!es_tabla) {
    paste0("atropellos <- st_read(\"", nombre_atr, "\")\n")
  } else if (ext_atr %in% c("xlsx", "xls")) {
    paste0("library(readxl)\n",
           "tabla <- read_excel(\"", nombre_atr, "\")\n",
           "atropellos <- st_as_sf(tabla, coords = c(\"x\", \"y\"), crs = ",
           crs_coords, ")\n")
  } else {
    paste0("tabla <- read.csv(\"", nombre_atr, "\", sep = \"", separador,
           "\", fileEncoding = \"UTF-8\")\n",
           "atropellos <- st_as_sf(tabla, coords = c(\"x\", \"y\"), crs = ",
           crs_coords, ")\n")
  }

  paste0(
    encabezado_script("StatRoad", "Datos y ajuste de registros a la vía"),
    "library(sf)\n\n",
    "# ── 1. Leer datos ──\n",
    "red <- st_read(\"", nombre_red, "\")\n",
    lectura_atr, "\n",
    "# ── 2. Reproyectar a CRS métrico (EPSG:", crs, ") ──\n",
    "red        <- st_transform(red, ", crs, ")\n",
    "atropellos <- st_transform(atropellos, ", crs, ")\n\n",
    "# ── 3. Ajustar cada registro al punto más cercano de la vía ──\n",
    "idx   <- st_nearest_feature(atropellos, red)\n",
    "conex <- st_nearest_points(st_geometry(atropellos),\n",
    "                           st_geometry(red)[idx], pairwise = TRUE)\n",
    "atropellos$dist_via_m <- as.numeric(st_length(conex))\n",
    "sobre_via <- st_cast(conex, \"POINT\")[seq(2, 2 * length(conex), by = 2)]\n",
    "ajustados <- st_set_geometry(atropellos, sobre_via)\n\n",
    "# ── 4. Aplicar la tolerancia (", tolerancia, " m) ──\n",
    "excluidos <- ajustados[ajustados$dist_via_m >  ", tolerancia, ", ]\n",
    "ajustados <- ajustados[ajustados$dist_via_m <= ", tolerancia, ", ]\n\n",
    "nrow(ajustados); nrow(excluidos)\n",
    "hist(atropellos$dist_via_m, breaks = 30,\n",
    "     main = \"Distancia a la vía\", xlab = \"m\")\n",
    "abline(v = ", tolerancia, ", lty = 2, col = \"red\")\n"
  )
}
