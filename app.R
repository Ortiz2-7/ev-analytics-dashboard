# ============================================================
#  Dashboard EV Analytics - Segundo Corte
#  Politécnico Grancolombiano | Estadística Descriptiva
#  Barreto, Durán, Moreno, Ortiz
# ============================================================

library(dplyr)
library(shiny)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(DT)
library(leaflet)

# ── Cargar datos ─────────────────────────────────────────────
data <- read.csv("electric_vehicle_analytics_.csv", stringsAsFactors = FALSE)
names(data)[names(data) == "Battery_Health_."] <- "Battery_Health_%"

# ── Coordenadas por región ───────────────────────────────────
region_coords <- data.frame(
  Region = c("Asia", "Europe", "North America", "South America",
             "Africa", "Australia", "Middle East"),
  lat    = c(35.86,  54.52,   45.42,           -8.78,
             1.65, -25.27,   29.37),
  lng    = c(104.19,  15.25,  -75.70,          -55.49,
             17.53,  133.77,   47.98),
  stringsAsFactors = FALSE
)
data <- left_join(data, region_coords, by = "Region")

# ── Clasificación de variables ───────────────────────────────
vars_continuas <- c(
  "Battery_Capacity_kWh", "Battery_Health_%", "Range_km",
  "Charging_Power_kW", "Charging_Time_hr", "Charge_Cycles",
  "Energy_Consumption_kWh_per_100km", "Mileage_km",
  "Avg_Speed_kmh", "Max_Speed_kmh", "Acceleration_0_100_kmh_sec",
  "Temperature_C", "CO2_Saved_tons", "Maintenance_Cost_USD",
  "Insurance_Cost_USD", "Electricity_Cost_USD_per_kWh",
  "Monthly_Charging_Cost_USD", "Resale_Value_USD"
)

vars_discretas <- c("Make", "Model", "Year", "Region",
                    "Vehicle_Type", "Usage_Type")

# ── UI ───────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$span("EV Analytics", style = "font-weight:bold;")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio",              tabName = "inicio",      icon = icon("home")),
      menuItem("Distribuciones",      tabName = "distribucion",icon = icon("chart-bar")),
      menuItem("Composicion",         tabName = "composicion", icon = icon("circle-notch")),
      menuItem("Mapa Geografico",     tabName = "mapa",        icon = icon("map-marked-alt")),
      menuItem("Tabla Cruzada",       tabName = "tabla",       icon = icon("table")),
      menuItem("Grafico Combinado",   tabName = "combinado",   icon = icon("chart-line")),
      menuItem("Dispersion y Corr",   tabName = "dispersion",  icon = icon("braille"))
    ),
    hr(),
    tags$div(style = "padding:0 15px;",
             tags$h5("Filtro General", style = "color:#ccc; margin-bottom:5px;"),
             tags$p(style = "color:#aaa; font-size:11px; margin-top:0;",
                    "Aplica a todas las pestanas"),
             selectInput("fg_region",   "Region:",
                         choices  = c("Todas", sort(unique(data$Region))),
                         selected = "Todas"),
             selectInput("fg_tipo",     "Tipo de vehiculo:",
                         choices  = c("Todos", sort(unique(data$Vehicle_Type))),
                         selected = "Todos"),
             selectInput("fg_uso",      "Uso:",
                         choices  = c("Todos", sort(unique(data$Usage_Type))),
                         selected = "Todos"),
             sliderInput("fg_year",     "Anio:",
                         min   = min(data$Year, na.rm = TRUE),
                         max   = max(data$Year, na.rm = TRUE),
                         value = c(min(data$Year, na.rm = TRUE),
                                   max(data$Year, na.rm = TRUE)),
                         sep = "", step = 1)
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f0f2f5; }
      .box { border-radius:10px; box-shadow: 0 2px 8px rgba(0,0,0,.08); }
      .small-box { border-radius:10px; }
      .small-box h3 { font-size:28px; }
      .select2-container { width:100% !important; }
    "))),
    
    tabItems(
      
      # ══════════════════════════════════════════════════════
      # TAB 1: INICIO / RESUMEN GENERAL
      # ══════════════════════════════════════════════════════
      tabItem(tabName = "inicio",
              fluidRow(
                valueBoxOutput("vb_total",    width = 3),
                valueBoxOutput("vb_marcas",   width = 3),
                valueBoxOutput("vb_range",    width = 3),
                valueBoxOutput("vb_co2",      width = 3)
              ),
              fluidRow(
                valueBoxOutput("vb_bat_cap",  width = 3),
                valueBoxOutput("vb_bat_hlth", width = 3),
                valueBoxOutput("vb_resale",   width = 3),
                valueBoxOutput("vb_maint",    width = 3)
              ),
              fluidRow(
                box(title = "Distribucion de vehiculos por Marca (Top 10)", width = 6,
                    solidHeader = TRUE, status = "primary",
                    plotlyOutput("inicio_barras_marca", height = 300)),
                box(title = "CO2 Ahorrado vs Autonomia", width = 6,
                    solidHeader = TRUE, status = "success",
                    tags$p(style = "color:#888; font-size:12px; margin-bottom:4px;",
                           "Relacion entre reduccion de emisiones y rango del vehiculo"),
                    plotlyOutput("inicio_co2_range", height = 300))
              ),
              fluidRow(
                box(title = "Contexto del proyecto", width = 12,
                    solidHeader = FALSE, status = "info",
                    tags$p("Este dashboard analiza el dataset ",
                           tags$b("Electric Vehicle Analytics"), " (Yadav, Kaggle),
                     con 3.000 observaciones y 25 variables sobre rendimiento,
                     bateria, costos y sostenibilidad de vehiculos electricos."),
                    tags$p("El analisis busca explorar los factores que determinan la
                     autonomia (Range_km) y la salud de la bateria, en linea con
                     la literatura que indica que la capacidad de la bateria y el
                     tipo de motor son los principales determinantes del rango
                     (Sierzchula et al., 2014)."))
              )
      ),
      
      # ══════════════════════════════════════════════════════
      # TAB 2: DISTRIBUCIONES (Histograma)
      # ══════════════════════════════════════════════════════
      tabItem(tabName = "distribucion",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary",
                    title = "Histograma - Distribucion de una variable continua",
                    fluidRow(
                      column(4,
                             selectInput("hist_var",  "Variable continua:",
                                         choices  = vars_continuas,
                                         selected = "Range_km")),
                      column(4,
                             selectInput("hist_color", "Colorear por:",
                                         choices  = vars_discretas,
                                         selected = "Vehicle_Type")),
                      column(4,
                             sliderInput("hist_bins", "Numero de bins:",
                                         min = 5, max = 80, value = 30))
                    ),
                    plotlyOutput("plot_hist", height = 420),
                    tags$p(style = "color:#888; font-size:12px; margin-top:8px;",
                           "Tip: cambia la variable y el color para explorar
                      diferencias entre tipos o marcas de vehiculos.")
                )
              )
      ),
      
      # ══════════════════════════════════════════════════════
      # TAB 3: COMPOSICION (Dona)
      # ══════════════════════════════════════════════════════
      tabItem(tabName = "composicion",
              fluidRow(
                box(width = 6, solidHeader = TRUE, status = "warning",
                    title = "Diagrama de Dona",
                    selectInput("dona_var1", "Variable principal:",
                                choices = vars_discretas, selected = "Vehicle_Type"),
                    selectInput("dona_var2", "Segundo filtro (opcional):",
                                choices = c("Ninguno", vars_discretas),
                                selected = "Region"),
                    uiOutput("dona_filtro2_ui"),
                    plotlyOutput("plot_dona", height = 360)
                ),
                box(width = 6, solidHeader = TRUE, status = "warning",
                    title = "Barras apiladas - Dos variables discretas",
                    selectInput("barra_x",    "Eje X:",
                                choices = vars_discretas, selected = "Region"),
                    selectInput("barra_fill", "Relleno:",
                                choices = vars_discretas, selected = "Vehicle_Type"),
                    plotlyOutput("plot_barra_apilada", height = 360)
                )
              )
      ),
      
      # ══════════════════════════════════════════════════════
      # TAB 4: MAPA GEOGRAFICO
      # ══════════════════════════════════════════════════════
      tabItem(tabName = "mapa",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "success",
                    title = "Mapa Geografico - Distribucion mundial de EVs",
                    fluidRow(
                      column(4,
                             selectInput("mapa_metrica", "Metrica a visualizar:",
                                         choices  = vars_continuas,
                                         selected = "Range_km")),
                      column(4,
                             selectInput("mapa_tipo", "Tipo de vehiculo:",
                                         choices  = c("Todos",
                                                      sort(unique(data$Vehicle_Type))),
                                         selected = "Todos")),
                      column(4,
                             selectInput("mapa_uso", "Uso:",
                                         choices  = c("Todos",
                                                      sort(unique(data$Usage_Type))),
                                         selected = "Todos"))
                    ),
                    leafletOutput("mapa_geo", height = 500),
                    tags$p(style="color:#888;font-size:12px;margin-top:6px;",
                           "El tamano del circulo indica numero de vehiculos;
                      el color indica el promedio de la metrica seleccionada.")
                )
              )
      ),
      
      # ══════════════════════════════════════════════════════
      # TAB 5: TABLA CRUZADA
      # ══════════════════════════════════════════════════════
      tabItem(tabName = "tabla",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "info",
                    title = "Tabla de Frecuencia Absoluta Cruzada",
                    fluidRow(
                      column(4,
                             selectInput("tbl_fila", "Variable fila:",
                                         choices  = vars_discretas,
                                         selected = "Vehicle_Type")),
                      column(4,
                             selectInput("tbl_col",  "Variable columna:",
                                         choices  = vars_discretas,
                                         selected = "Region")),
                      column(4,
                             selectInput("tbl_tipo", "Mostrar:",
                                         choices  = c("Frecuencia absoluta",
                                                      "Frecuencia relativa (%)"),
                                         selected = "Frecuencia absoluta"))
                    ),
                    DTOutput("tabla_cruzada"),
                    tags$p(style = "color:#888; font-size:12px; margin-top:6px;",
                           "La tabla muestra cuantos vehiculos hay en cada
                      combinacion de categorias.")
                )
              )
      ),
      
      # ══════════════════════════════════════════════════════
      # TAB 6: GRAFICO COMBINADO
      # ══════════════════════════════════════════════════════
      tabItem(tabName = "combinado",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "danger",
                    title = "Grafico Combinado - Barras + Linea de tendencia",
                    fluidRow(
                      column(3,
                             selectInput("comb_x",   "Eje X (discreta):",
                                         choices  = vars_discretas,
                                         selected = "Year")),
                      column(3,
                             selectInput("comb_bar", "Variable barras (promedio):",
                                         choices  = vars_continuas,
                                         selected = "Range_km")),
                      column(3,
                             selectInput("comb_lin", "Variable linea (promedio):",
                                         choices  = vars_continuas,
                                         selected = "Battery_Capacity_kWh")),
                      column(3,
                             selectInput("comb_color","Agrupar barras por:",
                                         choices  = c("Ninguno", vars_discretas),
                                         selected = "Ninguno"))
                    ),
                    plotlyOutput("plot_combinado", height = 460),
                    tags$p(style = "color:#888; font-size:12px; margin-top:6px;",
                           "Usa Year en el eje X para ver la evolucion temporal
                      del rango y capacidad de bateria.")
                )
              )
      ),
      
      # ══════════════════════════════════════════════════════
      # TAB 7: DISPERSION Y CORRELACION
      # ══════════════════════════════════════════════════════
      tabItem(tabName = "dispersion",
              fluidRow(
                box(width = 8, solidHeader = TRUE, status = "primary",
                    title = "Diagrama de Dispersion",
                    fluidRow(
                      column(4,
                             selectInput("disp_x",   "Variable X:",
                                         choices  = vars_continuas,
                                         selected = "Battery_Capacity_kWh")),
                      column(4,
                             selectInput("disp_y",   "Variable Y:",
                                         choices  = vars_continuas,
                                         selected = "Range_km")),
                      column(4,
                             selectInput("disp_col", "Color (cualitativa):",
                                         choices  = vars_discretas,
                                         selected = "Vehicle_Type"))
                    ),
                    checkboxInput("disp_tendencia",
                                  "Mostrar linea de tendencia", value = TRUE),
                    plotlyOutput("plot_dispersion", height = 420)
                ),
                box(width = 4, solidHeader = TRUE, status = "warning",
                    title = "Coeficiente de Correlacion",
                    selectInput("corr_x",    "Variable X:",
                                choices  = vars_continuas,
                                selected = "Battery_Capacity_kWh"),
                    selectInput("corr_y",    "Variable Y:",
                                choices  = vars_continuas,
                                selected = "Range_km"),
                    selectInput("corr_fil",  "Filtrar por (cualitativa):",
                                choices  = c("Ninguno", vars_discretas),
                                selected = "Ninguno"),
                    uiOutput("corr_fil_val_ui"),
                    hr(),
                    tags$div(style = "text-align:center;",
                             tags$h2(textOutput("corr_r"),
                                     style = "color:#2c3e50; font-weight:bold; margin:8px 0;"),
                             tags$p(textOutput("corr_label"),
                                    style = "color:#7f8c8d; font-size:13px; margin:0;"),
                             tags$p(textOutput("corr_p"),
                                    style = "color:#95a5a6; font-size:11px; margin-top:4px;")
                    ),
                    hr(),
                    plotlyOutput("plot_mini_corr", height = 200)
                )
              ),
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "info",
                    title = "Matriz de Correlacion - Variables clave",
                    tags$p(style="color:#888;font-size:12px;",
                           "Correlaciones entre las variables mas relevantes para
                      el analisis de autonomia y eficiencia."),
                    plotlyOutput("plot_corr_matriz", height = 400)
                )
              )
      )
      
    )
  )
)


# ── SERVER ───────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Datos filtrados (filtro general) ────────────────────────
  datos <- reactive({
    d <- data
    if (input$fg_region != "Todas")
      d <- d %>% filter(Region == input$fg_region)
    if (input$fg_tipo != "Todos")
      d <- d %>% filter(Vehicle_Type == input$fg_tipo)
    if (input$fg_uso != "Todos")
      d <- d %>% filter(Usage_Type == input$fg_uso)
    d <- d %>% filter(Year >= input$fg_year[1],
                      Year <= input$fg_year[2])
    d
  })
  
  # ── VALUE BOXES ─────────────────────────────────────────────
  output$vb_total   <- renderValueBox(valueBox(
    format(nrow(datos()), big.mark = ","),
    "Vehiculos", icon = icon("car"), color = "blue"))
  output$vb_marcas  <- renderValueBox(valueBox(
    n_distinct(datos()$Make),
    "Marcas distintas", icon = icon("industry"), color = "purple"))
  output$vb_range   <- renderValueBox(valueBox(
    paste0(round(mean(datos()$Range_km,  na.rm = TRUE), 1), " km"),
    "Autonomia promedio", icon = icon("road"), color = "green"))
  output$vb_co2     <- renderValueBox(valueBox(
    paste0(round(mean(datos()$CO2_Saved_tons, na.rm = TRUE), 2), " t"),
    "CO2 ahorrado prom.", icon = icon("leaf"), color = "teal"))
  output$vb_bat_cap <- renderValueBox(valueBox(
    paste0(round(mean(datos()$Battery_Capacity_kWh, na.rm = TRUE), 1), " kWh"),
    "Capacidad bateria prom.", icon = icon("battery-full"), color = "yellow"))
  output$vb_bat_hlth <- renderValueBox(valueBox(
    paste0(round(mean(datos()[["Battery_Health_%"]], na.rm = TRUE), 1), "%"),
    "Salud bateria prom.", icon = icon("heartbeat"), color = "orange"))
  output$vb_resale  <- renderValueBox(valueBox(
    paste0("$", format(round(mean(datos()$Resale_Value_USD, na.rm = TRUE)), big.mark = ",")),
    "Valor reventa prom.", icon = icon("dollar-sign"), color = "maroon"))
  output$vb_maint   <- renderValueBox(valueBox(
    paste0("$", format(round(mean(datos()$Maintenance_Cost_USD, na.rm = TRUE)), big.mark = ",")),
    "Mantenimiento prom.", icon = icon("wrench"), color = "navy"))
  
  # ── INICIO ───────────────────────────────────────────────────
  output$inicio_barras_marca <- renderPlotly({
    d <- datos() %>% count(Make) %>% arrange(desc(n)) %>% slice_head(n = 10)
    plot_ly(d, x = ~reorder(Make, n), y = ~n, type = "bar",
            marker = list(color = "steelblue")) %>%
      layout(xaxis = list(title = "Marca"),
             yaxis = list(title = "Cantidad"),
             margin = list(b = 80))
  })
  
  output$inicio_co2_range <- renderPlotly({
    d <- datos()
    plot_ly(d, x = ~Range_km, y = ~CO2_Saved_tons,
            color = ~Vehicle_Type, type = "scatter", mode = "markers",
            marker = list(size = 5, opacity = 0.6),
            text = ~paste("Marca:", Make, "<br>Modelo:", Model)) %>%
      layout(xaxis = list(title = "Autonomia (km)"),
             yaxis = list(title = "CO2 ahorrado (ton)"))
  })
  
  # ── HISTOGRAMA ───────────────────────────────────────────────
  output$plot_hist <- renderPlotly({
    req(input$hist_var, input$hist_color)
    d <- datos()
    plot_ly(d,
            x     = as.formula(paste0("~`", input$hist_var, "`")),
            color = as.formula(paste0("~as.factor(`", input$hist_color, "`)")),
            type  = "histogram",
            nbinsx = input$hist_bins,
            opacity = 0.75) %>%
      layout(barmode = "overlay",
             xaxis = list(title = input$hist_var),
             yaxis = list(title = "Frecuencia"),
             legend = list(title = list(text = input$hist_color)))
  })
  
  # ── DONA ─────────────────────────────────────────────────────
  output$dona_filtro2_ui <- renderUI({
    if (input$dona_var2 == "Ninguno") return(NULL)
    vals <- sort(unique(data[[input$dona_var2]]))
    selectInput("dona_fil2_val",
                paste("Valor de", input$dona_var2, ":"),
                choices = c("Todos", vals), selected = "Todos")
  })
  
  output$plot_dona <- renderPlotly({
    req(input$dona_var1)
    d <- datos()
    if (!is.null(input$dona_var2) && input$dona_var2 != "Ninguno" &&
        !is.null(input$dona_fil2_val) && input$dona_fil2_val != "Todos")
      d <- d %>% filter(.data[[input$dona_var2]] == input$dona_fil2_val)
    freq <- d %>% count(.data[[input$dona_var1]]) %>% rename(cat = 1)
    plot_ly(freq, labels = ~cat, values = ~n,
            type = "pie", hole = 0.45,
            textinfo = "label+percent",
            insidetextorientation = "radial") %>%
      layout(showlegend = TRUE)
  })
  
  output$plot_barra_apilada <- renderPlotly({
    req(input$barra_x, input$barra_fill)
    d <- datos()
    freq <- d %>%
      count(.data[[input$barra_x]], .data[[input$barra_fill]]) %>%
      rename(x_val = 1, fill_val = 2)
    plot_ly(freq, x = ~x_val, y = ~n,
            color = ~fill_val, type = "bar") %>%
      layout(barmode = "stack",
             xaxis = list(title = input$barra_x),
             yaxis = list(title = "Frecuencia"))
  })
  
  # ── MAPA ─────────────────────────────────────────────────────
  output$mapa_geo <- renderLeaflet({
    d <- datos()
    if (input$mapa_tipo != "Todos")
      d <- d %>% filter(Vehicle_Type == input$mapa_tipo)
    if (input$mapa_uso  != "Todos")
      d <- d %>% filter(Usage_Type   == input$mapa_uso)
    
    resumen_mapa <- d %>%
      group_by(Region, lat, lng) %>%
      summarise(n       = n(),
                metrica = mean(.data[[input$mapa_metrica]], na.rm = TRUE),
                .groups = "drop") %>%
      filter(!is.na(lat))
    
    pal <- colorNumeric("YlOrRd", resumen_mapa$metrica)
    leaflet(resumen_mapa) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius      = ~pmax(sqrt(n) * 2.5, 8),
        fillColor   = ~pal(metrica),
        fillOpacity = 0.85,
        color       = "white", weight = 1,
        popup = ~paste0("<b>", Region, "</b><br>",
                        "Vehiculos: <b>", n, "</b><br>",
                        input$mapa_metrica, ": <b>", round(metrica, 2), "</b>")
      ) %>%
      addLegend("bottomright", pal = pal, values = ~metrica,
                title = input$mapa_metrica, opacity = 0.9)
  })
  
  # ── TABLA CRUZADA ────────────────────────────────────────────
  output$tabla_cruzada <- renderDT({
    req(input$tbl_fila, input$tbl_col)
    d   <- datos()
    tbl <- table(d[[input$tbl_fila]], d[[input$tbl_col]])
    if (input$tbl_tipo == "Frecuencia relativa (%)") {
      tbl_df <- as.data.frame.matrix(round(prop.table(tbl) * 100, 2))
    } else {
      tbl_df <- as.data.frame.matrix(tbl)
    }
    tbl_df <- cbind(Categoria = rownames(tbl_df), tbl_df)
    datatable(tbl_df, rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # ── GRAFICO COMBINADO ────────────────────────────────────────
  output$plot_combinado <- renderPlotly({
    req(input$comb_x, input$comb_bar, input$comb_lin)
    d <- datos()
    resumen <- d %>%
      group_by(across(all_of(input$comb_x))) %>%
      summarise(media_bar = mean(.data[[input$comb_bar]], na.rm = TRUE),
                media_lin = mean(.data[[input$comb_lin]], na.rm = TRUE),
                .groups = "drop") %>%
      arrange(.data[[input$comb_x]])
    x_v <- as.character(resumen[[input$comb_x]])
    plot_ly() %>%
      add_bars(x = x_v, y = resumen$media_bar,
               name = paste("Media", input$comb_bar),
               marker = list(color = "steelblue")) %>%
      add_lines(x = x_v, y = resumen$media_lin,
                name = paste("Media", input$comb_lin),
                line = list(color = "tomato", width = 3),
                yaxis = "y2") %>%
      layout(xaxis  = list(title = input$comb_x),
             yaxis  = list(title = input$comb_bar),
             yaxis2 = list(title = input$comb_lin, overlaying = "y",
                           side = "right"),
             legend = list(orientation = "h"))
  })
  
  # ── DISPERSION ───────────────────────────────────────────────
  output$plot_dispersion <- renderPlotly({
    req(input$disp_x, input$disp_y, input$disp_col)
    d <- datos()
    p <- plot_ly(d,
                 x     = as.formula(paste0("~`", input$disp_x, "`")),
                 y     = as.formula(paste0("~`", input$disp_y, "`")),
                 color = as.formula(paste0("~as.factor(`", input$disp_col, "`)")),
                 type  = "scatter", mode = "markers",
                 marker = list(size = 5, opacity = 0.65),
                 text  = ~paste("Marca:", Make, "<br>Modelo:", Model,
                                "<br>Anio:", Year)) %>%
      layout(xaxis = list(title = input$disp_x),
             yaxis = list(title = input$disp_y))
    
    if (input$disp_tendencia) {
      fit <- lm(as.formula(
        paste0("`", input$disp_y, "` ~ `", input$disp_x, "`")), data = d)
      x_seq <- seq(min(d[[input$disp_x]], na.rm = TRUE),
                   max(d[[input$disp_x]], na.rm = TRUE), length.out = 200)
      y_pred <- predict(fit, newdata = setNames(data.frame(x_seq), input$disp_x))
      p <- p %>% add_lines(x = x_seq, y = y_pred,
                           name = "Tendencia lineal",
                           line = list(color = "black", width = 2, dash = "dash"),
                           inherit = FALSE)
    }
    p
  })
  
  # ── CORRELACION ──────────────────────────────────────────────
  output$corr_fil_val_ui <- renderUI({
    if (is.null(input$corr_fil) || input$corr_fil == "Ninguno") return(NULL)
    vals <- sort(unique(data[[input$corr_fil]]))
    selectInput("corr_fil_val",
                paste("Valor de", input$corr_fil, ":"),
                choices = c("Todos", vals), selected = "Todos")
  })
  
  datos_corr <- reactive({
    d <- datos()
    if (!is.null(input$corr_fil) && input$corr_fil != "Ninguno" &&
        !is.null(input$corr_fil_val) && input$corr_fil_val != "Todos")
      d <- d %>% filter(.data[[input$corr_fil]] == input$corr_fil_val)
    d
  })
  
  output$corr_r <- renderText({
    req(input$corr_x, input$corr_y)
    r <- cor(datos_corr()[[input$corr_x]],
             datos_corr()[[input$corr_y]], use = "complete.obs")
    paste0("r = ", round(r, 4))
  })
  
  output$corr_label <- renderText({
    req(input$corr_x, input$corr_y)
    r  <- cor(datos_corr()[[input$corr_x]],
              datos_corr()[[input$corr_y]], use = "complete.obs")
    ar <- abs(r)
    dir <- ifelse(r > 0, "positiva", "negativa")
    fuerza <- dplyr::case_when(
      ar >= 0.8 ~ "muy fuerte",
      ar >= 0.6 ~ "fuerte",
      ar >= 0.4 ~ "moderada",
      ar >= 0.2 ~ "debil",
      TRUE       ~ "muy debil / nula"
    )
    paste0("Correlacion ", dir, " ", fuerza)
  })
  
  output$corr_p <- renderText({
    req(input$corr_x, input$corr_y)
    d  <- datos_corr()
    ct <- tryCatch(cor.test(d[[input$corr_x]], d[[input$corr_y]]),
                   error = function(e) NULL)
    if (is.null(ct)) return("")
    paste0("p-valor: ", format(ct$p.value, digits = 3, scientific = TRUE))
  })
  
  output$plot_mini_corr <- renderPlotly({
    req(input$corr_x, input$corr_y)
    d <- datos_corr()
    plot_ly(d,
            x = as.formula(paste0("~`", input$corr_x, "`")),
            y = as.formula(paste0("~`", input$corr_y, "`")),
            type = "scatter", mode = "markers",
            marker = list(size = 4, color = "steelblue", opacity = 0.5)) %>%
      layout(xaxis = list(title = input$corr_x, tickfont = list(size = 9)),
             yaxis = list(title = input$corr_y, tickfont = list(size = 9)),
             margin = list(l = 40, r = 10, t = 10, b = 40))
  })
  
  # ── MATRIZ DE CORRELACION ────────────────────────────────────
  output$plot_corr_matriz <- renderPlotly({
    vars_mat <- c("Battery_Capacity_kWh", "Battery_Health_%",
                  "Range_km", "Charging_Power_kW",
                  "Charge_Cycles", "Energy_Consumption_kWh_per_100km",
                  "CO2_Saved_tons", "Mileage_km",
                  "Resale_Value_USD", "Maintenance_Cost_USD")
    d   <- datos()[, vars_mat]
    mat <- round(cor(d, use = "pairwise.complete.obs"), 2)
    plot_ly(
      x = colnames(mat), y = rownames(mat),
      z = mat,
      type = "heatmap",
      colorscale = list(
        c(0,   "rgb(215,48,39)"),
        c(0.5, "rgb(255,255,255)"),
        c(1,   "rgb(69,117,180)")
      ),
      zmin = -1, zmax = 1,
      text = mat,
      texttemplate = "%{text}",
      showscale = TRUE
    ) %>%
      layout(
        xaxis = list(tickangle = -35, tickfont = list(size = 10)),
        yaxis = list(tickfont = list(size = 10))
      )
  })
  
}

shinyApp(ui, server)

