library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

if (requireNamespace("sass", quietly = TRUE)) {
  sass_cache_dir <- file.path(tempdir(), "aerofleet-iq-sass")
  sass::sass_cache_set_dir(sass_cache_dir, sass::sass_file_cache(sass_cache_dir))
}

set.seed(42)

make_wind_data <- function(days = 120, turbines = 32) {
  timestamps <- seq.POSIXt(
    from = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    by = "hour",
    length.out = days * 24
  )

  turbine_meta <- tibble(
    turbine_id = sprintf("WT-%02d", seq_len(turbines)),
    zone = rep(c("North Ridge", "South Field", "Harbor Edge", "Valley Line"), length.out = turbines),
    capacity_mw = sample(c(2.5, 3.0, 3.6, 4.2), turbines, replace = TRUE),
    install_year = sample(2018:2025, turbines, replace = TRUE),
    service_risk = runif(turbines, 0.8, 1.25)
  )

  base <- tidyr::crossing(timestamp = timestamps, turbine_id = turbine_meta$turbine_id) %>%
    left_join(turbine_meta, by = "turbine_id") %>%
    mutate(
      hour = as.numeric(format(timestamp, "%H")),
      day_index = as.numeric(difftime(timestamp, min(timestamp), units = "days")),
      season_wave = sin(2 * pi * day_index / 35),
      diurnal_wave = sin(2 * pi * (hour - 5) / 24),
      wind_speed_ms = pmax(0.5, rnorm(n(), 8.8 + 1.4 * season_wave + 0.7 * diurnal_wave, 2.1)),
      temperature_c = rnorm(n(), 9 + 5 * sin(2 * pi * day_index / 90), 3),
      vibration_mm_s = pmax(0.3, rnorm(n(), 2.2 + 0.055 * wind_speed_ms + service_risk * 0.22, 0.42)),
      gearbox_temp_c = pmax(20, rnorm(n(), 42 + 1.7 * wind_speed_ms + 0.55 * temperature_c, 4.2)),
      yaw_error_deg = abs(rnorm(n(), 4.8, 3.2)),
      availability = pmin(1, pmax(0.72, rnorm(n(), 0.972 - 0.003 * service_risk, 0.021))),
      power_curve = pmin(capacity_mw, capacity_mw * (wind_speed_ms / 12)^3),
      power_mw = pmax(0, power_curve * availability * pmax(0.62, 1 - yaw_error_deg / 60) + rnorm(n(), 0, 0.12)),
      revenue_eur = power_mw * 1000 * runif(n(), 77, 118),
      maintenance_cost_eur = pmax(0, rnorm(n(), 18 + vibration_mm_s * 5.5 + gearbox_temp_c * 0.12, 7))
    )

  anomaly_rows <- sample(seq_len(nrow(base)), size = round(nrow(base) * 0.018))
  base$vibration_mm_s[anomaly_rows] <- base$vibration_mm_s[anomaly_rows] * runif(length(anomaly_rows), 1.9, 3.1)
  base$gearbox_temp_c[anomaly_rows] <- base$gearbox_temp_c[anomaly_rows] + runif(length(anomaly_rows), 18, 35)
  base$availability[anomaly_rows] <- pmax(0.6, base$availability[anomaly_rows] - runif(length(anomaly_rows), 0.05, 0.18))
  base$power_mw[anomaly_rows] <- base$power_mw[anomaly_rows] * runif(length(anomaly_rows), 0.55, 0.82)

  base %>%
    mutate(
      energy_mwh = power_mw,
      health_score = pmax(0, pmin(100, 100 - vibration_mm_s * 8 - pmax(0, gearbox_temp_c - 70) * 0.9 - yaw_error_deg * 0.9)),
      status = case_when(
        health_score < 54 ~ "Critical",
        health_score < 72 ~ "Watch",
        TRUE ~ "Healthy"
      )
    )
}

wind_data <- make_wind_data()

fit_power_model <- lm(
  power_mw ~ wind_speed_ms + I(wind_speed_ms^2) + I(wind_speed_ms^3) +
    temperature_c + yaw_error_deg + availability + capacity_mw,
  data = wind_data
)

anomaly_model <- kmeans(
  scale(wind_data[, c("wind_speed_ms", "power_mw", "vibration_mm_s", "gearbox_temp_c", "yaw_error_deg")]),
  centers = 4,
  nstart = 20,
  iter.max = 100,
  algorithm = "MacQueen"
)

cluster_summary <- wind_data %>%
  mutate(cluster = factor(anomaly_model$cluster)) %>%
  group_by(cluster) %>%
  summarise(
    avg_health = mean(health_score),
    avg_vibration = mean(vibration_mm_s),
    avg_temp = mean(gearbox_temp_c),
    avg_power = mean(power_mw),
    .groups = "drop"
  ) %>%
  arrange(avg_health)

risky_cluster <- as.character(cluster_summary$cluster[1])

wind_data <- wind_data %>%
  mutate(
    predicted_power_mw = pmax(0, predict(fit_power_model, newdata = wind_data)),
    residual_mw = power_mw - predicted_power_mw,
    cluster = factor(anomaly_model$cluster),
    ml_flag = if_else(cluster == risky_cluster | residual_mw < quantile(residual_mw, 0.03), "ML Alert", "Normal"),
    failure_risk = pmax(1, pmin(99, 100 - health_score + if_else(ml_flag == "ML Alert", 22, 0)))
  )

theme_set(theme_minimal(base_family = "Arial"))

metric_card <- function(title, value, subtext, accent = "#0f766e") {
  div(
    class = "metric-card",
    style = paste0("border-left-color:", accent, ";"),
    div(class = "metric-title", title),
    div(class = "metric-value", value),
    div(class = "metric-subtext", subtext)
  )
}

ui <- page_navbar(
  title = div(class = "brand-title", "AeroFleet IQ"),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0f766e",
    secondary = "#334155",
    success = "#16a34a",
    danger = "#dc2626",
    base_font = font_collection("Inter", "Arial", "sans-serif"),
    heading_font = font_collection("Inter", "Arial", "sans-serif")
  ),
  header = tags$head(
    tags$style(HTML("
      body { background: #eef4f2; }
      .navbar { box-shadow: 0 8px 26px rgba(15, 23, 42, 0.09); }
      .brand-title { font-weight: 800; letter-spacing: 0; }
      .hero-band {
        background: linear-gradient(135deg, #0f766e 0%, #114e60 52%, #243b53 100%);
        color: white;
        padding: 22px 28px;
        border-radius: 8px;
        margin: 18px 0 16px;
        min-height: 152px;
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 18px;
      }
      .hero-band h1 { font-size: 28px; margin: 0 0 8px; font-weight: 800; }
      .hero-band p { margin: 0; max-width: 760px; color: rgba(255,255,255,0.86); }
      .hero-pill { background: rgba(255,255,255,0.15); padding: 12px 14px; border-radius: 8px; min-width: 180px; }
      .hero-pill strong { display: block; font-size: 24px; }
      .metric-grid { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 14px; margin-bottom: 16px; }
      .metric-card {
        background: #ffffff;
        border: 1px solid #dce7e4;
        border-left: 5px solid #0f766e;
        border-radius: 8px;
        padding: 15px 16px;
        min-height: 118px;
        box-shadow: 0 10px 24px rgba(15, 23, 42, 0.05);
      }
      .metric-title { color: #64748b; font-size: 12px; text-transform: uppercase; font-weight: 800; }
      .metric-value { color: #102a43; font-size: 26px; line-height: 1.15; font-weight: 850; margin-top: 8px; }
      .metric-subtext { color: #64748b; font-size: 13px; margin-top: 6px; }
      .panel {
        background: #ffffff;
        border: 1px solid #dce7e4;
        border-radius: 8px;
        padding: 16px;
        box-shadow: 0 10px 24px rgba(15, 23, 42, 0.05);
        margin-bottom: 16px;
      }
      .panel h3 { font-size: 16px; font-weight: 800; margin: 0 0 10px; color: #102a43; }
      .control-panel { background: #ffffff; border: 1px solid #dce7e4; border-radius: 8px; padding: 14px; margin-bottom: 16px; }
      .shiny-input-container { width: 100%; }
      .risk-badge { border-radius: 999px; padding: 5px 9px; font-weight: 800; font-size: 12px; }
      @media (max-width: 950px) {
        .metric-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        .hero-band { align-items: flex-start; flex-direction: column; }
      }
      @media (max-width: 620px) {
        .metric-grid { grid-template-columns: 1fr; }
        .hero-band h1 { font-size: 22px; }
        .metric-value { font-size: 22px; }
      }
    "))
  ),
  nav_panel(
    "Executive Overview",
    layout_sidebar(
      sidebar = sidebar(
        class = "control-panel",
        dateRangeInput(
          "date_range",
          "Reporting window",
          start = as.Date(max(wind_data$timestamp)) - 30,
          end = as.Date(max(wind_data$timestamp)),
          min = as.Date(min(wind_data$timestamp)),
          max = as.Date(max(wind_data$timestamp))
        ),
        selectInput("zone", "Farm zone", choices = c("All", sort(unique(wind_data$zone))), selected = "All"),
        sliderInput("price", "Scenario power price, EUR/MWh", min = 60, max = 150, value = 96, step = 2),
        checkboxInput("alerts_only", "Show only ML alerts in tables", FALSE)
      ),
      div(
        class = "hero-band",
        div(
          h1("Wind farm performance intelligence for SaaS buyers"),
          p("Synthetic portfolio data demonstrates live production monitoring, revenue analytics, machine-learning fault detection, and renewal-ready operational reporting.")
        ),
        div(class = "hero-pill", span("Fleet uptime"), strong(textOutput("hero_uptime", inline = TRUE)))
      ),
      uiOutput("kpi_cards"),
      layout_columns(
        col_widths = c(7, 5),
        div(class = "panel", h3("Energy and revenue trend"), plotlyOutput("energy_trend", height = "360px")),
        div(class = "panel", h3("Zone performance"), plotlyOutput("zone_plot", height = "360px"))
      ),
      div(class = "panel", h3("Priority turbines"), DTOutput("priority_table"))
    )
  ),
  nav_panel(
    "ML Command Center",
    layout_columns(
      col_widths = c(4, 8),
      div(
        class = "panel",
        h3("Model controls"),
        selectInput("ml_turbine", "Inspect turbine", choices = sort(unique(wind_data$turbine_id)), selected = "WT-01"),
        sliderInput("wind_scenario", "Forecast wind speed, m/s", min = 2, max = 18, value = 10, step = 0.5),
        sliderInput("yaw_scenario", "Yaw error, degrees", min = 0, max = 22, value = 5, step = 1),
        sliderInput("temp_scenario", "Temperature, C", min = -5, max = 28, value = 10, step = 1),
        uiOutput("forecast_card")
      ),
      div(class = "panel", h3("Actual vs ML expected power"), plotlyOutput("ml_scatter", height = "440px"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      div(class = "panel", h3("Failure-risk ranking"), plotlyOutput("risk_plot", height = "360px")),
      div(class = "panel", h3("Anomaly clusters"), plotlyOutput("cluster_plot", height = "360px"))
    )
  ),
  nav_panel(
    "Commercial Story",
    layout_columns(
      col_widths = c(6, 6),
      div(class = "panel", h3("SaaS value model"), plotlyOutput("value_plot", height = "380px")),
      div(
        class = "panel",
        h3("Client-ready product modules"),
        tags$ul(
          tags$li("Fleet health scoring with ML anomaly detection."),
          tags$li("Power-curve forecasting for revenue and contract planning."),
          tags$li("Maintenance triage to reduce downtime and technician dispatch waste."),
          tags$li("Executive dashboards for operators, investors, and asset managers.")
        ),
        tags$hr(),
        h3("Synthetic demo assumptions"),
        tags$p("The demo simulates 32 turbines over 120 days with weather, operating, revenue, maintenance, and condition-monitoring signals.")
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$date_range)
    data <- wind_data %>%
      filter(as.Date(timestamp) >= input$date_range[1], as.Date(timestamp) <= input$date_range[2])

    if (input$zone != "All") {
      data <- data %>% filter(zone == input$zone)
    }

    data
  })

  latest_by_turbine <- reactive({
    filtered_data() %>%
      group_by(turbine_id, zone, capacity_mw) %>%
      summarise(
        energy_mwh = sum(energy_mwh),
        revenue_eur = sum(energy_mwh) * input$price,
        availability = mean(availability),
        health_score = mean(health_score),
        failure_risk = mean(failure_risk),
        ml_alerts = sum(ml_flag == "ML Alert"),
        .groups = "drop"
      ) %>%
      arrange(desc(failure_risk))
  })

  output$hero_uptime <- renderText({
    paste0(round(mean(filtered_data()$availability) * 100, 1), "%")
  })

  output$kpi_cards <- renderUI({
    data <- filtered_data()
    fleet <- latest_by_turbine()
    div(
      class = "metric-grid",
      metric_card("Energy generated", paste0(format(round(sum(data$energy_mwh)), big.mark = ","), " MWh"), "Across selected reporting window", "#0f766e"),
      metric_card("Revenue scenario", paste0("EUR ", format(round(sum(data$energy_mwh) * input$price), big.mark = ",")), "Uses selected EUR/MWh assumption", "#2563eb"),
      metric_card("ML alerts", format(sum(data$ml_flag == "ML Alert"), big.mark = ","), "Detected by clustering and power residuals", "#dc2626"),
      metric_card("Mean health score", paste0(round(mean(fleet$health_score), 1), "/100"), "Condition signal across active turbines", "#7c3aed")
    )
  })

  output$energy_trend <- renderPlotly({
    trend <- filtered_data() %>%
      mutate(date = as.Date(timestamp)) %>%
      group_by(date) %>%
      summarise(energy_mwh = sum(energy_mwh), revenue_eur = energy_mwh * input$price, .groups = "drop")

    ggplotly(
      ggplot(trend, aes(date, energy_mwh)) +
        geom_area(fill = "#99f6e4", alpha = 0.65) +
        geom_line(color = "#0f766e", linewidth = 1) +
        labs(x = NULL, y = "MWh") +
        theme(panel.grid.minor = element_blank()),
      tooltip = c("x", "y")
    )
  })

  output$zone_plot <- renderPlotly({
    zone_summary <- filtered_data() %>%
      group_by(zone) %>%
      summarise(energy_mwh = sum(energy_mwh), availability = mean(availability), alerts = sum(ml_flag == "ML Alert"), .groups = "drop")

    ggplotly(
      ggplot(zone_summary, aes(reorder(zone, energy_mwh), energy_mwh, fill = availability, text = paste("ML alerts:", alerts))) +
        geom_col(width = 0.68) +
        coord_flip() +
        scale_fill_gradient(low = "#f97316", high = "#0f766e", labels = scales::percent) +
        labs(x = NULL, y = "MWh", fill = "Availability") +
        theme(panel.grid.minor = element_blank()),
      tooltip = c("x", "y", "text")
    )
  })

  output$priority_table <- renderDT({
    table_data <- latest_by_turbine() %>%
      mutate(
        availability = scales::percent(availability, accuracy = 0.1),
        health_score = round(health_score, 1),
        failure_risk = paste0(round(failure_risk), "%"),
        revenue_eur = paste0("EUR ", format(round(revenue_eur), big.mark = ","))
      )

    if (input$alerts_only) {
      table_data <- table_data %>% filter(ml_alerts > 0)
    }

    datatable(
      table_data,
      rownames = FALSE,
      options = list(pageLength = 8, dom = "tip", scrollX = TRUE),
      colnames = c("Turbine", "Zone", "Capacity MW", "Energy MWh", "Revenue", "Availability", "Health", "Risk", "ML Alerts")
    )
  })

  output$forecast_card <- renderUI({
    turbine_row <- wind_data %>% filter(turbine_id == input$ml_turbine) %>% slice_tail(n = 1)
    scenario <- turbine_row %>%
      mutate(
        wind_speed_ms = input$wind_scenario,
        yaw_error_deg = input$yaw_scenario,
        temperature_c = input$temp_scenario,
        availability = 0.97
      )
    predicted <- pmax(0, predict(fit_power_model, newdata = scenario))
    annual_mwh <- predicted * 24 * 365
    div(
      class = "metric-card",
      style = "border-left-color:#2563eb;",
      div(class = "metric-title", "ML power forecast"),
      div(class = "metric-value", paste0(round(predicted, 2), " MW")),
      div(class = "metric-subtext", paste0("Annualized scenario: ", format(round(annual_mwh), big.mark = ","), " MWh"))
    )
  })

  output$ml_scatter <- renderPlotly({
    data <- filtered_data()
    ggplotly(
      ggplot(data, aes(predicted_power_mw, power_mw, color = ml_flag, text = turbine_id)) +
        geom_point(alpha = 0.45, size = 1.8) +
        geom_abline(slope = 1, intercept = 0, color = "#334155", linetype = "dashed") +
        scale_color_manual(values = c("ML Alert" = "#dc2626", "Normal" = "#0f766e")) +
        labs(x = "ML expected power (MW)", y = "Actual power (MW)", color = NULL) +
        theme(panel.grid.minor = element_blank()),
      tooltip = c("text", "x", "y", "color")
    )
  })

  output$risk_plot <- renderPlotly({
    ranked <- latest_by_turbine() %>% slice_max(failure_risk, n = 12)
    ggplotly(
      ggplot(ranked, aes(reorder(turbine_id, failure_risk), failure_risk, fill = health_score)) +
        geom_col(width = 0.7) +
        coord_flip() +
        scale_fill_gradient(low = "#dc2626", high = "#16a34a") +
        labs(x = NULL, y = "Failure risk score", fill = "Health") +
        theme(panel.grid.minor = element_blank()),
      tooltip = c("x", "y", "fill")
    )
  })

  output$cluster_plot <- renderPlotly({
    sampled <- filtered_data() %>% sample_n(min(2500, nrow(.)))
    ggplotly(
      ggplot(sampled, aes(vibration_mm_s, gearbox_temp_c, color = cluster, text = paste(turbine_id, ml_flag))) +
        geom_point(alpha = 0.55, size = 1.7) +
        labs(x = "Vibration mm/s", y = "Gearbox temp C", color = "Cluster") +
        theme(panel.grid.minor = element_blank()),
      tooltip = c("text", "x", "y", "color")
    )
  })

  output$value_plot <- renderPlotly({
    data <- filtered_data()
    baseline_loss <- sum(data$energy_mwh) * input$price * 0.052
    recovered <- baseline_loss * c(0.25, 0.46, 0.64, 0.72)
    value <- tibble(
      module = c("Monitoring", "ML Alerts", "Forecasting", "Maintenance AI"),
      recovered_revenue = recovered,
      margin = recovered * c(0.18, 0.24, 0.31, 0.36)
    )

    ggplotly(
      ggplot(value, aes(module, recovered_revenue, fill = margin)) +
        geom_col(width = 0.62) +
        scale_y_continuous(labels = scales::label_currency(prefix = "EUR ")) +
        scale_fill_gradient(low = "#38bdf8", high = "#0f766e", labels = scales::label_currency(prefix = "EUR ")) +
        labs(x = NULL, y = "Recovered value", fill = "SaaS margin") +
        theme(panel.grid.minor = element_blank()),
      tooltip = c("x", "y", "fill")
    )
  })
}

shinyApp(ui, server)
