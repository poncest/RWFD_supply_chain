# app.R

# Setup ------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, shiny, bslib, DT, scales, glue
)

source("R/weight_band_normalization.R")
source("R/key_metrics_validation.R")
source('R/service_level_analysis.R')



# Load Data ------------------------------------------------------------------
original_names <- excel_sheets("data/01_raw_data/rwfd_supply_chain.xlsx")
snake_names <- make_clean_names(original_names)
data_list <- original_names |>
  set_names(snake_names) |>
  map(~ read_excel(
    path = "data/01_raw_data/rwfd_supply_chain.xlsx",
    sheet = .x
  ) |> clean_names())

# Clean freight rates to remove duplicates
clean_freight_rates <- data_list$freight_rates |> distinct()

# Calculate shipping costs
shipping_costs <- data_list$order_list |>
  left_join(clean_freight_rates,
            by = c("carrier",
                   "origin_port" = "orig_port_cd",
                   "destination_port" = "dest_port_cd",
                   "service_level" = "svc_cd"
            ),
            relationship = "many-to-many"
  ) |>
  group_by(order_id) |>
  filter(
    weight >= minm_wgh_qty,
    weight <= max_wgh_qty
  ) |>
  slice_min(abs(weight - minm_wgh_qty), n = 1) |>
  ungroup() |>
  mutate(
    shipping_cost = pmax(minimum_cost, weight * rate),
    weight_band = paste(minm_wgh_qty, "-", max_wgh_qty, "kg")
  ) |>
  group_by(origin_port, destination_port, carrier, weight_band, mode_dsc) |>
  summarise(
    total_shipping_cost = sum(shipping_cost),
    total_weight = sum(weight),
    n_orders = n(),
    avg_cost_per_kg = total_shipping_cost / total_weight,
    .groups = "drop"
  )

# Calculate warehouse capacity
warehouse_capacity <- data_list$order_list |>
  count(plant_code) |>
  left_join(data_list$wh_capacities, by = c("plant_code" = "plant_id")) |>
  mutate(capacity_utilization = n / daily_capacity)



# UI Definition ----------------------------------------------------------------
ui <- page_navbar(
  theme = bs_theme(
    version = 5,
    bootswatch = "litera",
    primary = "#2C3E50",
    "navbar-bg" = "#f8f9fa"
  ),
  
  # Navigation title
  title = "Supply Chain Analytics Dashboard",
  bg = "#f8f9fa",
  
  # Navigation panels - each represents a tab
  nav_panel(
    "Overview",
    # Filters row
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        selectInput("warehouse", "Warehouse", choices = NULL)
      ),
      card(
        selectInput("carrier", "Carrier", choices = NULL)
      ),
      card(
        selectInput("service_level", "Service Level", choices = NULL)
      )
    ),
    
    # KPI Cards row
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Total Shipping Cost",
        value = textOutput("total_cost"),
        showcase = bsicons::bs_icon("currency-dollar"),
        theme = "primary"
      ),
      value_box(
        title = "Avg Cost per KG",
        value = textOutput("avg_cost_kg"),
        showcase = bsicons::bs_icon("graph-up"),
        theme = value_box_theme(bg = "#E74C3C")
      ),
      value_box(
        title = "Warehouse Utilization",
        value = textOutput("warehouse_util"),
        showcase = bsicons::bs_icon("building"),
        theme = value_box_theme(bg = "#F1C40F")
      ),
      value_box(
        title = "On-Time Delivery",
        value = textOutput("otd_rate"),
        showcase = bsicons::bs_icon("check-circle"),
        theme = value_box_theme(bg = "#2ECC71")
      )
    ),
    
    # Overview content
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Top Routes by Cost"),
        plotOutput("plot_routes")
      ),
      card(
        card_header("Storage Costs by Warehouse"),
        plotOutput("plot_storage")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Transport Mode Analysis"),
        plotOutput("plot_transport")
      ),
      card(
        card_header("Service Level Distribution"),
        plotOutput("plot_service")
      )
    )
  ),
  
  # Cost Analysis Tab
  nav_panel(
    "Cost Analysis",
    "Cost Analysis Content Here"
  ),
  
  # Capacity Tab
  nav_panel(
    "Capacity",
    "Capacity Content Here"
  ),
  
  # Service Levels Tab
  nav_panel(
    "Service Levels",
    "Service Levels Content Here"
  )
)

# Server logic -----------------------------------------------------------------
server <- function(input, output, session) {
  # Previous code remains the same...
  
  # Add visualization outputs
  output$plot_routes <- renderPlot({
    shipping_costs_summary |>
      group_by(origin_port, destination_port, carrier, mode_dsc) |>
      summarise(
        total_shipping_cost = sum(total_shipping_cost),
        n_orders = sum(n_orders),
        .groups = "drop"
      ) |>
      mutate(
        # Create cleaner route labels
        route_label = glue::glue("{origin_port} → {destination_port}\n{carrier}"),
        route_label = fct_reorder(route_label, total_shipping_cost),
        # Format costs and create detailed labels
        cost_label = scales::dollar(total_shipping_cost, accuracy = 1),
        label_text = glue::glue("{cost_label}\n({scales::comma(n_orders)} orders)")
      ) |>
      ggplot(aes(x = route_label, y = total_shipping_cost, fill = mode_dsc)) +
      geom_col() +
      geom_text(
        aes(label = label_text),
        hjust = -0.1,
        size = 3,
        color = text_col
      ) +
      coord_flip() +
      scale_y_continuous(
        labels = scales::dollar_format(),
        expand = expansion(mult = c(0, 0.25))
      ) +
      scale_fill_manual(
        values = setNames(palette_main[c(3, 4)], c("AIR", "GROUND"))
      ) +
      labs(
        title = "Top Routes by Cost",
        x = NULL,
        y = "Total Shipping Cost",
        fill = "Transport Mode"
      ) +
      theme_custom() +
      theme(
        legend.position = "top",
        panel.grid.major.y = element_blank()
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)

