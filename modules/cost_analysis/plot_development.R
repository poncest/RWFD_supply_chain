# modules/cost_analysis/plot_development.R

# Load required libraries
library(tidyverse)
library(scales)
library(ggiraph)

# Source data prep
source("R/utils/data_prep.R")

# Load data
data <- load_supply_chain_data()

# Create sample data structure
sample_data <- list(
  shipping_costs = data$shipping_costs,
  warehouse_costs = data$warehouse_costs
)

# 1. Cost Breakdown Plot
create_cost_breakdown_plot <- function(data) {
  # First map PORT to PLANT
  shipping_with_plant <- data$shipping_costs |>
    mutate(plant_code = str_replace(origin_port, "PORT", "PLANT"))
  
  # Now join with correct keys
  p <- shipping_with_plant |>
    left_join(data$warehouse_costs, by = "plant_code") |>
    group_by(plant_code) |>
    summarise(
      transport_cost = sum(total_shipping_cost),
      storage_cost = first(storage_cost),  # using first since it's the same for each plant
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = c(transport_cost, storage_cost),
      names_to = "cost_type",
      values_to = "amount"
    ) |>
    mutate(
      cost_type = str_to_title(str_replace(cost_type, "_", " ")),
      tooltip_text = glue::glue(
        "Location: {plant_code}\n",
        "Cost Type: {cost_type}\n",
        "Amount: {scales::dollar(amount)}"
      )
    ) |>
    ggplot(aes(x = plant_code, y = amount, fill = cost_type)) +
    geom_col_interactive(
      aes(tooltip = tooltip_text),
      position = "stack"
    ) +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
    labs(
      title = "Combined Transport and Storage Costs by Location",
      x = "Location",
      y = "Cost",
      fill = "Cost Type"
    ) +
    theme_minimal()
  
  return(p)
}

# Test the function and print data for debugging
create_cost_breakdown_plot(sample_data)


# 2. Unit Cost Analysis Plot
create_unit_cost_plot <- function(data) {
  p <- data$warehouse_costs |>
    mutate(
      tooltip_text = glue::glue(
        "Location: {plant_code}\n",
        "Cost per Unit: {scales::dollar(cost_per_unit)}\n",
        "Cost per KG: {scales::dollar(cost_per_kg)}\n",
        "Total Units: {scales::comma(total_units)}\n",
        "Total Orders: {scales::comma(total_orders)}"
      )
    ) |>
    ggplot(aes(x = cost_per_unit, y = cost_per_kg, size = total_units)) +
    geom_point_interactive(
      aes(tooltip = tooltip_text,
          data_id = plant_code,
          fill = total_orders),  # Color by number of orders
      alpha = 0.7,
      shape = 21  # Filled circle
    ) +
    scale_x_continuous(
      labels = scales::dollar_format(),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    scale_y_continuous(
      labels = scales::dollar_format(),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    scale_fill_viridis_c(option = "magma", begin = 0.2, end = 0.8) +
    scale_size_continuous(
      labels = scales::comma,
      breaks = scales::pretty_breaks(n = 4)
    ) +
    labs(
      title = "Unit Cost Analysis by Location",
      subtitle = "Cost per Unit vs Cost per KG, sized by volume",
      x = "Cost per Unit ($)",
      y = "Cost per KG ($)",
      size = "Total Units",
      fill = "Total Orders"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

create_unit_cost_plot(sample_data)

# 3. KPI Calculations
calculate_cost_kpis <- function(shipping_data, warehouse_data) {
  list(
    total_transport_cost = sum(shipping_data$total_shipping_cost),
    avg_cost_per_unit = mean(warehouse_data$cost_per_unit),
    air_ground_ratio = # calculation
  )
}

# Test plots
test_plots <- function() {
  # Test code here
}

# Example structure
cost_efficiency <- filtered_warehouse() |>
  select(plant_code, storage_cost, cost_per_unit, cost_per_kg) |>
  mutate(
    storage_efficiency = scale(storage_cost),
    unit_efficiency = scale(cost_per_unit),
    kg_efficiency = scale(cost_per_kg)
  ) |>
  pivot_longer(
    cols = ends_with("efficiency"),
    names_to = "metric",
    values_to = "score"
  )

# Example structure
carrier_costs <- filtered_shipping() |>
  group_by(carrier, mode_dsc) |>
  summarise(
    avg_cost = mean(total_shipping_cost),
    total_volume = sum(total_weight),
    cost_per_kg = sum(total_shipping_cost) / sum(total_weight)
  )
