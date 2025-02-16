# R/service_level_analysis.R

#' Service Level Analysis Functions
#' 
#' Functions to analyze and validate service level performance metrics
#' across different shipping modes and carriers.
#' 
#' @details 
#' Provides functionality for:
#'   * Service level performance calculations
#'   * Carrier performance analysis
#'   * Service type comparisons
#' 
#' @examples
#' analyze_service_levels(order_data)
#' calculate_service_performance(shipping_data)

#' Analyze Service Levels
#' 
#' @param data Data frame containing order and service level information
#' @return Data frame with service level metrics
#' @details
#' Calculates:
#'   * Service level distribution
#'   * Performance by carrier
#'   * Time-based metrics

# Service Level Analysis Functions
library(assertthat)  
library(dplyr)
library(tidyr)

analyze_service_levels <- function(data) {
  # Input validation
  assert_that(
    is.data.frame(data),
    msg = "Input must be a data frame"
  )
  
  required_cols <- c("service_level", "weight", "carrier", 
                     "origin_port", "destination_port")
  assert_that(
    all(required_cols %in% names(data)),
    msg = sprintf("Missing required columns: %s",
                  paste(setdiff(required_cols, names(data)), collapse = ", "))
  )
  
  # Basic service level summary
  service_summary <- data |>
    group_by(order_id) |>
    slice(1) |>
    ungroup() |>
    group_by(service_level) |>
    summarise(
      orders = n(),
      total_weight = sum(weight),
      total_cost = sum(shipping_cost, na.rm = TRUE),
      avg_weight_per_order = mean(weight),
      .groups = "drop"
    ) |>
    mutate(
      # Cost metrics - keep NA for CRF
      avg_cost_per_order = if_else(
        service_level == "CRF",
        NA_real_,
        total_cost / orders
      ),
      avg_cost_per_kg = if_else(
        service_level == "CRF",
        NA_real_,
        total_cost / total_weight
      ),
      # Percentage metrics - use 0 for CRF
      pct_orders = orders / sum(orders) * 100,
      pct_weight = total_weight / sum(total_weight) * 100,
      pct_cost = if_else(
        service_level == "CRF",
        0,  # Use 0 instead of NA for percentages
        total_cost / sum(total_cost[service_level != "CRF"]) * 100
      )
    )
  
  # Carrier service analysis
  carrier_service_summary <- data |>
    group_by(order_id) |>
    slice(1) |>
    ungroup() |>
    group_by(carrier, service_level) |>
    summarise(
      orders = n(),
      total_weight = sum(weight),
      total_cost = sum(shipping_cost, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      # Keep NA for cost metrics
      avg_cost_per_kg = if_else(
        service_level == "CRF",
        NA_real_,
        total_cost / total_weight
      )
    ) |>
    group_by(carrier) |>
    mutate(
      # Use 0 for percentage calculations
      pct_carrier_orders = orders / sum(orders) * 100,
      pct_carrier_weight = total_weight / sum(total_weight) * 100,
      pct_carrier_cost = if_else(
        service_level == "CRF",
        0,
        total_cost / sum(total_cost[service_level != "CRF"]) * 100
      )
    ) |>
    ungroup()
  
  # Route service analysis
  route_service_summary <- data |>
    group_by(order_id) |>
    slice(1) |>
    ungroup() |>
    group_by(origin_port, destination_port, service_level) |>
    summarise(
      orders = n(),
      total_weight = sum(weight),
      total_cost = sum(shipping_cost, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      avg_cost_per_kg = if_else(
        service_level == "CRF",
        NA_real_,
        total_cost / total_weight
      ),
      route = glue::glue("{origin_port} → {destination_port}"),
      pct_cost = if_else(
        service_level == "CRF",
        0,
        total_cost / sum(total_cost[service_level != "CRF"]) * 100
      )
    )
  
  list(
    service_summary = service_summary,
    carrier_service = carrier_service_summary,
    route_service = route_service_summary
  )
}