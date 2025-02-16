# R/key_metrics_validation.R

#' Key Metrics Validation Functions
#' 
#' A collection of functions to validate and check key business metrics
#' in the supply chain dashboard.
#' 
#' @details
#' These functions ensure data quality and business logic for:
#'   * Cost calculations
#'   * Shipping metrics
#'   * Warehouse performance indicators
#' 
#' @examples
#' validate_shipping_metrics(shipping_data)
#' validate_weight_bands(weight_data)

#' Validate Shipping Metrics
#' 
#' @param data Data frame containing shipping metrics
#' @return Data frame with validated metrics
#' @details
#' Checks for:
#'   * Positive costs and weights
#'   * Valid carrier codes
#'   * Consistent route information

# Add assertthat to dependencies
if (!require("assertthat")) install.packages("assertthat")
library(assertthat)

validate_shipping_metrics <- function(data) {
  # Critical validations that should stop execution
  assert_that(
    !any(data$total_weight <= 0, na.rm = TRUE),
    msg = "Critical Error: Found records with zero or negative weight"
  )
  
  assert_that(
    !any(data$total_shipping_cost < 0, na.rm = TRUE),
    msg = "Critical Error: Found records with negative shipping costs"
  )
  
  assert_that(
    !any(is.infinite(data$avg_cost_per_kg)),
    msg = "Critical Error: Found infinite values in cost per kg calculations"
  )
  
  # Warnings for potential issues that don't stop execution
  # Check for unreasonable values
  metrics <- list(
    cost_per_kg = list(
      metric = data$avg_cost_per_kg,
      min = 0.01,    # Minimum reasonable cost per kg
      max = 1000,    # Maximum reasonable cost per kg
      name = "Average cost per kg"
    ),
    total_cost = list(
      metric = data$total_shipping_cost,
      min = 0,       # Minimum reasonable total cost
      max = 1000000, # Maximum reasonable total cost
      name = "Total shipping cost"
    )
  )
  
  # Check each metric
  lapply(metrics, function(x) {
    outliers <- x$metric[x$metric < x$min | x$metric > x$max]
    if (length(outliers) > 0) {
      warning(sprintf(
        "Found %d outliers in %s (range: %.2f to %.2f)",
        length(outliers), x$name,
        min(outliers, na.rm = TRUE),
        max(outliers, na.rm = TRUE)
      ))
    }
  })
  
  # Check for missing values in key columns
  key_cols <- c("total_weight", "total_shipping_cost", "avg_cost_per_kg")
  missing_values <- colSums(is.na(data[key_cols]))
  if (any(missing_values > 0)) {
    warning(sprintf(
      "Found missing values in key columns: %s",
      paste(names(missing_values[missing_values > 0]), collapse = ", ")
    ))
  }
  
  return(invisible(data))
}

check_data_quality <- function(data, group_cols) {
  # Check for required columns
  assert_that(
    all(group_cols %in% colnames(data)),
    msg = sprintf("Missing required columns: %s", 
                  paste(setdiff(group_cols, colnames(data)), collapse = ", "))
  )
  
  # Check for empty dataset
  assert_that(
    nrow(data) > 0,
    msg = "Dataset is empty"
  )
  
  # Check for duplicate records - include weight_band if it exists
  check_cols <- group_cols
  if ("weight_band" %in% colnames(data)) {
    check_cols <- c(check_cols, "weight_band")
  }
  
  dupes <- data |>
    group_by(across(all_of(check_cols))) |>
    filter(n() > 1) |>
    ungroup()
  
  if (nrow(dupes) > 0) {
    warning(sprintf(
      "Found %d duplicate records for grouping: %s",
      nrow(dupes),
      paste(check_cols, collapse = ", ")
    ))
  }
  
  # Basic data type validations
  if ("total_weight" %in% colnames(data)) {
    assert_that(
      is.numeric(data$total_weight),
      msg = "total_weight must be numeric"
    )
  }
  
  if ("total_shipping_cost" %in% colnames(data)) {
    assert_that(
      is.numeric(data$total_shipping_cost),
      msg = "total_shipping_cost must be numeric"
    )
  }
  
  return(invisible(data))
}

# New function for service level specific validations
validate_service_levels <- function(data) {
  # Check for valid service level codes
  valid_service_levels <- c("CRF", "DTD", "DTP")
  
  assert_that(
    all(data$service_level %in% valid_service_levels, na.rm = TRUE),
    msg = sprintf(
      "Invalid service levels found. Valid levels are: %s",
      paste(valid_service_levels, collapse = ", ")
    )
  )
  
  # Check for complete route information
  assert_that(
    !any(is.na(data$origin_port) | is.na(data$destination_port)),
    msg = "Missing origin or destination port information"
  )
  
  return(invisible(data))
}