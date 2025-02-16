# R/utils/data_prep.R 

#' Load and Prepare Supply Chain Data
#' 
#' Reads supply chain data from Excel file and prepares it for dashboard use.
#' Includes data cleaning, formatting, and initial validation.
#' 
#' @return A list containing:
#'   \item{data_list}{Raw data from Excel sheets}
#'   \item{clean_freight_rates}{Cleaned freight rates data}
#'   \item{shipping_costs}{Calculated shipping costs}
#'   \item{warehouse_capacity}{Warehouse capacity metrics}
#'   \item{warehouse_costs}{Calculated warehouse costs}
#' @examples
#' data <- load_supply_chain_data()
#' shipping_costs <- data$shipping_costs
load_supply_chain_data <- function() {
  # Add error checking for file
  file_path <- "data/01_raw_data/rwfd_supply_chain.xlsx"
  assertthat::assert_that(
    file.exists(file_path),
    msg = "Data file not found: Please ensure rwfd_supply_chain.xlsx is in data/01_raw_data/"
  )
  
  # Wrap all operations in tryCatch
  tryCatch({
    original_names <- excel_sheets(file_path)
    snake_names <- make_clean_names(original_names)
    
    # Validate required sheets
    expected_sheets <- c("order_list", "freight_rates", "wh_costs", "wh_capacities")
    actual_sheets <- make_clean_names(original_names)
    
    assertthat::assert_that(
      all(expected_sheets %in% actual_sheets),
      msg = paste("Missing required sheets:", 
                  paste(setdiff(expected_sheets, actual_sheets), collapse = ", "))
    )
    
    # Load data
    data_list <- original_names |>
      set_names(snake_names) |>
      map(~ read_excel(
        path = file_path,
        sheet = .x
      ) |> clean_names())
    
    # Clean freight rates
    clean_freight_rates <- data_list$freight_rates |> distinct()
    
    # Calculate shipping costs
    shipping_costs <- calculate_shipping_costs(data_list, clean_freight_rates)
    
    # Calculate warehouse capacity
    warehouse_capacity <- calculate_warehouse_capacity(data_list)
    
    # Calculate warehouse costs
    warehouse_costs <- data_list$order_list |>
      group_by(plant_code) |>
      summarise(
        total_orders = n(),
        total_weight = sum(weight),
        total_units = sum(unit_quantity)
      ) |>
      left_join(data_list$wh_costs, by = c("plant_code" = "wh")) |>
      mutate(
        storage_cost = total_units * cost_unit,
        cost_per_unit = storage_cost / total_units,
        cost_per_kg = storage_cost / total_weight
      )
    
    # Return final list
    list(
      data_list = data_list,
      clean_freight_rates = clean_freight_rates,
      shipping_costs = shipping_costs,
      warehouse_capacity = warehouse_capacity,
      warehouse_costs = warehouse_costs
    )
    
  }, error = function(e) {
    stop(paste("Error loading data:", e$message))
  })
}

# Helper functions

#' Calculate Shipping Costs
#' 
#' Calculates total shipping costs based on order data and freight rates.
#' 
#' @param data_list List containing order and rate data
#' @param clean_freight_rates Cleaned freight rate data frame
#' @return A data frame with calculated shipping costs per route
#' @examples
#' shipping_costs <- calculate_shipping_costs(data_list, clean_freight_rates)
calculate_shipping_costs <- function(data_list, clean_freight_rates) {
  # Input validation
  assertthat::assert_that(
    !is.null(data_list$order_list),
    !is.null(clean_freight_rates),
    all(c("carrier", "origin_port", "destination_port", "service_level", "weight") %in% names(data_list$order_list)),
    all(c("orig_port_cd", "dest_port_cd", "svc_cd", "minm_wgh_qty", "max_wgh_qty", "rate") %in% names(clean_freight_rates)),
    msg = "Missing required data or columns for shipping cost calculation"
  )
  
  data_list$order_list |>
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
    ) |> 
    check_data_quality(group_cols = c("origin_port", "destination_port", "carrier")) |>
    normalize_weight_bands() |> 
    validate_weight_bands() |>
    validate_shipping_metrics() |> 
    arrange(origin_port, destination_port, carrier, weight_band) 
  
}

#' Calculate Warehouse Capacity Metrics
#' 
#' Calculates utilization metrics for each warehouse based on order volume
#' and capacity data.
#' 
#' @param data_list List containing order data and warehouse capacity information
#' @return A data frame with warehouse capacity utilization metrics:
#'   \item{plant_code}{Warehouse identifier}
#'   \item{n}{Number of orders processed}
#'   \item{daily_capacity}{Maximum daily order capacity}
#'   \item{capacity_utilization}{Current utilization ratio}
#' @examples
#' warehouse_metrics <- calculate_warehouse_capacity(data_list)
calculate_warehouse_capacity <- function(data_list) {
  # Input validation
  assertthat::assert_that(
    !is.null(data_list$order_list),
    !is.null(data_list$wh_capacities),
    all(c("plant_code") %in% names(data_list$order_list)),
    all(c("plant_id", "daily_capacity") %in% names(data_list$wh_capacities)),
    msg = "Missing required data or columns for warehouse capacity calculation"
  )
  
  data_list$order_list |>
    count(plant_code) |>
    left_join(data_list$wh_capacities, by = c("plant_code" = "plant_id")) |>
    mutate(capacity_utilization = n / daily_capacity)
}


#' Calculate Warehouse Storage Costs
#' 
#' Calculates storage costs and related metrics for each warehouse
#' based on unit volume and cost rates.
#' 
#' @param data_list List containing order and warehouse cost data
#' @return A data frame with warehouse cost metrics:
#'   \item{plant_code}{Warehouse identifier}
#'   \item{total_orders}{Total number of orders}
#'   \item{total_weight}{Total weight processed}
#'   \item{total_units}{Total units stored}
#'   \item{storage_cost}{Total storage cost}
#'   \item{cost_per_unit}{Cost per unit stored}
#'   \item{cost_per_kg}{Cost per kilogram}
#' @examples
#' storage_costs <- calculate_warehouse_costs(data_list)
calculate_warehouse_costs <- function(data_list) {
  # Input validation
  assertthat::assert_that(
    !is.null(data_list$order_list),
    !is.null(data_list$wh_costs),
    all(c("plant_code", "weight", "unit_quantity") %in% names(data_list$order_list)),
    all(c("wh", "cost_unit") %in% names(data_list$wh_costs)),
    msg = "Missing required data or columns for warehouse cost calculation"
  )
  data_list$order_list |>
    group_by(plant_code) |>
    summarise(
      total_orders = n(),
      total_weight = sum(weight),
      total_units = sum(unit_quantity)
    ) |>
    left_join(data_list$wh_costs, by = c("plant_code" = "wh")) |>
    mutate(
      storage_cost = total_units * cost_unit,
      cost_per_unit = storage_cost / total_units,
      cost_per_kg = storage_cost / total_weight
    )
}


#' Validate Order List Data
#' @param data Order list data frame
#' @return TRUE if valid, errors if not
validate_order_list <- function(data) {
  assertthat::assert_that(
    all(data$order_id > 0),
    all(!is.na(data$order_date)),
    all(data$weight > 0),
    all(data$unit_quantity > 0),
    all(data$ship_ahead_day_count >= 0),
    all(data$ship_late_day_count >= 0),
    all(data$service_level %in% c("CRF", "DTD", "DTP")),
    msg = "Order list validation failed - check data requirements"
  )
}

#' Validate Freight Rates Data
#' @param data Freight rates data frame
#' @return TRUE if valid, errors if not
validate_freight_rates <- function(data) {
  assertthat::assert_that(
    all(data$rate > 0),
    all(data$minimum_cost >= 0),
    all(data$max_wgh_qty > data$minm_wgh_qty),
    all(data$mode_dsc %in% c("AIR", "GROUND")),
    all(data$svc_cd %in% c("CRF", "DTD", "DTP")),
    msg = "Freight rates validation failed - check rate values and service codes"
  )
}