# tests/testthat/test-edge-cases.R

library(testthat)
library(shiny)
library(dplyr)

# Source the server file
# source("modules/cost_analysis/cost_analysis_server.R")

# Test data with edge cases
edge_case_data <- list(
  shipping_costs = tibble(
    origin_port = c("PLANT01", "PLANT02", "PLANT03", "PLANT04"),
    destination_port = c("DEST01", "DEST02", "DEST03", "DEST04"),
    carrier = c("CARRIER1", "CARRIER2", "CARRIER1", "CARRIER2"),
    total_shipping_cost = c(1000, NA, 0, -100),            # Normal, NA, Zero, Negative
    total_weight = c(100, 0, NA, 50),                      # Normal, Zero, NA, Normal
    n_orders = c(10, 0, 5, NA),                            # Normal, Zero, Normal, NA
    mode_dsc = c("AIR", "GROUND", "AIR", "GROUND")
  ),
  warehouse_costs = tibble(
    plant_code = c("PLANT01", "PLANT02", "PLANT03", "PLANT04"),
    storage_cost = c(5000, NA, 0, -1000),                  # Normal, NA, Zero, Negative
    cost_per_unit = c(50, 0, NA, -10),                     # Normal, Zero, NA, Negative
    cost_per_kg = c(5, NA, 0, -1),                         # Normal, NA, Zero, Negative
    total_units = c(100, 0, NA, 50),                       # Normal, Zero, NA, Normal
    total_orders = c(50, NA, 0, 25)                        # Normal, NA, Zero, Normal
  ),
  data_list = list(
    order_list = tibble(
      plant_code = c("PLANT01", "PLANT02", "PLANT03", "PLANT04"),
      carrier = c("CARRIER1", "CARRIER2", "CARRIER1", "CARRIER2"),
      service_level = c("CRF", "DTD", "CRF", "DTD")
    )
  )
)

# Test 1: Missing Values Handling
test_that("Missing values are handled gracefully", {
  testServer(costAnalysisServer, args = list(data = edge_case_data), {
    # Test storage cost with NA values
    session$setInputs(warehouse = "PLANT02", carrier = "All")
    filtered <- filtered_warehouse()
    total_storage <- sum(filtered$storage_cost, na.rm = TRUE)
    expect_equal(total_storage, 0)  # NA values should be removed
    
    # Test avg cost per unit with NA values
    session$setInputs(warehouse = "PLANT03", carrier = "All")
    filtered <- filtered_warehouse()
    avg_cost <- mean(filtered$cost_per_unit, na.rm = TRUE)
    expect_true(is.na(avg_cost))  # Should be NA when all values are NA
  })
})

# Test 2: Zero Values Handling
test_that("Zero values are handled correctly", {
  testServer(costAnalysisServer, args = list(data = edge_case_data), {
    # Test zero storage cost
    session$setInputs(warehouse = "PLANT03", carrier = "All")
    filtered <- filtered_warehouse()
    total_storage <- sum(filtered$storage_cost, na.rm = TRUE)
    expect_equal(total_storage, 0)
    
    # Test zero shipping cost
    filtered <- filtered_shipping()
    ground_total <- sum(filtered$total_shipping_cost[filtered$mode_dsc == "GROUND"], na.rm = TRUE)
    total_cost <- sum(filtered$total_shipping_cost, na.rm = TRUE)
    ground_pct <- if(total_cost > 0) ground_total / total_cost * 100 else 0
    expect_equal(ground_pct, 0)
  })
})

# Test 3: Negative Values Validation
test_that("Negative values trigger appropriate warnings", {
  testServer(costAnalysisServer, args = list(data = edge_case_data), {
    # Test negative storage cost
    session$setInputs(warehouse = "PLANT04", carrier = "All")
    
    # Check if validation function catches negative values
    expect_warning(
      validate_plot_data(
        edge_case_data$warehouse_costs |> 
          filter(plant_code == "PLANT04"),
        c("storage_cost", "cost_per_unit"),
        "Cost Analysis"
      ),
      "Negative values found"
    )
  })
})

# Test 4: Empty Filter Results
test_that("Empty filter results are handled appropriately", {
  testServer(costAnalysisServer, args = list(data = edge_case_data), {
    # Test non-existent warehouse
    session$setInputs(warehouse = "PLANT99", carrier = "All")
    filtered <- filtered_warehouse()
    expect_equal(nrow(filtered), 0)
    
    # Test non-existent carrier
    session$setInputs(warehouse = "All", carrier = "CARRIER99")
    filtered <- filtered_shipping()
    expect_equal(nrow(filtered), 0)
  })
})

# Test 5: Data Validation Function
test_that("Data validation function works correctly", {
  testServer(costAnalysisServer, args = list(data = edge_case_data), {
    # Test complete data - should pass
    expect_true(
      validate_plot_data(
        edge_case_data$warehouse_costs,
        c("storage_cost", "cost_per_unit"),
        "Cost Analysis"
      )
    )
    
    # Test data with missing columns - should fail but not error
    incomplete_data <- tibble(
      plant_code = "PLANT01",
      random_column = 100
    )
    
    expect_false(
      validate_plot_data(
        incomplete_data,
        c("storage_cost", "cost_per_unit"),
        "Test Plot"
      )
    )
    
    # Test NA values - should give warning but not fail
    na_data <- edge_case_data$warehouse_costs %>%
      filter(is.na(storage_cost) | is.na(cost_per_unit))
    
    expect_warning(
      validate_plot_data(
        na_data,
        c("storage_cost", "cost_per_unit"),
        "Test Plot"
      ),
      "Found NA values"
    )
  })
})