# tests/testthat/test-calculations.R

library(testthat)
library(shiny)
library(dplyr)

# Source the server file
# source("modules/cost_analysis/cost_analysis_server.R")


# Test data setup with known values for easy calculation verification
test_data <- list(
  shipping_costs = tibble(
    origin_port = c("PLANT01", "PLANT01", "PLANT02"),
    destination_port = c("DEST01", "DEST02", "DEST01"),
    carrier = c("CARRIER1", "CARRIER1", "CARRIER2"),
    total_shipping_cost = c(1000, 2000, 3000),  # Simple numbers for easy verification
    total_weight = c(100, 200, 300),            # 10 cost/weight ratio for each
    n_orders = c(10, 20, 30),                   # 100 cost/order ratio for each
    mode_dsc = c("AIR", "GROUND", "AIR")
  ),
  warehouse_costs = tibble(
    plant_code = c("PLANT01", "PLANT02"),
    storage_cost = c(5000, 7000),               # Simple storage costs
    cost_per_unit = c(50, 70),                  # Unit costs
    cost_per_kg = c(5, 7),                      # KG costs
    total_units = c(100, 100),                  # Equal units for comparison
    total_orders = c(50, 50)                    # Equal orders for comparison
  ),
  data_list = list(
    order_list = tibble(
      plant_code = c("PLANT01", "PLANT02"),
      carrier = c("CARRIER1", "CARRIER2"),
      service_level = c("CRF", "DTD")
    )
  )
)

# Test 1: Total Storage Cost Calculation
test_that("Total storage cost is calculated correctly", {
  testServer(costAnalysisServer, args = list(data = test_data), {
    # Test without filters
    session$setInputs(warehouse = "All", carrier = "All")
    
    # Test the filtered data directly
    filtered <- filtered_warehouse()
    total_storage <- sum(filtered$storage_cost, na.rm = TRUE)
    expect_equal(total_storage, 12000)  # 5000 + 7000
    
    # Test with warehouse filter
    session$setInputs(warehouse = "PLANT01", carrier = "All")
    filtered <- filtered_warehouse()
    total_storage <- sum(filtered$storage_cost, na.rm = TRUE)
    expect_equal(total_storage, 5000)
  })
})

# Test 2: Average Cost per Unit Calculation
test_that("Average cost per unit is calculated correctly", {
  testServer(costAnalysisServer, args = list(data = test_data), {
    # Test without filters
    session$setInputs(warehouse = "All", carrier = "All")
    
    filtered <- filtered_warehouse()
    avg_cost <- mean(filtered$cost_per_unit, na.rm = TRUE)
    expect_equal(avg_cost, 60)  # (50 + 70) / 2
    
    # Test with warehouse filter
    session$setInputs(warehouse = "PLANT01", carrier = "All")
    filtered <- filtered_warehouse()
    avg_cost <- mean(filtered$cost_per_unit, na.rm = TRUE)
    expect_equal(avg_cost, 50)
  })
})

# Test 3: Ground Transport Percentage Calculation
test_that("Ground transport percentage is calculated correctly", {
  testServer(costAnalysisServer, args = list(data = test_data), {
    # Test without filters
    session$setInputs(warehouse = "All", carrier = "All")
    
    filtered <- filtered_shipping()
    ground_total <- sum(filtered$total_shipping_cost[filtered$mode_dsc == "GROUND"])
    total_cost <- sum(filtered$total_shipping_cost)
    ground_pct <- ground_total / total_cost * 100
    expect_equal(round(ground_pct, 1), 33.3)  # 2000 / 6000 * 100
    
    # Test with warehouse filter
    session$setInputs(warehouse = "PLANT01", carrier = "All")
    filtered <- filtered_shipping()
    ground_total <- sum(filtered$total_shipping_cost[filtered$mode_dsc == "GROUND"])
    total_cost <- sum(filtered$total_shipping_cost)
    ground_pct <- ground_total / total_cost * 100
    expect_equal(round(ground_pct, 1), 66.7)  # 2000 / 3000 * 100
  })
})

# Test 4: Cost Matrix Calculations
test_that("Cost efficiency matrix calculations are correct", {
  testServer(costAnalysisServer, args = list(data = test_data), {
    session$setInputs(warehouse = "All", carrier = "All")
    
    # Get the efficiency matrix data
    filtered <- filtered_warehouse()
    efficiency_scores <- filtered |>
      mutate(
        storage_efficiency = scales::rescale(-storage_cost),
        unit_cost_efficiency = scales::rescale(-cost_per_unit),
        kg_cost_efficiency = scales::rescale(-cost_per_kg)
      )
    
    # Verify efficiency scores
    expect_equal(efficiency_scores$storage_efficiency[1], 1)  # PLANT01 should be most efficient
    expect_equal(efficiency_scores$unit_cost_efficiency[1], 1)
    expect_equal(efficiency_scores$kg_cost_efficiency[1], 1)
    
    expect_equal(efficiency_scores$storage_efficiency[2], 0)  # PLANT02 should be least efficient
    expect_equal(efficiency_scores$unit_cost_efficiency[2], 0)
    expect_equal(efficiency_scores$kg_cost_efficiency[2], 0)
  })
})