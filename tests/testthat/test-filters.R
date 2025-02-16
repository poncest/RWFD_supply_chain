# tests/testthat/test-filters.R

library(testthat)
library(shiny)
library(dplyr)

# Source the server file
# source("modules/cost_analysis/cost_analysis_server.R")

# Test data setup with unique combinations
test_data <- list(
  shipping_costs = tibble(
    origin_port = c("PLANT01", "PLANT02", "PLANT01"),
    destination_port = c("DEST01", "DEST02", "DEST03"),
    carrier = c("CARRIER1", "CARRIER1", "CARRIER2"),
    total_shipping_cost = c(100, 200, 300),
    total_weight = c(50, 75, 100),
    n_orders = c(5, 8, 10),
    mode_dsc = c("AIR", "GROUND", "AIR")
  ),
  warehouse_costs = tibble(
    plant_code = c("PLANT01", "PLANT02"),  
    storage_cost = c(150, 250),
    cost_per_unit = c(1.5, 2.5),
    cost_per_kg = c(0.5, 1.0),
    total_units = c(100, 200),
    total_orders = c(10, 20)
  ),
  data_list = list(
    order_list = tibble(
      plant_code = c("PLANT01", "PLANT02"),  
      carrier = c("CARRIER1", "CARRIER1"),
      service_level = c("CRF", "DTD")
    )
  ),
  warehouse_capacity = tibble(
    plant_code = c("PLANT01", "PLANT02"),  
    capacity_utilization = c(0.75, 0.85)
  )
)

# Test shipping costs filtering
test_that("Shipping costs filtering works correctly", {
  testServer(costAnalysisServer, args = list(data = test_data), {
    # Test 1: Filter by warehouse only
    session$setInputs(warehouse = "PLANT01", carrier = "All")
    filtered <- filtered_shipping()
    print("Test 1 Results:")
    print(filtered)
    expect_equal(nrow(filtered), 2)
    expect_true(all(filtered$origin_port == "PLANT01"))
    
    # Test 2: Filter by carrier only
    session$setInputs(warehouse = "All", carrier = "CARRIER1")
    filtered <- filtered_shipping()
    print("Test 2 Results:")
    print(filtered)
    expect_equal(nrow(filtered), 2)
    expect_true(all(filtered$carrier == "CARRIER1"))
    
    # Test 3: Filter by both warehouse and carrier
    session$setInputs(warehouse = "PLANT01", carrier = "CARRIER1")
    filtered <- filtered_shipping()
    print("Test 3 Results:")
    print(filtered)
    expect_equal(nrow(filtered), 1)
    expect_true(all(filtered$origin_port == "PLANT01" & filtered$carrier == "CARRIER1"))
  })
})

# Test warehouse costs filtering
test_that("Warehouse costs filtering works correctly", {
  testServer(costAnalysisServer, args = list(data = test_data), {
    # Test 1: Filter by warehouse only
    session$setInputs(warehouse = "PLANT01", carrier = "All")
    filtered <- filtered_warehouse()
    print("Warehouse Test 1 Results:")
    print(filtered)
    expect_equal(nrow(filtered), 1)  
    expect_true(all(filtered$plant_code == "PLANT01"))
    
    # Test 2: Filter by carrier only
    session$setInputs(warehouse = "All", carrier = "CARRIER1")
    filtered <- filtered_warehouse()
    print("Warehouse Test 2 Results:")
    print(filtered)
    expect_equal(nrow(filtered), 2)  # Both warehouses have CARRIER1
    
    # Test 3: Filter by both
    session$setInputs(warehouse = "PLANT01", carrier = "CARRIER1")
    filtered <- filtered_warehouse()
    print("Warehouse Test 3 Results:")
    print(filtered)
    expect_equal(nrow(filtered), 1)  # Only PLANT01 with CARRIER1
  })
})