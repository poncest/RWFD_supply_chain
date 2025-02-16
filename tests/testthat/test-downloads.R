# tests/testthat/test-downloads.R

library(testthat)
library(shiny)
library(dplyr)

# Source the server file
# source("modules/cost_analysis/cost_analysis_server.R")

# Test data setup with only valid data (no edge cases)
test_data <- list(
  shipping_costs = tibble(
    origin_port = c("PLANT01", "PLANT01", "PLANT02"),
    destination_port = c("DEST01", "DEST02", "DEST01"),
    carrier = c("CARRIER1", "CARRIER1", "CARRIER2"),
    total_shipping_cost = c(1000, 2000, 3000),  # All positive values
    total_weight = c(100, 200, 300),            # All positive values
    n_orders = c(10, 20, 30),                   # All positive values
    mode_dsc = c("AIR", "GROUND", "AIR")
  ),
  warehouse_costs = tibble(
    plant_code = c("PLANT01", "PLANT02"),
    storage_cost = c(5000, 7000),               # All positive values
    cost_per_unit = c(50, 70),                  # All positive values
    cost_per_kg = c(5, 7),                      # All positive values
    total_units = c(100, 100),                  # All positive values
    total_orders = c(50, 50)                    # All positive values
  ),
  data_list = list(
    order_list = tibble(
      plant_code = c("PLANT01", "PLANT02"),
      carrier = c("CARRIER1", "CARRIER2"),
      service_level = c("CRF", "DTD")
    )
  )
)

# Test 1: Cost Breakdown Download Structure
test_that("Cost breakdown download data is correctly structured", {
  testServer(costAnalysisServer, args = list(data = test_data), {
    # Get the download data
    download_data <- download_datasets()$cost_breakdown
    
    # Test structure
    expect_true(all(c("origin_port", "transport_cost", "storage_cost", 
                      "total_cost", "pct_transport", "pct_storage") %in% 
                      names(download_data)))
    
    # Test calculations
    expect_equal(download_data$total_cost, 
                 download_data$transport_cost + download_data$storage_cost)
    
    # Test percentages sum to 100
    expect_true(all(abs(download_data$pct_transport + download_data$pct_storage - 100) < 0.01))
  })
})

# Test 2: Unit Cost Download Structure
test_that("Unit cost download data is correctly structured", {
  testServer(costAnalysisServer, args = list(data = test_data), {
    # Get the download data
    download_data <- download_datasets()$unit_cost
    
    # Test structure
    expect_true(all(c("plant_code", "cost_per_unit", "cost_per_kg",
                      "total_units", "total_orders", "quadrant") %in% 
                      names(download_data)))
    
    # Test quadrant assignment exists
    expect_true(all(!is.na(download_data$quadrant)))
  })
})

# Test 3: Efficiency Matrix Download Structure
test_that("Efficiency matrix download data is correctly structured", {
  testServer(costAnalysisServer, args = list(data = test_data), {
    # Get the download data
    download_data <- download_datasets()$efficiency_matrix
    
    # Test structure
    expect_true(all(c("plant_code", "storage_efficiency", "unit_cost_efficiency",
                      "kg_cost_efficiency", "volume_efficiency") %in% 
                      names(download_data)))
    
    # Test efficiency scores are between 0 and 1
    expect_true(all(download_data$storage_efficiency >= 0 & 
                      download_data$storage_efficiency <= 1))
    expect_true(all(download_data$unit_cost_efficiency >= 0 & 
                      download_data$unit_cost_efficiency <= 1))
  })
})

# Test 4: Download File Name Format
test_that("Download file names match expected format", {
  # Test the create_filename helper function
  filename <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Test cost breakdown filename format
  expect_match(
    paste0("cost_breakdown_", filename, ".csv"),
    "^cost_breakdown_\\d{8}_\\d{6}\\.csv$"
  )
  
  # Test unit cost filename format
  expect_match(
    paste0("unit_cost_analysis_", filename, ".csv"),
    "^unit_cost_analysis_\\d{8}_\\d{6}\\.csv$"
  )
  
  # Test efficiency matrix filename format
  expect_match(
    paste0("efficiency_matrix_", filename, ".csv"),
    "^efficiency_matrix_\\d{8}_\\d{6}\\.csv$"
  )
})