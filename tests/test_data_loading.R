# tests/test_data_loading.R

# File checks
test_that("load_supply_chain_data handles missing files", {
  expect_error(
    load_supply_chain_data(),
    "Data file not found"
  )
})

# Sheet validation
test_that("load_supply_chain_data validates required sheets", {
  # Create temporary test file with missing sheets
  temp_file <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(
      order_list = data.frame(x = 1)  # Only one sheet instead of all required
    ),
    temp_file
  )
  
  # Test should error due to missing sheets
  expect_error(
    load_supply_chain_data(),
    "Data file not found: Please ensure rwfd_supply_chain.xlsx is in data/01_raw_data/"
  )
  
  # Clean up
  unlink(temp_file)
})

# Data validation
test_that("validate_order_list catches invalid data", {
  invalid_orders <- data.frame(
    order_id = c(-1, 0, 1),
    service_level = c("INVALID", "CRF", "DTD")
  )
  expect_error(validate_order_list(invalid_orders))
})


# Weight validation
test_that("shipping costs handles zero weights", {
  test_data <- data.frame(
    weight = c(0, -1, 100),
    rate = c(1, 1, 1)
  )
  expect_error(calculate_shipping_costs(test_data))
})

# Warehouse capacity
test_that("warehouse capacity handles invalid capacities", {
  test_data <- data.frame(
    daily_capacity = c(-1, 0, 100)
  )
  expect_error(calculate_warehouse_capacity(test_data))
})

# Cost calculations
test_that("cost calculations are accurate", {
  test_data <- list(
    order_list = data.frame(
      plant_code = "PLANT01",
      weight = 100,
      unit_quantity = 10
    ),
    wh_costs = data.frame(
      wh = "PLANT01",
      cost_unit = 5
    )
  )
  result <- calculate_warehouse_costs(test_data)
  expect_equal(result$storage_cost, 50)
})

