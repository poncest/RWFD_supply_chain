# tests/testthat/helper-data.R

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
  )
)