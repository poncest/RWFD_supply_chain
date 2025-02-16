# Testing Documentation - Supply Chain Analytics Dashboard

## Overview
This document outlines the testing approach for the Supply Chain Analytics Dashboard, focusing on the Cost Analysis module. Our testing strategy ensures data integrity, proper calculations, and robust error handling.

## Test Structure

### Location
```
tests/
  └── testthat/
      ├── test-filters.R       # Filter functionality tests
      ├── test-calculations.R  # Cost calculation tests
      └── test-edge-cases.R    # Edge case handling tests
```

### Test Data Structure

#### Basic Test Data
```r
test_data <- list(
  shipping_costs = tibble(
    origin_port = c("PLANT01", "PLANT01", "PLANT02"),
    destination_port = c("DEST01", "DEST02", "DEST01"),
    carrier = c("CARRIER1", "CARRIER1", "CARRIER2"),
    total_shipping_cost = c(1000, 2000, 3000),
    total_weight = c(100, 200, 300),
    n_orders = c(10, 20, 30),
    mode_dsc = c("AIR", "GROUND", "AIR")
  ),
  warehouse_costs = tibble(
    plant_code = c("PLANT01", "PLANT02"),
    storage_cost = c(5000, 7000),
    cost_per_unit = c(50, 70),
    cost_per_kg = c(5, 7),
    total_units = c(100, 100),
    total_orders = c(50, 50)
  )
)
```

Design principles:
- Simple, predictable numbers for easy verification
- Consistent ratios (e.g., cost/weight = 10)
- Realistic relationships between data points
- One-to-one mapping between warehouses and plants

## Test Coverage

### 1. Filter Tests
Tests basic and combined filtering functionality:

```r
test_that("Filter functionality works correctly", {
  # Test warehouse filter
  session$setInputs(warehouse = "PLANT01", carrier = "All")
  expect_equal(nrow(filtered), 2)
  
  # Test carrier filter
  session$setInputs(warehouse = "All", carrier = "CARRIER1")
  expect_equal(nrow(filtered), 2)
  
  # Test combined filters
  session$setInputs(warehouse = "PLANT01", carrier = "CARRIER1")
  expect_equal(nrow(filtered), 1)
})
```

### 2. Calculation Tests
Validates core cost calculations:

```r
test_that("Cost calculations are correct", {
  # Total storage cost
  expect_equal(total_storage, 12000)  # 5000 + 7000
  
  # Average cost per unit
  expect_equal(avg_cost, 60)  # (50 + 70) / 2
  
  # Ground transport percentage
  expect_equal(round(ground_pct, 1), 33.3)  # (2000 / 6000) * 100
})
```

### 3. Edge Case Tests
Tests handling of problematic data:

```r
edge_case_data <- list(
  # Normal, NA, Zero, Negative values
  shipping_costs = tibble(...),
  warehouse_costs = tibble(...)
)
```

Coverage includes:
- Missing values (NA)
- Zero values
- Negative values
- Empty filter results
- Invalid data structures

## Validation Patterns

### Data Validation
```r
validate_plot_data(data, required_cols, plot_name) {
  # Checks for:
  # 1. Required columns presence
  # 2. NA values in critical columns
  # 3. Negative values in cost columns
}
```

### Filter Validation
- Validates filter selections
- Ensures data consistency
- Handles empty results gracefully

## Test Results
All tests passing:
```r
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 6 ]
```

## Best Practices

### Writing Tests
1. Use descriptive test names
2. Test one thing at a time
3. Use meaningful test data
4. Include both positive and negative tests
5. Test edge cases thoroughly

### Test Data
1. Use simple, verifiable numbers
2. Maintain realistic relationships
3. Include various data scenarios
4. Document expected outcomes
5. Keep test data minimal but sufficient

### Running Tests
1. Tests run automatically with `runApp()`
2. Test files are sourced in `global.R`
3. All tests must pass before deployment
4. New features require new tests

## Future Test Coverage
Planned test additions:
- Download functionality
- "No data" states
- Performance testing
- Cross-module integration tests

## Maintaining Tests
Guidelines for maintaining test suite:
1. Update tests when modifying functionality
2. Keep test data current
3. Document test changes
4. Review test coverage regularly
5. Add tests for bug fixes