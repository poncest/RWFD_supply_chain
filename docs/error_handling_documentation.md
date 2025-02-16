# Error Handling Documentation - Supply Chain Analytics Dashboard

## Overview
This document outlines the error handling and validation patterns implemented in the Supply Chain Analytics Dashboard. These patterns ensure data quality, provide clear user feedback, and maintain application stability.

## Validation Patterns

### 1. Plot Data Validation
```r
validate_plot_data <- function(data, required_cols, plot_name) {
  # Check for required columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    warning(sprintf("Missing columns for %s: %s", plot_name, 
                    paste(missing_cols, collapse = ", ")))
    return(FALSE)
  }
  
  # Check for NA values in required columns
  na_counts <- sapply(data[required_cols], function(x) sum(is.na(x)))
  if (any(na_counts > 0)) {
    warning(sprintf("Found NA values in %s: %s", plot_name,
                    paste(names(na_counts[na_counts > 0]), collapse = ", ")))
  }
  
  # Check for negative values where inappropriate
  cost_cols <- grep("cost|amount", required_cols, value = TRUE)
  for (col in cost_cols) {
    if (any(data[[col]] < 0, na.rm = TRUE)) {
      warning(sprintf("Negative values found in %s for column: %s", 
                      plot_name, col))
    }
  }
  
  return(TRUE)
}
```

Usage example:
```r
output$cost_breakdown <- renderGirafe({
  validate(
    need(filtered_shipping(), "Loading shipping data..."),
    need(nrow(filtered_shipping()) > 0, "No data available for selected filters"),
    need(validate_plot_data(filtered_shipping(), 
                            c("origin_port", "total_shipping_cost"), 
                            "Cost Breakdown"), 
         "Invalid data structure for visualization")
  )
  # ... plot creation code
})
```

### 2. Filter Validation Messages
Standard message pattern:
```r
glue::glue("No data available",
           "{if (input$warehouse != 'All') paste(' for', input$warehouse)}",
           "{if (input$carrier != 'All') paste(' and', input$carrier)}")
```

### 3. KPI Validation
```r
output$total_storage_cost <- renderText({
  validate(
    need(filtered_warehouse(), "Loading warehouse data..."),
    need(nrow(filtered_warehouse()) > 0, 
         glue::glue("No Storage Cost Data Available",
                    "{if (input$warehouse != 'All') paste(' for', input$warehouse)}"))
  )
  
  cost <- sum(filtered_warehouse()$storage_cost, na.rm = TRUE)
  
  if (is.na(cost) || is.nan(cost) || cost == 0) {
    return("No Cost Data Available")
  }
  scales::dollar(cost)
})
```

## Error Message Standards

### 1. Loading States
- Use "Loading [data type]..." format
- Example: "Loading shipping data..."

### 2. No Data Messages
- Include filter context
- Format: "No [metric] Data Available for [filter values]"
- Example: "No Storage Cost Data Available for PLANT01"

### 3. Validation Error Messages
- Clear indication of the problem
- Actionable feedback when possible
- Example: "Missing required columns: cost_per_unit, storage_cost"

## Missing Value Handling

### 1. NA Values
```r
# Sum with NA handling
total_cost <- sum(filtered_warehouse()$storage_cost, na.rm = TRUE)

# Mean with NA handling
avg_cost <- mean(filtered_warehouse()$cost_per_unit, na.rm = TRUE)

# Percentage calculation with NA safeguard
if (is.na(avg_cost) || is.nan(avg_cost) || avg_cost == 0) {
  return("No Unit Cost Data Available")
}
```

### 2. Zero Values
```r
# Handle zero denominators
ground_pct <- if(total_cost > 0) ground_total / total_cost * 100 else 0
```

## Negative Value Handling

### 1. Cost Validation
```r
# Check for negative costs
cost_cols <- grep("cost|amount", names(data), value = TRUE)
for (col in cost_cols) {
  if (any(data[[col]] < 0, na.rm = TRUE)) {
    warning(sprintf("Negative values found in column: %s", col))
  }
}
```

### 2. Visualization Handling
```r
# Convert negative values to zero for visualization
data <- data |>
  mutate(across(starts_with("cost"), ~pmax(0, .)))
```

## Best Practices

### 1. Error Prevention
- Validate data early
- Use clear input constraints
- Provide default values

### 2. Error Recovery
- Graceful fallback options
- Clear error messages
- Recovery suggestions

### 3. User Communication
- Loading states
- Progress indicators
- Clear feedback messages

## Testing Error Handling
```r
test_that("Error handling works correctly", {
  # Test NA handling
  expect_equal(sum(c(1, NA, 3), na.rm = TRUE), 4)
  
  # Test zero handling
  expect_equal(safe_divide(5, 0), 0)
  
  # Test negative value handling
  expect_warning(validate_plot_data(data_with_negatives))
})
```

## Future Improvements
- Add input validation for file uploads
- Implement error logging
- Add error tracking analytics
- Enhance error recovery mechanisms