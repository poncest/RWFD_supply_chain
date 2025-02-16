# Supply Chain Dashboard - Download Features Documentation

## Overview
The Supply Chain Analytics Dashboard provides comprehensive data export functionality through CSV downloads. Each visualization includes its own download capability, with data reflecting current filter selections and view states.

## Download Options

### 1. Routes Cost Data
```plaintext
Content         | Description
----------------|------------------
Filename        | routes_data_[YYYYMMDD].csv
Filters Applied | Warehouse, Carrier, Service Level
Fields Included | 
- origin_port      | Origin location
- destination_port | Destination location
- carrier          | Shipping carrier
- mode_dsc         | Transport mode (AIR/GROUND)
- total_cost       | Total shipping cost (USD)
- n_orders         | Number of orders
- avg_cost_order   | Average cost per order
```

### 2. Storage Cost Data
```plaintext
Content         | Description
----------------|------------------
Filename        | storage_costs_[YYYYMMDD].csv
Filters Applied | Warehouse, Carrier
Fields Included |
- plant_code       | Warehouse identifier
- storage_cost     | Total storage cost (USD)
- total_units      | Total units stored
- cost_per_unit    | Cost per unit (USD)
- utilization      | Capacity utilization (%)
```

### 3. Transport Mode Analysis
```plaintext
Content         | Description
----------------|------------------
Filename        | transport_mode_[YYYYMMDD].csv
Filters Applied | All current selections
Fields Included |
- mode_dsc         | Transport mode
- total_cost       | Total cost by mode
- total_orders     | Order count
- total_weight     | Total weight (kg)
- avg_cost_order   | Average cost per order
- avg_cost_kg      | Average cost per kg
```

### 4. Service Level Distribution
```plaintext
Content         | Description
----------------|------------------
Filename        | service_level_[YYYYMMDD].csv
Filters Applied | All current selections
Fields Included |
- service_level    | Service type
- order_count      | Number of orders
- total_cost       | Total cost
- percentage       | % of total orders
```

## Implementation Details

### 1. Download Handler Implementation
```r
output$download_routes_csv <- downloadHandler(
  filename = function() {
    paste0("routes_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  },
  content = function(file) {
    # Validate data
    validate(
      need(filtered_data(), "No data available for export")
    )
    
    # Prepare export data
    export_data <- filtered_data() %>%
      select(required_columns) %>%
      mutate(
        timestamp = Sys.time(),
        filters_applied = get_filter_string()
      )
    
    # Write CSV
    write.csv(export_data, file, row.names = FALSE)
  }
)
```

### 2. Data Validation
Each download implements these validations:
```r
validate_export_data <- function(data, required_cols) {
  # Check data availability
  if (nrow(data) == 0) {
    return("No data available for export")
  }
  
  # Verify required columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    return(paste("Missing required columns:", 
                paste(missing_cols, collapse = ", ")))
  }
  
  return(TRUE)
}
```

## Usage Guide

### 1. Accessing Downloads
1. Navigate to desired visualization
2. Apply relevant filters
3. Click download icon (↓)
4. Choose save location
5. Verify downloaded file

### 2. File Formats
- All exports are in CSV format
- UTF-8 encoding
- First row contains headers
- Numeric formats:
  * Costs: 2 decimal places
  * Percentages: 1 decimal place
  * Weights: 2 decimal places

### 3. Error Handling

#### Common Issues and Solutions
```plaintext
Error Message          | Solution
----------------------|------------------
No data available     | Adjust filters
Export failed         | Check file permissions
Invalid format        | Contact support
```

### 4. Best Practices
1. Data Export
   - Verify filter selections before download
   - Check file size is reasonable
   - Validate exported data
   
2. File Management
   - Use consistent naming convention
   - Maintain organized file structure
   - Regular cleanup of old exports

## Maintenance

### 1. Download Feature Updates
- Regular validation checks
- Format consistency verification
- Error message maintenance

### 2. Performance Considerations
- Large dataset handling
- Export timeout management
- Memory usage optimization

## Getting Help
For download-related issues:
1. Check error messages
2. Verify filter selections
3. Contact support team
4. Provide export timestamp for troubleshooting