# Supply Chain Dashboard - Data Requirements

## Overview
This document outlines the data structure and validation requirements for the Supply Chain Analytics Dashboard. All data must conform to these specifications to ensure proper functionality.

## File Structure

### Base Requirements
- File Format: Excel (.xlsx)
- File Name: rwfd_supply_chain.xlsx
- Location: data/01_raw_data/
- Required Sheets: 
  * order_list
  * freight_rates
  * wh_costs
  * wh_capacities

## Detailed Sheet Requirements

### 1. Order List (order_list)
Required columns and validations:

```plaintext
Column Name    | Type     | Validation Rules
---------------|----------|------------------
order_id       | numeric  | > 0, unique
order_date     | datetime | YYYY-MM-DD format
origin_port    | text     | Must exist in freight_rates.orig_port_cd
carrier        | text     | Must exist in freight_rates.carrier
service_level  | text     | Must be: CRF, DTD, or DTP
weight         | numeric  | > 0, in kg
unit_quantity  | numeric  | > 0, whole numbers
plant_code     | text     | Must exist in wh_capacities.plant_id
dest_port      | text     | Must exist in freight_rates.dest_port_cd
```

### 2. Freight Rates (freight_rates)
Required columns and validations:

```plaintext
Column Name    | Type     | Validation Rules
---------------|----------|------------------
orig_port_cd   | text     | Non-empty
dest_port_cd   | text     | Non-empty
carrier        | text     | Non-empty
svc_cd         | text     | Must be: CRF, DTD, or DTP
mode_dsc       | text     | Must be: AIR or GROUND
rate_amount    | numeric  | > 0
```

### 3. Warehouse Costs (wh_costs)
Required columns and validations:

```plaintext
Column Name    | Type     | Validation Rules
---------------|----------|------------------
wh             | text     | Must exist in order_list.plant_code
cost_unit      | numeric  | > 0
year           | numeric  | YYYY format, current year ±1
month          | numeric  | 1-12
total_cost     | numeric  | > 0
```

### 4. Warehouse Capacities (wh_capacities)
Required columns and validations:

```plaintext
Column Name     | Type     | Validation Rules
----------------|----------|------------------
plant_id        | text     | Unique, non-empty
daily_capacity  | numeric  | > 0
region          | text     | Non-empty
type            | text     | Non-empty
status          | text     | Must be: ACTIVE or INACTIVE
```

## Data Validation Rules

### 1. General Validation
- No missing values in required fields
- No duplicate records in key fields
- Date formats must be consistent (YYYY-MM-DD)
- Numeric fields must be non-negative where appropriate
- Text fields must match predefined values

### 2. Cross-Sheet Validation
```r
# Example validation checks
validate_relationships <- function(data) {
  # Order List → Warehouse Capacities
  valid_plants <- all(data$order_list$plant_code %in% data$wh_capacities$plant_id)
  
  # Order List → Freight Rates
  valid_routes <- data$order_list %>%
    left_join(data$freight_rates,
              by = c("origin_port" = "orig_port_cd",
                    "destination_port" = "dest_port_cd",
                    "carrier" = "carrier",
                    "service_level" = "svc_cd"))
  
  # Warehouse Costs → Capacities
  valid_warehouses <- all(data$wh_costs$wh %in% data$wh_capacities$plant_id)
  
  return(list(
    valid_plants = valid_plants,
    valid_routes = nrow(valid_routes) > 0,
    valid_warehouses = valid_warehouses
  ))
}
```

### 3. Error Handling
The dashboard implements these validations with clear error messages:

```plaintext
Error Type           | Message
--------------------|---------------------------
Missing Required    | "Missing required column: [column_name]"
Invalid Format      | "Invalid date format in order_date"
Relationship Error  | "Plant code not found in warehouse capacity data"
Range Violation     | "Negative cost values detected"
```

## Sample Data
Example of valid data format:

### order_list
```r
tibble(
  order_id = c(1001, 1002),
  order_date = c("2024-01-15", "2024-01-16"),
  origin_port = c("PORT01", "PORT02"),
  carrier = c("CARRIER1", "CARRIER2"),
  service_level = c("DTD", "CRF"),
  weight = c(100, 150),
  unit_quantity = c(10, 15),
  plant_code = c("PLANT01", "PLANT02"),
  destination_port = c("DEST01", "DEST02")
)
```

## Best Practices

### 1. Data Preparation
- Validate data before upload
- Check for required relationships
- Ensure consistent formatting
- Remove duplicate records

### 2. Maintenance
- Regular data quality checks
- Update rates and capacities promptly
- Archive outdated records
- Maintain backup copies

### 3. Troubleshooting
Common issues and solutions:
1. Missing Relationships
   - Verify plant codes across sheets
   - Check carrier codes consistency
   - Validate service level codes

2. Invalid Values
   - Check for negative numbers
   - Verify date formats
   - Validate status values

## Getting Help
For data-related issues:
1. Check validation error messages
2. Verify data against requirements
3. Contact support team if needed