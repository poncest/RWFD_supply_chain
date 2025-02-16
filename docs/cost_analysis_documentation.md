# Cost Analysis Module Documentation

## Purpose
The Cost Analysis module provides detailed cost breakdowns and efficiency metrics for supply chain operations, focusing on storage costs, unit costs, and transportation mode analysis.

## Module Structure

### Files
- `cost_analysis_ui.R`: UI components and layout definitions
- `cost_analysis_server.R`: Server-side logic and cost calculations

### Components

#### 1. KPI Cards
- **Total Storage Cost**: Total warehouse storage costs
  - Function: `output$total_storage_cost`
  - Format: Currency (USD)
  - Error State: "No data"

- **Average Cost per Unit**: Unit cost efficiency metric
  - Function: `output$avg_cost_unit`
  - Calculation: storage_cost / total_units
  - Format: Currency per unit

- **Ground Transport %**: Transport mode distribution
  - Function: `output$ground_transport_pct`
  - Format: Percentage
  - Source: shipping_costs data

- **Most Expensive Route**: Highest cost shipping route
  - Function: `output$top_route_cost`
  - Format: Text (origin → destination)
  - Calculation: Based on total_shipping_cost


#### 2. Interactive Visualizations

1. **Routes by Total Cost**
  - Type: Stacked bar chart (interactive)
  - Data: Combined transport and storage costs
  - Features:
    * Cost type breakdown (Transport/Storage)
    * Percentage distributions
    * Interactive tooltips with cost details
  - Function: `output$cost_breakdown`
  - Error State: "No data available for selected filters"

2. **Unit Cost Analysis**
  - Type: Scatter plot with quadrants
  - Data: Cost per unit vs Cost per KG
  - Features:
    * Quadrant analysis (High Cost, Optimal, etc.)
    * Size encoding for total units
    * Color encoding for total orders
    * Reference lines at median values
  - Function: `output$unit_cost`
  - Error State: "No data available for selected filters"

3. **Efficiency Matrix**
  - Type: Interactive heatmap
  - Data: Storage, unit cost, and KG cost efficiencies
  - Features:
    * Normalized efficiency scores (0-1)
    * Performance categories
    * Text labels showing exact values
    * Detailed tooltips
  - Function: `output$cost_efficiency`
  - Error State: "No data available for selected filters"
  
#### 3. Filter Controls
- Located in sidebar panel (width = 250px)
- Components:
 * **Warehouse Selector**
   - Type: Dropdown (selectInput)
   - Options: All warehouses + "All"
   - Default: "All"
   - Updates: Dynamically based on data

 * **Carrier Selector**
   - Type: Dropdown (selectInput)
   - Options: All carriers + "All"
   - Default: "All"
   - Updates: Dynamically based on data

 * **Service Level Selector**
   - Type: Dropdown (selectInput)
   - Options: CRF, DTD, DTP + "All"
   - Default: "All"
   - Updates: Dynamically based on data

 * **Reset Button**
   - Type: actionButton
   - Function: Resets all filters to default
   - Class: "btn-secondary btn-sm w-100"

#### 4. Download Features
- Located below filters in sidebar
- Format: CSV exports with timestamps
- Components:
 * **Cost Breakdown Download**
   - Function: `download_cost_breakdown_csv`
   - Filename: cost_breakdown_YYYYMMDD_HHMMSS.csv
   - Content: Combined transport/storage costs

 * **Unit Cost Download**
   - Function: `download_unit_cost_csv`
   - Filename: unit_cost_analysis_YYYYMMDD_HHMMSS.csv
   - Content: Unit cost metrics and quadrants

 * **Efficiency Matrix Download**
   - Function: `download_efficiency_csv`
   - Filename: efficiency_matrix_YYYYMMDD_HHMMSS.csv
   - Content: Normalized efficiency scores
   
## Data Flow

### 1. Reactive Datasets
- **filtered_shipping**: Filters shipping cost data
 ```r
 filtered_shipping <- reactive({
   req(data$shipping_costs)
   out <- data$shipping_costs
   
   if (!is.null(input$warehouse) && input$warehouse != "All") {
     out <- out |> filter(origin_port == input$warehouse)
   }
   if (!is.null(input$carrier) && input$carrier != "All") {
     out <- out |> filter(carrier == input$carrier)
   }
   out
 })
 ```
 - **filtered_warehouse**: Filters warehouse data
  ```r
 filtered_warehouse <- reactive({
  req(data$warehouse_costs)
  out <- data$warehouse_costs
  
  if (!is.null(input$warehouse) && input$warehouse != "All") {
    out <- out |> filter(plant_code == input$warehouse)
  }
  out
})
 ```

### 2. Data Dependencies

 - **KPI Dependencies**:

 * Total Storage Cost → filtered_warehouse
 * Avg Cost per Unit → filtered_warehouse
 * Ground Transport % → filtered_shipping
 * Most Expensive Route → filtered_shipping


 - **Visualization Dependencies**:

 * Cost Breakdown → filtered_shipping, filtered_warehouse
 * Unit Cost Analysis → filtered_warehouse
 * Efficiency Matrix → filtered_warehouse


### 3. Data Transformations
 ```r
 # Cost breakdown calculations
shipping_with_plant <- filtered_shipping() |>
  mutate(plant_code = str_replace(origin_port, "PORT", "PLANT"))

cost_breakdown <- shipping_with_plant |>
  left_join(filtered_warehouse(), by = "plant_code") |>
  group_by(plant_code) |>
  summarise(
    transport_cost = sum(total_shipping_cost),
    storage_cost = first(storage_cost),
    total_cost = transport_cost + storage_cost,
    .groups = "drop"
  )

# Efficiency calculations
efficiency_matrix <- filtered_warehouse() |>
  mutate(
    storage_efficiency = scales::rescale(max(storage_cost) - storage_cost),
    unit_cost_efficiency = scales::rescale(max(cost_per_unit) - cost_per_unit),
    kg_cost_efficiency = scales::rescale(max(cost_per_kg) - cost_per_kg),
    volume_efficiency = scales::rescale(total_units)
  )
 ```
 
## Error Handling

### 1. Data Validation
- **Required Data Checks**
 ```r
 # Using req() for reactive dependencies
 validate(need(filtered_warehouse(), "Loading data..."))
 validate(
   need(nrow(filtered_warehouse()) > 0, "No data")
 )
 ```

- **Plot Data Validation**
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

### 2. Error Messages

- **Loading States**

 * Initial: "Loading data..."
 * Cost Data: "Loading warehouse data..."
 * Visualization: "Loading cost breakdown data..."


- **No Data States**

 * Generic: "No data"
 * Filtered: "No data available for selected filters"
 * Cost-specific: "No Storage Cost Data Available for {warehouse}"


- **Validation Messages**

 * Missing Columns: "Missing required columns: cost_per_unit, storage_cost"
 * Invalid Data: "Invalid data structure for visualization"
 * Calculation Errors: "Cannot calculate cost per unit (zero units)"


## Performance Considerations

### 1. Data Filtering Optimization
- **Filter Operation Monitoring**
 ```r
 # Filter operation logging
 message(sprintf("Applying warehouse filter: %s", input$warehouse))
 message(sprintf("Applying carrier filter: %s", input$carrier))
 ```

- **Execution Tracking**
 ```r
 # Cost calculations timing
 start_time <- Sys.time()
 # Cost breakdown calculations
 cost_data <- shipping_with_plant |>
   left_join(filtered_warehouse(), by = "plant_code") |>
   summarise(...)
 end_time <- Sys.time()
 message(sprintf("Cost breakdown calculation time: %.2f seconds", 
                 as.numeric(end_time - start_time)))
 ```

### 2. Visualization Optimization

- **Cost Breakdown Chart**

 * Limit to most significant cost contributors
 * Pre-calculate percentages
 * Efficient tooltip generation

 ```r
 # Efficient tooltip creation
 tooltip_text = glue::glue(
   "Location Details\n",
   "───────────────\n",
   "Warehouse: {plant_code}\n",
   "Cost Type: {cost_type}\n",
   "───────────────\n",
   "Amount: {scales::dollar(amount)}\n",
   "% of Total: {scales::percent(pct_of_total/100, accuracy=0.1)}"
 )
 ```

- **Efficiency Matrix**

 * Pre-calculate efficiency scores
 * Cache results where possible
 * Minimize recalculations
 
 ```r
 # Cache efficiency calculations
 full_stats <- reactive({
   data$warehouse_costs |>
     summarise(
       median_cost_unit = median(cost_per_unit),
       median_cost_kg = median(cost_per_kg)
     )
 })
 ```

### 3. Resource Management

- **Memory Usage**

 * Clear unused reactive values
 * Minimize data duplication
 * Efficient data transformations


- **Chart Rendering**

 * Use withSpinner for loading states
 * Optimize SVG generation
 * Minimize chart updates

## UI Layout

### 1. Sidebar Layout
- Width: 250px
- Structure:
  ```r
  costAnalysisUI_sidebar <- function(id) {
    ns <- NS(id)
    
    tagList(
      # Filter controls
      div(
        class = "mb-4",
        h6("Filters", class = "text-muted mb-3"),
        selectInput(ns("warehouse"), "Warehouse", choices = NULL),
        selectInput(ns("carrier"), "Carrier", choices = NULL),
        selectInput(ns("service_level"), "Service Level", choices = NULL),
        actionButton(ns("reset_filters"), "Reset Filters", icon = icon("rotate"))
      ),

      # Download section  
      div(
        class = "mb-4",
        h6("Downloads", class = "text-muted mb-3"),
        downloadButton(ns("download_cost_breakdown_csv"), "Cost Breakdown Data"),
        downloadButton(ns("download_unit_cost_csv"), "Unit Cost Data"),
        downloadButton(ns("download_efficiency_csv"), "Efficiency Matrix Data")
      )
    )
  }
  ```
### 2. Main Content Layout

- **Structure:**
  ```r
  costAnalysisUI_main <- function(id) {
    ns <- NS(id)
    
    tagList(
      # KPI Cards Row (4 cards)
      div(
        class = "mb-1",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = "Total Storage Cost",
            value = textOutput(ns("total_storage_cost")),
            ...
          ),
          value_box(...),  # Average Cost per Unit
          value_box(...),  # Ground Transport %
          value_box(...)   # Most Expensive Route
        )
      ),
  
      # Cost Analysis Charts
      div(
        class = "mt-2",
        layout_columns(
          col_widths = c(6, 6),
          card(
            full_screen = TRUE,
            card_header("Routes by Total Cost"),
            card_body(girafeOutput(ns("cost_breakdown")))
          ),
          card(
            full_screen = TRUE,
            card_header("Unit Cost Analysis"),
            card_body(girafeOutput(ns("unit_cost")))
          )
        )
      ),
  
      # Efficiency Matrix
      div(
        class = "mt-3",
        layout_columns(
          col_widths = c(12),
          card(
            full_screen = TRUE,
            card_header("Efficiency Matrix"),
            card_body(girafeOutput(ns("cost_efficiency")))
          )
        )
      )
    )
  }
  ```
## Testing

### 1. Data Validation Tests
```r
test_that("Cost Analysis data validation works", {
 testServer(costAnalysisServer, args = list(data = test_data), {
   # Test required data presence
   expect_true(!is.null(filtered_warehouse()))
   expect_true(nrow(filtered_warehouse()) > 0)
   
   # Test cost data structure
   expect_true(all(c("storage_cost", "cost_per_unit") %in% 
                   names(filtered_warehouse())))
   
   # Test for negative costs
   expect_true(all(filtered_warehouse()$storage_cost >= 0))
   expect_true(all(filtered_warehouse()$cost_per_unit >= 0))
 })
})
```

### 2. Filter Tests
```r
test_that("Cost Analysis filters work correctly", {
  testServer(costAnalysisServer, args = list(data = test_data), {
    # Test warehouse filter
    session$setInputs(warehouse = "PLANT01", carrier = "All")
    filtered <- filtered_warehouse()
    expect_equal(unique(filtered$plant_code), "PLANT01")
    
    # Test carrier filter
    session$setInputs(warehouse = "All", carrier = "CARRIER1")
    filtered <- filtered_shipping()
    expect_equal(unique(filtered$carrier), "CARRIER1")
    
    # Test combined filters
    session$setInputs(warehouse = "PLANT01", carrier = "CARRIER1")
    filtered <- filtered_shipping()
    expect_equal(nrow(filtered), 1)
  })
})
```

### 3. KPI Tests
```r
test_that("Cost KPI calculations are correct", {
  testServer(costAnalysisServer, args = list(data = test_data), {
    # Total storage cost
    expect_equal(
      sum(filtered_warehouse()$storage_cost, na.rm = TRUE),
      12000  # Expected sum from test data
    )
    
    # Average cost per unit
    expect_equal(
      mean(filtered_warehouse()$cost_per_unit, na.rm = TRUE),
      60  # Expected average from test data
    )
    
    # Ground transport percentage
    transport_data <- filtered_shipping()
    ground_total <- sum(transport_data$total_shipping_cost[
      transport_data$mode_dsc == "GROUND"
    ], na.rm = TRUE)
    total_cost <- sum(transport_data$total_shipping_cost, na.rm = TRUE)
    expect_equal(
      round(ground_total / total_cost * 100, 1),
      33.3  # Expected percentage from test data
    )
  })
})
```

### 4. Visualization Tests
```r
test_that("Cost visualizations handle edge cases", {
  testServer(costAnalysisServer, args = list(data = test_data), {
    # Test empty data handling
    session$setInputs(warehouse = "NONEXISTENT")
    expect_error(output$cost_breakdown)
    expect_error(output$unit_cost)
    expect_error(output$cost_efficiency)
    
    # Test single record handling
    session$setInputs(warehouse = "PLANT01", carrier = "CARRIER1")
    expect_no_error(output$cost_breakdown)
    expect_no_error(output$unit_cost)
    expect_no_error(output$cost_efficiency)
  })
})
```

## Best Practices

### 1. Code Organization
- **Module Structure**
 * Separate UI and Server functions
 * Clear function documentation
 * Consistent naming conventions
```r
# Function naming convention
costAnalysisUI_sidebar()    # UI components
costAnalysisUI_main()       # Main layout
costAnalysisServer()        # Server logic
```

- **Documentation Standards**
 * Use roxygen2 style comments
 * Include examples and parameter descriptions
 * Document dependencies and requirements

```r
#' Cost Analysis Tab Server Module
#' 
#' @param id The module ID corresponding to the UI element
#' @param data A list containing the supply chain datasets
#' @return A Shiny module server function
#' 
#' @details The cost analysis module handles:
#'   - Storage cost calculations
#'   - Unit cost analysis
#'   - Efficiency metrics
```

### 2. Error Handling Practices

- **Input Validation**
```r
# Cost data validation
validate_cost_data <- function(data) {
  # Check for negative costs
  if (any(data$storage_cost < 0, na.rm = TRUE)) {
    warning("Negative storage costs detected")
  }
  
  # Check for zero denominators
  if (any(data$total_units == 0, na.rm = TRUE)) {
    warning("Zero units detected - will affect unit cost calculations")
  }
}
```

- **Clear Error Messages**
 * Cost-specific error messages
 * Data validation feedback
 * Calculation error handling

### 3. Performance Best Practices

- **Cost Calculations**
 * Cache expensive calculations
 * Optimize joins between cost data
 * Minimize redundant calculations


- **Resource Management**
 * Clean up temporary cost data
 * Efficient memory usage
 * Proper reactive scoping


### 4. UI/UX Best Practices

- **Cost Data Presentation**
 *  Consistent decimal places
 * Clear unit labels
 * Informative tooltips


- **User Interactions**
 * Intuitive filter behavior
 * Clear cost breakdowns
 * Responsive feedback

## Maintenance and Updates

### 1. Version Control
- **Code Management**
 * Organized commits for cost module changes
 * Clear commit messages describing cost-related updates
 * Feature branches for cost analysis enhancements
 ```r
 # Example structure for cost module
 supply_chain_dashboard/
 ├── modules/
 │   ├── cost_analysis/
 │   │   ├── cost_analysis_ui.R
 │   │   ├── cost_analysis_server.R
 │   │   └── cost_calculations.R
 │   └── ...
 ├── tests/
 │   └── testthat/
 │       ├── test-cost-calculations.R
 │       └── test-cost-validation.R
 ```

### 2. Module Updates

- **Adding Cost Features**
 * Document new cost metrics
 * Update cost calculation tests
 * Maintain backwards compatibility
 
 ```r
 # Example of backwards compatible cost feature
 calculate_cost_metrics <- function(data, include_new_metric = FALSE) {
   base_metrics <- calculate_base_cost_metrics(data)
   
   if (include_new_metric) {
     base_metrics$new_metric <- calculate_new_cost_metric(data)
   }
   
   return(base_metrics)
 }
 ```

### 3. Update Checklist

- **Before Deployment**
 * Run all cost calculation tests
 * Verify cost metrics accuracy
 * Check performance with large datasets
 * Update documentation
 * Test cost data downloads


- **After Deployment**
 * Monitor cost calculation performance
 * Verify cost metric accuracy
 * Check error logs
 * Gather user feedback



### 4. Future Enhancements

- **Planned Improvements**
 * Enhanced cost forecasting
 * Additional efficiency metrics
 * Cost trend analysis
 * Advanced filtering options
 * Cost optimization recommendations

