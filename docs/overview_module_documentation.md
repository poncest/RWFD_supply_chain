# Overview Module Documentation

## Purpose
The Overview module serves as the main landing page of the Supply Chain Dashboard, providing high-level metrics and key visualizations of supply chain operations.

## Module Structure

### Files
- `overview_ui.R`: UI components and layout definitions
- `overview_server.R`: Server-side logic and data processing

### Components

#### 1. KPI Cards
- **Total Shipping Cost**: Aggregate shipping costs across selected data
  - Function: `output$total_cost`
  - Format: Currency (USD)
  - Error State: "No data"

- **Avg Cost per KG**: Cost efficiency metric
  - Function: `output$avg_cost_kg`
  - Calculation: total_cost / total_weight
  - Format: Currency per KG

- **Warehouse Utilization**: Capacity usage metric
  - Function: `output$warehouse_util`
  - Format: Percentage
  - Source: warehouse_capacity data

- **On-Time Delivery**: Service level metric
  - Function: `output$otd_rate`
  - Format: Percentage
  - Calculation: Based on order completion times

#### 2. Interactive Visualizations

1. **Top Routes by Cost**
   - Type: Interactive bar chart
   - Data: Filtered shipping costs by route
   - Features:
     * Cost breakdown by route
     * Transport mode coloring
     * Interactive tooltips with detailed route information
     * Cost and order metrics
   - Function: `output$plot_routes`
   - Error State: "No Routes Data Available for {filter}"

2. **Storage Costs by Warehouse**
   - Type: Interactive bar chart
   - Data: Top 10 warehouses by cost
   - Features:
     * Cost rankings
     * Percentage breakdowns
     * Unit cost metrics
     * Interactive tooltips with warehouse details
   - Function: `output$plot_storage`
   - Error State: "No Storage Data Available for {filter}"

3. **Transport Mode Analysis**
   - Type: Interactive bar chart
   - Data: Cost comparison between transport modes
   - Features:
     * Air vs Ground comparison
     * Cost breakdowns
     * Weight and order metrics
     * Percentage distributions
   - Function: `output$plot_transport`
   - Error State: "No Transport Data Available for {filter}"

4. **Service Level Distribution**
   - Type: Interactive bar chart
   - Data: Order distribution by service type
   - Features:
     * Service level breakdown (CRF, DTD, DTP)
     * Order counts
     * Percentage distributions
     * Service type definitions in tooltips
   - Function: `output$plot_service`
   - Error State: "No Service Level Data Available for {filter}"

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
  * **Routes Data Download**
    - Function: `download_routes_csv`
    - Filename: routes_data_YYYYMMDD.csv
    - Content: Filtered route cost summaries

  * **Storage Data Download**
    - Function: `download_storage_csv`
    - Filename: storage_costs_YYYYMMDD.csv
    - Content: Warehouse storage metrics

  * **Transport Data Download**
    - Function: `download_transport_csv`
    - Filename: transport_mode_YYYYMMDD.csv
    - Content: Transport mode analysis

  * **Service Level Data Download**
    - Function: `download_service_csv`
    - Filename: service_level_YYYYMMDD.csv
    - Content: Service level distributions

## Data Flow

### 1. Reactive Datasets
- **filtered_shipping**: Filters shipping cost data
  ```r
  filtered_shipping <- reactive({
    req(data$shipping_costs)
    out <- data$shipping_costs
    
    # Filter by warehouse/origin_port
    if (!is.null(input$warehouse) && input$warehouse != "All") {
      out <- out |> filter(origin_port == input$warehouse)
    }
    
    # Filter by carrier
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
  
  # Filter by warehouse
  if (!is.null(input$warehouse) && input$warehouse != "All") {
    out <- out |> filter(plant_code == input$warehouse)
  }
  
  # Filter by carrier relationships
  if (!is.null(input$carrier) && input$carrier != "All") {
    carriers_plants <- data$data_list$order_list |>
      filter(carrier == input$carrier) |>
      distinct(plant_code)
    
    out <- out |> semi_join(carriers_plants, by = "plant_code")
  }
  
  out
 })
 ```
 
 - **filtered_orders**: Filters order list data
  ```r
  filtered_orders <- reactive({
  req(data$data_list$order_list)
  out <- data$data_list$order_list
  
  # Apply all filters
  if (!is.null(input$warehouse) && input$warehouse != "All") {
    out <- out |> filter(plant_code == input$warehouse)
  }
  if (!is.null(input$carrier) && input$carrier != "All") {
    out <- out |> filter(carrier == input$carrier)
  }
  if (!is.null(input$service_level) && input$service_level != "All") {
    out <- out |> filter(service_level == input$service_level)
  }
  
  out
 })
 ```
 
### 2. Data Dependencies

- Each KPI depends on specific filtered datasets:

  * Total Cost → filtered_shipping
  * Avg Cost per KG → filtered_shipping
  * Warehouse Utilization → filtered_warehouse_capacity
  * On-Time Delivery → filtered_orders

- Each visualization depends on relevant filtered data:

  * Routes Chart → filtered_shipping
  * Storage Chart → filtered_warehouse
  * Transport Chart → filtered_shipping
  * Service Chart → filtered_orders

## Error Handling

### 1. Data Validation
- **Required Data Checks**
  ```r
  # Using req() for reactive dependencies
  validate(need(filtered_shipping(), "Loading data..."))
  ```

- **Row Count Validation**
  ```r
  # Check for empty results
  validate(
  need(nrow(filtered_shipping()) > 0, 
       sprintf("No Routes Data Available for %s",
               if(input$warehouse != "All") input$warehouse else "Selected Filters")
  )
 )
  ```

- **NA Value Handling**
  ```r
  # Sum with NA handling
  cost <- sum(filtered_shipping()$total_shipping_cost, na.rm = TRUE)

  # Mean with NA handling
  avg_cost <- mean(filtered_warehouse()$cost_per_unit, na.rm = TRUE)
  ```

### 2. Error Messages

- Loading States

  * Initial load: "Loading data..."
  * Specific data: "Loading shipping data...", "Loading warehouse data..."


- No Data States

  * Generic: "No data"
  * Filtered: "No data available for selected filters"
  * Specific: "No Routes Data Available for PLANT01"



## Performance Considerations

### 1. Data Filtering Optimization

  - Filter Operation Logging
  ```r
  message(sprintf("Applying filters - Warehouse: %s, Carrier: %s", 
                input$warehouse, input$carrier))
  ```
  
  - Execution Time Tracking
  ```r
  start_time <- Sys.time()
  # ... filtering operations ...
  end_time <- Sys.time()
  message(sprintf("filtered_shipping execution time: %.2f seconds - Rows: %d", 
                  as.numeric(end_time - start_time), nrow(out)))
  ```

### 2. Visualization Performance

- Data Limitations

  * Top 10 records for storage costs
  * Efficient data transformations using dplyr
  * Minimal reactive dependencies


- Chart Optimization

  * Use of withSpinner for loading states
  * Efficient tooltip generation
  * Minimal chart updates

## UI Layout

### 1. Sidebar Layout
- Width: 250px
- Structure:
 ```r
 overviewUI_sidebar <- function(id) {
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
       downloadButton(...),
     ),

     # Social Links
     div(
       class = "mt-auto",
       hr(),
       h6("Connect With Us", class = "text-muted mb-3"),
       social_links_div()
     )
   )
 }
 ```


### 2. Main Content Layout
- Structure:
 ```r
 overviewUI_main <- function(id) {
  ns <- NS(id)
  
  tagList(
    # KPI Cards Row (4 cards)
    div(
      class = "mb-1",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(...),
        value_box(...),
        value_box(...),
        value_box(...)
      )
    ),

    # First Row of Charts (2 cards)
    div(
      class = "mt-2",
      layout_columns(
        col_widths = c(6, 6),
        card(...),
        card(...)
      )
    ),

    # Second Row of Charts (2 cards)
    div(
      class = "mt-3",
      layout_columns(
        col_widths = c(6, 6),
        card(...),
        card(...)
      )
    )
  )
 }
 ```

## Testing

### 1. Data Validation Test
```r
test_that("Required data is available", {
  # Test shipping data presence
  expect_true(!is.null(data$shipping_costs))
  expect_true(nrow(data$shipping_costs) > 0)
  
  # Test required columns
  expect_true(all(c("total_shipping_cost", "total_weight") %in% 
                  names(data$shipping_costs)))
})
```

### 2. Filter Test
```r
test_that("Filters work correctly", {
  testServer(overviewServer, {
    # Test warehouse filter
    session$setInputs(warehouse = "PLANT01", carrier = "All")
    filtered <- filtered_shipping()
    expect_equal(unique(filtered$origin_port), "PLANT01")
    
    # Test carrier filter
    session$setInputs(warehouse = "All", carrier = "CARRIER1")
    filtered <- filtered_shipping()
    expect_equal(unique(filtered$carrier), "CARRIER1")
    
    # Test reset functionality
    session$setInputs(reset_filters = 1)
    expect_equal(input$warehouse, "All")
    expect_equal(input$carrier, "All")
  })
})
```

### 3. KPI Test
```r
test_that("KPI calculations are correct", {
  testServer(overviewServer, {
    # Test total cost calculation
    total <- sum(filtered_shipping()$total_shipping_cost, na.rm = TRUE)
    expect_type(total, "double")
    expect_true(!is.na(total))
    
    # Test avg cost per kg
    cost_kg <- total / sum(filtered_shipping()$total_weight, na.rm = TRUE)
    expect_type(cost_kg, "double")
    expect_true(!is.na(cost_kg))
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
 overviewUI_sidebar()    # UI components
 overviewUI_main()       # Main layout
 overviewServer()        # Server logic
 
 ```
- **Documentation Standards**

 * Use roxygen2 style comments
 * Include examples and parameter descriptions
 * Document dependencies and requirements
 
 ```r
 #' Overview Tab Server Module
 #' 
 #' @param id The module ID corresponding to the UI element
 #' @param data A list containing the supply chain datasets
 #' @return A Shiny module server function
 ```

### 2. Error Handling Practices
- **Input Validation**
  ```r
  # Always check data requirements
  req(data$shipping_costs)
  
  # Validate filter combinations
  validate(
    need(filtered_shipping(), "Loading data..."),
    need(nrow(filtered_shipping()) > 0, "No data available")
  )
  ```
- **Clear Error Messages**
 * User-friendly messages
 * Context-specific errors
 * Actionable feedback



### 3. Performance Best Practices
- **Reactive Dependencies**

 * Minimize unnecessary reactions
 * Use appropriate reactive scope
 * Cache expensive calculations


- **Data Transformation**
  ```r
  # Efficient dplyr operations
  filtered_data |>
    group_by(key_column) |>
    summarise(
      total = sum(value, na.rm = TRUE),
      .groups = "drop"
    )
  ```

- **Resource Management**

 * Release resources properly
 * Clean up observers
 * Handle large datasets efficiently



### 4. UI/UX Best Practices

- **Consistent Layout**

 * Standard margins and padding
 * Aligned components
 * Responsive design


- **User Feedback**

 * Loading indicators
 * Clear error states
 * Interactive elements


- **Accessibility**

 * Descriptive button labels
 * Color contrast
 * Keyboard navigation

## Maintenance and Updates

### 1. Version Control
- **Code Management**
 * Organized commits
 * Clear commit messages
 * Feature branches for updates
 ```r
 # Example structure
 supply_chain_dashboard/
 ├── modules/
 │   ├── overview/
 │   │   ├── overview_ui.R
 │   │   └── overview_server.R
 │   └── ...
 ├── R/
 │   └── utils/
 └── tests/
 ```

### 2. Module Updates

- **Adding Features**

 * Update documentation
 * Add corresponding tests
 * Maintain backwards compatibility

  
  