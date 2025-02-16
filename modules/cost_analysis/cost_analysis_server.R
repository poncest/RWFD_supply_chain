# modules/cost_analysis/cost_analysis_server.R

costAnalysisServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
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
    
    # Reuse filter datasets from overview
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
    
    filtered_warehouse <- reactive({
      req(data$warehouse_costs)
      out <- data$warehouse_costs
      
      if (!is.null(input$warehouse) && input$warehouse != "All") {
        out <- out |> filter(plant_code == input$warehouse)
      }
      
      out
    })
    
    # KPI Outputs
    # Total Storage Cost KPI
    output$total_storage_cost <- renderText({
      validate(
        need(filtered_warehouse(), "Loading data..."),
        need(nrow(filtered_warehouse()) > 0, "No data")
      )
      
      cost <- sum(filtered_warehouse()$storage_cost, na.rm = TRUE)
      
      if (is.na(cost) || is.nan(cost) || cost == 0) {
        return("No data") 
      }
      scales::dollar(cost)
    })
    
    # Average Cost per Unit KPI
    output$avg_cost_unit <- renderText({
      validate(
        need(filtered_warehouse(), "Loading data..."),
        need(nrow(filtered_warehouse()) > 0, "No data")
      )
      
      avg <- mean(filtered_warehouse()$cost_per_unit, na.rm = TRUE)
      
      if (is.na(avg) || is.nan(avg) || avg == 0) {
        return(tags$span(style = "color: white;", "No data"))
      }
      scales::dollar(avg)
    })
    
    # Ground Transport Percentage KPI
    output$ground_transport_pct <- renderText({
      validate(
        need(filtered_shipping(), "Loading data..."),
        need(nrow(filtered_shipping()) > 0, "No data")
      )
      
      transport_summary <- filtered_shipping() %>%
        group_by(mode_dsc) %>%
        summarise(total_cost = sum(total_shipping_cost, na.rm = TRUE)) %>%
        ungroup()
      
      ground_data <- transport_summary %>%
        filter(mode_dsc == "GROUND")
      
      if (nrow(ground_data) == 0) {
        return("No data") 
      }
      
      total_cost <- sum(transport_summary$total_cost)
      ground_pct <- (ground_data$total_cost / total_cost) * 100
      
      if (is.na(ground_pct) || is.nan(ground_pct)) {
        return("No data") 
      }
      
      paste0(round(ground_pct, 1), "%")
    })
    
    # Most Expensive Route KPI
    output$top_route_cost <- renderText({
      validate(
        need(filtered_shipping(), "Loading data..."),
        need(nrow(filtered_shipping()) > 0, "No data")
      )
      
      route <- filtered_shipping() %>%
        group_by(origin_port, destination_port) %>%
        summarise(
          total_cost = sum(total_shipping_cost, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(total_cost)) %>%
        slice(1)
      
      if (nrow(route) == 0 || 
          is.na(route$total_cost) || 
          route$total_cost == 0) {
        return("No data") 
      }
      
      paste0(route$origin_port, " → ", route$destination_port)
    })
    
    # Update filter choices with sorted values
    observe({
      # Get unique values and sort them
      warehouse_choices <- unique(data$data_list$order_list$plant_code)
      warehouse_choices <- sort(warehouse_choices)
      warehouse_choices <- c("All", warehouse_choices)
      
      carrier_choices <- unique(data$data_list$order_list$carrier)
      carrier_choices <- sort(carrier_choices)
      carrier_choices <- c("All", carrier_choices)
      
      service_choices <- unique(data$data_list$order_list$service_level)
      service_choices <- sort(service_choices)
      service_choices <- c("All", service_choices)
      
      updateSelectInput(session, "warehouse",
                        choices = warehouse_choices)
      
      updateSelectInput(session, "carrier",
                        choices = carrier_choices)
      
      updateSelectInput(session, "service_level",
                        choices = service_choices)
    })
    
    # Reset filters observer
    observeEvent(input$reset_filters, {
      updateSelectInput(session, "warehouse", selected = "All")
      updateSelectInput(session, "carrier", selected = "All")
      updateSelectInput(session, "service_level", selected = "All")
    })
    
    # Plot Outputs
    #' Cost Breakdown Analysis Chart
    #' 
    #' Creates an interactive stacked bar chart visualizing the breakdown between transport
    #' and storage costs across different locations in the supply chain network.
    #'
    #' @param filtered_shipping Reactive dataset containing filtered shipping costs
    #' @param filtered_warehouse Reactive dataset containing filtered warehouse costs
    #'
    #' @details
    #' The visualization is designed to provide a comprehensive view of total costs,
    #' helping identify locations with high cost contributions and the balance between
    #' transport and storage costs.
    #' 
    #' Features:
    #'   * Stacked bar representation of costs by location
    #'   * Transport and storage cost breakdown
    #'   * Interactive tooltips with detailed cost information
    #'   * Percentage calculations for cost distribution
    #'   * Color coding to distinguish cost types
    #'   * Sorting by total cost for easy comparison
    #'   * Responsive to warehouse and carrier filters
    #'
    #' Cost Components:
    #'   * Transport Cost: Total shipping costs for the location
    #'   * Storage Cost: Associated warehouse storage costs
    #'   * Total Cost: Combined transport and storage costs
    #'
    #' @returns A ggiraph interactive plot object with:
    #'   * Stacked bar chart with interactive bars
    #'   * Legend for cost types
    #'   * Dollar-formatted axis labels
    #'   * Tooltips showing detailed cost breakdowns
    #'
    #' @examples
    #' output$cost_breakdown <- renderGirafe({
    #'   cost_breakdown_chart(filtered_shipping(), filtered_warehouse())
    #' })
    #'
    #' @section Data Requirements:
    #' Required columns in shipping_costs:
    #'   * origin_port (text): Origin location identifier
    #'   * total_shipping_cost (numeric): Total transport cost
    #'   * carrier (text): Carrier identifier
    #'   * mode_dsc (text): Transport mode (AIR/GROUND)
    #'
    #' Required columns in warehouse_costs:
    #'   * plant_code (text): Location identifier matching origin_port
    #'   * storage_cost (numeric): Associated storage cost
    #'
    #' @section Calculations:
    #' The visualization performs these key calculations:
    #'   * Aggregates transport costs by location
    #'   * Maps warehouse storage costs to shipping locations
    #'   * Calculates percentage contribution to total costs
    #'   * Handles cost summation with NA handling
    #'
    #' @section Validation:
    #' The function validates:
    #'   * Required columns presence
    #'   * Numeric cost values
    #'   * Matching location identifiers
    #'   * Appropriate handling of missing values
    output$cost_breakdown <- renderGirafe({
        validate(
          need(filtered_shipping(), "Loading shipping data..."),
          need(nrow(filtered_shipping()) > 0, "No data available for selected filters"),
          need(validate_plot_data(filtered_shipping(), 
                                  c("origin_port", "total_shipping_cost"), "Cost Breakdown"), 
               "Invalid data structure for visualization")
        )
      
      # First map PORT to PLANT
      shipping_with_plant <- filtered_shipping() |>
        mutate(plant_code = str_replace(origin_port, "PORT", "PLANT"))
      
      # Create the plot
      p <- shipping_with_plant |>
        left_join(filtered_warehouse(), by = "plant_code") |>
        group_by(plant_code) |>
        summarise(
          transport_cost = sum(total_shipping_cost),
          storage_cost = first(storage_cost),
          total_cost = transport_cost + storage_cost,
          .groups = "drop"
        ) |>
        pivot_longer(
          cols = c(transport_cost, storage_cost),
          names_to = "cost_type",
          values_to = "amount"
        ) |>
        mutate(
          cost_type = str_to_title(str_replace(cost_type, "_", " ")),
          pct_of_total = amount / sum(amount) * 100,
          tooltip_text = glue::glue(
            "Location Details\n",
            "───────────────\n",
            "Warehouse: {plant_code}\n",
            "Cost Type: {cost_type}\n",
            "───────────────\n",
            "Amount: {scales::dollar(amount)}\n",
            "% of Total: {scales::percent(pct_of_total/100, accuracy=0.1)}"
          )
        ) |>
        ggplot(aes(x = reorder(plant_code, -amount), y = amount, fill = cost_type)) +
        geom_col_interactive(
          aes(
            tooltip = tooltip_text,
            data_id = plant_code
          ),
          position = "stack"
        ) +
        scale_y_continuous(
          labels = scales::dollar_format(),
          expand = expansion(mult = c(0, 0.1))
        ) +
        scale_fill_viridis_d(
          option = "magma", 
          begin = 0.2, 
          end = 0.8
        ) +
        labs(
          title = "Routes by Total Cost",
          subtitle = "Top routes by combined transport and storage costs",
          x = NULL,
          y = "Total Cost (USD)",
          fill = "Cost Type"
        ) +
        theme_custom() +
        theme(
          legend.position = "top",
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(hjust = 0.5)
        )
      
      girafe(
        ggobj = p,
        width_svg = 8,
        height_svg = 6,
        options = list(
          opts_hover(css = "fill:orange;"),
          opts_tooltip(
            css = "background-color:white;color:black;padding:10px;border-radius:5px;",
            opacity = 0.8
          ),
          opts_selection(css = "fill:orange;stroke:black;stroke-width:1.5px;")
        )
      )
    })
    
    #' Unit Cost Analysis Chart
    #' 
    #' Creates an interactive scatter plot comparing cost per unit vs cost per KG,
    #' with quadrant analysis for cost efficiency categorization.
    #'
    #' @param filtered_warehouse Reactive dataset containing filtered warehouse costs
    #' @param full_warehouse Complete warehouse dataset for median calculations
    #'
    #' @details
    #' The visualization is designed to analyze warehouse efficiency by comparing
    #' two key metrics: cost per unit and cost per KG. It helps identify warehouses
    #' that may need optimization in their operations.
    #' 
    #' Features:
    #'   * Interactive quadrant analysis showing cost efficiency categories
    #'   * Size encoding representing total units handled
    #'   * Color encoding showing total order volume
    #'   * Reference lines at median values for both metrics
    #'   * Hover tooltips with detailed metrics
    #'   * Responsive to warehouse and carrier filters
    #' 
    #' Quadrant Definitions:
    #'   * High Cost: Above median for both unit and KG costs
    #'   * Optimal: Below median for both metrics
    #'   * High Unit Cost: Above median unit cost, below median KG cost
    #'   * High KG Cost: Above median KG cost, below median unit cost
    #'
    #' @returns A ggiraph interactive plot object with the following elements:
    #'   * Scatter plot with interactive points
    #'   * Quadrant labels and backgrounds
    #'   * Legend for size and color scales
    #'   * Tooltips with detailed metrics
    #'
    #' @examples
    #' output$unit_cost <- renderGirafe({
    #'   unit_cost_analysis_chart(filtered_warehouse(), data$warehouse_costs)
    #' })
    #'
    #' @section Data Requirements:
    #' Required columns in warehouse_costs:
    #'   * plant_code (text): Unique warehouse identifier
    #'   * cost_per_unit (numeric): Cost per handling unit
    #'   * cost_per_kg (numeric): Cost per kilogram
    #'   * total_units (numeric): Total units handled
    #'   * total_orders (numeric): Total number of orders
    #'
    #' @section Validation:
    #' The function performs the following validations:
    #'   * Checks for required columns
    #'   * Validates numeric values
    #'   * Handles missing values appropriately
    #'   * Warns about negative costs
    output$unit_cost <- renderGirafe({
      validate(
        need(filtered_warehouse(), "Loading warehouse data..."),
        need(nrow(filtered_warehouse()) > 0, "No data available for selected filters"),
        need(validate_plot_data(filtered_warehouse(),
                                c("cost_per_unit", "cost_per_kg", "total_units", "total_orders"),
                                "Unit Cost Analysis"),
             "Invalid data structure for visualization")
      )
      
      # Calculate statistics from full dataset for consistent quadrants
      full_stats <- data$warehouse_costs |>
        summarise(
          median_cost_unit = median(cost_per_unit),
          median_cost_kg = median(cost_per_kg)
        )
      
      p <- filtered_warehouse() |>
        mutate(
          quadrant = case_when(
            cost_per_unit >= full_stats$median_cost_unit & cost_per_kg >= full_stats$median_cost_kg ~ "High Cost",
            cost_per_unit < full_stats$median_cost_unit & cost_per_kg < full_stats$median_cost_kg ~ "Optimal",
            cost_per_unit >= full_stats$median_cost_unit & cost_per_kg < full_stats$median_cost_kg ~ "High Unit Cost",
            TRUE ~ "High KG Cost"
          ),
          tooltip_text = glue::glue(
            "Location Details\n",
            "───────────────\n",
            "Warehouse: {plant_code}\n",
            "Zone: {quadrant}\n",
            "───────────────\n",
            "Cost Details:\n",
            "Cost per Unit: {scales::dollar(cost_per_unit)}\n",
            "Cost per KG: {scales::dollar(cost_per_kg)}\n",
            "───────────────\n",
            "Volume Details:\n",
            "Total Units: {scales::comma(total_units)}\n",
            "Total Orders: {scales::comma(total_orders)}"
          )
        ) |>
        ggplot(aes(x = cost_per_unit, y = cost_per_kg)) +
        # Add quadrant reference lines using full dataset medians
        geom_vline(xintercept = full_stats$median_cost_unit, linetype = "dashed", alpha = 0.3) +
        geom_hline(yintercept = full_stats$median_cost_kg, linetype = "dashed", alpha = 0.3) +
        # Add points
        geom_point_interactive(
          aes(
            size = total_units,
            fill = total_orders,
            tooltip = tooltip_text,
            data_id = plant_code
          ),
          alpha = 0.7,
          shape = 21
        ) +
        # Add more visible quadrant labels with backgrounds
        annotate("rect", 
                 xmin = min(data$warehouse_costs$cost_per_unit), 
                 xmax = min(data$warehouse_costs$cost_per_unit) + diff(range(data$warehouse_costs$cost_per_unit))*0.2,
                 ymin = max(data$warehouse_costs$cost_per_kg) - diff(range(data$warehouse_costs$cost_per_kg))*0.2,
                 ymax = max(data$warehouse_costs$cost_per_kg),
                 alpha = 0.1,
                 fill = "gray90") +
        annotate("label", 
                 x = min(data$warehouse_costs$cost_per_unit) + diff(range(data$warehouse_costs$cost_per_unit))*0.1,
                 y = max(data$warehouse_costs$cost_per_kg) - diff(range(data$warehouse_costs$cost_per_kg))*0.1,
                 label = "High KG Cost\nLow Unit Cost",
                 hjust = 0.5,
                 vjust = 0.5,
                 alpha = 0.7,
                 fill = "white",
                 label.size = 0.5) +
        # Repeat for other quadrants with appropriate positioning
        annotate("rect",
                 xmin = max(data$warehouse_costs$cost_per_unit) - diff(range(data$warehouse_costs$cost_per_unit))*0.2,
                 xmax = max(data$warehouse_costs$cost_per_unit),
                 ymin = max(data$warehouse_costs$cost_per_kg) - diff(range(data$warehouse_costs$cost_per_kg))*0.2,
                 ymax = max(data$warehouse_costs$cost_per_kg),
                 alpha = 0.1,
                 fill = "gray90") +
        annotate("label",
                 x = max(data$warehouse_costs$cost_per_unit) - diff(range(data$warehouse_costs$cost_per_unit))*0.1,
                 y = max(data$warehouse_costs$cost_per_kg) - diff(range(data$warehouse_costs$cost_per_kg))*0.1,
                 label = "High Cost\nBoth Metrics",
                 hjust = 0.5,
                 vjust = 0.5,
                 alpha = 0.7,
                 fill = "white",
                 label.size = 0.5) +
        # Bottom left quadrant
        annotate("rect",
                 xmin = min(data$warehouse_costs$cost_per_unit),
                 xmax = min(data$warehouse_costs$cost_per_unit) + diff(range(data$warehouse_costs$cost_per_unit))*0.2,
                 ymin = min(data$warehouse_costs$cost_per_kg),
                 ymax = min(data$warehouse_costs$cost_per_kg) + diff(range(data$warehouse_costs$cost_per_kg))*0.2,
                 alpha = 0.1,
                 fill = "gray90") +
        annotate("label",
                 x = min(data$warehouse_costs$cost_per_unit) + diff(range(data$warehouse_costs$cost_per_unit))*0.1,
                 y = min(data$warehouse_costs$cost_per_kg) + diff(range(data$warehouse_costs$cost_per_kg))*0.1,
                 label = "Optimal\nLow Cost",
                 hjust = 0.5,
                 vjust = 0.5,
                 alpha = 0.7,
                 fill = "white",
                 label.size = 0.5) +
        # Bottom right quadrant
        annotate("rect",
                 xmin = max(data$warehouse_costs$cost_per_unit) - diff(range(data$warehouse_costs$cost_per_unit))*0.2,
                 xmax = max(data$warehouse_costs$cost_per_unit),
                 ymin = min(data$warehouse_costs$cost_per_kg),
                 ymax = min(data$warehouse_costs$cost_per_kg) + diff(range(data$warehouse_costs$cost_per_kg))*0.2,
                 alpha = 0.1,
                 fill = "gray90") +
        annotate("label",
                 x = max(data$warehouse_costs$cost_per_unit) - diff(range(data$warehouse_costs$cost_per_unit))*0.1,
                 y = min(data$warehouse_costs$cost_per_kg) + diff(range(data$warehouse_costs$cost_per_kg))*0.1,
                 label = "High Unit Cost\nLow KG Cost",
                 hjust = 0.5,
                 vjust = 0.5,
                 alpha = 0.7,
                 fill = "white",
                 label.size = 0.5) +
        # Scales
        scale_x_continuous(
          labels = scales::dollar_format(),
          breaks = scales::pretty_breaks(n = 6)
        ) +
        scale_y_continuous(
          labels = scales::dollar_format(),
          breaks = scales::pretty_breaks(n = 6)
        ) +
        scale_fill_viridis_c(option = "magma", begin = 0.2, end = 0.8) +
        scale_size_continuous(
          labels = scales::comma,
          breaks = scales::pretty_breaks(n = 4)
        ) +
        # Labels
        labs(
          title = "Unit Cost Analysis by Location",
          subtitle = "Cost per Unit vs Cost per KG, sized by volume",
          x = "Cost per Unit ($)",
          y = "Cost per KG ($)",
          size = "Total Units",
          fill = "Total Orders"
        ) +
        theme_custom() +
        theme(
          legend.position = "right",
          panel.grid.minor = element_blank()
        )
      
      girafe(
        ggobj = p,
        width_svg = 8,
        height_svg = 6,
        options = list(
          opts_hover(css = "fill:orange;"),
          opts_tooltip(
            css = "background-color:white;color:black;padding:10px;border-radius:5px;",
            opacity = 0.8
          ),
          opts_selection(css = "fill:orange;stroke:black;stroke-width:1.5px;")
        )
      )
    })
    
    #' Cost Efficiency Matrix Visualization
    #' 
    #' Creates an interactive heatmap displaying efficiency scores across multiple cost 
    #' and performance metrics for each warehouse location.
    #'
    #' @param filtered_warehouse Reactive dataset containing filtered warehouse costs
    #'
    #' @details
    #' The visualization provides a comprehensive view of warehouse performance across
    #' different efficiency metrics, normalized to allow direct comparisons. Higher
    #' scores (darker colors) indicate better performance.
    #' 
    #' Features:
    #'   * Heatmap representation of efficiency scores
    #'   * Interactive cells with detailed tooltips
    #'   * Percentage-based efficiency scores
    #'   * Performance categorization (Excellent to Needs Improvement)
    #'   * Text labels showing exact values
    #'   * Responsive to warehouse and carrier filters
    #'
    #' Efficiency Metrics:
    #'   * Storage Efficiency: Normalized storage cost (inverted)
    #'   * Unit Cost Efficiency: Normalized cost per unit (inverted)
    #'   * KG Cost Efficiency: Normalized cost per KG (inverted)
    #'   * Volume Efficiency: Normalized total units handled
    #'
    #' Performance Categories:
    #'   * Excellent: Score >= 75%
    #'   * Good: Score >= 50%
    #'   * Fair: Score >= 25%
    #'   * Needs Improvement: Score < 25%
    #'
    #' @returns A ggiraph interactive plot object with:
    #'   * Heatmap with interactive cells
    #'   * Percentage-based color scale
    #'   * Text labels showing scores
    #'   * Detailed tooltips
    #'
    #' @examples
    #' output$cost_efficiency <- renderGirafe({
    #'   efficiency_matrix_chart(filtered_warehouse())
    #' })
    #'
    #' @section Data Requirements:
    #' Required columns in warehouse_costs:
    #'   * plant_code (text): Warehouse identifier
    #'   * storage_cost (numeric): Total storage cost
    #'   * cost_per_unit (numeric): Cost per handling unit
    #'   * cost_per_kg (numeric): Cost per kilogram
    #'   * total_units (numeric): Total units handled
    #'
    #' @section Calculations:
    #' The visualization performs these normalizations:
    #'   * Rescales all metrics to 0-1 range
    #'   * Inverts cost metrics so higher is better
    #'   * Calculates relative performance scores
    #'   * Handles score categorization
    #'
    #' @section Validation:
    #' The function validates:
    #'   * Required columns presence
    #'   * Numeric values for calculations
    #'   * Non-negative cost values
    #'   * Appropriate handling of missing values
    output$cost_efficiency <- renderGirafe({
      validate(
        need(filtered_warehouse(), "Loading warehouse data..."),
        need(nrow(filtered_warehouse()) > 0, "No data available for selected filters"),
        need(validate_plot_data(filtered_warehouse(),
                                c("storage_cost", "cost_per_unit", "cost_per_kg", "total_units"),
                                "Efficiency Matrix"),
             "Invalid data structure for visualization")
      )
      
      # Calculate efficiency scores
      efficiency_matrix <- filtered_warehouse() |>
        mutate(
          storage_efficiency = scales::rescale(max(storage_cost) - storage_cost),
          unit_cost_efficiency = scales::rescale(max(cost_per_unit) - cost_per_unit),
          kg_cost_efficiency = scales::rescale(max(cost_per_kg) - cost_per_kg),
          volume_efficiency = scales::rescale(total_units)
        ) |>
        select(plant_code, ends_with("efficiency")) |>
        pivot_longer(
          cols = ends_with("efficiency"),
          names_to = "metric",
          values_to = "score"
        ) |>
        mutate(
          metric = str_replace(metric, "_efficiency", ""),
          metric = str_to_title(str_replace(metric, "_", " ")),
          performance = case_when(
            score >= 0.75 ~ "Excellent",
            score >= 0.50 ~ "Good",
            score >= 0.25 ~ "Fair",
            TRUE ~ "Needs Improvement"
          ),
          tooltip_text = glue::glue(
            "Location Details\n",
            "───────────────\n",
            "Warehouse: {plant_code}\n",
            "Metric: {metric}\n",
            "───────────────\n",
            "Efficiency Score: {scales::percent(score, accuracy=0.1)}\n",
            "Relative Performance: {performance}"
          )
        )
      
      # Create enhanced heatmap
      p <- efficiency_matrix |>
        ggplot(aes(x = plant_code, y = metric)) +
        geom_tile_interactive(
          aes(
            fill = score,
            tooltip = tooltip_text,
            data_id = paste(plant_code, metric) 
          ),
          color = "white",
          linewidth = 0.5
        ) +
        geom_text(
          aes(label = scales::percent(score, accuracy = 1)),
          color = "white",
          size = 3
        ) +
        scale_fill_viridis_c(
          option = "magma",
          begin = 0.2,
          end = 0.8,
          labels = scales::percent,
          limits = c(0, 1),
          breaks = seq(0, 1, 0.25)
        ) +
        labs(
          title = "Efficiency Matrix by Metric",
          subtitle = "Relative performance across key metrics (higher is better)",
          x = "Warehouse Location",
          y = NULL,
          fill = "Efficiency\nScore"
        ) +
        theme_custom() +
        theme(
          axis.text.x = element_text(hjust = 1),
          panel.grid = element_blank(),
          legend.position = "right",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          plot.subtitle = element_text(size = 10, color = "gray40")
        )
      
      girafe(
        ggobj = p,
        width_svg = 12,
        height_svg = 4,
        options = list(
          opts_hover(css = "fill:orange;"),
          opts_tooltip(
            css = "background-color:white;color:black;padding:10px;border-radius:5px;",
            opacity = 0.8
          ),
          opts_selection(css = "fill:orange;stroke:black;stroke-width:1.5px;")
        )
      )
    })
    
    # Download Datasets
    download_datasets <- reactive({
      list(
        cost_breakdown = filtered_shipping() |>
          left_join(
            filtered_warehouse() |> select(plant_code, storage_cost),
            by = c("origin_port" = "plant_code")
          ) |>
          group_by(origin_port) |>
          summarise(
            transport_cost = sum(total_shipping_cost, na.rm = TRUE),
            storage_cost = first(storage_cost, na_rm = TRUE),
            total_cost = transport_cost + storage_cost,
            .groups = "drop"
          ) |>
          # Calculate percentages from total_cost
          mutate(
            pct_transport = ifelse(total_cost > 0,
                                   transport_cost / total_cost * 100, 0),
            pct_storage = ifelse(total_cost > 0,
                                 storage_cost / total_cost * 100, 0)
          ),
        
        unit_cost = filtered_warehouse() |>
          select(plant_code, cost_per_unit, cost_per_kg, total_units, total_orders) |>
          mutate(
            quadrant = case_when(
              cost_per_unit >= median(cost_per_unit) & cost_per_kg >= median(cost_per_kg) ~ "High Cost",
              cost_per_unit < median(cost_per_unit) & cost_per_kg < median(cost_per_kg) ~ "Optimal",
              cost_per_unit >= median(cost_per_unit) & cost_per_kg < median(cost_per_kg) ~ "High Unit Cost",
              TRUE ~ "High KG Cost"
            )
          ),
        
        efficiency_matrix = filtered_warehouse() |>
          mutate(
            storage_efficiency = scales::rescale(-storage_cost),
            unit_cost_efficiency = scales::rescale(-cost_per_unit),
            kg_cost_efficiency = scales::rescale(-cost_per_kg),
            volume_efficiency = scales::rescale(total_units)
          ) |>
          select(
            plant_code,
            storage_efficiency,
            unit_cost_efficiency,
            kg_cost_efficiency,
            volume_efficiency
          )
      )
    })
    
    # Download Handlers
    output$download_cost_breakdown_csv <- downloadHandler(
      filename = function() {
        paste0("cost_breakdown_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        write.csv(download_datasets()$cost_breakdown, file, row.names = FALSE)
      }
    )
    
    output$download_unit_cost_csv <- downloadHandler(
      filename = function() {
        paste0("unit_cost_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        write.csv(download_datasets()$unit_cost, file, row.names = FALSE)
      }
    )
    
    output$download_efficiency_csv <- downloadHandler(
      filename = function() {
        paste0("efficiency_matrix_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        write.csv(download_datasets()$efficiency_matrix, file, row.names = FALSE)
      }
    )
    
  })  # End of moduleServer
} # End of costAnalysisServer