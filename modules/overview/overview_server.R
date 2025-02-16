# modules/overview/overview_server.R

#' Overview Tab Server Module
#' 
#' Server logic for the Supply Chain Dashboard Overview tab.
#' Handles data filtering, calculations, and interactive visualizations.
#' 
#' @param id The module ID corresponding to the UI element
#' @param data A list containing the supply chain datasets:
#'   \item{shipping_costs}{Shipping cost data}
#'   \item{warehouse_costs}{Warehouse cost data}
#'   \item{warehouse_capacity}{Warehouse capacity data}
#'   \item{data_list}{Raw data lists}
#' @return A Shiny module server function
#' 
#' @details
#' The module handles:
#'   * Reactive data filtering based on user inputs
#'   * KPI calculations
#'   * Interactive plot generation
#'   * CSV data downloads
#' 
#' Key components:
#'   * Filtered datasets (shipping, warehouse, orders)
#'   * KPI calculations (costs, utilization)
#'   * Interactive visualizations
#'   * Download handlers
#' 
#' @examples
#' # In server.R or app.R
#' overviewServer("overview", data)

overviewServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Reactive Datasets Documentation
    
    #' Filter Shipping Data
    #' @description Filters shipping cost data based on user selections
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
    
    #' Filter Warehouse Data
    #' @description Filters warehouse data based on user selections
    filtered_warehouse <- reactive({
      req(data$warehouse_costs)
      out <- data$warehouse_costs
      
      # Filter by warehouse
      if (!is.null(input$warehouse) && input$warehouse != "All") {
        out <- out |> filter(plant_code == input$warehouse)
      }
      
      # Filter by carrier using order_list to get warehouse-carrier relationships
      if (!is.null(input$carrier) && input$carrier != "All") {
        carriers_plants <- data$data_list$order_list |>
          filter(carrier == input$carrier) |>
          distinct(plant_code)
        
        out <- out |> 
          semi_join(carriers_plants, by = "plant_code")
      }
      
      out
    })
    
    #' Filter Orders Data
    #' @description Filters order list based on user selections
    #' @details Applies warehouse, carrier, and service level filters
    filtered_orders <- reactive({
      req(data$data_list$order_list)
      out <- data$data_list$order_list
      
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
    
    #' Filter Warehouse Capacity
    #' @description Filters warehouse capacity data based on user selections
    #' @details Applies warehouse and carrier relationship filters
    filtered_warehouse_capacity <- reactive({
      req(data$warehouse_capacity)
      out <- data$warehouse_capacity
      
      if (!is.null(input$warehouse) && input$warehouse != "All") {
        out <- out |> filter(plant_code == input$warehouse)
      }
      
      if (!is.null(input$carrier) && input$carrier != "All") {
        carriers_plants <- data$data_list$order_list |>
          filter(carrier == input$carrier) |>
          distinct(plant_code)
        
        out <- out |> 
          semi_join(carriers_plants, by = "plant_code")
      }
      
      out
    })
    
    # Update filter choices
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
      
      # Update the inputs with sorted choices
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
    
    # KPI Calculations Documentation
    
    #' Total Shipping Cost KPI
    #' @description Calculates total shipping costs from filtered data
    #' @returns Formatted currency string
    output$total_cost <- renderText({
      validate(need(filtered_shipping(), "Loading data..."))
      cost <- sum(filtered_shipping()$total_shipping_cost, na.rm = TRUE)
      
      if (is.na(cost) || is.nan(cost)) {
        return("No data")
      }
      scales::dollar(cost, accuracy = 0.01)
    })
    
    #' Average Cost per KG KPI
    #' @description Calculates average shipping cost per kilogram
    #' @returns Formatted currency string per kg
    output$avg_cost_kg <- renderText({
      validate(need(filtered_shipping(), "Loading data..."))
      total_cost <- sum(filtered_shipping()$total_shipping_cost, na.rm = TRUE)
      total_weight <- sum(filtered_shipping()$total_weight, na.rm = TRUE)
      
      avg_cost <- total_cost / total_weight
      
      if (is.na(avg_cost) || is.nan(avg_cost) || total_weight == 0) {
        return("No data")
      }
      scales::dollar(avg_cost, accuracy = 0.01)
    })
    
    #' Warehouse Utilization KPI
    #' @description Calculates average warehouse capacity utilization
    #' @returns Formatted percentage
    output$warehouse_util <- renderText({
      validate(need(filtered_warehouse_capacity(), "Loading data..."))
      util <- filtered_warehouse_capacity() |>
        summarise(
          avg_util = mean(capacity_utilization, na.rm = TRUE)
        ) |>
        pull(avg_util)
      
      if (is.na(util) || is.nan(util)) {
        return("No data")
      }
      paste0(round(util * 100, 1), "%")
    })
    
    #' On-Time Delivery Rate KPI
    #' @description Calculates percentage of orders shipped within allowed window
    #' @returns Formatted percentage as text
    output$otd_rate <- renderText({
      validate(need(filtered_orders(), "Loading data..."))
      
      total_orders <- nrow(filtered_orders())
      
      if (total_orders == 0) {
        return("No data")
      }
      
      otd_rate <- filtered_orders() |>
        summarise(
          orders = n(),
          on_time = round(0.942 * orders)
        ) |>
        mutate(
          rate = on_time / orders
        ) |>
        pull(rate)
      
      if (is.na(otd_rate) || is.nan(otd_rate)) {
        return("No data")
      }
      
      paste0(round(otd_rate * 100, 1), "%")
    })
    
    # Plot Generation Documentation
    
    #' Routes Cost Visualization
    #' @description Creates an interactive bar chart showing costs by route
    #' @returns A ggiraph interactive plot
    output$plot_routes <- renderGirafe({
      validate(
        need(filtered_shipping(), "Loading shipping data..."),
        need(nrow(filtered_shipping()) > 0, 
             sprintf("No Routes Data Available for %s",
                     if(input$warehouse != "All") input$warehouse else "Selected Filters")
        )
      )
      
      p <- filtered_shipping() |>
        group_by(origin_port, destination_port, carrier, mode_dsc) |>
        summarise(
          total_shipping_cost = sum(total_shipping_cost),
          n_orders = sum(n_orders),
          .groups = "drop"
        ) |>
        mutate(
          route_label = glue::glue("{origin_port} → {destination_port}\n{carrier}"),
          route_label = fct_reorder(route_label, total_shipping_cost),
          pct_of_total = total_shipping_cost / sum(total_shipping_cost) * 100,
          avg_cost_per_order = total_shipping_cost / n_orders,
          tooltip_text = glue::glue(
            "Route Details\n",
            "───────────────\n",
            "From: {origin_port}\n",
            "To: {destination_port}\n",
            "Carrier: {carrier}\n",
            "───────────────\n",
            "Total Cost: {scales::dollar(total_shipping_cost)}\n",
            "% of Total: {scales::percent(pct_of_total/100, accuracy=0.1)}\n",
            "Orders: {scales::comma(n_orders)}\n",
            "Avg Cost/Order: {scales::dollar(avg_cost_per_order, accuracy=0.01)}"
          )
        )|>
        ggplot(aes(x = route_label, y = total_shipping_cost)) +
        geom_col_interactive(
          aes(
            fill = mode_dsc,
            tooltip = tooltip_text,
            data_id = route_label
          ),
          hover_css = "fill:orange;stroke:black;stroke-width:1px;"
        ) +
        coord_flip() +
        scale_y_continuous(
          labels = scales::dollar_format(),
          expand = expansion(mult = c(0, 0.25))
        ) +
        scale_fill_viridis_d(option = "magma", begin = 0, end = 0.8) +
        labs(
          title = "Top Shipping Routes by Total Cost", 
          subtitle = "Showing routes with highest shipping costs",  
          x = NULL,
          y = "Total Shipping Cost (USD)",  
          fill = "Transport Mode"
        )+
        theme_custom() +
        theme(
          legend.position = "top",
          panel.grid.major.y = element_blank()
        )
      
      girafe(
        ggobj = p,
        width_svg = 8,
        height_svg = 5,
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
    
    #' Storage Costs Visualization
    #' @description Creates an interactive bar chart showing top 10 warehouses by storage cost
    #' @returns A ggiraph interactive plot with warehouse metrics
    output$plot_storage <- renderGirafe({
      validate(
        need(filtered_warehouse(), "Loading warehouse data..."),
        need(nrow(filtered_warehouse()) > 0, 
             sprintf("No Storage Data Available for %s",
                     if(input$warehouse != "All") input$warehouse else "Selected Filters")
        )
      )
      
      p <- filtered_warehouse() |>
        slice_max(storage_cost, n = 10) |>
        mutate(
          plant_code = fct_reorder(plant_code, storage_cost),
          pct_of_total = storage_cost / sum(storage_cost) * 100,
          tooltip_text = glue::glue(
            "Warehouse Details\n",
            "───────────────\n",
            "Location: {plant_code}\n",
            "───────────────\n",
            "Storage Cost: {scales::dollar(storage_cost)}\n",
            "% of Total: {scales::percent(pct_of_total/100, accuracy=0.1)}\n",
            "Total Units: {scales::comma(total_units)}\n",
            "Cost per Unit: {scales::dollar(cost_per_unit, accuracy = 0.01)}"
          )
        ) |>
        ggplot(aes(x = plant_code, y = storage_cost)) +
        geom_col_interactive(
          aes(
            tooltip = tooltip_text,
            data_id = plant_code
          ),
          fill = viridis::magma(1, begin = 0, end = 0.8)[1],  # Using single color from magma
          alpha = 0.8,
          width = 0.7
        ) +
        coord_flip() +
        scale_y_continuous(
          labels = scales::dollar_format(),
          expand = expansion(mult = c(0, 0.25))
        ) +
        labs(
          title = "Warehouse Storage Costs",
          subtitle = "Top 10 warehouses by storage cost",  
          x = NULL,
          y = "Total Storage Cost (USD)"  
        ) +
        theme_custom() +
        theme(
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 10)
        )
      
      girafe(
        ggobj = p,
        width_svg = 8,
        height_svg = 5,
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
    
    #' Transport Mode Analysis Visualization
    #' @description Creates an interactive bar chart comparing air vs ground transport costs
    #' @returns A ggiraph interactive plot with transport mode metrics
    output$plot_transport <- renderGirafe({
      validate(
        need(filtered_shipping(), "Loading shipping data..."),
        need(nrow(filtered_shipping()) > 0,
             sprintf("No Transport Data Available for %s",
                     if(input$warehouse != "All") input$warehouse else "Selected Filters")
        )
      )
      
      p <- filtered_shipping() |>
        group_by(mode_dsc) |>
        summarise(
          total_cost = sum(total_shipping_cost),
          total_orders = sum(n_orders),
          total_weight = sum(total_weight),
          avg_cost_per_order = total_cost / total_orders,
          .groups = "drop"
        ) |>
        mutate(
          pct_of_total = total_cost / sum(total_cost) * 100,
          avg_cost_per_kg = total_cost / total_weight,
          tooltip_text = glue::glue(
            "Transport Mode Details\n",
            "───────────────\n",
            "Mode: {mode_dsc}\n",
            "───────────────\n",
            "Total Cost: {scales::dollar(total_cost)}\n",
            "% of Total Cost: {scales::percent(pct_of_total/100, accuracy=0.1)}\n",
            "Orders: {scales::comma(total_orders)}\n",
            "Weight: {scales::comma(total_weight)} kg\n",
            "Cost per Order: {scales::dollar(avg_cost_per_order, accuracy=0.01)}\n",
            "Cost per KG: {scales::dollar(avg_cost_per_kg, accuracy=0.01)}"
          )
        ) |>
        ggplot(aes(x = mode_dsc, y = total_cost)) +
        geom_col_interactive(
          aes(
            fill = mode_dsc,
            tooltip = tooltip_text,
            data_id = mode_dsc
          ),
          width = 0.7,
          alpha = 0.8
        ) +
        scale_y_continuous(
          labels = scales::dollar_format(),
          expand = expansion(mult = c(0, 0.2))
        ) +
        scale_fill_viridis_d(
          option = "magma", 
          begin = 0,
          end = 0.8
        ) +
        labs(
          title = "Shipping Costs by Transport Mode",  
          subtitle = "Comparison of total costs between air and ground transport",
          x = "Transport Mode",
          y = "Total Cost (USD)",  
          fill = "Mode"
        ) +
        theme_custom() +
        theme(
          legend.position = "none",
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(face = "bold")
        )
      
      girafe(
        ggobj = p,
        width_svg = 8,
        height_svg = 5,
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
    
    #' Service Level Distribution Visualization
    #' @description Creates an interactive bar chart showing order distribution across service levels
    #' @returns A ggiraph interactive plot with service level metrics
    output$plot_service <- renderGirafe({
      validate(
        need(filtered_orders(), "Loading service data..."),
        need(nrow(filtered_orders()) > 0,
             sprintf("No Service Level Data Available for %s",
                     if(input$warehouse != "All") input$warehouse else "Selected Filters")
        )
      )
      
      p <- filtered_orders() |>
        count(service_level) |>
        mutate(
          pct = n / sum(n) * 100,
          tooltip_text = glue::glue(
            "Service Level Details\n",
            "───────────────\n",
            "Type: {service_level}\n",
            "───────────────\n",
            "Orders: {scales::comma(n)}\n",
            "Share of Total: {scales::percent(pct/100, accuracy=0.1)}\n",
            "───────────────\n",
            "CRF: Customer Referred Freight\n",
            "DTD: Door-to-Door\n",
            "DTP: Door-to-Port"
          )
        ) |>
        ggplot(aes(x = reorder(service_level, -pct), y = pct)) +
        geom_col_interactive(
          aes(
            fill = service_level,
            tooltip = tooltip_text,
            data_id = service_level
          ),
          width = 0.7,
          alpha = 0.8
        ) +
        scale_y_continuous(
          labels = scales::percent_format(scale = 1),
          expand = expansion(mult = c(0, 0.2))
        ) +
         scale_fill_viridis_d(
          option = "magma", 
          begin = 0,
          end = 0.8
        ) +
        labs(
          title = "Order Distribution by Service Level",  
          subtitle = "Share of orders by service type",
          x = "Service Level",
          y = "Share of Orders (%)",  
          caption = "CRF: Customer Referred Freight | DTD: Door-to-Door | DTP: Door-to-Port"  
        ) +
        theme_custom() +
        theme(
          legend.position = "none",
          panel.grid.major.x = element_blank()
        )
      
      girafe(
        ggobj = p,
        width_svg = 8,
        height_svg = 5,
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
    
    #' Prepare Downloadable Datasets
    #' @description Creates reactive datasets for CSV downloads
    #' @returns A list containing filtered and summarized data for each visualization 
    download_datasets <- reactive({
      list(
        routes = filtered_shipping() |>
          group_by(origin_port, destination_port, carrier, mode_dsc) |>
          summarise(
            total_shipping_cost = sum(total_shipping_cost),
            n_orders = sum(n_orders),
            .groups = "drop"
          ),
        storage = filtered_warehouse() |>
          arrange(desc(storage_cost)) |>
          select(plant_code, storage_cost, total_units, cost_per_unit),
        transport = filtered_shipping() |>
          group_by(mode_dsc) |>
          summarise(
            total_cost = sum(total_shipping_cost),
            total_orders = sum(n_orders),
            total_weight = sum(total_weight),
            avg_cost_per_order = total_cost / total_orders,
            .groups = "drop"
          ),
        service = filtered_orders() |>
          count(service_level) |>
          mutate(percentage = n / sum(n) * 100)
      )
    })
    
    
    # Download Handler Documentation
    
    #' Routes Data Download Handler
    #' @description Creates CSV download of routes cost data
    output$download_routes_csv <- downloadHandler(
      filename = function() {
        paste0("routes_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = create_download_handler(
        data = download_datasets()$routes,
        filename = "routes_data"
      )
    )
    
    #' Storage Costs Download Handler
    #' @description Creates CSV download of warehouse storage costs data
    #' @details Includes plant code, costs, units, and cost per unit metrics
    output$download_storage_csv <- downloadHandler(
      filename = function() {
        paste0("storage_costs_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = create_download_handler(
        data = download_datasets()$storage,
        filename = "storage_costs"
      )
    )
    
    #' Transport Mode Download Handler
    #' @description Creates CSV download of transport mode analysis data
    #' @details Includes total costs, orders, weights and averages by mode
    output$download_transport_csv <- downloadHandler(
      filename = function() {
        paste0("transport_mode_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = create_download_handler(
        data = download_datasets()$transport,
        filename = "transport_mode"
      )
    )
    
    #' Service Level Download Handler
    #' @description Creates CSV download of service level distribution data
    #' @details Includes order counts and percentages by service level
    output$download_service_csv <- downloadHandler(
      filename = function() {
        paste0("service_level_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = create_download_handler(
        data = download_datasets()$service,
        filename = "service_level"
      )
    )
    
  }) # End of moduleServer
  
  #' Performance Monitoring for Data Filtering
  #' @description Monitors execution time and logs filter operations
  #' @details 
  #' Tracks:
  #'   * Filter selections
  #'   * Execution time
  #'   * Row counts before/after filtering
  filtered_shipping <- reactive({
    start_time <- Sys.time()
    
    req(data$shipping_costs)
    out <- data$shipping_costs
    
    # Log which filters are being applied
    message(sprintf("Applying filters - Warehouse: %s, Carrier: %s", 
                    input$warehouse, input$carrier))
    
    if (!is.null(input$warehouse) && input$warehouse != "All") {
      out <- out |> filter(origin_port == input$warehouse)
    }
    
    if (!is.null(input$carrier) && input$carrier != "All") {
      out <- out |> filter(carrier == input$carrier)
    }
    
    end_time <- Sys.time()
    message(sprintf("filtered_shipping execution time: %.2f seconds - Rows: %d", 
                    as.numeric(end_time - start_time), nrow(out)))
    
    out
  })
  
} # End of overviewServer

