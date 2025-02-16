# modules/overview/overview_ui.R

#' Overview Tab UI Module
#' 
#' Creates the UI components for the dashboard's Overview tab.
#' This includes filters, KPI cards, and visualization layouts.
#' 
#' @param id The module ID used for namespace isolation in Shiny
#' @return A tagList containing the Overview tab UI elements:
#'   * Filter controls in sidebar (Warehouse, Carrier, Service Level)
#'   * KPI value boxes in main area
#'   * Visualization cards with download options in main area
#' 
#' @details
#' Layout structure:
#'   * Sidebar: Filter controls and reset button
#'   * Main Content:
#'     - First row: Four KPI value boxes
#'     - Second row: Two visualization cards (Routes and Storage)
#'     - Third row: Two visualization cards (Transport and Service)
#' 
#' @examples
#' # In ui.R or app.R with new sidebar layout:
#' nav_panel(
#'   title = "Overview",
#'   layout_sidebar(
#'     sidebar = sidebar(width = 250, overviewUI_sidebar("overview")),
#'     overviewUI_main("overview")
#'   )
#' )

#' Sidebar UI Component
#' @param id Namespace ID
#' @return Sidebar UI elements including filters and reset button
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
      
      # Reset button
      actionButton(
        ns("reset_filters"), 
        "Reset Filters",
        icon = icon("rotate"),
        class = "btn-secondary btn-sm w-100 mt-2"
      )
    ),
    
    # Download section
    div(
      class = "mb-4",
      h6("Downloads", class = "text-muted mb-3"),
      div(
        class = "d-flex flex-column align-items-start",
        downloadButton(
          ns("download_routes_csv"), 
          "Routes Data",
          icon = icon("download"),
          class = "btn-sm btn-light text-start mb-2 w-100"
        ),
        downloadButton(
          ns("download_storage_csv"), 
          "Storage Data",
          icon = icon("download"),
          class = "btn-sm btn-light text-start mb-2 w-100"
        ),
        downloadButton(
          ns("download_transport_csv"), 
          "Transport Data",
          icon = icon("download"),
          class = "btn-sm btn-light text-start mb-2 w-100"
        ),
        downloadButton(
          ns("download_service_csv"), 
          "Service Level Data",
          icon = icon("download"),
          class = "btn-sm btn-light text-start w-100"
        )
      )
    ),
    
    # Social Links
    div(
      class = "mt-auto",  # Push to bottom
      hr(),
      h6("Connect With Us", class = "text-muted mb-3"),
      div(
        class = "d-flex justify-content-start gap-3",
        a(
          href = "https://www.linkedin.com/in/stevenponce/",
          target = "_blank",
          icon("linkedin", class = "fa-lg"),
          class = "text-muted text-decoration-none",
          title = "LinkedIn Profile"
        ),
        a(
          href = "https://stevenponce.netlify.app/",
          target = "_blank",
          icon("globe", class = "fa-lg"),
          class = "text-muted text-decoration-none",
          title = "Personal Website"
        )
      )
    )
  )  
}

#' Main Content UI Component
#' @param id Namespace ID
#' @return Main content UI elements including KPIs and visualizations
overviewUI_main <- function(id) {
  ns <- NS(id)
  
  tagList(
    # KPI row with more compact design
    div(
      class = "mb-1",  # Minimal bottom margin
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(
          title = "Total Shipping Cost",
          showcase = bsicons::bs_icon("currency-dollar", class = "mb-1"),
          value = textOutput(ns("total_cost")),
          theme = "primary",
          min_height = "100px",  # Ensure enough space for content
          fill = TRUE,
          class = "value-box-card"
        ),
        value_box(
          title = "Avg Cost per KG",
          showcase = bsicons::bs_icon("graph-up", class = "mb-1"),
          value = textOutput(ns("avg_cost_kg")),
          theme = "primary",
          min_height = "100px",
          fill = TRUE,
          class = "value-box-card"
        ),
        value_box(
          title = "Warehouse Utilization",
          showcase = bsicons::bs_icon("building", class = "mb-1"),
          value = textOutput(ns("warehouse_util")),
          theme = "primary",
          min_height = "100px",
          fill = TRUE,
          class = "value-box-card"
        ),
        value_box(
          title = "On-Time Delivery",
          showcase = bsicons::bs_icon("check-circle", class = "mb-1"),
          value = textOutput(ns("otd_rate")),
          theme = "primary",
          min_height = "100px",
          fill = TRUE,
          class = "value-box-card"
        )
      )
    ),
    
    # Charts section with minimal spacing
    div(
      class = "mt-2",  # Small gap after KPIs
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          min_height = "400px",
          card_header(
            div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("bar-chart", class = "me-2"),
              span("Top Routes by Cost")
            )
          ),
          card_body(
            withSpinner(
              girafeOutput(ns("plot_routes"), height = "350px"),
              type = 4,
              color = palette_main[1]
            )
          )
        ),
        card(
          full_screen = TRUE,
          min_height = "400px",
          card_header(
            div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("building", class = "me-2"),
              span("Storage Costs by Warehouse")
            )
          ),
          card_body(
            withSpinner(
              girafeOutput(ns("plot_storage"), height = "350px"),
              type = 4,
              color = palette_main[1]
            )
          )
        )
      )
    ),
    
    div(
      class = "mt-3",
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          min_height = "400px",
          card_header(
            div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("truck", class = "me-2"),
              span("Transport Mode Analysis")
            )
          ),
          card_body(
            withSpinner(
              girafeOutput(ns("plot_transport"), height = "350px"),
              type = 4,
              color = palette_main[1]
            )
          )
        ),
        card(
          full_screen = TRUE,
          min_height = "400px",
          card_header(
            div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("pie-chart", class = "me-2"),
              span("Service Level Distribution")
            )
          ),
          card_body(
            withSpinner(
              girafeOutput(ns("plot_service"), height = "350px"),
              type = 4,
              color = palette_main[1]
            )
          )
        )
      )
    )
  )
}

#' Legacy function for backward compatibility
#' @param id Namespace ID
overviewUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 250,
      class = "p-3",  # Added padding
      overviewUI_sidebar(id)
    ),
    overviewUI_main(id)
  )
}