# modules/cost_analysis/cost_analysis_ui.R

#' Cost Analysis Tab UI Module
#' 
#' Creates the UI components for the Cost Analysis tab including filters,
#' KPI cards, and interactive visualizations.
#' 
#' @param id The module ID used for namespace isolation in Shiny
#' @return A tagList containing the Cost Analysis tab UI elements:
#'   * Filter controls in sidebar (Warehouse, Carrier, Service Level)
#'   * KPI value boxes in main area
#'   * Cost analysis visualizations
#'   * Download options for each visualization
#' 
#' @details
#' Layout structure:
#'   * Sidebar: Filter controls and reset button
#'   * Main Content:
#'     - First row: Four KPI value boxes
#'     - Second row: Cost breakdown and unit cost analysis
#'     - Third row: Efficiency matrix
#'
#' @examples
#' # In ui.R or app.R with new sidebar layout:
#' nav_panel(
#'   title = "Cost Analysis",
#'   layout_sidebar(
#'     sidebar = sidebar(width = 250, costAnalysisUI_sidebar("cost_analysis")),
#'     costAnalysisUI_main("cost_analysis")
#'   )
#' )

#' Sidebar UI Component
#' @param id Namespace ID
#' @return Sidebar UI elements including filters and reset button
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
          ns("download_cost_breakdown_csv"), 
          "Cost Breakdown Data",
          icon = icon("download"),
          class = "btn-sm btn-light text-start mb-2 w-100"
        ),
        downloadButton(
          ns("download_unit_cost_csv"), 
          "Unit Cost Data",
          icon = icon("download"),
          class = "btn-sm btn-light text-start mb-2 w-100"
        ),
        downloadButton(
          ns("download_efficiency_csv"), 
          "Efficiency Matrix Data",
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
costAnalysisUI_main <- function(id) {
  ns <- NS(id)
  
  tagList(
    # KPI row with compact design
    div(
      class = "mb-1",  # Minimal bottom margin
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(
          title = "Total Storage Cost",
          showcase = bsicons::bs_icon("building", class = "mb-1"),
          # value = textOutput(ns("total_storage_cost")),
          value = div(style = "color: white !important;", 
                      textOutput(ns("total_storage_cost"))),
          theme = "primary",
          min_height = "100px",  # Match Overview height
          fill = TRUE,
          class = "value-box-card text-white" 
        ),
        value_box(
          title = "Average Cost per Unit",
          showcase = bsicons::bs_icon("box", class = "mb-1"),
          value = textOutput(ns("avg_cost_unit")),
          theme = "primary",
          min_height = "100px",
          fill = TRUE,
          class = "value-box-card text-white" 
        ),
        value_box(
          title = "Ground Transport %",
          showcase = bsicons::bs_icon("truck", class = "mb-1"),
          value = textOutput(ns("ground_transport_pct")),
          theme = "primary",
          min_height = "100px",
          fill = TRUE,
          class = "value-box-card text-white" 
        ),
        value_box(
          title = "Most Expensive Route",
          showcase = bsicons::bs_icon("graph-up", class = "mb-1"),
          value = div(style = "color: white !important;", 
                      textOutput(ns("top_route_cost"))),
          theme = "primary",
          min_height = "100px",
          fill = TRUE,
          class = "value-box-card"
        )
      )
    ),
    
    # Cost Analysis Charts
    div(
      class = "mt-2",  # Minimal top margin
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          min_height = "400px",
          card_header(
            div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("bar-chart", class = "me-2"),
              span("Routes by Total Cost")
            )
          ),
          card_body(
            withSpinner(
              girafeOutput(ns("cost_breakdown"), height = "350px"),
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
              bsicons::bs_icon("clipboard-data", class = "me-2"),
              span("Unit Cost Analysis")
            )
          ),
          card_body(
            withSpinner(
              girafeOutput(ns("unit_cost"), height = "350px"),
              type = 4,
              color = palette_main[1]
            )
          )
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
          min_height = "400px",
          card_header(
            div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("grid-3x3", class = "me-2"),
              span("Efficiency Matrix")
            )
          ),
          card_body(
            withSpinner(
              girafeOutput(ns("cost_efficiency"), height = "350px"),
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
costAnalysisUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 250,
      class = "p-3",  # Added padding
      costAnalysisUI_sidebar(id)
    ),
    costAnalysisUI_main(id)
  )
}