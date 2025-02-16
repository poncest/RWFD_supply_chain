# app.R

source("global.R")
source("modules/overview/overview_ui.R")
source("modules/overview/overview_server.R")
source("modules/cost_analysis/cost_analysis_ui.R")
source("modules/cost_analysis/cost_analysis_server.R")

# Load data
data <- load_supply_chain_data()

# Create theme with all rules
custom_theme <- bs_theme(
  version = 5,
  bootswatch = "litera",
  primary = palette_main[1],
  "navbar-bg" = "#f8f9fa"
)

# Add custom CSS rules
custom_theme <- custom_theme |> bs_add_rules(c(
  ".nav { padding-left: 2.5rem !important; }",
  ".value-box-card { color: white !important; }",
  ".value-box-card .value-output { color: white !important; }",
  ".value-box-card .shiny-text-output { color: white !important; }",
  ".value-box-card .bslib-value-box-value { color: white !important; }",
  ".value-box-card .bslib-value-box-value * { color: white !important; }"
))

# UI Definition
ui <- page_navbar(
  theme = custom_theme,
  
  # Title section
  title = div(
    class = "d-flex justify-content-between align-items-center w-100 pe-3 pb-2",
    span(
      class = "fw-bold",
      "RWFD Supply Chain (Manufacturing) Dashboard"
    ),
    actionButton(
      "show_info", 
      "", 
      icon = icon("info-circle"),
      class = "btn btn-link",
      style = "color: #2C3E50; position: absolute; right: 1rem;"
    )
  ),
  
  bg = "#f8f9fa",
  
  # Tabs
  nav_panel(
    title = "Overview",
    layout_sidebar(
      sidebar = sidebar(width = 250, overviewUI_sidebar("overview")),
      overviewUI_main("overview")
    )
  ),
  nav_panel(
    title = "Cost Analysis",
    layout_sidebar(
      sidebar = sidebar(width = 250, costAnalysisUI_sidebar("cost_analysis")),
      costAnalysisUI_main("cost_analysis")
    )
  ),
  # nav_panel("Capacity", "Content coming soon"),
  # nav_panel("Service Levels", "Content coming soon")
)

# Server logic 
server <- function(input, output, session) {
  # Info modal observer
  observeEvent(input$show_info, {
    showModal(modalDialog(
      title = "About This Dashboard",
      
      tags$div(
        tags$h5("Data Period"),
        "2013-05-26",
        tags$hr(),
        
        tags$h5("Data Source"),
        tags$p(
          "Data originally from: ",
          tags$br(),
          "Kalganova, Tatiana; Dzalbs, Ivars (2019). Supply Chain Logistics Problem Dataset. ",
          "Brunel University London. Dataset. ",
          tags$a(
            href = "https://doi.org/10.17633/rd.brunel.7558679.v2",
            "https://doi.org/10.17633/rd.brunel.7558679.v2", 
            target = "_blank"
          ),
          tags$br(),
          tags$br(),
          "Accessed via: ",
          tags$a(
            href = "https://sonsofhierarchies.com/real-world-fake-data/",
            "RWFD Real World Fake Data", 
            target = "_blank"
          )
        ),
        tags$hr(),
        
        tags$h5("Citation"),
        tags$p(
          "To cite this dashboard:",
          tags$br(),
          "Ponce, S. (2024). RWFD Supply Chain Dashboard [Shiny Application]. ",
          tags$a(href = "https://github.com/your-repo", "https://github.com/your-repo", target = "_blank")
        ),
        tags$hr(),
        
        tags$h5("Credits"),
        "Designed by: Steven Ponce, 2025"
      ),
      
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Module servers
  overviewServer("overview", data)
  costAnalysisServer("cost_analysis", data)  
}

# Run the app
shinyApp(ui = ui, server = server)