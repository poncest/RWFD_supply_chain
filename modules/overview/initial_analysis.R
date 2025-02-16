
# RWFD - Supply Chain
# The data comes from the Real World Fake Data website
# Data: https://data.world/markbradbourne/rwfd-real-world-fake-data/workspace/file?filename=Supply+Chain.xlsx
# paper: https://www.sciencedirect.com/science/article/pii/S0360835220303442?ref=pdf_download&fr=RR-9&rr=8f067506c9ce5e60#s0055
# Citation:
# Kalganova, Tatiana; Dzalbs, Ivars (2019). Supply Chain Logistics Problem Dataset.
# Brunel University London. Dataset. https://doi.org/10.17633/rd.brunel.7558679.v2


# SETUP AND DATA LOADING ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, janitor, skimr, scales, glue,
  readxl, purrr, lubridate, ggrepel
)

source("R/weight_band_normalization.R")
source("R/key_metrics_validation.R")
source('R/service_level_analysis.R')


# Visualization settings
theme_set(theme_minimal(base_size = 14, base_family = "text"))
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)

# Load Data
original_names <- excel_sheets("data/01_raw_data/rwfd_supply_chain.xlsx")
snake_names <- make_clean_names(original_names)
data_list <- original_names |>
  set_names(snake_names) |>
  map(~ read_excel(
    path = "data/01_raw_data/rwfd_supply_chain.xlsx",
    sheet = .x
  ) |> clean_names())

glimpse(data_list)


# 1. COST METRICS --------------------------------------------------------------

## A. Shipping Costs ----
# Data Quality Notes:
# - Carrier V44_3 appears in OrderList but not in FreightRates because:
#   * It was historically used but has been discontinued
#   * All V44_3 instances are CRF (Customer Referred Freight)
#   * For CRF, customers arrange their own shipping (costs not calculated)
# - Some ports (e.g., PORT05) may not show in final analysis due to:
#   * No matching freight rates
#   * Customer arranged shipping (CRF)
# - Multiple weight bands may exist for same route-carrier combination

# Clean freight rates to remove duplicates
clean_freight_rates <- data_list$freight_rates |> distinct()

# Calculate shipping costs
shipping_costs <- data_list$order_list |>
  left_join(clean_freight_rates,
            by = c("carrier",
                   "origin_port" = "orig_port_cd",
                   "destination_port" = "dest_port_cd",
                   "service_level" = "svc_cd"
            ),
            relationship = "many-to-many"
  ) |>
  group_by(order_id) |>
  filter(
    weight >= minm_wgh_qty,
    weight <= max_wgh_qty
  ) |>
  slice_min(abs(weight - minm_wgh_qty), n = 1) |>
  ungroup() |>
  mutate(
    shipping_cost = pmax(minimum_cost, weight * rate),
    weight_band = paste(minm_wgh_qty, "-", max_wgh_qty, "kg")
  ) |>
  group_by(origin_port, destination_port, carrier, weight_band, mode_dsc) |>
  summarise(
    total_shipping_cost = sum(shipping_cost),
    total_weight = sum(weight),
    n_orders = n(),
    avg_cost_per_kg = total_shipping_cost / total_weight,
    .groups = "drop"
  ) |> 
  check_data_quality(group_cols = c("origin_port", "destination_port", "carrier")) |>
  normalize_weight_bands() |> 
  validate_weight_bands() |>
  validate_shipping_metrics() |> 
  arrange(origin_port, destination_port, carrier, weight_band) 


shipping_costs_summary <- shipping_costs |>
  mutate(
    pct_total_cost = total_shipping_cost / sum(total_shipping_cost) * 100,
    pct_total_orders = n_orders / sum(n_orders) * 100
  ) |>
  select(
    origin_port, destination_port, carrier, weight_band, mode_dsc,
    total_shipping_cost, n_orders, pct_total_cost, pct_total_orders
  )


## B. Storage Costs ----
warehouse_costs <- data_list$order_list |>
  group_by(plant_code) |>
  summarise(
    total_orders = n(),
    total_weight = sum(weight),
    total_units = sum(unit_quantity)
  ) |>
  left_join(data_list$wh_costs, by = c("plant_code" = "wh")) |>
  mutate(
    storage_cost = total_units * cost_unit,
    cost_per_order = storage_cost / total_orders,
    cost_per_kg = storage_cost / total_weight
  )



# 2. CAPACITY UTILIZATION ------------------------------------------------------
## A. Warehouse Capacity ----
warehouse_capacity <- data_list$order_list |>
  count(plant_code) |>
  left_join(data_list$wh_capacities, by = c("plant_code" = "plant_id")) |>
  mutate(capacity_utilization = n / daily_capacity)

## B. Weight Band Utilization ----
weight_band_util <- shipping_costs |>
  group_by(carrier, weight_band) |>
  summarise(
    total_orders = sum(n_orders),
    total_weight = sum(total_weight),
    .groups = "drop"
  )


## C. Weight Band Analysis ----
weight_bands_detail <- shipping_costs |>
  mutate(
    weight_band = str_replace(weight_band, " kg", ""),
    # Format labels to show both order count and total weight
    order_label = glue::glue("{scales::comma(n_orders)} orders\n{scales::comma(round(total_weight))} kg"),
    # Add a flag for significant bars
    significant = total_weight > 10000  # Only label bars with > 10,000 kg
  ) |>
  # Create a filtered dataset for labels
  mutate(
    label_text = if_else(significant, order_label, "")
  ) 



## D. Weight Band Temporal Analysis (skipped) ----
# Only one date in May 2013


## E. Weight Band Efficiency Analysis ----
weight_band_efficiency <- shipping_costs |>
  mutate(
    cost_per_kg = total_shipping_cost / total_weight,
    cost_per_order = total_shipping_cost / n_orders,
    efficiency_score = total_weight / total_shipping_cost  # Higher score = more weight per dollar
  ) |>
  group_by(carrier, mode_dsc, weight_band) |>
  summarise(
    total_cost = sum(total_shipping_cost),
    total_weight = sum(total_weight),
    total_orders = sum(n_orders),
    avg_cost_per_kg = mean(cost_per_kg),
    avg_cost_per_order = mean(cost_per_order),
    .groups = "drop"
  ) |>
  # Clean up weight band format for display
  mutate(
    weight_band = str_replace(weight_band, " kg", ""),
    weight_band = fct_reorder(weight_band, avg_cost_per_kg)
  )

## F. Weight Band Service Level Patterns ----

# Prepare service level data - modified approach
weight_band_service <- data_list$order_list |>
  left_join(clean_freight_rates, 
            by = c(
              "carrier",
              "origin_port" = "orig_port_cd",
              "destination_port" = "dest_port_cd",
              "service_level" = "svc_cd"
            ),
            relationship = "many-to-many"
  ) |>
  filter(!is.na(service_level)) |>  # Remove any missing service levels
  group_by(carrier, weight_band = paste(minm_wgh_qty, "-", max_wgh_qty, "kg"), 
           service_level) |>
  summarise(
    total_weight = sum(weight),
    total_orders = n(),
    total_cost = sum(weight * rate),
    avg_cost_per_kg = total_cost / total_weight,
    .groups = "drop"
  ) |>
  # Calculate percentage within weight band
  group_by(carrier, weight_band) |>
  mutate(
    pct_orders = total_orders / sum(total_orders) * 100,
    weight_band = str_replace(weight_band, " kg", "")
  ) |>
  ungroup()

# Simplify the data first
service_level_summary <- weight_band_service |>
  group_by(carrier, service_level) |>
  summarise(
    total_orders = sum(total_orders),
    total_weight = sum(total_weight),
    .groups = "drop"
  ) |>
  group_by(carrier) |>
  mutate(
    pct_orders = total_orders / sum(total_orders) * 100,
    pct_weight = total_weight / sum(total_weight) * 100
  )

## G. Weight Band Route Analysis ----

weight_band_routes <- shipping_costs |>
  mutate(
    route = glue::glue("{origin_port} → {destination_port}"),
    weight_band = str_replace(weight_band, " kg", ""),
    # Handle NAs differently based on route
    weight_band = case_when(
      is.na(weight_band) & origin_port == "PORT09" ~ "0 - 5000",
      is.na(weight_band) & origin_port == "PORT04" ~ weight_band,  # Keep as NA for now
      TRUE ~ weight_band
    )
  ) |>
  # Define proper weight band order including all bands from the data
  mutate(
    weight_band = factor(
      weight_band,
      levels = c(
        "0 - 99.99",
        "0.01 - 0.5",
        "0.51 - 1",
        "1.01 - 1.5",
        "1.51 - 2",
        "2.01 - 2.5",
        "70.51 - 99.99",
        "100 - 249.99",
        "100 - 299.99",
        "250 - 499.99",
        "300 - 499.99",
        "500 - 1999.99",
        "0 - 5000"
      )
    )
  ) |>
  group_by(route, carrier, weight_band) |>
  summarise(
    total_weight = sum(total_weight),
    total_orders = sum(n_orders),
    avg_cost_per_kg = mean(avg_cost_per_kg),
    .groups = "drop"
  )




# 3. SERVICE LEVEL METRICS -----------------------------------------------------
## A. Service Level Metrics Calculation ----
service_analysis <- data_list$order_list %>%
  left_join(
    clean_freight_rates,
    by = c(
      "carrier",
      "origin_port" = "orig_port_cd",
      "destination_port" = "dest_port_cd",
      "service_level" = "svc_cd"
    ),
    relationship = "many-to-many"
  ) %>%
  mutate(
    shipping_cost = if_else(
      service_level == "CRF",
      NA_real_,
      coalesce(pmax(minimum_cost, weight * rate), 0)
    )
  ) %>%
  analyze_service_levels()

# Now we can reference different summaries from service_analysis:
# service_analysis$service_summary     # Overall service level distribution
# service_analysis$carrier_service     # Service levels by carrier
# service_analysis$route_service       # Service levels by route


## B. Transport Mode Analysis ----
transport_mode_summary <- shipping_costs %>%
  group_by(mode_dsc) %>%
  summarise(
    total_cost = sum(total_shipping_cost),
    total_orders = sum(n_orders),
    avg_cost_per_order = total_cost / total_orders
  )


# VISUALIZATION SETUP  ---------------------------------------------------------

### |- color palette ----
palette_main <- c(
  "#2C3E50", # Dark Blue-Gray (primary)
  "#E74C3C", # Coral Red
  "#3498DB", # Bright Blue
  "#2ECC71", # Emerald Green
  "#F1C40F" # Sun Yellow
)

### |- plot aesthetics ----
bkg_col <- "#f5f5f2"
title_col <- "gray20"
subtitle_col <- "gray20"
caption_col <- "gray30"
text_col <- "gray30"

### |- theme setup ----
theme_custom <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = bkg_col, color = NA),
      panel.background = element_rect(fill = bkg_col, color = NA),
      plot.title = element_text(
        color = title_col,
        size = 16,
        face = "bold",
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        color = subtitle_col,
        size = 12,
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        color = caption_col,
        size = 8,
        margin = margin(t = 10)
      ),
      axis.text = element_text(color = text_col, size = 10),
      axis.title = element_text(color = text_col, size = 11),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_text(color = text_col, size = 10),
      legend.text = element_text(color = text_col, size = 9)
    )
}

# Set as default theme
theme_set(theme_custom())



# 1. COST METRICS VISUALIZATIONS -----------------------------------------------

## A. Shipping Routes Cost Analysis ----
viz_routes <- shipping_costs_summary |>
  group_by(origin_port, destination_port, carrier, mode_dsc) |>
  summarise(
    total_shipping_cost = sum(total_shipping_cost),
    n_orders = sum(n_orders),
    .groups = "drop"
  ) |>
  mutate(
    # Create cleaner route labels
    route_label = glue::glue("{origin_port} → {destination_port}\n{carrier}"),
    route_label = fct_reorder(route_label, total_shipping_cost),
    # Format costs and create detailed labels
    cost_label = scales::dollar(total_shipping_cost, accuracy = 1),
    label_text = glue::glue("{cost_label}\n({scales::comma(n_orders)} orders)")
  ) |>
  ggplot(aes(x = route_label, y = total_shipping_cost, fill = mode_dsc)) +
  geom_col() +
  geom_text(
    aes(label = label_text),
    hjust = -0.1,
    size = 3,
    color = text_col
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::dollar_format(),
    expand = expansion(mult = c(0, 0.25))
  ) +
  scale_fill_manual(
    values = setNames(palette_main[c(3, 4)], c("AIR", "GROUND"))
  ) +
  labs(
    title = "1.A Shipping Routes Cost Analysis",
    subtitle = "Cost breakdown by transportation mode and order volume",
    x = NULL,
    y = "Total Shipping Cost",
    fill = "Transport Mode",
    caption = "Note: Data shows only active shipping routes\nData source: RWFD Supply Chain Dataset"
  ) +
  theme_custom() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

print(viz_routes)

## B. Warehouse Storage Costs ----
viz_storage_costs <- warehouse_costs |>
  # Calculate additional metrics for context
  mutate(
    cost_per_unit = storage_cost / total_units,
    efficiency_score = total_units / storage_cost,
    pct_of_total = storage_cost / sum(storage_cost) * 100
  ) |>
  slice_max(storage_cost, n = 10) |>
  mutate(
    # Create better labels
    plant_code = fct_reorder(plant_code, storage_cost),
    cost_label = scales::dollar(storage_cost),
    units_label = scales::comma(total_units),
    efficiency_label = scales::dollar(cost_per_unit, accuracy = 0.01)
  ) |>
  ggplot(aes(x = plant_code, y = storage_cost)) +
  # Add subtle background for readability
  geom_col(
    fill = palette_main[1],
    alpha = 0.8,
    width = 0.7
  ) +
  # Add labels with multiple metrics
  geom_text(
    aes(label = glue::glue("{cost_label}\n{units_label} units")),
    hjust = -0.1,
    size = 3,
    family = "text",
    color = text_col
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::dollar_format(),
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "1.B Top 10 Warehouses by Storage Cost",
    subtitle = "Storage cost and total units handled per facility",
    x = "Warehouse",
    y = "Total Storage Cost ($)",
    caption = "Note: Labels show total cost and number of units handled"
  ) +
  theme_custom() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(
      size = 10,
      margin = margin(r = 5)
    ),
    plot.margin = margin(10, 25, 10, 10)
  )

print(viz_storage_costs)




# 2. CAPACITY UTILIZATION VISUALIZATIONS ---------------------------------------

## A. Warehouse Capacity Utilization ----
viz_capacity <- warehouse_capacity |>
  mutate(
    utilization_pct = capacity_utilization * 100,
    status = case_when(
      capacity_utilization >= 1 ~ "Over Capacity",
      TRUE ~ "Under Capacity"
    ),
    plant_code = fct_reorder(plant_code, capacity_utilization),
    util_label = scales::percent(capacity_utilization, accuracy = 0.1),
    detail_label = glue::glue("{util_label}\n{scales::comma(n)} orders")
  ) |>
  ggplot(aes(x = plant_code, y = capacity_utilization)) +
  # Add subtle gridlines
  geom_hline(
    yintercept = seq(0, 2, 0.25),
    color = "gray90",
    linewidth = 0.2
  ) +
  # Enhanced capacity threshold line
  geom_hline(
    yintercept = 1,
    linetype = "longdash",
    color = "gray40",
    linewidth = 0.5
  ) +
  # Add bars
  geom_col(
    aes(fill = status),
    width = 0.7,
    alpha = 0.8
  ) +
  # Add labels
  geom_text(
    aes(label = detail_label),
    hjust = -0.1,
    size = 3,
    family = "text",
    color = text_col
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, 0.3))
  ) +
  scale_fill_manual(
    values = c(
      "Over Capacity" = "#E74C3C",
      "Under Capacity" = "#2ECC71"
    )
  ) +
  labs(
    title = "2.A Warehouse Capacity Utilization",
    subtitle = "Daily orders as percentage of maximum capacity",
    x = "Warehouse",
    y = "Capacity Utilization",
    fill = "Status",
    caption = "Note: Dashed line indicates maximum designed capacity (100%). Labels show utilization percentage and total orders handled."
  ) +
  theme_custom() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(
      size = 10,
      margin = margin(r = 5)
    ),
    plot.margin = margin(10, 25, 10, 10),
    legend.position = "top"
  )

print(viz_capacity)


## B. Weight Band Distribution ----
viz_weight_bands <- weight_band_util |>
  mutate(
    avg_weight_per_order = total_weight / total_orders,
    point_label = case_when(
      total_orders > 10000 | total_weight > 100000 ~
        glue::glue("{carrier}\n{scales::comma(total_orders)} orders"),
      TRUE ~ ""
    )
  ) |>
  ggplot(aes(x = total_orders, y = total_weight)) +
  geom_hline(yintercept = 0, color = "gray90", linewidth = 0.2) +
  geom_vline(xintercept = 0, color = "gray90", linewidth = 0.2) +
  geom_point(
    aes(
      size = avg_weight_per_order,
      color = carrier,
      fill = carrier
    ),
    alpha = 0.7,
    shape = 21,
    stroke = 0.5
  ) +
  ggrepel::geom_text_repel(
    aes(label = point_label),
    size = 3,
    family = "text",
    color = text_col,
    box.padding = 0.5,
    segment.color = "gray70",
    min.segment.length = 0,
    show.legend = FALSE
  ) +
  scale_size_continuous(
    name = "Avg Weight per Order (kg)",
    range = c(5, 20),
    labels = scales::comma,
    breaks = c(200, 400, 600)
  ) +
  # Using only 2 colors for the carriers
  scale_color_manual(
    name = "Carrier",
    values = palette_main[1:2]
  ) +
  scale_fill_manual(
    name = "Carrier",
    values = alpha(palette_main[1:2], 0.3)
  ) +
  scale_x_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0.1, 0.2))
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0.1, 0.2))
  ) +
  labs(
    title = "2.B Weight Band Distribution by Carrier",
    subtitle = "Comparing order volume and total weight shipped",
    x = "Number of Orders",
    y = "Total Weight (kg)",
    caption = "Note: Bubble size represents average weight per order"
  ) +
  theme_custom() +
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.margin = margin(0, 0, 10, 0),
    legend.spacing.x = unit(40, "pt"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    panel.grid = element_line(color = "gray95", linewidth = 0.2),
    plot.margin = margin(10, 25, 10, 10)
  ) +
  guides(
    color = guide_legend(
      title.position = "top",
      nrow = 1,
      override.aes = list(
        size = 4,
        alpha = 0.7,
        shape = 21,
        fill = alpha(palette_main[1:2], 0.3) # Only using 2 colors
      )
    ),
    fill = "none",
    size = guide_legend(
      title.position = "top",
      nrow = 1
    )
  )

print(viz_weight_bands)


## C. Weight Band Analysis ----
viz_weight_bands_detail <- weight_bands_detail |>
  ggplot(aes(x = weight_band, y = total_weight)) +
  geom_col(aes(fill = mode_dsc)) +
  # Add labels using the computed label_text
  geom_text(
    aes(
      label = label_text,
      color = mode_dsc
    ),
    position = position_stack(vjust = 0.5),
    size = 3,
    lineheight = 0.8
  ) +
  facet_wrap(~carrier, scales = "free_x", nrow = 2) +
  scale_fill_manual(values = setNames(palette_main[c(3, 4)], c("AIR", "GROUND"))) +
  scale_color_manual(values = c("AIR" = "white", "GROUND" = "black")) +
  scale_y_continuous(
    labels = scales::comma,
    breaks = seq(0, 120000, 30000),
    name = "Total Weight (kg)"
  ) +
  labs(
    title = "2.C Weight Band Distribution by Carrier",
    subtitle = "Showing total weight shipped and number of orders per weight band",
    x = "Weight Band Range (kg)",
    fill = "Transport Mode",
    caption = "Note: Each bar shows number of orders and total weight shipped in that band"
  ) +
  theme_custom() +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 11, face = "bold"),
    legend.title = element_text(face = "bold")
  ) +
  guides(color = "none")

print(viz_weight_bands_detail)


## D. Weight Band Temporal Analysis ----
# Skipped - need to remove section
# Does not apply since we only have data for one day in May 2013


## E. Weight Band Efficiency Analysis ----
# Create visualization
viz_weight_efficiency <- weight_band_efficiency |>
  ggplot(aes(x = weight_band, y = avg_cost_per_kg)) +
  # Add bars colored by mode
  geom_col(
    aes(fill = mode_dsc),
    alpha = 0.8,
    width = 0.7
  ) +
  # Add cost labels
  geom_text(
    aes(
      label = scales::dollar(avg_cost_per_kg, accuracy = 0.01),
      color = mode_dsc
    ),
    hjust = -0.1,
    size = 3,
    family = "text"
  ) +
  # Facet by carrier
  facet_wrap(~carrier, scales = "free_y") +
  # Use custom color scheme
  scale_fill_manual(values = setNames(palette_main[c(3, 4)], c("AIR", "GROUND"))) +
  scale_color_manual(values = c("AIR" = "white", "GROUND" = "black")) +
  # Flip coordinates for better label reading
  coord_flip() +
  # Format axes
  scale_y_continuous(
    labels = scales::dollar_format(),
    expand = expansion(mult = c(0, 0.3))
  ) +
  # Add labels
  labs(
    title = "2.E Shipping Cost Efficiency by Weight Band",
    subtitle = "Average cost per kilogram across different weight bands and carriers",
    x = "Weight Band Range (kg)",
    y = "Average Cost per Kg ($)",
    fill = "Transport Mode",
    caption = "Note: Lower cost per kg indicates better cost efficiency"
  ) +
  theme_custom() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    strip.text = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 9),
    plot.margin = margin(10, 25, 10, 10)
  ) +
  guides(color = "none")

print(viz_weight_efficiency)



## F. Weight Band Service Level Patterns ----
viz_service_patterns <- service_level_summary |>
  ggplot(aes(x = carrier, y = total_orders)) +
  # Add bars
  geom_col(
    aes(fill = service_level),
    width = 0.7,
    alpha = 0.8
  ) +
  # Add percentage labels
  geom_text(
    aes(
      label = paste0(round(pct_orders), "%\n", 
                     scales::comma(total_orders), " orders"),
      color = service_level
    ),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  # Use custom colors
  scale_fill_manual(
    values = setNames(palette_main[c(1, 2, 3)], c("CRF", "DTD", "DTP"))
  ) +
  scale_color_manual(
    values = c("CRF" = "white", "DTD" = "white", "DTP" = "white")
  ) +
  # Format y-axis
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.1))
  ) +
  # Add labels
  labs(
    title = "2.F Service Level Distribution by Carrier",
    subtitle = "Order volume and percentage breakdown by service level",
    x = "Carrier",
    y = "Number of Orders",
    fill = "Service Level",
    caption = "Service Levels: CRF = Customer Referred Freight, DTD = Door-to-Door, DTP = Door-to-Port"
  ) +
  theme_custom() +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold")
  ) +
  guides(color = "none")

print(viz_service_patterns)


## G. Weight Band Route Analysis ----

viz_weight_routes <- weight_band_routes |>
  ggplot(aes(x = weight_band, y = total_weight)) +
  geom_col(
    aes(fill = carrier),
    position = "stack",
    width = 0.7,
    alpha = 0.8
  ) +
  # Improved labels
  geom_text(
    aes(
      label = if_else(total_weight > 1000,
                      glue::glue("{scales::comma(round(total_weight, -1))} kg\n{scales::comma(total_orders)} orders"), 
                      "")
    ),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white",
    lineheight = 0.8
  ) +
  facet_wrap(
    ~route, 
    ncol = 2,
    scales = "free_x"
  ) +
  scale_fill_manual(
    values = palette_main[1:2]
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "2.G Weight Band Distribution by Route",
    subtitle = "Total weight shipped across different routes and carriers\nNote: PORT09 → PORT09 represents internal transfers within the same port",
    x = "Weight Band Range (kg)",
    y = "Total Weight (kg)",
    fill = "Carrier",
    caption = "Note: Labels show total weight and number of orders for segments > 1,000 kg"
  ) +
  theme_custom() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(hjust = 1),
    strip.text = element_text(size = 11, face = "bold"),
    panel.spacing = unit(2, "lines"),
    # Ensure y-axis labels are clear
    axis.text.y = element_text(size = 9)
  )

print(viz_weight_routes)



# 3. SERVICE LEVEL METRICS VISUALIZATIONS --------------------------------------

## A. Service Level Distribution ----
viz_service_levels <- service_analysis$service_summary %>%
  ggplot(aes(x = reorder(service_level, -pct_orders), y = pct_orders)) +
  # Add bars
  geom_col(aes(fill = service_level), alpha = 0.8) +
  # Add percentage labels
  geom_text(
    aes(label = glue::glue("{round(pct_orders, 1)}%\n{scales::comma(orders)} orders")),
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.2))
  ) +
  scale_fill_manual(
    values = setNames(palette_main[1:3], c("CRF", "DTD", "DTP"))
  ) +
  labs(
    title = "3.A Service Level Distribution",
    subtitle = "Percentage of orders and total volume by service type",
    x = "Service Level",
    y = "Percentage of Orders",
    caption = "Service Levels: CRF = Customer Referred Freight, DTD = Door-to-Door, DTP = Door-to-Port"
  ) +
  theme_custom() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

## B. Service Level by Carrier ----
viz_service_carrier <- service_analysis$carrier_service %>%
  ggplot(aes(x = carrier, y = orders)) +
  geom_col(
    aes(fill = service_level),
    position = "fill",
    width = 0.7,
    alpha = 0.8
  ) +
  # Add percentage labels
  geom_text(
    aes(
      label = glue::glue("{round(pct_carrier_orders)}%\n{scales::comma(orders)} orders"),
      y = pct_carrier_orders/200  # Position labels in middle of their segments
    ),
    position = position_fill(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(
    values = setNames(palette_main[1:3], c("CRF", "DTD", "DTP"))
  ) +
  labs(
    title = "3.B Service Level Distribution by Carrier",
    subtitle = "Percentage breakdown of service levels for each carrier",
    x = "Carrier",
    y = "Percentage of Carrier's Orders",
    fill = "Service Level"
  ) +
  theme_custom() +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

## C. Route Service Level Patterns ----
viz_route_service <- service_analysis$route_service %>%
  # Sort routes by total volume
  mutate(
    route = fct_reorder(route, total_weight),
    # Create better labels
    label_text = if_else(
      total_weight > 1000,
      glue::glue("{scales::comma(round(total_weight))} kg\n({scales::comma(orders)} orders)"),
      ""
    )
  ) %>%
  ggplot(aes(x = route, y = total_weight)) +
  # Add bars
  geom_col(
    aes(fill = service_level),
    position = "stack",
    width = 0.7,
    alpha = 0.8
  ) +
  # Add volume labels with better positioning
  geom_text(
    aes(
      label = label_text,
      group = service_level
    ),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white",
    lineheight = 0.9
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.05)),
    breaks = seq(0, 160000, by = 40000)
  ) +
  scale_fill_manual(
    values = setNames(palette_main[c(1:3)], c("CRF", "DTD", "DTP"))
  ) +
  labs(
    title = "3.C Service Level Volume by Route",
    subtitle = "Total weight shipped and order counts across routes",
    x = NULL,
    y = "Total Weight (kg)",
    fill = "Service Level"
  ) +
  theme_custom() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    # Ensure enough room for labels
    plot.margin = margin(10, 25, 10, 10)
  )

## D. Service Level Cost Efficiency ----
viz_service_efficiency <- service_analysis$service_summary %>%
  filter(service_level != "CRF") %>%  # Exclude CRF as it has no cost data
  ggplot(aes(x = service_level, y = avg_cost_per_kg)) +
  geom_col(
    aes(fill = service_level),
    width = 0.7,
    alpha = 0.8
  ) +
  # Add cost labels
  geom_text(
    aes(
      label = glue::glue("${round(avg_cost_per_kg, 2)}/kg\n{scales::comma(total_weight)} kg total")
    ),
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(
    labels = scales::dollar,
    expand = expansion(mult = c(0, 0.2))
  ) +
  scale_fill_manual(
    values = setNames(palette_main[2:3], c("DTD", "DTP"))
  ) +
  labs(
    title = "3.D Cost Efficiency by Service Level",
    subtitle = "Average cost per kilogram for each service type",
    x = "Service Level",
    y = "Average Cost per Kg",
    caption = "Note: CRF (Customer Referred Freight) excluded as costs are handled by customers"
  ) +
  theme_custom() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

# Print visualizations
print(viz_service_levels)
print(viz_service_carrier)
print(viz_route_service)
print(viz_service_efficiency)


