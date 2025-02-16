# Supply Chain Analytics Dashboard

## Overview
An interactive R Shiny dashboard for supply chain analytics, featuring:
- Cost analysis and visualization
- Warehouse utilization metrics
- Service level monitoring
- Interactive filters and downloads

## Requirements

### Software Requirements
- R (>= 4.0.0)
- RStudio (recommended for development)

### Required R Packages
```r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,       # for data manipulation
  shiny,           # for the web app
  bslib,           # for modern bootstrap styling
  DT,              # for interactive tables
  scales,          # for nice formatting
  glue,            # for string interpolation
  readxl,          # for reading Excel files
  writexl,         # for writing Excel files
  janitor,         # for clean_names
  ggiraph,         # for interactive plots
  shinycssloaders, # for loading spinners
  assertthat       # for input validation
)
```

### Data Requirements
The dashboard expects an Excel file (`rwfd_supply_chain.xlsx`) with the following sheets:

- Order List
- Freight Rates
- Warehouse Costs
- Warehouse Capacities

## Installation

1. Clone this repository
2. Install required packages using the code above
3. Place your data file in `data/01_raw_data/rwfd_supply_chain.xlsx`
4. Run the app: `shiny::runApp()`


## Data Schema and Validation Rules

### 1. Order List (order_list)
| Column Name          | Data Type    | Example        | Validation Rules                   |
|---------------------|--------------|----------------|-----------------------------------|
| order_id            | numeric      | 1450000001     | Must be unique, > 0              |
| order_date          | datetime     | 2024-01-15     | Cannot be future date            |
| origin_port         | character    | PORT09         | Must exist in freight_rates      |
| carrier             | character    | V44_3          | Must exist in freight_rates      |
| tpt                 | numeric      | 1              | Must be > 0                      |
| service_level       | character    | CRF            | Must be CRF, DTD, or DTP         |
| ship_ahead_day_count| numeric      | 3              | Must be >= 0                     |
| ship_late_day_count | numeric      | 0              | Must be >= 0                     |
| customer            | character    | V55555_53      | Cannot be empty                  |
| product_id          | numeric      | 1700106        | Must be > 0                      |
| plant_code          | character    | PLANT16        | Must exist in wh_capacities      |
| destination_port    | character    | PORT09         | Must exist in freight_rates      |
| unit_quantity       | numeric      | 808            | Must be > 0                      |
| weight              | numeric      | 14.3           | Must be > 0                      |

### 2. Freight Rates (freight_rates)
| Column Name    | Data Type | Example    | Validation Rules                    |
|---------------|-----------|------------|-------------------------------------|
| orig_port_cd  | character | PORT09     | Must match order_list ports         |
| dest_port_cd  | character | PORT09     | Must match order_list ports         |
| carrier       | character | V44_3      | Must match order_list carriers      |
| svc_cd        | character | CRF        | Must be CRF, DTD, or DTP           |
| mode_dsc      | character | AIR        | Must be AIR or GROUND              |
| minm_wgh_qty  | numeric   | 0          | Must be >= 0                       |
| max_wgh_qty   | numeric   | 1000       | Must be > minm_wgh_qty             |
| rate          | numeric   | 2.50       | Must be > 0                        |
| minimum_cost  | numeric   | 100        | Must be >= 0                       |

### 3. Warehouse Costs (wh_costs)
| Column Name | Data Type | Example  | Validation Rules                     |
|------------|-----------|----------|--------------------------------------|
| wh         | character | PLANT16  | Must exist in wh_capacities          |
| cost_unit  | numeric   | 1.25     | Must be > 0                         |
| year       | numeric   | 2024     | Must be current year ±1             |
| month      | numeric   | 1        | Must be between 1 and 12            |

### 4. Warehouse Capacities (wh_capacities)
| Column Name    | Data Type | Example  | Validation Rules                     |
|---------------|-----------|----------|--------------------------------------|
| plant_id      | character | PLANT16  | Must be unique                      |
| daily_capacity| numeric   | 1000     | Must be > 0                         |
| region        | character | EAST     | Must be valid region code           |
| type          | character | MAIN     | Must be valid warehouse type        |
| status        | character | ACTIVE   | Must be ACTIVE or INACTIVE          |


## Table Relationships

1. Order List → Warehouse Capacities
   - order_list.plant_code = wh_capacities.plant_id

2. Order List → Freight Rates
   - order_list.origin_port = freight_rates.orig_port_cd
   - order_list.destination_port = freight_rates.dest_port_cd
   - order_list.carrier = freight_rates.carrier
   - order_list.service_level = freight_rates.svc_cd

3. Warehouse Costs → Warehouse Capacities
   - wh_costs.wh = wh_capacities.plant_id

## Data Validation Notes
- All dates must be in ISO format (YYYY-MM-DD)
- Currency values must be in base units (no formatting)
- Weights must be in kilograms
- Port codes must follow standard format (PORTxx)
- Plant codes must follow standard format (PLANTxx)

