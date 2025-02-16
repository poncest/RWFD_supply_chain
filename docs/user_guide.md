# Supply Chain Dashboard - User Guide

## Overview
The Supply Chain Analytics Dashboard provides interactive visualizations and analysis of shipping costs, warehouse utilization, and service levels.

## Navigation

### Filters
Located at the top of the dashboard:
- Warehouse: Filter by specific warehouse/origin port
- Carrier: Filter by shipping carrier
- Service Level: Filter by service type (CRF, DTD, DTP)

### Key Performance Indicators (KPIs)
Four main metrics displayed:
1. Total Shipping Cost
   - Shows total costs across all selected shipments
   - Updates based on filter selections
   - Shows "No Cost Data Available" if no valid data for selection

2. Average Cost per KG
   - Shows cost efficiency of shipping
   - Calculated as total cost divided by total weight
   - Indicates if data is missing or unavailable

3. Warehouse Utilization
   - Shows current capacity usage
   - Percentage of daily capacity being used
   - Warns if utilization exceeds thresholds

4. On-Time Delivery
   - Shows percentage of on-time deliveries
   - Based on service level agreements
   - Indicates if data is insufficient for calculation

### Visualizations
1. Top Routes by Cost
   - Bar chart showing most expensive shipping routes
   - Color-coded by transport mode (Air/Ground)
   - Hover for detailed cost and order information
   - Shows "No data available" if filters return no results

2. Storage Costs by Warehouse
   - Shows top 10 warehouses by storage cost
   - Hover to see:
     * Total storage cost
     * Number of units
     * Cost per unit
     * Percentage of total cost
   - Indicates if costs are missing or negative

3. Transport Mode Analysis
   - Compares costs between air and ground shipping
   - Shows:
     * Total cost per mode
     * Number of orders
     * Weight shipped
     * Average cost per order
   - Handles missing or incomplete data appropriately

4. Service Level Distribution
   - Shows breakdown of orders by service type
   - Includes:
     * CRF (Customer Referred Freight)
     * DTD (Door-to-Door)
     * DTP (Door-to-Port)
   - Indicates if service levels are undefined

### Understanding Error Messages

#### Loading States
- "Loading shipping data..." - Data is being retrieved
- "Loading warehouse data..." - Warehouse information is being processed
- "Loading service data..." - Service level data is being loaded

#### No Data Messages
- "No data available for selected filters" - Current filter combination returns no results
- "No Storage Cost Data Available for [Warehouse]" - Missing cost data for specific warehouse
- "No transport data available for [Carrier]" - No shipping data for selected carrier

#### Validation Messages
- "Invalid data structure" - Data format issues detected
- "Missing required information" - Critical data fields are empty
- "Negative values detected" - Cost values are negative and need review

### Troubleshooting Guide

#### Common Issues and Solutions

1. No Data Showing
   - Check if filters are too restrictive
   - Try resetting filters to "All"
   - Verify data is available for selected combination

2. Missing Costs
   - Ensure all carriers have associated rates
   - Check for missing warehouse cost data
   - Verify shipping cost calculations

3. Calculation Warnings
   - Review source data for negative values
   - Check for missing required fields
   - Verify cost and weight relationships

4. Performance Issues
   - Reduce filter complexity
   - Clear browser cache
   - Refresh the application

### Data Updates
The dashboard updates when:
- Filters are changed
- New data is loaded
- Calculations are refreshed

Automatic validations include:
- Data completeness checks
- Cost value validation
- Relationship verification
- Missing value detection

### Getting Help
For technical issues or questions:
1. Check data requirements documentation
2. Verify input data formats
3. Review error messages thoroughly
4. Contact support team for assistance

### Export Features
Each visualization includes a download button (↓) that:
- Exports data in CSV format
- Includes current filter selections
- Validates data before export
- Provides timestamps in filenames

### Best Practices
1. Filter Usage:
   - Start broad, then refine
   - Check for data availability
   - Monitor validation messages

2. Data Quality:
   - Review warnings and errors
   - Verify cost calculations
   - Check for missing values

3. Performance:
   - Use specific filters when possible
   - Allow loading to complete
   - Clear filters if performance degrades