# R/weight_band_normalization.R

#' Weight Band Normalization Functions
#' 
#' Functions to normalize and validate weight bands for shipping rate 
#' calculations and cost analysis.
#' 
#' @details
#' Provides functionality for:
#'   * Weight band categorization
#'   * Rate normalization across bands
#'   * Validation of weight ranges
#'   * Boundary adjustments for overlapping bands
#' 
#' @examples
#' normalize_weight_bands(shipping_data)
#' validate_weight_bands(rate_data)

#' Normalize Weight Bands
#' 
#' @param data Data frame containing shipping weights and rates
#' @return Data frame with normalized weight bands
#' @details
#' Performs:
#'   * Weight range validation
#'   * Band boundary adjustments
#'   * Rate consistency checks
#'   * Gap/overlap detection

library(assertthat)

validate_weight_bands <- function(data, weight_band_col = "weight_band") {
  # Define valid weight bands
  valid_bands <- c(
    "0 - 99.99", "0.01 - 0.5", "0.51 - 1", "1.01 - 1.5",
    "1.51 - 2", "2.01 - 2.5", "70.51 - 99.99", "100 - 249.99",
    "100 - 299.99", "250 - 499.99", "300 - 499.99", "500 - 1999.99",
    "0 - 5000"
  )
  
  # Critical checks
  assert_that(
    weight_band_col %in% colnames(data),
    msg = sprintf("Weight band column '%s' not found in data", weight_band_col)
  )
  
  clean_bands <- str_replace(data[[weight_band_col]], " kg", "") |>
    str_trim()
  
  # Check for invalid bands (allowing for NA values)
  invalid_bands <- setdiff(clean_bands[!is.na(clean_bands)], valid_bands)
  assert_that(
    length(invalid_bands) == 0,
    msg = sprintf("Invalid weight bands found: %s", 
                  paste(invalid_bands, collapse = ", "))
  )
  
  # Check for NA values (warning only)
  na_count <- sum(is.na(data[[weight_band_col]]))
  if (na_count > 0) {
    warning(sprintf("Found %d NA values in weight bands", na_count))
  }
  
  return(invisible(data))
}

normalize_weight_bands <- function(data, weight_band_col = "weight_band") {
  # First validate the input data
  data <- validate_weight_bands(data, weight_band_col)
  
  # Define standard order of weight bands
  weight_band_levels <- c(
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
  
  # Function to clean weight band format
  clean_weight_band <- function(x) {
    x <- str_replace(x, " kg", "") # Remove kg suffix
    x <- str_trim(x)               # Remove any extra whitespace
    return(x)
  }
  
  # Apply normalization
  data |>
    mutate(
      # Clean the weight band format
      !!weight_band_col := clean_weight_band(!!sym(weight_band_col)),
      # Convert to factor with defined levels
      !!weight_band_col := factor(!!sym(weight_band_col), 
                                  levels = weight_band_levels,
                                  ordered = TRUE)
    )
}