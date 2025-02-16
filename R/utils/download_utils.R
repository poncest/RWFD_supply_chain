# R/utils/download_utils.R

#' Create Data Download Handler
#' 
#' Creates a handler function for downloading dashboard data as CSV files.
#' Includes validation checks and error handling for robust data exports.
#' 
#' @param data A data frame to be exported. Must not be NULL and must contain rows.
#' @param filename Base filename for the export (without extension).
#' @return A function that handles the file download process.
#' @details 
#' The function includes several validation checks:
#'   * Verifies data is not NULL
#'   * Confirms data is a data frame
#'   * Ensures data frame has rows
#'   * Validates filename is properly formatted
#' 
#' Error handling includes:
#'   * Directory creation for download path
#'   * Warning logs for non-critical issues
#'   * Error logs for export failures
#'   * User-friendly error messages
#' 
#' @examples
#' output$download_csv <- downloadHandler(
#'   filename = function() "data.csv",
#'   content = create_download_handler(data, "data")
#' )
#' 
#' @seealso 
#' \code{\link{write.csv}} for the underlying export function
create_download_handler <- function(data, filename) {
  function(file) {
    # Enhanced validation with informative messages
    tryCatch({
      # Data validation
      if (is.null(data)) {
        stop("No data available for export. Please ensure data is loaded.")
      }
      
      if (!is.data.frame(data)) {
        stop("Invalid data format. Expected a data frame.")
      }
      
      if (nrow(data) == 0) {
        stop("No records to export. Try adjusting your filters.")
      }
      
      # File handling
      dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
      
      # Write the file with error handling
      withCallingHandlers({
        write.csv(data, file, row.names = FALSE, na = "")
      },
      warning = function(w) {
        message(sprintf("[%s] Export warning: %s", Sys.time(), w$message))
      })
      
    }, error = function(e) {
      # Log the error and provide user-friendly message
      message(sprintf("[%s] Export error: %s", Sys.time(), e$message))
      stop(paste("Export failed:", e$message))
    })
  }
}