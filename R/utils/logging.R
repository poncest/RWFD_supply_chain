# R/logging.R

#' Logging Functions for Supply Chain Dashboard
#' 
#' Provides centralized logging functionality for error tracking,
#' performance monitoring, and system diagnostics.
#' 
#' @details
#' Implements logging for:
#'   * Error messages
#'   * Warning notifications
#'   * Performance metrics
#'   * User actions
#'   * System events
#' 
#' @examples
#' log_error("Failed to load data")
#' log_warning("Missing carrier code")

#' Log Error Messages
#' 
#' @param message Error message to be logged
#' @param timestamp Include timestamp in log (default: TRUE)
#' @return NULL, writes to error log file
#' @details Records errors with timestamps and context

log_error <- function(message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  error_log <- file.path("logs", "error.log")
  cat(sprintf("[ERROR] [%s] %s\n", timestamp, message), 
      file = error_log, 
      append = TRUE)
}

#' Log Warning Messages
#' 
#' @param message Warning message to be logged
#' @param timestamp Include timestamp in log (default: TRUE)
#' @return NULL, writes to warning log file
#' @details Records warnings with timestamps and context
log_warning <- function(message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  warning_log <- file.path("logs", "warning.log")
  cat(sprintf("[WARNING] [%s] %s\n", timestamp, message), 
      file = warning_log, 
      append = TRUE)
}

#' Log validation failures and warnings
#' @param level Error level (ERROR, WARNING)
#' @param message Message to log
#' @param data Optional data for context
log_validation <- function(level, message, data = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_file <- file.path("logs", paste0("validation_", tolower(level), ".log"))
  
  entry <- sprintf("[%s] [%s] %s\n", level, timestamp, message)
  
  # Create logs directory if it doesn't exist
  dir.create("logs", showWarnings = FALSE)
  
  # Write log entry
  cat(entry, file = log_file, append = TRUE)
  
  # Return message for error handling
  if(level == "ERROR") {
    stop(message)
  } else {
    warning(message)
  }
}
