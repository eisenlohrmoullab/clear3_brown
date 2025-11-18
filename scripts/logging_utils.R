# ============================================================================
# logging_utils.R
# 
# Purpose: Logging utilities for SMM cyclic analysis
# Provides functions for file-based logging with timestamps and log levels
# ============================================================================

# --- Initialize the logging system ---
# Call this function at the start of your analysis to set up logging
init_logger <- function(log_file = NULL, log_dir = NULL) {
  # Set default log directory if not provided
  if (is.null(log_dir)) {
    log_dir <- file.path(getwd(), "logs")
  }
  
  # Create logs directory if it doesn't exist
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Set default log file name if not provided
  if (is.null(log_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_file <- file.path(log_dir, paste0("smm_analysis_", timestamp, ".log"))
  } else if (!grepl("^/", log_file) && !grepl("^[A-Za-z]:", log_file)) {
    # If relative path, put it in log_dir
    log_file <- file.path(log_dir, log_file)
  }
  
  # Store log file path in global option
  options(smm_log_file = log_file)
  
  # Write initial log entry
  log_message("INFO", "Logging initialized")
  log_message("INFO", paste("Log file:", log_file))
  
  return(log_file)
}

# --- Core logging function ---
log_message <- function(level = "INFO", message, print_console = TRUE) {
  # Get log file from options
  log_file <- getOption("smm_log_file")
  
  # Create timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Format log entry
  log_entry <- sprintf("[%s] [%s] %s\n", timestamp, level, message)
  
  # Write to file if log file is set
  if (!is.null(log_file)) {
    tryCatch({
      cat(log_entry, file = log_file, append = TRUE)
    }, error = function(e) {
      warning(paste("Failed to write to log file:", e$message))
    })
  }
  
  # Print to console if requested
  if (print_console) {
    # Add color/formatting based on level
    console_message <- switch(level,
      "ERROR" = paste0("❌ ERROR: ", message),
      "WARN" = paste0("⚠️  WARNING: ", message),
      "INFO" = paste0("ℹ️  INFO: ", message),
      "SUCCESS" = paste0("✅ SUCCESS: ", message),
      paste0(level, ": ", message)
    )
    message(console_message)
  }
  
  invisible(NULL)
}

# --- Convenience wrappers for different log levels ---
log_info <- function(message, print_console = TRUE) {
  log_message("INFO", message, print_console)
}

log_warn <- function(message, print_console = TRUE) {
  log_message("WARN", message, print_console)
}

log_error <- function(message, print_console = TRUE) {
  log_message("ERROR", message, print_console)
}

log_success <- function(message, print_console = TRUE) {
  log_message("SUCCESS", message, print_console)
}

# --- Function to close/finalize logging ---
finalize_logger <- function() {
  log_file <- getOption("smm_log_file")
  if (!is.null(log_file)) {
    log_message("INFO", "Analysis complete. Closing log file.")
    message(paste("\n📄 Log file saved at:", log_file))
  }
  options(smm_log_file = NULL)
}
