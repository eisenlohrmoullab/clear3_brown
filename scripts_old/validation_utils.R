# ============================================================================
# validation_utils.R
# 
# Purpose: Validation functions for user-defined parameters
# ============================================================================

# --- Validate that a variable exists in the data ---
validate_variable_exists <- function(data, var_name, var_description = NULL) {
  if (is.null(var_description)) {
    var_description <- var_name
  }
  
  if (!var_name %in% names(data)) {
    stop(paste0(
      "ERROR: Variable '", var_name, "' (", var_description, ") not found in data.\n",
      "Available variables: ", paste(head(names(data), 20), collapse = ", "), 
      if (length(names(data)) > 20) "..." else ""
    ))
  }
  
  return(TRUE)
}

# --- Validate that a directory path exists ---
validate_directory <- function(dir_path, create_if_missing = TRUE, dir_description = "Directory") {
  if (is.null(dir_path) || dir_path == "") {
    stop(paste0("ERROR: ", dir_description, " path is NULL or empty."))
  }
  
  # Expand ~ and other path shortcuts
  dir_path <- path.expand(dir_path)
  
  if (!dir.exists(dir_path)) {
    if (create_if_missing) {
      tryCatch({
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
        message(paste0("✓ Created ", dir_description, ": ", dir_path))
      }, error = function(e) {
        stop(paste0(
          "ERROR: Failed to create ", dir_description, " at: ", dir_path, "\n",
          "Error message: ", e$message, "\n",
          "Please check that you have write permissions for this location."
        ))
      })
    } else {
      stop(paste0(
        "ERROR: ", dir_description, " does not exist: ", dir_path, "\n",
        "Please create this directory or provide a valid path."
      ))
    }
  }
  
  return(dir_path)
}

# --- Validate that required parameters are provided ---
validate_parameters <- function(
    data,
    menses_time_variable,
    ovulation_time_variable,
    outcomes_to_run,
    base_save_dir
) {
  
  message("\n========================================")
  message("Validating Analysis Parameters")
  message("========================================\n")
  
  # Check that data exists and is a data frame
  if (missing(data) || is.null(data)) {
    stop("ERROR: No data provided. Please ensure 'cycle_df_scaled' or your data object is loaded.")
  }
  
  if (!is.data.frame(data)) {
    stop("ERROR: Data must be a data.frame or tibble.")
  }
  
  if (nrow(data) == 0) {
    stop("ERROR: Data is empty (0 rows).")
  }
  
  message(paste0("✓ Data validated: ", nrow(data), " rows, ", ncol(data), " columns"))
  
  # Check time variables
  validate_variable_exists(data, menses_time_variable, "menses time variable")
  message(paste0("✓ Menses time variable found: ", menses_time_variable))
  
  validate_variable_exists(data, ovulation_time_variable, "ovulation time variable")
  message(paste0("✓ Ovulation time variable found: ", ovulation_time_variable))
  
  # Check outcomes
  if (is.null(outcomes_to_run) || length(outcomes_to_run) == 0) {
    stop("ERROR: No outcomes specified in 'outcomes_to_run'. Please provide at least one outcome variable.")
  }
  
  missing_outcomes <- character(0)
  for (outcome in outcomes_to_run) {
    if (!outcome %in% names(data)) {
      missing_outcomes <- c(missing_outcomes, outcome)
    }
  }
  
  if (length(missing_outcomes) > 0) {
    message(paste0(
      "NOTE: The following outcomes were not found in data and will be skipped: ",
      paste(missing_outcomes, collapse = ", ")
    ))
    outcomes_to_run <- setdiff(outcomes_to_run, missing_outcomes)
  }
  
  if (length(outcomes_to_run) == 0) {
    stop("ERROR: None of the specified outcomes exist in the data.")
  }
  
  message(paste0("✓ Outcomes validated: ", paste(outcomes_to_run, collapse = ", ")))
  
  # Check and create save directory
  base_save_dir <- validate_directory(base_save_dir, create_if_missing = TRUE, "Output directory")
  message(paste0("✓ Output directory validated: ", base_save_dir))
  
  # Check for 'id' variable (required for analysis)
  if (!"id" %in% names(data)) {
    stop("ERROR: Data must contain an 'id' column to identify participants.")
  }
  message("✓ ID variable found in data")
  
  message("\n========================================")
  message("Parameter Validation Complete!")
  message("========================================\n")
  
  return(list(
    data_valid = TRUE,
    outcomes_to_run = outcomes_to_run,
    base_save_dir = base_save_dir
  ))
}
