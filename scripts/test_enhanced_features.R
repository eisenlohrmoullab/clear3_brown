# ============================================================================
# test_enhanced_features.R
# 
# Purpose: Test the enhanced SMM cyclic analysis features
# This script validates that all new utilities work correctly
# ============================================================================

# Set working directory to the 999_cycle_pipeline folder using the 'here' package
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
setwd(here::here())

cat("\n========================================\n")
cat("Testing Enhanced SMM Features\n")
cat("========================================\n\n")

# --- Test 1: Dependency Checking ---
cat("Test 1: Checking dependency management...\n")
tryCatch({
  source("check_dependencies.R")
  # Test with a subset of packages to avoid long installation
  test_packages <- c("dplyr", "ggplot2", "glue")
  result <- check_and_install_packages(test_packages, quiet = TRUE)
  cat("✓ Test 1 PASSED: Dependency checking works\n\n")
}, error = function(e) {
  cat("✗ Test 1 FAILED:", e$message, "\n\n")
})

# --- Test 2: Logging Utilities ---
cat("Test 2: Testing logging utilities...\n")
tryCatch({
  source("logging_utils.R")
  
  # Create a test log in temp directory
  test_log_dir <- file.path(tempdir(), "test_logs")
  test_log_file <- init_logger(log_dir = test_log_dir)
  
  # Test different log levels
  log_info("Test info message", print_console = FALSE)
  log_warn("Test warning message", print_console = FALSE)
  log_error("Test error message", print_console = FALSE)
  log_success("Test success message", print_console = FALSE)
  
  # Check that log file was created and contains entries
  if (file.exists(test_log_file)) {
    log_lines <- readLines(test_log_file)
    if (length(log_lines) >= 4) {
      cat("✓ Test 2 PASSED: Logging utilities work\n")
      cat("  Log file created at:", test_log_file, "\n\n")
    } else {
      cat("✗ Test 2 FAILED: Log file has insufficient entries\n\n")
    }
  } else {
    cat("✗ Test 2 FAILED: Log file not created\n\n")
  }
  
  finalize_logger()
}, error = function(e) {
  cat("✗ Test 2 FAILED:", e$message, "\n\n")
})

# --- Test 3: Validation Utilities ---
cat("Test 3: Testing validation utilities...\n")
tryCatch({
  source("validation_utils.R")
  
  # Create test data
  test_data <- data.frame(
    id = rep(1:10, each = 20),
    cyclic_time_impute = rep(seq(-1, 1, length.out = 20), 10),
    cyclic_time_imp_ov = rep(seq(-1, 1, length.out = 20), 10),
    outcome1 = rnorm(200),
    outcome2 = rnorm(200)
  )
  
  # Test variable validation
  validate_variable_exists(test_data, "id", "ID variable")
  validate_variable_exists(test_data, "cyclic_time_impute", "Time variable")
  
  # Test directory validation
  test_dir <- file.path(tempdir(), "test_output")
  validate_directory(test_dir, create_if_missing = TRUE)
  
  if (dir.exists(test_dir)) {
    cat("✓ Test 3 PASSED: Validation utilities work\n\n")
  } else {
    cat("✗ Test 3 FAILED: Directory creation failed\n\n")
  }
}, error = function(e) {
  cat("✗ Test 3 FAILED:", e$message, "\n\n")
})

# --- Test 4: Load Helpers ---
cat("Test 4: Testing centralized script loading...\n")
tryCatch({
  # Check that all helper scripts exist
  helper_scripts <- c(
    "run_SMM_cyclic",
    "subset_by_phase_cyclic",
    "compare_bic_cyclic",
    "model_plot_modx_gam_cyclic"
  )
  
  missing_scripts <- character(0)
  for (script in helper_scripts) {
    if (!file.exists(script)) {
      missing_scripts <- c(missing_scripts, script)
    }
  }
  
  if (length(missing_scripts) == 0) {
    # Test loading
    source("load_helpers.R")
    cat("✓ Test 4 PASSED: All helper scripts found and loaded\n\n")
  } else {
    cat("✗ Test 4 FAILED: Missing scripts:", paste(missing_scripts, collapse = ", "), "\n\n")
  }
}, error = function(e) {
  cat("✗ Test 4 FAILED:", e$message, "\n\n")
})

# --- Test 5: Parameter Validation ---
cat("Test 5: Testing full parameter validation...\n")
tryCatch({
  source("validation_utils.R")
  
  # Create test data
  test_data <- data.frame(
    id = rep(1:10, each = 20),
    cyclic_time_impute = rep(seq(-1, 1, length.out = 20), 10),
    cyclic_time_imp_ov = rep(seq(-1, 1, length.out = 20), 10),
    E2 = rnorm(200),
    P4 = rnorm(200)
  )
  
  test_dir <- file.path(tempdir(), "test_validation_output")
  
  result <- validate_parameters(
    data = test_data,
    menses_time_variable = "cyclic_time_impute",
    ovulation_time_variable = "cyclic_time_imp_ov",
    outcomes_to_run = c("E2", "P4"),
    base_save_dir = test_dir
  )
  
  if (result$data_valid && dir.exists(result$base_save_dir)) {
    cat("✓ Test 5 PASSED: Parameter validation works\n\n")
  } else {
    cat("✗ Test 5 FAILED: Validation did not complete successfully\n\n")
  }
}, error = function(e) {
  cat("✗ Test 5 FAILED:", e$message, "\n\n")
})

# --- Summary ---
cat("\n========================================\n")
cat("Testing Complete!\n")
cat("========================================\n")
cat("\nNote: This test validates the new utility functions.\n")
cat("Full SMM analysis testing requires real data.\n\n")
