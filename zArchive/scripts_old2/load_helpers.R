# ============================================================================
# load_helpers.R
# 
# Purpose: Centralized script loading for SMM cyclic analysis
# This script sources all necessary auxiliary scripts with error handling
# ============================================================================

# --- Define the directory where helper scripts are located ---
# By default, assumes scripts are in the same directory as this file
# Robustly determine the script directory
script_dir <- if (exists("script_dir_override")) {
  script_dir_override
} else {
  # Try to get the directory of this script file
  this_file <- tryCatch({
    # Method 1: Use getSrcFilename if available (works when sourced)
    srcfile <- getSrcFilename(sys.call(sys.nframe()))
    if (!is.null(srcfile) && srcfile != "" && srcfile != "<text>") {
      dirname(normalizePath(srcfile, winslash = "/", mustWork = FALSE))
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  if (is.null(this_file)) {
    # Method 2: Try rstudioapi
    this_file <- tryCatch({
      if (requireNamespace("rstudioapi", quietly = TRUE) &&
          rstudioapi::isAvailable() &&
          !is.null(rstudioapi::getActiveDocumentContext()$path) &&
          rstudioapi::getActiveDocumentContext()$path != "") {
        dirname(rstudioapi::getActiveDocumentContext()$path)
      } else {
        NULL
      }
    }, error = function(e) NULL)
  }
  
  if (is.null(this_file)) {
    # Method 3: Try sys.frame (for older R versions)
    this_file <- tryCatch({
      if (!is.null(sys.frame(1)$ofile) && sys.frame(1)$ofile != "") {
        dirname(sys.frame(1)$ofile)
      } else {
        NULL
      }
    }, error = function(e) NULL)
  }
  
  # Validate that the detected directory contains helper scripts
  # If not, try common fallback locations
  detected_dir <- this_file
  
  # Helper function to check if directory contains required scripts
  check_scripts_exist <- function(dir) {
    if (is.null(dir) || !dir.exists(dir)) return(FALSE)
    # Check for at least one key helper script
    file.exists(file.path(dir, "run_SMM_cyclic.R"))
  }
  
  if (!check_scripts_exist(detected_dir)) {
    # Try scripts/ subdirectory of working directory
    scripts_subdir <- file.path(getwd(), "scripts")
    if (check_scripts_exist(scripts_subdir)) {
      detected_dir <- scripts_subdir
    } else if (!is.null(detected_dir)) {
      # Try scripts/ subdirectory of the detected directory (in case we're one level up)
      scripts_subdir <- file.path(detected_dir, "scripts")
      if (check_scripts_exist(scripts_subdir)) {
        detected_dir <- scripts_subdir
      }
    }
  }
  
  # Final fallback
  if (is.null(detected_dir) || !check_scripts_exist(detected_dir)) {
    # Last resort - check if we're already in the scripts directory
    if (check_scripts_exist(getwd())) {
      detected_dir <- getwd()
    } else {
      detected_dir <- getwd()  # Will fail later with clear error message
    }
  }
  
  detected_dir
}
# --- List of required helper scripts ---
helper_scripts <- c(
  "run_SMM_cyclic.R",
  "subset_by_phase_cyclic.R",
  "compare_bic_cyclic.R",
  "model_plot_modx_gam_cyclic.R",
  "create_comparison_report.R",
  "create_outcome_pdf_report.R"
)

# --- Function to safely source a script with error handling ---
safe_source <- function(script_name, script_dir) {
  script_path <- file.path(script_dir, script_name)
  
  if (!file.exists(script_path)) {
    stop(paste0(
      "ERROR: Required script '", script_name, "' not found at: ", script_path, "\n",
      "Please ensure all helper scripts are in the same directory as load_helpers.R\n",
      "or set 'script_dir_override' before sourcing this file."
    ))
  }
  
  tryCatch({
    source(script_path)
    message(paste0("✓ Successfully loaded: ", script_name))
  }, error = function(e) {
    stop(paste0(
      "ERROR: Failed to source '", script_name, "'\n",
      "Error message: ", e$message, "\n",
      "Please check the script for syntax errors."
    ))
  })
}

# --- Source all helper scripts ---
message("\n========================================")
message("Loading SMM Cyclic Analysis Helper Scripts")
message("========================================\n")
message(paste0("Script directory: ", script_dir))

for (script in helper_scripts) {
  safe_source(script, script_dir)
}

message("\n========================================")
message("All helper scripts loaded successfully!")
message("========================================\n")
