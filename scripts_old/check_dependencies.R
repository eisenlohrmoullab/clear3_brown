# ============================================================================
# check_dependencies.R
# 
# Purpose: Check and install required R packages for SMM cyclic analysis
# ============================================================================

# --- List of required packages ---
required_packages <- c(
  "dplyr",
  "tidyverse",
  "tidyr",          # For data reshaping (used in group comparisons)
  "ggplot2",
  "glue",
  "mgcv",
  "gamm4",
  "slider",
  "zoo",
  "foreach",        # For parallelization
  "doParallel",     # For parallel backend
  "future.apply",   # Alternative parallelization package
  "marginaleffects", # For GAM predictions
  "gridExtra",      # For PDF report layouts
  "grid",           # For PDF report layouts
  "png"             # For loading PNG images in PDF reports
)

# --- Function to check and install packages ---
check_and_install_packages <- function(packages = required_packages, quiet = FALSE) {
  
  if (!quiet) {
    message("\n========================================")
    message("Checking Required R Packages")
    message("========================================\n")
  }
  
  missing_packages <- character(0)
  installed_packages <- character(0)
  
  # Check each package
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  # Install missing packages
  if (length(missing_packages) > 0) {
    if (!quiet) {
      message(paste0("⚠️  Missing packages detected: ", paste(missing_packages, collapse = ", ")))
      message("📦 Installing missing packages...")
    }
    
    for (pkg in missing_packages) {
      tryCatch({
        install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org/")
        installed_packages <- c(installed_packages, pkg)
        if (!quiet) {
          message(paste0("✓ Successfully installed: ", pkg))
        }
      }, error = function(e) {
        stop(paste0(
          "ERROR: Failed to install package '", pkg, "'\n",
          "Error message: ", e$message, "\n",
          "Please install this package manually using: install.packages('", pkg, "')"
        ))
      })
    }
    
    if (!quiet && length(installed_packages) > 0) {
      message(paste0("\n✅ Successfully installed ", length(installed_packages), " package(s)"))
    }
  } else {
    if (!quiet) {
      message("✅ All required packages are already installed!")
    }
  }
  
  # Load all packages
  if (!quiet) {
    message("\n📚 Loading packages...")
  }
  
  for (pkg in packages) {
    tryCatch({
      library(pkg, character.only = TRUE)
      if (!quiet) {
        message(paste0("✓ Loaded: ", pkg))
      }
    }, error = function(e) {
      stop(paste0(
        "ERROR: Failed to load package '", pkg, "'\n",
        "Error message: ", e$message
      ))
    })
  }
  
  if (!quiet) {
    message("\n========================================")
    message("Package Check Complete!")
    message("========================================\n")
  }
  
  invisible(list(
    required = packages,
    missing = missing_packages,
    installed = installed_packages
  ))
}

# --- Function to check specific packages ---
check_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste0("Installing package: ", pkg))
    install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  }
  library(pkg, character.only = TRUE)
}
