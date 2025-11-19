## ============================================================================
## AUTOMATED GROUP PROBABILITY COMPARISONS
## ============================================================================
## This script provides a function to automatically run group probability
## comparisons for a given SMM configuration (centering + winsorization combo).
## 
## It performs:
## 1. Consolidation of results within the configuration
## 2. Identification of best solutions based on BIC
## 3. Categorical comparisons between all outcome pairs
## 4. Probability correlation heatmaps for all groups
## 5. LPA analysis if tidyLPA is available
## 
## All outputs are saved to the comparison_reports directory.
## ============================================================================

library(tidyverse)  # Includes dplyr, ggplot2

#' Run automated group probability comparisons for a specific SMM configuration
#' 
#' @param smm_results_list Named list of SMM results (names should be outcome names)
#' @param bic_results_list Named list of BIC results (names should match smm_results_list)
#' @param centering Centering method ("menses" or "ovulation")
#' @param winsorized Logical indicating if data was winsorized
#' @param rolling_avg Rolling average window ("none", "3day", or "5day")
#' @param base_save_dir Base directory where results are saved
#' @param date_folder Date folder (default: today's date in YYYYMMDD format)
#' @param outcomes Vector of outcome names
#' @param manual_selections Optional dataframe with manual group selections (columns: analysis, groups)
#'                          Use select_final_groupings() to create this interactively
#' 
#' @return List containing all comparison results and output file paths
#' @export
run_group_probability_comparisons <- function(
    smm_results_list,
    bic_results_list,
    centering,
    winsorized,
    rolling_avg = "5day",
    base_save_dir,
    date_folder = format(Sys.Date(), "%Y%m%d"),
    outcomes = NULL,
    manual_selections = NULL
) {
  
  # Load comparison functions
  source("scripts/compare_group_memberships.R")
  
  # Determine configuration label
  winsorized_label <- if (winsorized) "winsorized" else "unwinsorized"
  rolling_label <- paste0("roll", rolling_avg)
  config_label <- paste0(centering, "_", winsorized_label, "_", rolling_label)
  
  # Create output directory
  report_dir <- file.path(
    base_save_dir, 
    "smm", 
    date_folder, 
    "group_probability_comparisons",
    config_label
  )
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  
  message("\n================================================================================")
  message(paste0("Running group probability comparisons for: ", config_label))
  message(paste0("Results will be saved to: ", report_dir))
  message("================================================================================\n")
  
  # If outcomes not provided, extract from results
  if (is.null(outcomes)) {
    outcomes <- names(smm_results_list)
  }
  
  # Filter results to only include outcomes that match the current configuration
  # This ensures we only compare outcomes within the same version
  filtered_smm_results <- list()
  filtered_bic_results <- list()
  
  for (outcome in outcomes) {
    # Construct the expected list name for this configuration
    list_name <- paste0(outcome, "_", centering, "_", winsorized_label, "_", rolling_label)
    
    if (list_name %in% names(smm_results_list)) {
      # Use outcome name as key for simplicity in comparison functions
      filtered_smm_results[[outcome]] <- smm_results_list[[list_name]]
      filtered_bic_results[[outcome]] <- bic_results_list[[list_name]]
    }
  }
  
  # Check if we have at least 2 outcomes to compare
  if (length(filtered_smm_results) < 2) {
    message(paste0("⚠ WARNING: Only ", length(filtered_smm_results), 
                   " outcome(s) found for ", config_label, ". Need at least 2 for comparisons. Skipping."))
    if (length(filtered_smm_results) == 1) {
      message(paste0("  Available outcome: ", names(filtered_smm_results)[1]))
    } else {
      message("  No outcomes found for this configuration.")
    }
    return(list(
      config = config_label,
      report_dir = report_dir,
      status = "skipped",
      reason = "insufficient_outcomes"
    ))
  }
  
  message(paste0("Found ", length(filtered_smm_results), " outcomes to compare: ", 
                 paste(names(filtered_smm_results), collapse = ", ")))
  
  # ============================================================================
  # Step 1: Consolidate Results
  # ============================================================================
  message("\n>>> Step 1: Consolidating SMM results...")
  
  master_df <- tryCatch({
    consolidate_smm_results(
      all_smm_results = filtered_smm_results,
      base_save_dir = base_save_dir,
      date_folder = date_folder,
      load_labels = TRUE
    )
  }, error = function(e) {
    message(paste0("  ✗ ERROR consolidating results: ", e$message))
    message("  Stack trace:")
    message(paste0("    ", capture.output(traceback()), collapse = "\n"))
    return(NULL)
  })
  
  if (is.null(master_df)) {
    return(list(
      config = config_label,
      report_dir = report_dir,
      status = "failed",
      reason = "consolidation_error"
    ))
  }
  
  tryCatch({
    # Save consolidated results
    master_df_file <- file.path(report_dir, paste0(config_label, "_consolidated_results.csv"))
    write.csv(master_df, master_df_file, row.names = FALSE)
    message(paste0("  ✓ Saved consolidated results: ", basename(master_df_file)))
    
  }, error = function(e) {
    message(paste0("  ✗ ERROR saving consolidated results: ", e$message))
  })
  
  # ============================================================================
  # Step 2: Get Best Solutions (with optional manual override)
  # ============================================================================
  if (!is.null(manual_selections)) {
    message("\n>>> Step 2: Identifying group solutions (with manual selections)...")
  } else {
    message("\n>>> Step 2: Identifying best group solutions based on BIC...")
  }
  
  best_solutions <- tryCatch({
    get_best_solutions(filtered_bic_results, manual_selections = manual_selections)
  }, error = function(e) {
    message(paste0("  ✗ ERROR identifying best solutions: ", e$message))
    message("  Stack trace:")
    message(paste0("    ", capture.output(traceback()), collapse = "\n"))
    return(NULL)
  })
  
  if (is.null(best_solutions)) {
    return(list(
      config = config_label,
      report_dir = report_dir,
      status = "failed",
      reason = "best_solutions_error"
    ))
  }
  
  tryCatch({
    # Save best solutions summary
    best_solutions_file <- file.path(report_dir, paste0(config_label, "_best_solutions.csv"))
    write.csv(best_solutions, best_solutions_file, row.names = FALSE)
    message(paste0("  ✓ Saved best solutions: ", basename(best_solutions_file)))
    
  }, error = function(e) {
    message(paste0("  ✗ ERROR saving best solutions: ", e$message))
  })
  
  # ============================================================================
  # Step 3: Categorical Comparisons (All Pairs)
  # ============================================================================
  message("\n>>> Step 3: Running categorical comparisons between all outcome pairs...")
  
  categorical_results <- list()
  outcome_names <- names(filtered_smm_results)
  
  # Create all unique pairs
  if (length(outcome_names) >= 2) {
    outcome_pairs <- combn(outcome_names, 2, simplify = FALSE)
    
    for (pair in outcome_pairs) {
      symptom1 <- pair[1]
      symptom2 <- pair[2]
      pair_label <- paste0(symptom1, "_vs_", symptom2)
      
      message(paste0("  Comparing: ", symptom1, " vs ", symptom2))
      
      tryCatch({
        # Note: We use plain outcome names as keys in filtered_smm_results
        # (the full names with centering/winsorization are in the original lists)
        # So we construct column names directly from outcome names.
        
        # Get best g for each outcome
        g1 <- best_solutions$groups[best_solutions$analysis == symptom1]
        g2 <- best_solutions$groups[best_solutions$analysis == symptom2]
        
        if (length(g1) == 0 || length(g2) == 0) {
          message(paste0("    ⚠ Could not find best solution for one or both outcomes. Skipping."))
          next
        }
        
        # Construct column names (outcome name is the key in our filtered results)
        col1 <- paste0(symptom1, "_g", g1, "_group")
        col2 <- paste0(symptom2, "_g", g2, "_group")
        col1_label <- paste0(col1, "_label")
        col2_label <- paste0(col2, "_label")
        
        # Use labeled columns if available
        use_col1 <- col1
        use_col2 <- col2
        if (col1_label %in% names(master_df)) {
          use_col1 <- col1_label
          message(paste0("    ✓ Using labeled groups for ", symptom1))
        }
        if (col2_label %in% names(master_df)) {
          use_col2 <- col2_label
          message(paste0("    ✓ Using labeled groups for ", symptom2))
        }
        
        # Check if columns exist
        if (!(use_col1 %in% names(master_df)) || !(use_col2 %in% names(master_df))) {
          message(paste0("    ⚠ Required columns not found in master_df. Skipping."))
          next
        }
        
        # Create contingency table
        contingency_table <- table(master_df[[use_col1]], master_df[[use_col2]],
                                   dnn = c(symptom1, symptom2))
        
        # Perform Chi-squared test with proper warning/error handling
        chi_test <- tryCatch({
          suppressWarnings({
            test_result <- chisq.test(contingency_table)
            
            # Check for low expected frequencies
            expected_freqs <- test_result$expected
            low_freq_cells <- sum(expected_freqs < 5)
            total_cells <- length(expected_freqs)
            
            if (low_freq_cells > 0) {
              warning_msg <- sprintf(
                "Chi-squared test may be unreliable: %d of %d cells (%.1f%%) have expected frequency < 5",
                low_freq_cells, total_cells, 100 * low_freq_cells / total_cells
              )
              message(paste0("    ⚠ WARNING: ", warning_msg))
            }
            
            test_result
          })
        }, warning = function(w) {
          message(paste0("    ⚠ Chi-squared test warning: ", w$message))
          suppressWarnings(chisq.test(contingency_table))
        }, error = function(e) {
          message(paste0("    ✗ ERROR in chi-squared test: ", e$message))
          stop(paste("Chi-squared test failed:", e$message))
        })
        
        comparison <- list(
          contingency_table = contingency_table,
          chi_test = chi_test
        )
        
        categorical_results[[pair_label]] <- comparison
        
        # Save contingency table
        contingency_file <- file.path(report_dir, paste0(config_label, "_", pair_label, "_contingency.csv"))
        write.csv(comparison$contingency_table, contingency_file)
        
        # Save chi-squared test results
        chisq_file <- file.path(report_dir, paste0(config_label, "_", pair_label, "_chisquared.txt"))
        sink(chisq_file)
        print(comparison$chi_test)
        sink()
        
        message(paste0("    ✓ Chi-squared p-value: ", round(comparison$chi_test$p.value, 4)))
        
      }, error = function(e) {
        message(paste0("    ✗ ERROR comparing ", symptom1, " vs ", symptom2, ": ", e$message))
        message("    Stack trace:")
        message(paste0("      ", capture.output(traceback()), collapse = "\n"))
      })
    }
  }
  
  # ============================================================================
  # Step 4: Probability Correlation Heatmaps
  # ============================================================================
  message("\n>>> Step 4: Creating labeled pattern probability correlation heatmap...")
  
  heatmap_results <- list()
  
  # Create comprehensive heatmap with all labeled patterns
  tryCatch({
    comprehensive_heatmap <- create_labeled_pattern_correlation_heatmap(
      master_results_df = master_df,
      best_solution_summary = best_solutions,
      title = paste0("Labeled Pattern Probability Correlations (", config_label, ")"),
      use_labels = TRUE
    )
    
    heatmap_results[["comprehensive_labeled"]] <- comprehensive_heatmap
    
    # Save comprehensive heatmap
    heatmap_file <- file.path(report_dir, paste0(config_label, "_labeled_pattern_correlation_heatmap.png"))
    ggsave(heatmap_file, plot = comprehensive_heatmap, width = 14, height = 12, dpi = 300)
    message(paste0("  ✓ Saved comprehensive labeled pattern heatmap: ", basename(heatmap_file)))
    
  }, error = function(e) {
    message(paste0("  ✗ ERROR creating comprehensive heatmap: ", e$message))
    message("  Stack trace:")
    message(paste0("    ", capture.output(traceback()), collapse = "\n"))
  })
  
  # Also create individual group-number heatmaps for backward compatibility (optional)
  # Determine the maximum number of groups across all outcomes
  max_groups <- max(best_solutions$groups, na.rm = TRUE)
  
  # Check if max_groups is valid
  if (!is.null(max_groups) && !is.infinite(max_groups) && !is.na(max_groups) && max_groups >= 1) {
    message(paste0("  Creating individual group-number heatmaps (deprecated approach)..."))
    message(paste0("  Maximum groups found: ", max_groups))
    
    for (g in 1:max_groups) {
      message(paste0("  Creating heatmap for Group ", g, "..."))
      
      tryCatch({
        suppressWarnings({
          heatmap_plot <- create_probability_heatmap(
            master_results_df = master_df,
            best_solution_summary = best_solutions,
            group_of_interest = g,
            title = paste0("Probability Correlations: Group ", g, " (", config_label, ")")
          )
        })
        
        heatmap_results[[paste0("group_", g)]] <- heatmap_plot
        
        # Save heatmap
        heatmap_file <- file.path(report_dir, paste0(config_label, "_probability_heatmap_group", g, ".png"))
        ggsave(heatmap_file, plot = heatmap_plot, width = 10, height = 8, dpi = 300)
        message(paste0("    ✓ Saved heatmap: ", basename(heatmap_file)))
        
      }, error = function(e) {
        message(paste0("    ✗ ERROR creating heatmap for group ", g, ": ", e$message))
        message("    Stack trace:")
        message(paste0("      ", capture.output(traceback()), collapse = "\n"))
      })
    }
  }
  
  # ============================================================================
  # Step 5: Latent Profile Analysis (if tidyLPA is available)
  # ============================================================================
  message("\n>>> Step 5: Running Latent Profile Analysis...")
  
  lpa_results_output <- NULL
  
  # Check if tidyLPA is available
  if (requireNamespace("tidyLPA", quietly = TRUE)) {
    message("  ✓ tidyLPA package is available")
    
    tryCatch({
      # Run LPA with warning capture
      lpa_warnings <- character(0)
      lpa_results <- withCallingHandlers(
        run_lpa_analysis(
          master_results_df = master_df,
          best_solution_summary = best_solutions,
          centering_filter = NULL,  # Already filtered by configuration
          n_profiles = 1:4
        ),
        warning = function(w) {
          lpa_warnings <<- c(lpa_warnings, w$message)
          invokeRestart("muffleWarning")
        }
      )
      
      # Report any warnings captured
      if (length(lpa_warnings) > 0) {
        message("  ⚠ LPA completed with warnings:")
        for (warn in unique(lpa_warnings)) {
          message(paste0("    - ", warn))
        }
      }
      
      # Save LPA fit statistics
      lpa_fit_file <- file.path(report_dir, paste0(config_label, "_lpa_fit_statistics.txt"))
      sink(lpa_fit_file)
      cat(capture.output(print(lpa_results)), sep = "\n")
      sink()
      message(paste0("  ✓ Saved LPA fit statistics: ", basename(lpa_fit_file)))
      
      # Determine best number of profiles (lowest BIC)
      fit_stats <- tidyLPA::get_fit(lpa_results)
      
      # Check if we have valid fit statistics
      if (is.null(fit_stats) || nrow(fit_stats) == 0) {
        message("  ⚠ WARNING: No valid LPA models were fitted. Skipping profile extraction.")
        lpa_results_output <- list(
          fit_results = lpa_results,
          best_n_profiles = NA,
          profiles = NULL,
          warnings = lpa_warnings
        )
      } else {
        best_n_profiles_str <- fit_stats$Model[which.min(fit_stats$BIC)]
        # Extract the first integer from the string robustly
        best_n_profiles <- suppressWarnings(as.numeric(regmatches(best_n_profiles_str, regexpr("\\d+", best_n_profiles_str))))
        
        if (is.na(best_n_profiles)) {
          message(paste0("  ⚠ WARNING: Could not extract number of profiles from string: '", best_n_profiles_str, "'. Skipping profile extraction."))
          lpa_results_output <- list(
            fit_results = lpa_results,
            best_n_profiles = NA,
            profiles = NULL,
            warnings = lpa_warnings
          )
        } else {
          message(paste0("  ✓ Best number of profiles: ", best_n_profiles, " (based on BIC)"))
          
          # Extract profiles for the best solution with error handling
          message(paste0("  Attempting to extract profiles for ", best_n_profiles, " profile(s)..."))
          profiles <- tryCatch({
            get_lpa_profiles(lpa_results, n_profiles = best_n_profiles)
          }, error = function(e) {
            message(paste0("  ✗ ERROR extracting LPA profiles: ", e$message))
            message("  Possible causes:")
            message("    - Model may not have converged successfully")
            message("    - tidyLPA version compatibility issue")
            message("    - Data structure mismatch")
            message("  Continuing without profile extraction.")
            return(NULL)
          })
          
          if (!is.null(profiles)) {
            # Save profiles
            profiles_file <- file.path(report_dir, paste0(config_label, "_lpa_profiles.csv"))
            write.csv(profiles, profiles_file, row.names = FALSE)
            message(paste0("  ✓ Saved LPA profiles: ", basename(profiles_file)))
            
            lpa_results_output <- list(
              fit_results = lpa_results,
              best_n_profiles = best_n_profiles,
              profiles = profiles,
              warnings = lpa_warnings
            )
          } else {
            message("  ⚠ WARNING: Could not extract profiles. Saving fit results only.")
            lpa_results_output <- list(
              fit_results = lpa_results,
              best_n_profiles = best_n_profiles,
              profiles = NULL,
              warnings = lpa_warnings
            )
          }
        }
      }
      
    }, error = function(e) {
      message(paste0("  ✗ ERROR running LPA: ", e$message))
      message("  Stack trace:")
      message(paste0("    ", capture.output(traceback()), collapse = "\n"))
    })
  } else {
    message("  ⚠ tidyLPA package not available. Skipping LPA analysis.")
    message("  To enable LPA, install with: install.packages('tidyLPA')")
  }
  
  # ============================================================================
  # Step 6: Create Summary Report and Labeled Pattern Summary
  # ============================================================================
  message("\n>>> Step 6: Creating summary report and labeled pattern summary...")
  
  # Create labeled pattern summary table
  pattern_summary <- tryCatch({
    create_labeled_pattern_summary(
      master_results_df = master_df,
      best_solution_summary = best_solutions,
      outcomes = outcome_names
    )
  }, error = function(e) {
    message(paste0("  ⚠️ Could not create pattern summary: ", e$message))
    NULL
  })
  
  # Save pattern summary if created
  if (!is.null(pattern_summary)) {
    pattern_summary_file <- file.path(report_dir, paste0(config_label, "_labeled_pattern_summary.csv"))
    write.csv(pattern_summary, pattern_summary_file, row.names = FALSE)
    message(paste0("  ✓ Saved labeled pattern summary: ", basename(pattern_summary_file)))
  }
  
  summary_file <- file.path(report_dir, paste0(config_label, "_summary_report.txt"))
  
  sink(summary_file)
  cat("================================================================================\n")
  cat("GROUP PROBABILITY COMPARISON REPORT\n")
  cat("================================================================================\n\n")
  cat("Configuration:", config_label, "\n")
  cat("Date:", date_folder, "\n")
  cat("Number of outcomes:", length(filtered_smm_results), "\n")
  cat("Outcomes:", paste(names(filtered_smm_results), collapse = ", "), "\n\n")
  
  cat("================================================================================\n")
  cat("BEST GROUP SOLUTIONS (by BIC)\n")
  cat("================================================================================\n\n")
  print(best_solutions)
  
  cat("\n================================================================================\n")
  cat("CATEGORICAL COMPARISONS\n")
  cat("================================================================================\n\n")
  
  if (length(categorical_results) > 0) {
    for (pair_name in names(categorical_results)) {
      cat("\n---", pair_name, "---\n")
      cat("Chi-squared statistic:", categorical_results[[pair_name]]$chi_test$statistic, "\n")
      cat("p-value:", categorical_results[[pair_name]]$chi_test$p.value, "\n")
      cat("Interpretation:", 
          if(categorical_results[[pair_name]]$chi_test$p.value < 0.05) 
            "Significant association (p < 0.05)" 
          else 
            "No significant association (p >= 0.05)", "\n")
    }
  } else {
    cat("No categorical comparisons performed.\n")
  }
  
  cat("\n================================================================================\n")
  cat("OUTPUT FILES\n")
  cat("================================================================================\n\n")
  cat("All results saved to:", report_dir, "\n\n")
  cat("Files generated:\n")
  cat("  - Consolidated results CSV\n")
  cat("  - Best solutions CSV\n")
  if (!is.null(pattern_summary)) {
    cat("  - Labeled pattern summary CSV (documents group labels across outcomes)\n")
  }
  cat("  - Contingency tables (", length(categorical_results), " pairs)\n")
  cat("  - Comprehensive labeled pattern correlation heatmap (NEW - correlates all labeled patterns)\n")
  cat("  - Individual group-number probability heatmaps (", length(heatmap_results) - 1, " groups, deprecated)\n")
  if (!is.null(lpa_results_output)) {
    cat("  - LPA profiles CSV\n")
    cat("  - LPA fit statistics\n")
  }
  cat("  - This summary report\n")
  cat("\n")
  cat("NOTE: The comprehensive labeled pattern heatmap shows correlations between\n")
  cat("ALL labeled patterns across outcomes (e.g., 'perimenstrual_CSS_Inatt' vs\n")
  cat("'stable_DRSP1'). This is the recommended visualization for understanding\n")
  cat("pattern relationships across symptoms.\n")
  
  if (!is.null(pattern_summary)) {
    cat("\n================================================================================\n")
    cat("LABELED PATTERN SUMMARY\n")
    cat("================================================================================\n\n")
    print(pattern_summary)
    cat("\nThis table documents the meaningful labels assigned to each group pattern.\n")
    cat("Labels make cross-outcome comparisons more interpretable.\n")
  }
  
  sink()
  
  message(paste0("  ✓ Saved summary report: ", basename(summary_file)))
  
  # ============================================================================
  # Final Summary
  # ============================================================================
  message("\n================================================================================")
  message("GROUP PROBABILITY COMPARISONS COMPLETE!")
  message(paste0("Configuration: ", config_label))
  message(paste0("All results saved to: ", report_dir))
  message("================================================================================\n")
  
  return(list(
    config = config_label,
    report_dir = report_dir,
    status = "success",
    master_df = master_df,
    best_solutions = best_solutions,
    pattern_summary = pattern_summary,
    categorical_results = categorical_results,
    heatmap_results = heatmap_results,
    lpa_results = lpa_results_output,
    summary_file = summary_file
  ))
}
