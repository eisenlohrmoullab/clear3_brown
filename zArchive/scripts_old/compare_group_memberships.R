## ============================================================================
## GROUP MEMBERSHIP COMPARISON FUNCTIONS
## ============================================================================
## This script provides three complementary methods for comparing group
## memberships across different outcomes after running SMM analyses:
##
## 1. Categorical Comparison - Chi-squared test of group overlap
## 2. Probabilistic Correlation - Heatmap of probability patterns
## 3. Latent Profile Analysis - Meta-clustering of symptom profiles
##
## Usage: source("scripts/compare_group_memberships.R")
##        Then call functions as shown in docs/GROUP_PROBABILITIES_GUIDE.md
## ============================================================================

library(tidyverse)  # Includes dplyr, ggplot2, tidyr

## ============================================================================
## FUNCTION 1: Consolidate Results
## ============================================================================
## Merges results from multiple SMM analyses into a single wide-format
## dataframe. Each row = one participant, columns = group assignments and
## probabilities for all outcomes/group solutions.
##
## Example output columns:
##   id, CSS_Inatt_g2_group, CSS_Inatt_g2_prob_group1, CSS_Inatt_g2_prob_group2, ...
##
## If group labels are available, also includes labeled group columns:
##   id, CSS_Inatt_g2_group, CSS_Inatt_g2_group_label, CSS_Inatt_g2_prob_group1, ...
## ============================================================================

#' Consolidate all SMM results into a single master dataframe
#' 
#' @param all_smm_results List of SMM results from multiple analyses
#' @param base_save_dir Optional: Base directory where labels are saved (for loading group labels)
#' @param date_folder Optional: Date folder for loading labels (default: today)
#' @param load_labels Logical: whether to load and apply group labels if available (default: TRUE)
#' @return A dataframe with columns: id, and for each analysis: group assignments and probabilities
#' @export
consolidate_smm_results <- function(all_smm_results, base_save_dir = NULL, 
                                    date_folder = format(Sys.Date(), "%Y%m%d"),
                                    load_labels = TRUE) {
  
  # Extract all unique participant IDs across all analyses
  all_ids <- unique(unlist(lapply(all_smm_results, function(res) {
    if ("all_results" %in% names(res)) {
      # Multi-g result (from run_smm_cyclic with g = 2:5)
      res$all_results[[1]]$class$id
    } else {
      # Single-g result (from run_smm_cyclic with g = 2)
      res$class$id
    }
  })))
  
  # Initialize master dataframe with participant IDs
  master_results_df <- data.frame(id = all_ids)
  
  # Track whether we're loading labels
  labels_loaded <- FALSE
  
  # Check if we should attempt to load labels
  if (load_labels && !is.null(base_save_dir)) {
    # Try to source the label loading functions
    if (file.exists("scripts/label_smm_groups.R")) {
      tryCatch({
        source("scripts/label_smm_groups.R", local = TRUE)
        labels_loaded <- TRUE
        message("  ℹ️ Group label functions available - will load labels if they exist")
      }, error = function(e) {
        message(paste0("  ⚠️ Could not load label functions: ", e$message))
      })
    }
  }
  
  # Loop through each analysis result
  for (result_name in names(all_smm_results)) {
    
    # Get the results for the current outcome/centering combo
    smm_result <- all_smm_results[[result_name]]
    
    # Parse result_name to extract outcome, centering, and winsorized info
    # Expected format: "outcome_centering_winsorized" or "outcome_centering_unwinsorized"
    # Use regex to robustly extract components
    # Example: "CSS_Inatt_menses_winsorized"
    # Pattern matches: (outcome name with any characters including underscores)_(menses|ovulation)_(winsorized|unwinsorized) at end
    re <- "^(.+)_(menses|ovulation)_(winsorized|unwinsorized)$"
    matches <- regexec(re, result_name)
    match <- regmatches(result_name, matches)[[1]]
    if (length(match) == 4) {
      outcome <- match[2]
      centering <- match[3]
      winsorized <- (match[4] == "winsorized")
    } else {
      # Fallback to previous logic - define name_parts FIRST before using it
      name_parts <- strsplit(result_name, "_")[[1]]
      warning(paste0("  ⚠️ result_name format unexpected: ", result_name, 
                     ". Using defaults: outcome = '", name_parts[1], 
                     "', centering = '", if(any(grepl("menses|ovulation", name_parts))) {
                       name_parts[grepl("menses|ovulation", name_parts)][1]
                     } else {
                       "menses"
                     }, 
                     "', winsorized = ", any(grepl("winsorized", name_parts)), "."))
      outcome <- name_parts[1]
      centering <- if(any(grepl("menses|ovulation", name_parts))) {
        name_parts[grepl("menses|ovulation", name_parts)][1]
      } else {
        "menses"  # default
      }
      winsorized <- any(grepl("winsorized", name_parts))
    }
    # Check if this is a multi-g result
    if ("all_results" %in% names(smm_result)) {
      # Loop through each group solution (g=2, g=3, etc.)
      for (g_solution in names(smm_result$all_results)) {
        
        # Extract the class assignments and probabilities
        class_df <- smm_result$all_results[[g_solution]]$class
        # Note: class_df already contains probability columns (prob_group1, prob_group2, etc.)
        # from the cbind operation in run_SMM_cyclic.R, so we don't need to join with probabilities
        
        # Try to load group labels if available
        if (labels_loaded && exists("load_group_labels", mode = "function")) {
          group_labels <- tryCatch({
            load_group_labels(
              base_save_dir = base_save_dir,
              outcome = outcome,
              centering = centering,
              winsorized = winsorized,
              g = as.numeric(g_solution),
              date_folder = date_folder
            )
          }, error = function(e) {
            NULL
          })
          
          # Apply labels if found
          if (!is.null(group_labels) && exists("apply_group_labels", mode = "function")) {
            class_df <- tryCatch({
              apply_group_labels(class_df, group_labels)
            }, error = function(e) {
              message(paste0("  ⚠️ Could not apply labels for ", result_name, " g=", g_solution, ": ", e$message))
              class_df
            })
          }
        }
        
        # Create descriptive column names
        # Example: "CSS_Inatt_menses_g2_group" or "CSS_Inatt_menses_g2_prob_group1"
        base_col_name <- paste0(result_name, "_g", g_solution)
        names(class_df)[-1] <- paste0(base_col_name, "_", names(class_df)[-1])
        
        # Merge into the master dataframe
        master_results_df <- left_join(master_results_df, class_df, by = "id")
      }
    } else {
      # Single-g result
      class_df <- smm_result$class
      # Note: class_df already contains probability columns (prob_group1, prob_group2, etc.)
      # from the cbind operation in run_SMM_cyclic.R, so we don't need to join with probabilities
      
      # For single-g results, try to infer g from the probability columns
      prob_cols <- grep("^prob_group", names(class_df), value = TRUE)
      g <- length(prob_cols)
      
      # Try to load group labels if available
      if (labels_loaded && exists("load_group_labels", mode = "function") && g > 0) {
        group_labels <- tryCatch({
          load_group_labels(
            base_save_dir = base_save_dir,
            outcome = outcome,
            centering = centering,
            winsorized = winsorized,
            g = g,
            date_folder = date_folder
          )
        }, error = function(e) {
          NULL
        })
        
        # Apply labels if found
        if (!is.null(group_labels) && exists("apply_group_labels", mode = "function")) {
          class_df <- tryCatch({
            apply_group_labels(class_df, group_labels)
          }, error = function(e) {
            message(paste0("  ⚠️ Could not apply labels for ", result_name, ": ", e$message))
            class_df
          })
        }
      }
      
      # Create descriptive column names
      base_col_name <- result_name
      names(class_df)[-1] <- paste0(base_col_name, "_", names(class_df)[-1])
      
      # Merge into the master dataframe
      master_results_df <- left_join(master_results_df, class_df, by = "id")
    }
  }
  
  message("✅ Master results dataframe created with all group assignments and probabilities.")
  if (labels_loaded && !is.null(base_save_dir)) {
    # Check if any label columns were added
    label_cols <- grep("_group_label$", names(master_results_df), value = TRUE)
    if (length(label_cols) > 0) {
      message(paste0("  ✓ Applied group labels for ", length(label_cols), " analysis/analyses"))
    } else {
      message("  ℹ️ No group labels found - using numeric group numbers")
    }
  }
  return(master_results_df)
}


## ============================================================================
## FUNCTION 2: Identify Best Solutions
## ============================================================================
## For each analysis, finds the group solution (g=2, g=3, etc.) with the
## lowest BIC (Bayesian Information Criterion). This tells you the optimal
## number of groups for each outcome.
## ============================================================================

#' Determine the best group solution for each outcome based on BIC
#' 
#' @param all_bic_results List of BIC results from multiple analyses
#' @param manual_selections Optional dataframe with manual selections (columns: analysis, groups)
#' @return A dataframe with the best solution for each analysis
#' @export
get_best_solutions <- function(all_bic_results, manual_selections = NULL) {
  
  # Compile BIC values across all analyses
  bic_summary <- do.call(rbind, lapply(names(all_bic_results), function(name) {
    data.frame(
      analysis = name,
      groups = all_bic_results[[name]]$bic_table$groups,
      BIC = all_bic_results[[name]]$bic_table$BIC
    )
  }))
  
  # For each analysis, find the solution with lowest BIC
  best_solution_summary <- bic_summary %>%
    group_by(analysis) %>%
    slice_min(order_by = BIC, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # Apply manual selections if provided
  if (!is.null(manual_selections)) {
    message("  ℹ️ Applying manual group selections...")
    
    for (i in 1:nrow(manual_selections)) {
      analysis_name <- manual_selections$analysis[i]
      selected_g <- manual_selections$groups[i]
      
      # Check if this analysis exists
      if (analysis_name %in% best_solution_summary$analysis) {
        old_g <- best_solution_summary$groups[best_solution_summary$analysis == analysis_name]
        
        # Update the selection
        best_solution_summary$groups[best_solution_summary$analysis == analysis_name] <- selected_g
        
        # Update BIC value for the selected g
        selected_bic <- bic_summary$BIC[bic_summary$analysis == analysis_name & bic_summary$groups == selected_g]
        if (length(selected_bic) > 0) {
          best_solution_summary$BIC[best_solution_summary$analysis == analysis_name] <- selected_bic[1]
        }
        
        message(paste0("    ✓ ", analysis_name, ": overriding g=", old_g, " (BIC-based) with g=", selected_g, " (manual)"))
      } else {
        message(paste0("    ⚠️ Analysis '", analysis_name, "' not found. Skipping."))
      }
    }
  }
  
  message("--- Final Group Solution Selection ---")
  print(as.data.frame(best_solution_summary))
  message("--------------------------------------")
  
  return(best_solution_summary)
}


## ============================================================================
## FUNCTION 2b: Interactive Selection Helper
## ============================================================================
## Helps users interactively select their preferred group solutions
## ============================================================================

#' Interactively select group solutions for each outcome
#' 
#' @param all_bic_results List of BIC results from multiple analyses
#' @param auto_best Logical: if TRUE, starts with BIC-based best solutions
#' @return A dataframe with columns: analysis, groups (for use with get_best_solutions)
#' @export
select_final_groupings <- function(all_bic_results, auto_best = TRUE) {
  
  message("\n==============================================================================")
  message("INTERACTIVE GROUP SOLUTION SELECTION")
  message("==============================================================================\n")
  
  # Get BIC-based recommendations
  if (auto_best) {
    auto_selections <- get_best_solutions(all_bic_results)
    message("\nBIC-based recommendations shown above.")
    message("You can accept these or override with your preferred grouping.\n")
  }
  
  # Compile all available solutions
  all_analyses <- names(all_bic_results)
  selections <- data.frame(analysis = character(), groups = numeric(), stringsAsFactors = FALSE)
  
  for (analysis_name in all_analyses) {
    available_g <- all_bic_results[[analysis_name]]$bic_table$groups
    bic_values <- all_bic_results[[analysis_name]]$bic_table$BIC
    
    message(paste0("--- ", analysis_name, " ---"))
    message("Available group solutions:")
    for (i in 1:length(available_g)) {
      is_best <- if (auto_best) {
        available_g[i] == auto_selections$groups[auto_selections$analysis == analysis_name]
      } else {
        FALSE
      }
      marker <- if (is_best) " ← BIC best" else ""
      message(paste0("  g=", available_g[i], " (BIC=", round(bic_values[i], 2), ")", marker))
    }
    
    # Prompt for selection
    if (interactive()) {
      default_g <- if (auto_best) {
        auto_selections$groups[auto_selections$analysis == analysis_name]
      } else {
        available_g[1]
      }
      
      selected_g <- readline(prompt = paste0("Select g for ", analysis_name, " (default=", default_g, "): "))
      
      if (selected_g == "" || is.na(as.numeric(selected_g))) {
        selected_g <- default_g
      } else {
        selected_g <- as.numeric(selected_g)
      }
      
      # Validate selection
      if (!(selected_g %in% available_g)) {
        message(paste0("  ⚠️ Invalid selection. Using default g=", default_g))
        selected_g <- default_g
      }
      
      message(paste0("  ✓ Selected: g=", selected_g, "\n"))
    } else {
      # Non-interactive mode: use BIC best
      selected_g <- if (auto_best) {
        auto_selections$groups[auto_selections$analysis == analysis_name]
      } else {
        available_g[which.min(bic_values)]
      }
      message(paste0("  (Non-interactive mode: using g=", selected_g, ")\n"))
    }
    
    selections <- rbind(selections, data.frame(analysis = analysis_name, groups = selected_g))
  }
  
  message("==============================================================================")
  message("SELECTION COMPLETE")
  message("==============================================================================\n")
  message("Summary of selections:")
  print(selections)
  message("\nTo use these selections, pass this dataframe to:")
  message("  get_best_solutions(all_bic_results, manual_selections = your_selections)")
  
  return(selections)
}


## ============================================================================
## FUNCTION 3: Categorical Comparison (Chi-squared Test)
## ============================================================================
## Tests whether group memberships in two outcomes are related using a
## chi-squared test. Useful for asking: "Do people in Group 1 for symptom A
## tend to also be in Group 1 for symptom B?"
## ============================================================================

#' Compare categorical group assignments between two symptoms using Chi-squared test
#' 
#' @param master_results_df Master dataframe with all results
#' @param best_solution_summary Dataframe with best solutions
#' @param symptom1 Name of first symptom/outcome
#' @param symptom2 Name of second symptom/outcome
#' @param centering Centering method ("menses" or "ovulation")
#' @param winsorized Logical: whether data was winsorized (default: TRUE). Must match the naming convention used in master_results_df and best_solution_summary.
#' @param use_labels Logical: use group labels instead of numeric groups if available (default: TRUE)
#' @export
compare_categorical_groups <- function(master_results_df, best_solution_summary, 
                                       symptom1, symptom2, centering, winsorized = TRUE, use_labels = TRUE) {
  
  # Find the best 'g' for each symptom from the summary table
  # Construct analysis names based on actual naming convention: outcome_centering_winsorized/unwinsorized
  winsorized_label <- if (winsorized) "winsorized" else "unwinsorized"
  analysis_name1 <- paste0(symptom1, "_", centering, "_", winsorized_label)
  analysis_name2 <- paste0(symptom2, "_", centering, "_", winsorized_label)
  
  g1 <- best_solution_summary$groups[best_solution_summary$analysis == analysis_name1]
  g2 <- best_solution_summary$groups[best_solution_summary$analysis == analysis_name2]
  
  if (length(g1) == 0 || length(g2) == 0) {
    stop("Could not find best solution for one or both symptoms. Check names.")
  }
  
  # Construct the column names - first try with labels if requested
  col1 <- paste0(analysis_name1, "_g", g1, "_group")
  col2 <- paste0(analysis_name2, "_g", g2, "_group")
  col1_label <- paste0(col1, "_label")
  col2_label <- paste0(col2, "_label")
  
  # Use labeled columns if available and requested
  use_col1 <- col1
  use_col2 <- col2
  if (use_labels) {
    if (col1_label %in% names(master_results_df)) {
      use_col1 <- col1_label
      message(paste0("  ✓ Using labeled groups for ", symptom1))
    }
    if (col2_label %in% names(master_results_df)) {
      use_col2 <- col2_label
      message(paste0("  ✓ Using labeled groups for ", symptom2))
    }
  }
  
  # Create the contingency table
  contingency_table <- table(master_results_df[[use_col1]], master_results_df[[use_col2]],
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
        warning(warning_msg)
        cat("⚠️  WARNING:", warning_msg, "\n")
      }
      
      test_result
    })
  }, warning = function(w) {
    cat("⚠️  Chi-squared test warning:", w$message, "\n")
    suppressWarnings(chisq.test(contingency_table))
  }, error = function(e) {
    cat("❌ ERROR in chi-squared test:", e$message, "\n")
    stop(paste("Chi-squared test failed:", e$message))
  })
  
  # Print results
  cat("\n--- Categorical Comparison: '", symptom1, "' vs '", symptom2, "' ---\n")
  print(contingency_table)
  cat("\n")
  print(chi_test)
  cat("-----------------------------------------------------------\n")
  
  return(list(contingency_table = contingency_table, chi_test = chi_test))
}


#' Create correlation heatmap of probabilities for ALL labeled patterns
#' 
#' This function creates a comprehensive correlation heatmap showing how
#' probabilities of being in each labeled pattern correlate across outcomes.
#' For example: "perimenstrual_CSS_Inatt" vs "stable_DRSP1" vs "luteal_E2", etc.
#' 
#' @param master_results_df Master dataframe with all results (must include label columns)
#' @param best_solution_summary Dataframe with best solutions
#' @param title Title for the heatmap
#' @param use_labels Logical: if TRUE (default), use labeled pattern names; if FALSE, use numeric groups
#' @return ggplot object with the comprehensive heatmap
#' @export
create_labeled_pattern_correlation_heatmap <- function(master_results_df, best_solution_summary, 
                                                       title = NULL, use_labels = TRUE) {
  
  # Build list of all probability columns with their corresponding labels
  prob_cols_list <- list()
  
  for (analysis_name in best_solution_summary$analysis) {
    g <- best_solution_summary$groups[best_solution_summary$analysis == analysis_name]
    
    # Get all probability columns for this analysis's best solution
    for (group_num in 1:g) {
      prob_col <- paste0(analysis_name, "_g", g, "_prob_group", group_num)
      
      # Check if this probability column exists
      if (prob_col %in% names(master_results_df)) {
        
        # Try to get the label for this group
        label_col <- paste0(analysis_name, "_g", g, "_group_label")
        group_col <- paste0(analysis_name, "_g", g, "_group")
        
        # Determine the pattern name
        if (use_labels && label_col %in% names(master_results_df)) {
          # Get the label for this group number
          # Find what label corresponds to this group number
          group_label_mapping <- master_results_df %>%
            filter(!is.na(!!sym(group_col)) & !!sym(group_col) == group_num) %>%
            select(all_of(c(group_col, label_col))) %>%
            distinct()
          
          if (nrow(group_label_mapping) > 0) {
            pattern_name <- paste0(analysis_name, "_", group_label_mapping[[label_col]][1])
          } else {
            # Fallback to numeric if no label found
            pattern_name <- paste0(analysis_name, "_group", group_num)
          }
        } else {
          # Use numeric group name
          pattern_name <- paste0(analysis_name, "_group", group_num)
        }
        
        prob_cols_list[[pattern_name]] <- prob_col
      }
    }
  }
  
  # Check if we have enough patterns to correlate
  if (length(prob_cols_list) < 2) {
    stop("Need at least 2 labeled patterns to create correlation heatmap. Only found ", 
         length(prob_cols_list), " pattern(s).")
  }
  
  message(paste0("  ℹ️ Creating correlation heatmap for ", length(prob_cols_list), " labeled patterns"))
  
  # Extract probability columns with new names
  prob_df_for_corr <- master_results_df %>%
    select(id, all_of(unname(unlist(prob_cols_list))))
  
  # Rename columns to pattern names
  names(prob_df_for_corr) <- c("id", names(prob_cols_list))
  
  # Create the Correlation Matrix
  corr_matrix <- cor(prob_df_for_corr[-1], use = "pairwise.complete.obs")
  
  # Check if correlation matrix is valid
  if (is.null(corr_matrix) || length(dim(corr_matrix)) < 2 || any(dim(corr_matrix) == 0)) {
    stop("Failed to create valid correlation matrix. Check for sufficient non-missing data.")
  }
  
  # Convert correlation matrix to long format for ggplot
  corr_df <- as.data.frame(as.table(corr_matrix))
  names(corr_df) <- c("Pattern1", "Pattern2", "Correlation")
  
  # Final check that we have data to plot
  if (nrow(corr_df) == 0) {
    stop("Correlation dataframe is empty. Cannot create heatmap.")
  }
  
  # Create the heatmap using ggplot
  if (is.null(title)) {
    title <- "Correlation of Labeled Pattern Probabilities Across Outcomes"
  }
  
  p <- ggplot(corr_df, aes(x = Pattern1, y = Pattern2, fill = Correlation)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", Correlation)), size = 2.5) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0, limit = c(-1, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8)) +
    labs(title = title, x = "", y = "") +
    coord_fixed()
  
  return(p)
}


#' Create correlation heatmap of probabilities for a specific group pattern (DEPRECATED)
#' 
#' @deprecated Use create_labeled_pattern_correlation_heatmap() instead for better labeling support
#' 
#' This function is maintained for backward compatibility but creates heatmaps
#' by group NUMBER rather than by labeled pattern, which can be misleading.
#' 
#' @param master_results_df Master dataframe with all results
#' @param best_solution_summary Dataframe with best solutions
#' @param group_of_interest Which group number to analyze (e.g., 1 for "cyclical worsening")
#' @param title Title for the heatmap
#' @return ggplot object with the heatmap
#' @export
create_probability_heatmap <- function(master_results_df, best_solution_summary, 
                                       group_of_interest = 1, title = NULL) {
  
  warning("create_probability_heatmap() is deprecated. Use create_labeled_pattern_correlation_heatmap() instead for better labeling support.")
  
  # Select only the probability columns for the group of interest from the best solution
  prob_df_for_corr <- master_results_df %>%
    select(id, ends_with(paste0("_prob_group", group_of_interest)))
  
  # Check if we have any probability columns
  if (ncol(prob_df_for_corr) <= 1) {
    stop(paste0("No probability columns found for group ", group_of_interest, 
                ". Check that master_results_df contains columns ending with '_prob_group", 
                group_of_interest, "'"))
  }
  
  # Filter to include only the columns from the BEST solution for each analysis
  best_prob_cols <- sapply(best_solution_summary$analysis, function(analysis_name) {
    g <- best_solution_summary$groups[best_solution_summary$analysis == analysis_name]
    paste0(analysis_name, "_g", g, "_prob_group", group_of_interest)
  })
  
  # Keep only columns that exist in the dataframe
  best_prob_cols <- best_prob_cols[best_prob_cols %in% names(prob_df_for_corr)]
  
  # Check if any best solution columns were found
  if (length(best_prob_cols) == 0) {
    stop(paste0("No probability columns found matching best solutions for group ", 
                group_of_interest, ". Available columns: ", 
                paste(names(prob_df_for_corr)[-1], collapse = ", ")))
  }
  
  prob_df_for_corr <- prob_df_for_corr %>%
    select(id, all_of(best_prob_cols))
  
  # Verify we still have data after selection
  if (ncol(prob_df_for_corr) <= 1) {
    stop(paste0("After filtering, no probability columns remain for group ", 
                group_of_interest))
  }
  
  # Check if we have at least 2 columns for correlation (need at least 2 variables)
  if (ncol(prob_df_for_corr) < 3) {  # id + at least 2 probability columns
    stop(paste0("Need at least 2 outcomes to create correlation heatmap. Only found ", 
                ncol(prob_df_for_corr) - 1, " outcome(s) with group ", group_of_interest))
  }
  
  # Clean up column names for the plot
  clean_names <- gsub(paste0("_g._prob_group", group_of_interest), "", names(prob_df_for_corr)[-1])
  names(prob_df_for_corr) <- c("id", clean_names)
  
  # Create the Correlation Matrix
  corr_matrix <- cor(prob_df_for_corr[-1], use = "pairwise.complete.obs")
  
  # Check if correlation matrix is valid
  if (is.null(corr_matrix) || length(dim(corr_matrix)) < 2 || any(dim(corr_matrix) == 0)) {
    stop("Failed to create valid correlation matrix. Check for sufficient non-missing data.")
  }
  
  # Convert correlation matrix to long format for ggplot
  corr_df <- as.data.frame(as.table(corr_matrix))
  names(corr_df) <- c("Var1", "Var2", "Correlation")
  
  # Final check that we have data to plot
  if (nrow(corr_df) == 0) {
    stop("Correlation dataframe is empty. Cannot create heatmap.")
  }
  
  # Create the heatmap using ggplot
  if (is.null(title)) {
    title <- paste0("Correlation of Probabilities for Group ", group_of_interest)
  }
  
  p <- ggplot(corr_df, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0, limit = c(-1, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title, x = "", y = "") +
    coord_fixed()
  
  return(p)
}


#' Run Latent Profile Analysis on group memberships
#' 
#' This requires the tidyLPA package to be installed
#' 
#' @param master_results_df Master dataframe with all results
#' @param best_solution_summary Dataframe with best solutions
#' @param centering_filter Optional filter for centering method (e.g., "_menses")
#' @param n_profiles Vector of profile numbers to test (e.g., 1:4)
#' @return tidyLPA results object
#' @export
run_lpa_analysis <- function(master_results_df, best_solution_summary, 
                             centering_filter = NULL, n_profiles = 1:4) {
  
  # Check if tidyLPA is installed
  if (!requireNamespace("tidyLPA", quietly = TRUE)) {
    stop("Package 'tidyLPA' is required for this function. Install it with: install.packages('tidyLPA')")
  }
  
  # Select only the categorical group columns from the best solution for each analysis
  lpa_data <- master_results_df
  
  best_group_cols <- sapply(best_solution_summary$analysis, function(analysis_name) {
    g <- best_solution_summary$groups[best_solution_summary$analysis == analysis_name]
    paste0(analysis_name, "_g", g, "_group")
  })
  
  lpa_data <- lpa_data %>%
    select(id, all_of(best_group_cols)) %>%
    # tidyLPA requires all variables to be numeric
    mutate(across(-id, as.numeric))
  
  # Clean up column names for easier interpretation
  names(lpa_data)[-1] <- gsub("_g._group", "", names(lpa_data)[-1])
  
  # Apply centering filter if specified
  if (!is.null(centering_filter)) {
    filtered_cols <- grep(centering_filter, names(lpa_data), value = TRUE)
    lpa_data <- lpa_data %>% select(id, all_of(filtered_cols))
  }
  
  # Remove rows with missing data (mclust/tidyLPA doesn't handle NA values)
  n_rows_before <- nrow(lpa_data)
  lpa_data_complete <- lpa_data %>%
    select(-id) %>%
    na.omit()
  n_rows_after <- nrow(lpa_data_complete)
  
  if (n_rows_after < n_rows_before) {
    message(paste0("  ⚠ Removed ", n_rows_before - n_rows_after, " rows with missing data (", 
                   round(100 * (n_rows_before - n_rows_after) / n_rows_before, 1), "%)"))
    message(paste0("  Proceeding with ", n_rows_after, " complete cases for LPA"))
  }
  
  if (n_rows_after < 10) {
    stop(paste0("Insufficient data for LPA: only ", n_rows_after, 
                " complete cases available. Need at least 10 observations."))
  }
  
  message(paste("Running LPA with", ncol(lpa_data_complete), "variables and", 
                n_rows_after, "observations..."))
  message(paste("Testing", length(n_profiles), "different numbers of profiles:", 
                paste(n_profiles, collapse = ", ")))
  
  # Run the LPA with warning suppression for known mclust issues
  lpa_results <- suppressWarnings({
    lpa_data_complete %>%
      tidyLPA::estimate_profiles(n_profiles = n_profiles)
  })
  
  # Diagnostic: Check the structure of returned object
  message("\n--- LPA Results Structure ---")
  message(paste("Object class:", paste(class(lpa_results), collapse = ", ")))
  message(paste("Number of models fitted:", length(lpa_results)))
  if (is.list(lpa_results) && length(lpa_results) > 0) {
    message(paste("Model names:", paste(names(lpa_results)[1:min(3, length(lpa_results))], collapse = ", "), 
                 if(length(lpa_results) > 3) "..." else ""))
  }
  message("-----------------------------")
  
  message("\n--- LPA Fit Statistics ---")
  print(lpa_results)
  message("Look for the model with the lowest BIC and high Entropy (>0.8 is good)")
  
  return(lpa_results)
}


#' Extract profile assignments from LPA results
#' 
#' @param lpa_results Results from run_lpa_analysis
#' @param n_profiles Number of profiles to extract
#' @return Dataframe with profile assignments
#' @export
get_lpa_profiles <- function(lpa_results, n_profiles) {
  
  # Check if tidyLPA is installed
  if (!requireNamespace("tidyLPA", quietly = TRUE)) {
    stop("Package 'tidyLPA' is required for this function. Install it with: install.packages('tidyLPA')")
  }
  
  # Method 1: Try tidyLPA::get_data() with which_models parameter
  # Force evaluation to avoid NSE issues
  lpa_results_local <- lpa_results
  n_profiles_local <- n_profiles
  
  final_profiles <- tryCatch({
    tidyLPA::get_data(lpa_results_local, which_models = n_profiles_local)
  }, error = function(e1) {
    message(paste0("  ℹ tidyLPA::get_data() with which_models failed: ", e1$message))
    
    # Method 2: Try the old parameter name for backward compatibility
    tryCatch({
      tidyLPA::get_data(lpa_results_local, n_profiles = n_profiles_local)
    }, error = function(e2) {
      message(paste0("  ℹ tidyLPA::get_data() with n_profiles failed: ", e2$message))
      
      # Method 3: Direct extraction from tidyLPA object structure
      tryCatch({
        message("  ℹ Attempting direct extraction from model object...")
        
        # Check if lpa_results is a valid tidyLPA object
        if (!inherits(lpa_results, "tidyLPA") && !is.list(lpa_results)) {
          stop("lpa_results is not a valid tidyLPA object or list")
        }
        
        # Get model names
        model_names <- names(lpa_results)
        if (is.null(model_names) || length(model_names) == 0) {
          stop("lpa_results does not contain named models")
        }
        
        # Find the model for the requested number of profiles
        # Pattern: "model_X_class_N" where N is the number of profiles
        # Match class_N at end of string or followed by underscore (for different model types)
        target_pattern <- paste0("class_", n_profiles, "($|_)")
        matching_models <- grep(target_pattern, model_names, value = TRUE, perl = TRUE)
        
        if (length(matching_models) == 0) {
          stop(paste0("No model found for ", n_profiles, " profiles. ",
                     "Available models: ", paste(model_names, collapse = ", ")))
        }
        
        # Use the first matching model
        target_model_name <- matching_models[1]
        message(paste0("  ℹ Found model: ", target_model_name))
        
        # Extract the model object
        model_obj <- lpa_results[[target_model_name]]
        
        # Check if model has classification data
        if (is.null(model_obj$classification)) {
          stop("Model object does not contain classification data. Model may have failed to converge.")
        }
        
        # Extract the original data from the model
        # The model should have the data stored in its structure
        if (!is.null(model_obj$data)) {
          result_df <- as.data.frame(model_obj$data)
        } else if (!is.null(attr(lpa_results, "data"))) {
          result_df <- as.data.frame(attr(lpa_results, "data"))
        } else {
          # Last resort: try to get data dimensions from classification
          n_obs <- length(model_obj$classification)
          n_vars <- model_obj$d
          if (is.null(n_vars)) n_vars <- ncol(model_obj$data)
          
          # Create minimal data frame with just the classifications
          message("  ⚠ Could not extract original variable values, returning classifications only")
          result_df <- data.frame(observation = 1:n_obs)
        }
        
        # Add classification as Class column
        result_df$Class <- factor(model_obj$classification)
        result_df$Model <- target_model_name
        
        message("  ✓ Successfully extracted profiles via direct model access")
        return(result_df)
        
      }, error = function(e3) {
        # All methods failed - provide comprehensive error message
        stop(paste0("Failed to extract LPA profiles using all available methods:\n",
                   "  Method 1 (get_data with which_models): ", e1$message, "\n",
                   "  Method 2 (get_data with n_profiles): ", e2$message, "\n",
                   "  Method 3 (direct model access): ", e3$message, "\n",
                   "Check that tidyLPA is properly installed and the model converged successfully."))
      })
    })
  })
  
  # Display profile counts if we have classifications
  if ("Class" %in% names(final_profiles)) {
    cat("\n--- LPA Profile Counts ---\n")
    print(table(final_profiles$Class))
    cat("--------------------------\n")
  }
  
  return(final_profiles)
}


# Example usage documentation
#' @examples
#' \dontrun{
#' # Assume you have run SMM analyses and stored results in all_smm_results
#' # and BIC results in all_bic_results
#' 
#' # Step 1: Consolidate results (with labels if available)
#' master_df <- consolidate_smm_results(
#'   all_smm_results, 
#'   base_save_dir = "~/output/MYPROJECT",
#'   load_labels = TRUE
#' )
#' 
#' # Step 2: Get best solutions (or use manual selections)
#' # Option 2a: Automatic BIC-based selection
#' best_solutions <- get_best_solutions(all_bic_results)
#' 
#' # Option 2b: Interactive selection
#' my_selections <- select_final_groupings(all_bic_results, auto_best = TRUE)
#' best_solutions <- get_best_solutions(all_bic_results, manual_selections = my_selections)
#' 
#' # Option 2c: Manual specification
#' my_selections <- data.frame(
#'   analysis = c("CSS_Inatt", "E2", "P4"),
#'   groups = c(3, 2, 4),  # Specific g values you want to use
#'   stringsAsFactors = FALSE
#' )
#' best_solutions <- get_best_solutions(all_bic_results, manual_selections = my_selections)
#' 
#' # Step 3: Compare two symptoms categorically (will use labels if available)
#' compare_categorical_groups(master_df, best_solutions, 
#'                            "CSS_Inatt", "CSS_Hyper", 
#'                            centering = "menses",
#'                            winsorized = TRUE,  # Specify if data is winsorized
#'                            use_labels = TRUE)
#' 
#' # Step 4: Create comprehensive labeled pattern correlation heatmap (RECOMMENDED)
#' # This correlates ALL labeled patterns across outcomes
#' # e.g., "perimenstrual_CSS_Inatt" vs "stable_DRSP1" vs "luteal_E2"
#' p_comprehensive <- create_labeled_pattern_correlation_heatmap(
#'   master_df, 
#'   best_solutions,
#'   use_labels = TRUE
#' )
#' print(p_comprehensive)
#' ggsave("labeled_pattern_correlation_heatmap.png", p_comprehensive, 
#'        width = 14, height = 12)
#' 
#' # Step 4b: (DEPRECATED) Create heatmap for a specific group number
#' # This approach is less meaningful because group numbers are arbitrary
#' p_old <- create_probability_heatmap(master_df, best_solutions, 
#'                                     group_of_interest = 1)
#' 
#' # Step 5: Run Latent Profile Analysis
#' # For menses-centered results only
#' lpa_results <- run_lpa_analysis(master_df, best_solutions, 
#'                                 centering_filter = "_menses",
#'                                 n_profiles = 1:4)
#' 
#' # Extract profiles (assuming 3 profiles is best based on fit stats)
#' profiles <- get_lpa_profiles(lpa_results, n_profiles = 3)
#' 
#' # Step 6: Create summary table of labeled patterns
#' summary_table <- create_labeled_pattern_summary(
#'   master_df, 
#'   best_solutions,
#'   outcomes = c("CSS_Inatt", "CSS_Hyper", "E2", "P4")
#' )
#' }


## ============================================================================
## FUNCTION 7: Create Labeled Pattern Summary Table
## ============================================================================
## Creates a summary table documenting labeled patterns across outcomes.
## Useful for publications and sharing with collaborators.
##
## @param master_results_df Master dataframe with all results
## @param best_solution_summary Dataframe with best solutions
## @param outcomes Vector of outcome names to include in summary
## @return Dataframe with pattern summary
## @export
create_labeled_pattern_summary <- function(master_results_df, best_solution_summary, 
                                           outcomes = NULL) {
  
  # If outcomes not specified, use all from best_solutions
  if (is.null(outcomes)) {
    outcomes <- best_solution_summary$analysis
  }
  
  summary_rows <- list()
  
  for (outcome in outcomes) {
    # Get best g for this outcome
    best_g <- best_solution_summary$groups[best_solution_summary$analysis == outcome]
    
    if (length(best_g) == 0) {
      message(paste0("  ⚠️ No best solution found for ", outcome, ". Skipping."))
      next
    }
    
    # Construct column names
    group_col <- paste0(outcome, "_g", best_g, "_group")
    label_col <- paste0(group_col, "_label")
    
    # Check if columns exist
    if (!(group_col %in% names(master_results_df))) {
      message(paste0("  ⚠️ Group column not found for ", outcome, ". Skipping."))
      next
    }
    
    # Check if labels exist
    has_labels <- label_col %in% names(master_results_df)
    
    if (has_labels) {
      # Count participants by labeled pattern
      pattern_counts <- master_results_df %>%
        group_by(!!sym(label_col)) %>%
        summarise(n = n(), .groups = "drop") %>%
        mutate(
          outcome = outcome,
          best_g = best_g,
          pattern_label = !!sym(label_col),
          percent = round(100 * n / sum(n), 1)
        ) %>%
        select(outcome, best_g, pattern_label, n, percent)
      
      summary_rows[[outcome]] <- pattern_counts
      
    } else {
      # No labels available - use numeric groups
      pattern_counts <- master_results_df %>%
        group_by(!!sym(group_col)) %>%
        summarise(n = n(), .groups = "drop") %>%
        mutate(
          outcome = outcome,
          best_g = best_g,
          pattern_label = paste0("Group ", !!sym(group_col)),
          percent = round(100 * n / sum(n), 1)
        ) %>%
        select(outcome, best_g, pattern_label, n, percent)
      
      summary_rows[[outcome]] <- pattern_counts
      message(paste0("  ℹ️ No labels found for ", outcome, " - using numeric groups"))
    }
  }
  
  if (length(summary_rows) == 0) {
    message("  ⚠️ No summary data could be created")
    return(NULL)
  }
  
  # Combine all summaries
  summary_table <- bind_rows(summary_rows)
  
  message("✅ Created labeled pattern summary table")
  message(paste0("  Outcomes included: ", length(summary_rows)))
  
  return(summary_table)
}
