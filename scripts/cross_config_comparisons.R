## ============================================================================
## CROSS-CONFIGURATION GROUP COMPARISONS
## ============================================================================
## This script provides functions to select and compare specific SMM 
## configurations across different outcomes, allowing you to mix centering
## methods, winsorization options, and group counts.
##
## For example:
## - CSS_Inatt: ovulation-centered, winsorized, g=3
## - DRSP1: menses-centered, unwinsorized, g=4  
## - E2: ovulation-centered, winsorized, g=2
##
## Then create comparisons and correlations using only these selected versions.
## ============================================================================

library(tidyverse)  # Includes dplyr, ggplot2, tidyr

#' Select specific SMM configurations for cross-outcome comparison
#' 
#' Allows you to select one specific configuration (centering + winsorization + g)
#' for each outcome, even if they come from different analysis runs.
#' 
#' @param all_results Named list of ALL SMM results across all configurations
#' @param base_save_dir Base directory where results are saved
#' @param date_folder Date folder (default: today's date)
#' @param interactive Logical: if TRUE, prompts user to select configurations
#' @param preselections Optional dataframe with preselected configurations
#'                      Columns: outcome, centering, winsorized, groups
#' @return A list with selected configurations and consolidated data
#' @export
select_cross_config_groupings <- function(all_results,
                                         base_save_dir,
                                         date_folder = format(Sys.Date(), "%Y%m%d"),
                                         interactive = TRUE,
                                         preselections = NULL) {
  
  message("\n==============================================================================")
  message("CROSS-CONFIGURATION GROUP SELECTION")
  message("==============================================================================\n")
  message("Select one specific configuration for each outcome to compare.")
  message("You can mix centering methods, winsorization, and group counts.\n")
  
  # Build catalog of all available configurations
  catalog <- list()
  
  for (result_name in names(all_results)) {
    # Parse result_name to extract components
    # Expected formats: 
    #   "outcome_centering_winsorized" or "outcome_centering_unwinsorized"
    # Handle outcomes with underscores like "CSS_Inatt"
    
    parts <- strsplit(result_name, "_")[[1]]
    n_parts <- length(parts)
    
    # Last part is winsorization status
    winsor_status <- parts[n_parts]
    is_winsorized <- (winsor_status == "winsorized")
    
    # Second to last is centering
    centering <- parts[n_parts - 1]
    
    # Everything before is the outcome name
    outcome <- paste(parts[1:(n_parts - 2)], collapse = "_")
    
    # Get available g values
    smm_result <- all_results[[result_name]]
    if ("all_results" %in% names(smm_result)) {
      # Multi-g result
      available_g <- as.numeric(names(smm_result$all_results))
    } else {
      # Single-g result - infer from probability columns
      prob_cols <- grep("^prob_group", names(smm_result$class), value = TRUE)
      available_g <- length(prob_cols)
    }
    
    # Add to catalog
    for (g in available_g) {
      config_id <- paste0(outcome, "_", centering, "_", 
                         ifelse(is_winsorized, "win", "nowin"), "_g", g)
      catalog[[config_id]] <- list(
        outcome = outcome,
        centering = centering,
        winsorized = is_winsorized,
        groups = g,
        result_name = result_name,
        config_id = config_id
      )
    }
  }
  
  message(paste0("Found ", length(unique(sapply(catalog, function(x) x$outcome))), 
                " unique outcomes with ", length(catalog), " total configurations.\n"))
  
  # Get unique outcomes
  unique_outcomes <- unique(sapply(catalog, function(x) x$outcome))
  
  # Selection process
  selections <- data.frame(
    outcome = character(),
    centering = character(),
    winsorized = logical(),
    groups = numeric(),
    config_id = character(),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(preselections)) {
    # Use preselections
    message("Using provided preselections...\n")
    selections <- preselections
    
    # Validate and add config_ids
    for (i in 1:nrow(selections)) {
      config_id <- paste0(selections$outcome[i], "_", selections$centering[i], "_",
                         ifelse(selections$winsorized[i], "win", "nowin"), 
                         "_g", selections$groups[i])
      
      if (!(config_id %in% names(catalog))) {
        stop(paste0("Configuration not found: ", config_id))
      }
      selections$config_id[i] <- config_id
    }
    
  } else if (interactive) {
    # Interactive selection
    message("=== Interactive Selection ===\n")
    
    for (outcome in unique_outcomes) {
      # Find all configurations for this outcome
      outcome_configs <- catalog[sapply(catalog, function(x) x$outcome == outcome)]
      
      message(paste0("--- ", outcome, " ---"))
      message("Available configurations:")
      
      for (i in seq_along(outcome_configs)) {
        cfg <- outcome_configs[[i]]
        center_label <- ifelse(cfg$centering == "menses", "menses", "ovulation")
        winsor_label <- ifelse(cfg$winsorized, "winsorized", "unwinsorized")
        message(paste0("  [", i, "] ", center_label, "-centered, ", 
                      winsor_label, ", g=", cfg$groups))
      }
      
      if (interactive()) {
        choice <- readline(prompt = paste0("Select configuration for ", outcome, " [1-", length(outcome_configs), "]: "))
        choice_num <- as.numeric(choice)
        
        if (is.na(choice_num) || choice_num < 1 || choice_num > length(outcome_configs)) {
          message(paste0("  ⚠️ Invalid choice. Skipping ", outcome))
          next
        }
        
        selected_cfg <- outcome_configs[[choice_num]]
        message(paste0("  ✓ Selected: ", selected_cfg$config_id, "\n"))
        
      } else {
        # Non-interactive: use first option
        selected_cfg <- outcome_configs[[1]]
        message(paste0("  (Non-interactive mode: using ", selected_cfg$config_id, ")\n"))
      }
      
      selections <- rbind(selections, data.frame(
        outcome = selected_cfg$outcome,
        centering = selected_cfg$centering,
        winsorized = selected_cfg$winsorized,
        groups = selected_cfg$groups,
        config_id = selected_cfg$config_id,
        stringsAsFactors = FALSE
      ))
    }
    
  } else {
    # Non-interactive, no preselections: use first available for each outcome
    message("Non-interactive mode with no preselections: using first available configuration for each outcome.\n")
    
    for (outcome in unique_outcomes) {
      outcome_configs <- catalog[sapply(catalog, function(x) x$outcome == outcome)]
      selected_cfg <- outcome_configs[[1]]
      
      selections <- rbind(selections, data.frame(
        outcome = selected_cfg$outcome,
        centering = selected_cfg$centering,
        winsorized = selected_cfg$winsorized,
        groups = selected_cfg$groups,
        config_id = selected_cfg$config_id,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  message("\n==============================================================================")
  message("SELECTIONS COMPLETE")
  message("==============================================================================\n")
  print(selections)
  message("\n")
  
  return(list(
    selections = selections,
    catalog = catalog
  ))
}


#' Consolidate selected configurations into a comparison-ready dataframe
#' 
#' @param all_results Named list of ALL SMM results
#' @param selections Dataframe from select_cross_config_groupings()
#' @param catalog Catalog from select_cross_config_groupings()
#' @param base_save_dir Base directory where results and labels are saved
#' @param date_folder Date folder
#' @param load_labels Logical: whether to load group labels
#' @return Dataframe with selected configurations consolidated
#' @export
consolidate_selected_configs <- function(all_results,
                                        selections,
                                        catalog,
                                        base_save_dir,
                                        date_folder = format(Sys.Date(), "%Y%m%d"),
                                        load_labels = TRUE) {
  
  message("\n>>> Consolidating selected configurations...")
  
  # Load label functions if needed
  labels_loaded <- FALSE
  if (load_labels && file.exists("scripts/label_smm_groups.R")) {
    tryCatch({
      source("scripts/label_smm_groups.R", local = TRUE)
      labels_loaded <- TRUE
      message("  ℹ️ Group label functions available")
    }, error = function(e) {
      message(paste0("  ⚠️ Could not load label functions: ", e$message))
    })
  }
  
  # Get all unique participant IDs
  all_ids <- unique(unlist(lapply(all_results, function(res) {
    if ("all_results" %in% names(res)) {
      res$all_results[[1]]$class$id
    } else {
      res$class$id
    }
  })))
  
  # Initialize master dataframe
  master_df <- data.frame(id = all_ids)
  
  # Process each selected configuration
  for (i in 1:nrow(selections)) {
    sel <- selections[i, ]
    cfg <- catalog[[sel$config_id]]
    
    message(paste0("  Processing: ", sel$config_id))
    
    # Get the SMM result
    smm_result <- all_results[[cfg$result_name]]
    
    # Extract the specific g solution
    if ("all_results" %in% names(smm_result)) {
      # Multi-g result
      g_key <- as.character(sel$groups)
      class_df <- smm_result$all_results[[g_key]]$class
    } else {
      # Single-g result
      class_df <- smm_result$class
    }
    
    # Try to load labels
    if (labels_loaded && exists("load_group_labels", mode = "function")) {
      group_labels <- tryCatch({
        load_group_labels(
          base_save_dir = base_save_dir,
          outcome = sel$outcome,
          centering = sel$centering,
          winsorized = sel$winsorized,
          g = sel$groups,
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
          message(paste0("    ⚠️ Could not apply labels: ", e$message))
          class_df
        })
      }
    }
    
    # Rename columns to include config info
    # Format: outcome_group, outcome_group_label, outcome_prob_group1, etc.
    new_names <- names(class_df)[-1]  # Exclude 'id'
    new_names <- gsub("^group$", paste0(sel$outcome, "_group"), new_names)
    new_names <- gsub("^group_label$", paste0(sel$outcome, "_group_label"), new_names)
    new_names <- gsub("^prob_group", paste0(sel$outcome, "_prob_group"), new_names)
    names(class_df) <- c("id", new_names)
    
    # Merge into master dataframe
    master_df <- left_join(master_df, class_df, by = "id")
  }
  
  message("  ✅ Consolidation complete\n")
  
  return(master_df)
}


#' Run comparisons on cross-configuration selections
#' 
#' @param all_results Named list of ALL SMM results
#' @param selections Dataframe with selected configurations
#' @param catalog Catalog from select_cross_config_groupings()
#' @param base_save_dir Base directory
#' @param date_folder Date folder
#' @param output_name Name for output files (default: "cross_config_comparison")
#' @return List with comparison results and file paths
#' @export
run_cross_config_comparisons <- function(all_results,
                                        selections,
                                        catalog,
                                        base_save_dir,
                                        date_folder = format(Sys.Date(), "%Y%m%d"),
                                        output_name = "cross_config_comparison") {
  
  message("\n================================================================================")
  message("RUNNING CROSS-CONFIGURATION COMPARISONS")
  message("================================================================================\n")
  
  # Consolidate selected configurations
  master_df <- consolidate_selected_configs(
    all_results = all_results,
    selections = selections,
    catalog = catalog,
    base_save_dir = base_save_dir,
    date_folder = date_folder,
    load_labels = TRUE
  )
  
  # Create output directory
  report_dir <- file.path(
    base_save_dir,
    "smm",
    date_folder,
    "cross_config_comparisons",
    output_name
  )
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  
  message(paste0("Results will be saved to: ", report_dir, "\n"))
  
  # Save consolidated data
  consolidated_file <- file.path(report_dir, paste0(output_name, "_consolidated_data.csv"))
  write.csv(master_df, consolidated_file, row.names = FALSE)
  message(paste0("✅ Saved consolidated data: ", basename(consolidated_file)))
  
  # Save selections metadata
  selections_file <- file.path(report_dir, paste0(output_name, "_selections.csv"))
  write.csv(selections, selections_file, row.names = FALSE)
  message(paste0("✅ Saved selections metadata: ", basename(selections_file)))
  
  # Load comparison functions
  source("scripts/compare_group_memberships.R")
  
  # Create categorical comparisons (chi-squared tests)
  message("\n>>> Creating categorical comparisons...")
  categorical_results <- list()
  
  outcome_names <- selections$outcome
  if (length(outcome_names) >= 2) {
    outcome_pairs <- combn(outcome_names, 2, simplify = FALSE)
    
    for (pair in outcome_pairs) {
      outcome1 <- pair[1]
      outcome2 <- pair[2]
      pair_label <- paste0(outcome1, "_vs_", outcome2)
      
      message(paste0("  Comparing: ", outcome1, " vs ", outcome2))
      
      tryCatch({
        # Get column names
        col1 <- paste0(outcome1, "_group")
        col2 <- paste0(outcome2, "_group")
        col1_label <- paste0(col1, "_label")
        col2_label <- paste0(col2, "_label")
        
        # Use labeled columns if available
        use_col1 <- if (col1_label %in% names(master_df)) col1_label else col1
        use_col2 <- if (col2_label %in% names(master_df)) col2_label else col2
        
        if (use_col1 != col1) message(paste0("    ✓ Using labeled groups for ", outcome1))
        if (use_col2 != col2) message(paste0("    ✓ Using labeled groups for ", outcome2))
        
        # Create contingency table
        contingency_table <- table(master_df[[use_col1]], master_df[[use_col2]],
                                   dnn = c(outcome1, outcome2))
        
        # Chi-squared test
        chi_test <- suppressWarnings(chisq.test(contingency_table))
        
        categorical_results[[pair_label]] <- list(
          contingency_table = contingency_table,
          chi_test = chi_test
        )
        
        # Save results
        contingency_file <- file.path(report_dir, paste0(output_name, "_", pair_label, "_contingency.csv"))
        write.csv(contingency_table, contingency_file)
        
        message(paste0("    ✓ Chi-squared p-value: ", round(chi_test$p.value, 4)))
        
      }, error = function(e) {
        message(paste0("    ✗ ERROR: ", e$message))
      })
    }
  }
  
  # Create comprehensive probability correlation heatmap
  message("\n>>> Creating probability correlation heatmap...")
  
  tryCatch({
    # Build probability columns for each outcome-pattern
    prob_cols_list <- list()
    
    for (i in 1:nrow(selections)) {
      outcome <- selections$outcome[i]
      n_groups <- selections$groups[i]
      
      for (g in 1:n_groups) {
        prob_col <- paste0(outcome, "_prob_group", g)
        
        if (prob_col %in% names(master_df)) {
          # Try to get label
          group_col <- paste0(outcome, "_group")
          label_col <- paste0(outcome, "_group_label")
          
          if (label_col %in% names(master_df)) {
            # Extract label for this group number
            group_label_mapping <- master_df %>%
              filter(!is.na(!!sym(group_col)) & !!sym(group_col) == g) %>%
              select(all_of(c(group_col, label_col))) %>%
              distinct()
            
            if (nrow(group_label_mapping) > 0) {
              pattern_name <- paste0(outcome, "_", group_label_mapping[[label_col]][1])
            } else {
              pattern_name <- paste0(outcome, "_group", g)
            }
          } else {
            pattern_name <- paste0(outcome, "_group", g)
          }
          
          prob_cols_list[[pattern_name]] <- prob_col
        }
      }
    }
    
    if (length(prob_cols_list) >= 2) {
      # Create correlation matrix
      prob_df <- master_df %>%
        select(id, all_of(unname(unlist(prob_cols_list))))
      
      names(prob_df) <- c("id", names(prob_cols_list))
      
      corr_matrix <- cor(prob_df[-1], use = "pairwise.complete.obs")
      
      # Convert to long format for plotting
      corr_df <- as.data.frame(as.table(corr_matrix))
      names(corr_df) <- c("Pattern1", "Pattern2", "Correlation")
      
      # Create heatmap
      p <- ggplot(corr_df, aes(x = Pattern1, y = Pattern2, fill = Correlation)) +
        geom_tile() +
        geom_text(aes(label = sprintf("%.2f", Correlation)), size = 2.5) +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                            midpoint = 0, limit = c(-1, 1)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
              axis.text.y = element_text(size = 8)) +
        labs(title = "Cross-Configuration Pattern Probability Correlations",
             x = "", y = "") +
        coord_fixed()
      
      # Save heatmap
      heatmap_file <- file.path(report_dir, paste0(output_name, "_probability_correlation_heatmap.png"))
      ggsave(heatmap_file, plot = p, width = 14, height = 12, dpi = 300)
      message(paste0("  ✅ Saved heatmap: ", basename(heatmap_file)))
      
    } else {
      message("  ⚠️ Not enough patterns for correlation heatmap")
    }
    
  }, error = function(e) {
    message(paste0("  ✗ ERROR creating heatmap: ", e$message))
  })
  
  # Create summary report
  message("\n>>> Creating summary report...")
  
  summary_file <- file.path(report_dir, paste0(output_name, "_summary.txt"))
  
  sink(summary_file)
  cat("================================================================================\n")
  cat("CROSS-CONFIGURATION COMPARISON REPORT\n")
  cat("================================================================================\n\n")
  cat("Date:", date_folder, "\n")
  cat("Number of outcomes:", nrow(selections), "\n\n")
  
  cat("================================================================================\n")
  cat("SELECTED CONFIGURATIONS\n")
  cat("================================================================================\n\n")
  print(selections)
  
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
    cat("No comparisons performed.\n")
  }
  
  cat("\n================================================================================\n")
  cat("OUTPUT FILES\n")
  cat("================================================================================\n\n")
  cat("All results saved to:", report_dir, "\n\n")
  cat("Files generated:\n")
  cat("  -", basename(consolidated_file), "(consolidated data with selected configs)\n")
  cat("  -", basename(selections_file), "(metadata about selected configurations)\n")
  cat("  - Contingency tables (", length(categorical_results), " pairs)\n")
  cat("  - Probability correlation heatmap\n")
  cat("  - This summary report\n")
  
  sink()
  
  message(paste0("✅ Saved summary report: ", basename(summary_file)))
  
  message("\n================================================================================")
  message("CROSS-CONFIGURATION COMPARISONS COMPLETE")
  message("================================================================================\n")
  
  return(list(
    status = "success",
    report_dir = report_dir,
    master_df = master_df,
    selections = selections,
    categorical_results = categorical_results,
    consolidated_file = consolidated_file,
    selections_file = selections_file,
    summary_file = summary_file
  ))
}
