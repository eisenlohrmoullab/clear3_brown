library(tidyverse)  # Includes dplyr, ggplot2
library(glue)


#' Create Cross-Version Comparison Report
#'
#' This function collects BIC results from all four versions of SMM analysis
#' (menses/ovulation × winsorized/unwinsorized), ranks them by best BIC,
#' and generates a comprehensive report with GAM plots from the top groupings
#' per version (top 2 by BIC plus g=2 and g=3 if not already included).
#'
#' @param base_save_dir Base directory where SMM results are saved
#' @param outcomes Vector of outcome variable names OR data.frame with outcome config
#' @param date_folder Date folder name (e.g., "20231215"), defaults to today
#' @param report_dir Directory where report will be saved, defaults to base_save_dir/reports
#'
#' @return List containing comparison tables and plots
#' @export
create_comparison_report <- function(
    base_save_dir,
    outcomes,
    date_folder = format(Sys.Date(), "%Y%m%d"),
    report_dir = NULL
) {
  
  # Handle both old (vector) and new (data.frame) format for outcomes
  if (is.data.frame(outcomes)) {
    outcomes_config <- outcomes
    outcomes_vec <- outcomes_config$outcome
  } else {
    # Legacy support: if just a vector, assume default values
    outcomes_vec <- outcomes
    outcomes_config <- data.frame(
      outcome = outcomes_vec,
      winsorized = TRUE,
      rolling_avg = "none",
      stringsAsFactors = FALSE
    )
  }
  
  if (is.null(report_dir)) {
    report_dir <- file.path(base_save_dir, "smm", date_folder, "comparison_reports")
  }
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  
  message(glue("Creating comparison report for {length(outcomes_vec)} outcomes..."))
  message(glue("Results directory: {base_save_dir}"))
  message(glue("Report directory: {report_dir}"))
  
  # Define all four version combinations
  versions <- expand.grid(
    centering = c("menses", "ovulation"),
    winsorized = c("winsorized", "unwinsorized"),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      centering_folder = if_else(centering == "ovulation", "ovulation_centered", "menses_centered"),
      version_label = paste0(centering, "_", winsorized)
    )
  
  # Process each outcome
  all_outcomes_results <- list()
  
  for (outcome in outcomes_vec) {
    message(glue("\n>>> Processing outcome: {outcome} <<<"))
    
    # Get the rolling_avg and winsorized settings for this outcome
    outcome_row <- outcomes_config[outcomes_config$outcome == outcome, ]
    if (nrow(outcome_row) == 0) {
      message(glue("  WARNING: No config found for outcome {outcome}, using defaults"))
      outcome_rolling_avg <- "none"
      outcome_winsorized <- TRUE
    } else {
      outcome_rolling_avg <- outcome_row$rolling_avg[1]
      outcome_winsorized <- outcome_row$winsorized[1]
    }
    rolling_folder <- paste0("roll", outcome_rolling_avg)
    
    # Collect BIC data from all versions
    bic_data <- data.frame()
    
    for (i in 1:nrow(versions)) {
      version_info <- versions[i, ]
      centering_folder <- version_info$centering_folder
      winsorization_folder <- version_info$winsorized
      version_label <- version_info$version_label
      
      # Skip versions that don't match the outcome's winsorization setting
      if ((winsorization_folder == "winsorized" && !outcome_winsorized) ||
          (winsorization_folder == "unwinsorized" && outcome_winsorized)) {
        message(glue("  ⊘ Skipping {version_label} (outcome configured as {ifelse(outcome_winsorized, 'winsorized', 'unwinsorized')})"))
        next
      }
      
      # Path to BIC comparison file with rolling_folder
      bic_file <- file.path(
        base_save_dir, 
        "smm",
        date_folder, 
        outcome,
        winsorization_folder,
        rolling_folder,
        centering_folder,
        paste0(outcome, "_", version_info$centering, "_bic_comparison.csv")
      )
      
      if (file.exists(bic_file)) {
        bic_table <- read.csv(bic_file)
        bic_table <- bic_table %>%
          mutate(
            version = version_label,
            centering = version_info$centering,
            winsorized = version_info$winsorized,
            outcome = outcome
          )
        bic_data <- bind_rows(bic_data, bic_table)
        message(glue("  ✓ Loaded BIC data for {version_label}"))
      } else {
        message(glue("  ✗ BIC file not found: {bic_file}"))
      }
    }
    
    if (nrow(bic_data) == 0) {
      message(glue("  WARNING: No BIC data found for outcome {outcome}. Skipping."))
      next
    }
    
    # Find the best (lowest) BIC for each version
    best_bic_per_version <- bic_data %>%
      group_by(version, centering, winsorized) %>%
      arrange(BIC) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(BIC) %>%
      mutate(rank = row_number())
    
    message(glue("\nBest BIC models for {outcome}:"))
    for (j in 1:nrow(best_bic_per_version)) {
      row <- best_bic_per_version[j, ]
      message(glue("  Rank {row$rank}: {row$version} - g={row$groups}, BIC={round(row$BIC, 2)}"))
    }
    
    # Get top 2 groupings (by BIC) for each version, plus ensure g=2 and g=3 are included
    # First get the top 2 by BIC
    top2_by_bic <- bic_data %>%
      group_by(version, centering, winsorized) %>%
      arrange(BIC) %>%
      slice(1:2) %>%
      ungroup()
    
    # Then get g=2 and g=3 if they exist
    g2_g3 <- bic_data %>%
      filter(groups %in% c(2, 3)) %>%
      group_by(version, centering, winsorized) %>%
      arrange(groups) %>%
      ungroup()
    
    # Combine and remove duplicates
    top2_per_version <- bind_rows(top2_by_bic, g2_g3) %>%
      distinct(version, centering, winsorized, groups, .keep_all = TRUE) %>%
      arrange(version, BIC)
    
    # Create comprehensive BIC comparison plot across all versions
    bic_plot <- ggplot(bic_data, aes(x = groups, y = BIC, color = version, linetype = version)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = unique(bic_data$groups)) +
      labs(
        title = glue("BIC Comparison Across All Versions: {outcome}"),
        subtitle = "Lower BIC indicates better fit",
        x = "Number of Groups",
        y = "BIC",
        color = "Version",
        linetype = "Version"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Save BIC comparison plot
    bic_plot_file <- file.path(report_dir, glue("{outcome}_BIC_comparison_all_versions.png"))
    ggsave(bic_plot_file, plot = bic_plot, width = 10, height = 7, dpi = 300)
    message(glue("  ✓ Saved BIC comparison plot: {bic_plot_file}"))
    
    # Save best BIC summary table
    best_bic_file <- file.path(report_dir, glue("{outcome}_best_BIC_summary.csv"))
    write.csv(best_bic_per_version, best_bic_file, row.names = FALSE)
    message(glue("  ✓ Saved best BIC summary: {best_bic_file}"))
    
    # Save top groupings per version (top 2 by BIC + g=2 and g=3)
    top_groupings_file <- file.path(report_dir, glue("{outcome}_top_groupings_per_version.csv"))
    write.csv(top2_per_version, top_groupings_file, row.names = FALSE)
    message(glue("  ✓ Saved top groupings per version: {top_groupings_file}"))
    
    # Collect GAM plots for top 2 groupings from each version
    gam_plot_info <- data.frame()
    
    for (j in 1:nrow(top2_per_version)) {
      row <- top2_per_version[j, ]
      
      # Determine the outcome name used in saved files
      outcome_name <- if (row$winsorized == "winsorized") {
        paste0(outcome, "_win")
      } else {
        outcome
      }
      
      # Construct paths
      centering_folder <- if_else(row$centering == "ovulation", "ovulation_centered", "menses_centered")
      gam_dir <- file.path(
        base_save_dir,
        "smm",
        date_folder,
        outcome,
        row$winsorized,
        rolling_folder,
        centering_folder
      )
      
      # GAM plot file
      gam_plot_file <- file.path(gam_dir, glue("{outcome_name}_log_g{row$groups}_GAM_plot.png"))
      
      if (file.exists(gam_plot_file)) {
        # Copy GAM plot to report directory with descriptive name
        dest_file <- file.path(
          report_dir, 
          glue("{outcome}_{row$version}_g{row$groups}_GAM_plot.png")
        )
        file.copy(gam_plot_file, dest_file, overwrite = TRUE)
        
        gam_plot_info <- bind_rows(
          gam_plot_info,
          data.frame(
            outcome = outcome,
            version = row$version,
            groups = row$groups,
            BIC = row$BIC,
            source_file = gam_plot_file,
            report_file = dest_file,
            stringsAsFactors = FALSE
          )
        )
        message(glue("  ✓ Copied GAM plot: {basename(dest_file)}"))
      } else {
        message(glue("  ✗ GAM plot not found: {gam_plot_file}"))
      }
    }
    
    # Save GAM plot index
    if (nrow(gam_plot_info) > 0) {
      gam_index_file <- file.path(report_dir, glue("{outcome}_GAM_plot_index.csv"))
      write.csv(gam_plot_info, gam_index_file, row.names = FALSE)
      message(glue("  ✓ Saved GAM plot index: {gam_index_file}"))
    }
    
    # Store results for this outcome
    all_outcomes_results[[outcome]] <- list(
      bic_data = bic_data,
      best_bic_per_version = best_bic_per_version,
      top2_per_version = top2_per_version,
      bic_plot = bic_plot,
      gam_plot_info = gam_plot_info
    )
  }
  
  # Create a master summary table across all outcomes
  master_summary <- data.frame()
  for (outcome in names(all_outcomes_results)) {
    outcome_result <- all_outcomes_results[[outcome]]
    best_models <- outcome_result$best_bic_per_version %>%
      select(outcome, version, centering, winsorized, groups, BIC, rank)
    master_summary <- bind_rows(master_summary, best_models)
  }
  
  if (nrow(master_summary) > 0) {
    master_file <- file.path(report_dir, "master_summary_all_outcomes.csv")
    write.csv(master_summary, master_file, row.names = FALSE)
    message(glue("\n✓✓ Saved master summary: {master_file}"))
    
    # Create a summary plot showing best version for each outcome
    best_overall <- master_summary %>%
      group_by(outcome) %>%
      arrange(BIC) %>%
      slice(1) %>%
      ungroup()
    
    summary_plot <- ggplot(best_overall, aes(x = outcome, y = BIC, fill = version)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0("g=", groups)), vjust = -0.5, size = 3) +
      labs(
        title = "Best Model (Lowest BIC) for Each Outcome",
        subtitle = "Across all versions (menses/ovulation × winsorized/unwinsorized)",
        x = "Outcome",
        y = "Best BIC",
        fill = "Version"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    summary_plot_file <- file.path(report_dir, "master_summary_plot.png")
    ggsave(summary_plot_file, plot = summary_plot, width = 10, height = 7, dpi = 300)
    message(glue("✓✓ Saved master summary plot: {summary_plot_file}"))
  }
  
  message(glue("\n✓✓✓ Comparison report complete! All files saved to: {report_dir}"))
  
  return(list(
    outcomes_results = all_outcomes_results,
    master_summary = master_summary,
    report_dir = report_dir
  ))
}
