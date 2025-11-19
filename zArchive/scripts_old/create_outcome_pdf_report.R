library(tidyverse)  # Includes dplyr, ggplot2
library(glue)
library(gridExtra)
library(grid)
library(png)


#' Create Per-Outcome PDF Report with All Versions
#'
#' This function creates a comprehensive PDF report for each outcome that includes:
#' - 1 cover page with BIC comparison across all versions
#' - 4 pages (one per version: menses-wins, menses-unwins, ov-wins, ov-unwins)
#' - Each page shows all g1-g5 GAM plots with shared Y-axis scale
#' - A smaller BIC plot on each page
#' - Y-axis standardized within each page for easy visual comparison
#'
#' @param base_save_dir Base directory where SMM results are saved
#' @param outcomes Vector of outcome variable names OR data.frame with outcome config
#' @param date_folder Date folder name (e.g., "20231215"), defaults to today
#' @param report_dir Directory where PDF reports will be saved
#' @param groups_to_show Vector of group numbers to include (default: 1:5)
#'
#' @return List containing report metadata
#' @export
create_outcome_pdf_report <- function(
    base_save_dir,
    outcomes,
    date_folder = format(Sys.Date(), "%Y%m%d"),
    report_dir = NULL,
    groups_to_show = 1:5
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
    report_dir <- file.path(base_save_dir, "smm", date_folder, "outcome_comparison_reports")
  }
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  
  message(glue("Creating per-outcome PDF reports for {length(outcomes_vec)} outcomes..."))
  message(glue("Results directory: {base_save_dir}"))
  message(glue("Report directory: {report_dir}"))
  
  # Define all four version combinations
  versions <- data.frame(
    centering = c("menses", "menses", "ovulation", "ovulation"),
    winsorized = c("winsorized", "unwinsorized", "winsorized", "unwinsorized"),
    centering_folder = c("menses_centered", "menses_centered", "ovulation_centered", "ovulation_centered"),
    version_label = c("Menses-Centered Winsorized", "Menses-Centered Unwinsorized", 
                      "Ovulation-Centered Winsorized", "Ovulation-Centered Unwinsorized"),
    short_label = c("menses-wins", "menses-unwins", "ov-wins", "ov-unwins"),
    stringsAsFactors = FALSE
  )
  
  # Process each outcome
  report_files <- list()
  
  for (outcome in outcomes_vec) {
    message(glue("\n>>> Creating PDF report for outcome: {outcome} <<<"))
    
    # Get the rolling_avg and winsorized settings for this outcome
    outcome_row <- outcomes_config[outcomes_config$outcome == outcome, ]
    if (nrow(outcome_row) == 0) {
      message(glue("  WARNING: No config found for outcome {outcome}, using defaults"))
    }
    outcome_rolling_avg <- if (nrow(outcome_row) > 0 && !is.na(outcome_row$rolling_avg[1])) outcome_row$rolling_avg[1] else "none"
    outcome_winsorized <- if (nrow(outcome_row) > 0 && !is.na(outcome_row$winsorized[1])) outcome_row$winsorized[1] else TRUE
    rolling_folder <- paste0("roll", outcome_rolling_avg)
    
    # Create PDF for this outcome
    pdf_file <- file.path(report_dir, glue("{outcome}_comparison_report.pdf"))
    pdf(pdf_file, width = 11, height = 8.5, onefile = TRUE)
    
    # === COVER PAGE: BIC Comparison Across All Versions ===
    message(glue("  Creating cover page with BIC comparison across all versions"))
    
    # Path to the BIC comparison image from comparison_reports
    comparison_reports_dir <- file.path(base_save_dir, "smm", date_folder, "comparison_reports")
    bic_comparison_file <- file.path(comparison_reports_dir, glue("{outcome}_BIC_comparison_all_versions.png"))
    
    if (file.exists(bic_comparison_file)) {
      # Load and display the BIC comparison image
      bic_img <- readPNG(bic_comparison_file)
      
      # Create cover page layout
      grid.newpage()
      
      # Add title
      cover_title <- textGrob(
        glue("{outcome}\nBIC Comparison Across All Versions"),
        gp = gpar(fontsize = 20, fontface = "bold")
      )
      
      # Create layout: title at top, image below
      cover_layout <- arrangeGrob(
        cover_title,
        rasterGrob(bic_img, interpolate = TRUE),
        heights = c(0.15, 0.85),
        ncol = 1
      )
      
      grid.draw(cover_layout)
      message(glue("  ✓ Cover page created with BIC comparison"))
    } else {
      # Fallback if BIC comparison image is not found
      grid.newpage()
      cover_title <- textGrob(
        glue("{outcome}\nComparison Report"),
        gp = gpar(fontsize = 20, fontface = "bold")
      )
      grid.draw(cover_title)
      message(glue("  ⚠ BIC comparison file not found: {bic_comparison_file}"))
      message(glue("  ⚠ Created cover page without BIC comparison"))
    }
    
    # Process each version as a separate page
    for (v in 1:nrow(versions)) {
      version_info <- versions[v, ]
      message(glue("  Processing page {v+1}/5: {version_info$version_label}"))
      
      # Skip versions that don't match the outcome's winsorization setting
      if ((version_info$winsorized == "winsorized" && !outcome_winsorized) ||
          (version_info$winsorized == "unwinsorized" && outcome_winsorized)) {
        message(glue("    ⊘ Skipping this version (outcome configured as {ifelse(outcome_winsorized, 'winsorized', 'unwinsorized')})"))
        next
      }
      
      # Collect data for all groups in this version
      gam_plots_data <- list()
      bic_data <- NULL
      y_min <- Inf
      y_max <- -Inf
      
      # Determine outcome name used in files
      outcome_name <- if (version_info$winsorized == "winsorized") {
        paste0(outcome, "_win")
      } else {
        outcome
      }
      
      # Path to this version's results with rolling_folder
      version_dir <- file.path(
        base_save_dir,
        "smm",
        date_folder,
        outcome,
        version_info$winsorized,
        rolling_folder,
        version_info$centering_folder
      )
      
      # Check if version directory exists
      if (!dir.exists(version_dir)) {
        message(glue("    ⚠ Version directory does not exist: {version_dir}"))
        message(glue("    → This version may not have been analyzed yet"))
      }
      
      # Load BIC data
      bic_file <- file.path(version_dir, paste0(outcome, "_", version_info$centering, "_bic_comparison.csv"))
      if (file.exists(bic_file)) {
        bic_data <- read.csv(bic_file)
        message(glue("    ✓ Loaded BIC data"))
      } else {
        message(glue("    ✗ BIC file not found: {bic_file}"))
      }
      
      # Load GAM plot data for each group
      for (g in groups_to_show) {
        # Determine model filename - baseline is saved as _noGroup_GAM_model.rds
        if (g == 1) {
          gam_model_file <- file.path(version_dir, glue("{outcome_name}_log_noGroup_GAM_model.rds"))
        } else {
          gam_model_file <- file.path(version_dir, glue("{outcome_name}_log_g{g}_GAM_model.rds"))
        }
        
        if (file.exists(gam_model_file)) {
          # Load the GAM model
          gam_model <- readRDS(gam_model_file)
          
          # Generate predictions for plotting
          # Extract data from model
          model_data <- gam_model$model
          time_var <- names(model_data)[grep("cyclic_time", names(model_data))[1]]
          
          # Check if this is a group-moderated model (g > 1) or baseline (g = 1)
          has_group <- "smm_group" %in% names(model_data)
          
          if (has_group) {
            group_levels <- sort(unique(model_data$smm_group))
            
            # Calculate number of IDs per group
            group_n <- model_data %>%
              mutate(smm_group = as.factor(smm_group)) %>%
              group_by(smm_group) %>%
              summarise(n_id = n_distinct(id), .groups = "drop") %>%
              mutate(
                group_label = paste0("Group ", smm_group, " (N=", n_id, ")")
              )
            
            pred_grid <- expand.grid(
              temp_time = seq(-1, 1, length.out = 100),
              smm_group = factor(group_levels, levels = group_levels),
              id = 0
            )
            names(pred_grid)[names(pred_grid) == "temp_time"] <- time_var
            
            # Generate predictions
            pred <- marginaleffects::predictions(
              gam_model,
              newdata = pred_grid,
              type = "response",
              transform = function(x) exp(x) - 1  # Reverse log transform
            )
            
            # Merge group labels with counts into pred_grid
            # Directly join group labels for group_label column
            
            pred_df <- data.frame(
              time = pred_grid[[time_var]],
              group = pred_grid$smm_group,
              group_label = pred_grid %>% left_join(group_n, by = "smm_group") %>% pull(group_label),
              estimate = pred$estimate,
              conf.low = pred$conf.low,
              conf.high = pred$conf.high
            )
          } else {
            # Baseline model without groups
            pred_grid <- data.frame(
              temp_time = seq(-1, 1, length.out = 100),
              id = 0
            )
            names(pred_grid)[names(pred_grid) == "temp_time"] <- time_var
            
            pred <- marginaleffects::predictions(
              gam_model,
              newdata = pred_grid,
              type = "response",
              transform = function(x) exp(x) - 1
            )
            
            # Count total unique IDs in baseline model
            n_id <- n_distinct(model_data$id)
            
            pred_df <- data.frame(
              time = pred_grid[[time_var]],
              group = factor("1"),
              group_label = paste0("Group 1 (N=", n_id, ")"),
              estimate = pred$estimate,
              conf.low = pred$conf.low,
              conf.high = pred$conf.high
            )
          }
          
          gam_plots_data[[as.character(g)]] <- pred_df
          
          # Track min/max for shared Y-axis
          y_min <- min(y_min, min(pred_df$conf.low, na.rm = TRUE))
          y_max <- max(y_max, max(pred_df$conf.high, na.rm = TRUE))
          
          message(glue("    ✓ Loaded GAM model for g={g}"))
        } else {
          message(glue("    ✗ GAM model not found for g={g}"))
        }
      }
      
      # Create the page layout
      if (length(gam_plots_data) > 0) {
        # Add some padding to Y-axis limits
        y_range <- y_max - y_min
        y_min <- y_min - 0.05 * y_range
        y_max <- y_max + 0.05 * y_range
        
        # Create GAM plots with shared Y-axis
        gam_plot_list <- list()
        for (g in names(gam_plots_data)) {
          pred_df <- gam_plots_data[[g]]
          
          # Determine centering for labels
          centering <- version_info$centering
          x_breaks <- seq(-1, 1, by = 0.5)
          x_labels <- if (centering == "menses") {
            c("Ovulation", "50%L", "Menses", "50%F", "Ovulation")
          } else {
            c("Menses", "50%F", "Ovulation", "50%L", "Menses")
          }
          
          # Create rectangle data for shading
          rect_data <- if (centering == "menses") {
            data.frame(
              xmin = c(-0.04, 0.92, -1),
              xmax = c(0.04, 1, -0.92),
              fill = c("grey70", "grey87", "grey87")
            )
          } else {
            data.frame(
              xmin = c(-0.04, -1, 0.92),
              xmax = c(0.04, -0.92, 1),
              fill = c("grey87", "grey70", "grey70")
            )
          }
          
          p <- ggplot(pred_df, aes(x = time, y = estimate, color = group_label)) +
            geom_rect(
              data = rect_data,
              inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = rect_data$fill,
              alpha = 0.2,
              color = NA
            ) +
            geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group_label),
                       alpha = 0.2, color = NA, show.legend = FALSE) +
            geom_line(linewidth = 0.8) +
            scale_x_continuous(limits = c(-1, 1), breaks = x_breaks, labels = x_labels) +
            coord_cartesian(ylim = c(y_min, y_max)) +
            labs(
              title = glue("g={g}"),
              x = "",
              y = if (g == names(gam_plots_data)[1]) outcome else "",
              color = "Group"
            ) +
            guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
            theme_minimal() +
            theme(
              legend.position = "bottom",
              legend.text = element_text(size = 7),
              legend.title = element_text(size = 8),
              legend.box.spacing = unit(0.1, "cm"),
              legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
              plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
            )
          
          gam_plot_list[[g]] <- p
        }
        
        # Create page layout with title
        page_title <- textGrob(
          glue("{outcome}: {version_info$version_label}"),
          gp = gpar(fontsize = 16, fontface = "bold")
        )
        
        # Arrange GAM plots in a grid
        if (length(gam_plot_list) <= 3) {
          # If 3 or fewer, arrange in a single row
          gam_grid <- arrangeGrob(grobs = gam_plot_list, ncol = length(gam_plot_list))
        } else {
          # Otherwise, arrange in 2 rows
          gam_grid <- arrangeGrob(grobs = gam_plot_list, ncol = 3, nrow = 2)
        }
        
        # Arrange full page: title and GAM plots only (BIC comparison is on cover page)
        page_layout <- arrangeGrob(
          page_title,
          gam_grid,
          heights = c(0.10, 0.90),
          ncol = 1
        )
        
        grid.newpage()
        grid.draw(page_layout)
        
        message(glue("    ✓ Page {v+1}/5 complete"))
      } else {
        # No data available for this version
        grid.newpage()
        grid.text(glue("No GAM data available for\n{outcome}: {version_info$version_label}"),
                 gp = gpar(fontsize = 14))
        message(glue("    ⚠ No data for page {v+1}/5"))
      }
    }
    
    dev.off()
    message(glue("  ✓✓ PDF report saved: {pdf_file}"))
    report_files[[outcome]] <- pdf_file
  }
  
  message(glue("\n✓✓✓ All outcome PDF reports complete! Files saved to: {report_dir}"))
  
  return(list(
    report_files = report_files,
    report_dir = report_dir
  ))
}
