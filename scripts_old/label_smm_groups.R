## ============================================================================
## SMM GROUP LABELING FUNCTIONS
## ============================================================================
## This script provides interactive functionality for labeling SMM groups with
## meaningful names (e.g., "Perimenstrual-Onset", "Luteal-Peak", "Stable").
##
## The group numbers (1, 2, 3, etc.) from SMM analyses don't have inherent 
## meaning. This script helps users:
## 1. View plots for each SMM analysis
## 2. Interactively assign meaningful labels to each group
## 3. Save label mappings for reproducibility
## 4. Apply labels to analysis results
##
## Usage: source("scripts/label_smm_groups.R")
##        See docs/examples/GROUP_LABELING_GUIDE.md for detailed workflow
## ============================================================================

library(tidyverse)  # Includes dplyr, ggplot2
library(glue)
library(png)
library(grid)

## ============================================================================
## FUNCTION 1: Interactive Group Labeling
## ============================================================================
## Displays plots for a specific SMM analysis and prompts user to enter
## meaningful labels for each group. Returns a dataframe with the mappings.
##
## @param base_save_dir Base directory where SMM results are saved
## @param outcome Outcome variable name (e.g., "E2", "CSS_Inatt")
## @param centering Centering method: "menses" or "ovulation"
## @param winsorized Logical: TRUE for winsorized, FALSE for unwinsorized
## @param rolling_avg Rolling average window: "none", "3day", or "5day"
## @param g Number of groups in the solution to label
## @param date_folder Date folder (YYYYMMDD format), defaults to today
## @param interactive Logical: if TRUE, prompts user for input; if FALSE, returns template
## @return Dataframe with columns: group (numeric), group_label (character)
## ============================================================================
label_smm_groups <- function(
    base_save_dir,
    outcome,
    centering = "menses",
    winsorized = TRUE,
    rolling_avg = "5day",
    g = 2,
    date_folder = format(Sys.Date(), "%Y%m%d"),
    interactive = TRUE
) {
  
  # Construct path to analysis results
  centering_folder <- if (centering == "ovulation") "ovulation_centered" else "menses_centered"
  winsorization_folder <- if (winsorized) "winsorized" else "unwinsorized"
  rolling_folder <- paste0("roll", rolling_avg)
  analysis_dir <- file.path(base_save_dir, "smm", date_folder, outcome, winsorization_folder, rolling_folder, centering_folder)
  
  # Check if directory exists
  if (!dir.exists(analysis_dir)) {
    stop(glue("Analysis directory not found: {analysis_dir}\n",
              "Please ensure SMM analyses have been run first."))
  }
  
  # Check if plots exist
  plot_file <- file.path(analysis_dir, glue("{outcome}_g{g}_centered.png"))
  if (!file.exists(plot_file)) {
    warning(glue("Plot file not found: {plot_file}\n",
                 "Continuing with labeling, but plot cannot be displayed."))
  }
  
  # Display context
  cat("\n")
  cat("==============================================================================\n")
  cat("GROUP LABELING FOR SMM ANALYSIS\n")
  cat("==============================================================================\n")
  cat(glue("Outcome:      {outcome}\n"))
  cat(glue("Centering:    {centering}\n"))
  cat(glue("Winsorized:   {winsorized}\n"))
  cat(glue("Rolling Avg:  {rolling_avg}\n"))
  cat(glue("Groups (g):   {g}\n"))
  cat(glue("Analysis Dir: {analysis_dir}\n"))
  cat("==============================================================================\n\n")
  
  if (file.exists(plot_file)) {
    cat(glue("📊 Plot file available at: {plot_file}\n"))
    cat("   Please open this file to view the group patterns before labeling.\n\n")
    
    # Try to display plot if in RStudio
    if (interactive() && .Platform$GUI == "RStudio") {
      tryCatch({
        img <- png::readPNG(plot_file)
        grid::grid.raster(img)
      }, error = function(e) {
        # Silently fail if can't display
      })
    }
  }
  
  # Create template dataframe
  group_labels <- data.frame(
    group = 1:g,
    group_label = rep("", g),
    stringsAsFactors = FALSE
  )
  
  if (!interactive) {
    cat("Non-interactive mode: returning template dataframe.\n")
    cat("Fill in the 'group_label' column and save to CSV.\n\n")
    return(group_labels)
  }
  
  # Interactive labeling
  cat("==============================================================================\n")
  cat("INSTRUCTIONS\n")
  cat("==============================================================================\n")
  cat("For each group, enter a descriptive label based on the pattern you observe.\n\n")
  cat("Common label examples:\n")
  cat("  - Perimenstrual-Onset    (symptoms peak around menses onset)\n")
  cat("  - Luteal-Onset           (symptoms peak in luteal phase)\n")
  cat("  - Luteal-Peak            (symptoms highest in luteal phase)\n")
  cat("  - Periovulatory-Peak     (symptoms peak around ovulation)\n")
  cat("  - Stable                 (little variation across cycle)\n")
  cat("  - Low-Stable             (consistently low levels)\n")
  cat("  - High-Stable            (consistently high levels)\n")
  cat("  - Dual-Peak              (two peaks across the cycle)\n")
  cat("  - Follicular-Peak        (symptoms peak in follicular phase)\n\n")
  cat("You can use any descriptive label that makes sense for your data.\n")
  cat("Avoid spaces; use hyphens or underscores instead.\n")
  cat("==============================================================================\n\n")
  
  # Prompt for each group
  for (i in 1:g) {
    repeat {
      label <- readline(prompt = glue("Enter label for Group {i}: "))
      label <- trimws(label)
      
      # Validate input
      if (label == "") {
        cat("❌ Label cannot be empty. Please try again.\n")
        next
      }
      
      if (grepl("\\s", label)) {
        cat("⚠️  Warning: Label contains spaces. Consider using hyphens or underscores.\n")
        proceed <- readline(prompt = "   Use this label anyway? (y/n): ")
        if (tolower(trimws(proceed)) != "y") {
          next
        }
      }
      
      # Check for duplicates
      if (label %in% group_labels$group_label[1:(i-1)]) {
        cat("❌ This label has already been used for another group. Please use a unique label.\n")
        next
      }
      
      # Accept label
      group_labels$group_label[i] <- label
      cat(glue("✅ Group {i} labeled as: {label}\n\n"))
      break
    }
  }
  
  # Summary
  cat("==============================================================================\n")
  cat("LABELING COMPLETE\n")
  cat("==============================================================================\n")
  print(group_labels)
  cat("==============================================================================\n\n")
  
  return(group_labels)
}


## ============================================================================
## FUNCTION 2: Save Group Labels
## ============================================================================
## Saves group label mappings to a CSV file for reproducibility.
##
## @param group_labels Dataframe with columns: group, group_label
## @param base_save_dir Base directory where SMM results are saved
## @param outcome Outcome variable name
## @param centering Centering method: "menses" or "ovulation"
## @param winsorized Logical: TRUE for winsorized, FALSE for unwinsorized
## @param g Number of groups
## @param date_folder Date folder (YYYYMMDD format)
## @return Path to saved file
## ============================================================================
save_group_labels <- function(
    group_labels,
    base_save_dir,
    outcome,
    centering = "menses",
    winsorized = TRUE,
    rolling_avg = "5day",
    g = 2,
    date_folder = format(Sys.Date(), "%Y%m%d")
) {
  
  # Construct path
  centering_folder <- if (centering == "ovulation") "ovulation_centered" else "menses_centered"
  winsorization_folder <- if (winsorized) "winsorized" else "unwinsorized"
  rolling_folder <- paste0("roll", rolling_avg)
  analysis_dir <- file.path(base_save_dir, "smm", date_folder, outcome, winsorization_folder, rolling_folder, centering_folder)
  
  # Ensure directory exists
  if (!dir.exists(analysis_dir)) {
    dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Save file
  output_file <- file.path(analysis_dir, glue("{outcome}_g{g}_group_labels.csv"))
  write.csv(group_labels, output_file, row.names = FALSE)
  
  cat(glue("✅ Group labels saved to: {output_file}\n"))
  return(output_file)
}


## ============================================================================
## FUNCTION 3: Load Group Labels
## ============================================================================
## Loads previously saved group label mappings from CSV file.
##
## @param base_save_dir Base directory where SMM results are saved
## @param outcome Outcome variable name
## @param centering Centering method: "menses" or "ovulation"
## @param winsorized Logical: TRUE for winsorized, FALSE for unwinsorized
## @param rolling_avg Rolling average window: "none", "3day", or "5day"
## @param g Number of groups
## @param date_folder Date folder (YYYYMMDD format)
## @return Dataframe with columns: group, group_label (or NULL if not found)
## ============================================================================
load_group_labels <- function(
    base_save_dir,
    outcome,
    centering = "menses",
    winsorized = TRUE,
    rolling_avg = "5day",
    g = 2,
    date_folder = format(Sys.Date(), "%Y%m%d")
) {
  
  # Construct path
  centering_folder <- if (centering == "ovulation") "ovulation_centered" else "menses_centered"
  winsorization_folder <- if (winsorized) "winsorized" else "unwinsorized"
  rolling_folder <- paste0("roll", rolling_avg)
  analysis_dir <- file.path(base_save_dir, "smm", date_folder, outcome, winsorization_folder, rolling_folder, centering_folder)
  
  # Check if file exists
  label_file <- file.path(analysis_dir, glue("{outcome}_g{g}_group_labels.csv"))
  
  if (!file.exists(label_file)) {
    return(NULL)
  }
  
  # Load and return
  group_labels <- read.csv(label_file, stringsAsFactors = FALSE)
  cat(glue("✅ Group labels loaded from: {label_file}\n"))
  return(group_labels)
}


## ============================================================================
## FUNCTION 4: Apply Group Labels to Class Data
## ============================================================================
## Applies group labels to the class assignment dataframe, adding a new
## column with the meaningful group names.
##
## @param class_df Dataframe with class assignments (must have 'group' column)
## @param group_labels Dataframe with columns: group, group_label
## @return Updated class_df with 'group_label' column added
## ============================================================================
apply_group_labels <- function(class_df, group_labels) {
  
  if (!"group" %in% names(class_df)) {
    stop("class_df must contain a 'group' column")
  }
  
  if (!all(c("group", "group_label") %in% names(group_labels))) {
    stop("group_labels must contain 'group' and 'group_label' columns")
  }
  
  # Join labels to class data
  class_df_labeled <- class_df %>%
    left_join(group_labels, by = "group")
  
  return(class_df_labeled)
}


## ============================================================================
## FUNCTION 5: Batch Label All Analyses
## ============================================================================
## Guides user through labeling all SMM analyses for a given configuration.
## Useful when you have multiple outcomes and want to label them all at once.
##
## @param base_save_dir Base directory where SMM results are saved
## @param outcomes Vector of outcome names to label
## @param centering Centering method: "menses" or "ovulation"
## @param winsorized Logical: TRUE for winsorized, FALSE for unwinsorized
## @param g_values Vector of group numbers to label (e.g., c(2, 3, 4))
## @param date_folder Date folder (YYYYMMDD format)
## @param interactive Logical: if TRUE, prompts for input; if FALSE, creates templates
## @return List of all label mappings
## ============================================================================
batch_label_analyses <- function(
    base_save_dir,
    outcomes,
    centering = "menses",
    winsorized = TRUE,
    rolling_avg = "5day",
    g_values = 2:5,
    date_folder = format(Sys.Date(), "%Y%m%d"),
    interactive = TRUE
) {
  
  all_labels <- list()
  
  cat("\n")
  cat("================================================================================\n")
  cat("BATCH GROUP LABELING\n")
  cat("================================================================================\n")
  cat(glue("Centering:     {centering}\n"))
  cat(glue("Winsorized:    {winsorized}\n"))
  cat(glue("Rolling Avg:   {rolling_avg}\n"))
  cat(glue("Outcomes:      {paste(outcomes, collapse=', ')}\n"))
  cat(glue("Group sizes:   {paste(g_values, collapse=', ')}\n"))
  cat(glue("Total tasks:   {length(outcomes) * length(g_values)}\n"))
  cat("================================================================================\n\n")
  
  if (interactive) {
    proceed <- readline(prompt = "Proceed with batch labeling? (y/n): ")
    if (tolower(trimws(proceed)) != "y") {
      cat("Batch labeling cancelled.\n")
      return(invisible(NULL))
    }
  }
  
  task_num <- 0
  total_tasks <- length(outcomes) * length(g_values)
  
  for (outcome in outcomes) {
    for (g in g_values) {
      task_num <- task_num + 1
      
      cat("\n")
      cat(glue("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"))
      cat(glue("TASK {task_num}/{total_tasks}: {outcome}, g={g}\n"))
      cat(glue("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"))
      
      # Check if labels already exist
      existing_labels <- load_group_labels(
        base_save_dir = base_save_dir,
        outcome = outcome,
        centering = centering,
        winsorized = winsorized,
        rolling_avg = rolling_avg,
        g = g,
        date_folder = date_folder
      )
      
      if (!is.null(existing_labels)) {
        cat("Labels already exist for this analysis:\n")
        print(existing_labels)
        
        if (interactive) {
          overwrite <- readline(prompt = "Re-label this analysis? (y/n): ")
          if (tolower(trimws(overwrite)) != "y") {
            cat("Skipping - keeping existing labels.\n")
            all_labels[[glue("{outcome}_g{g}")]] <- existing_labels
            next
          }
        }
      }
      
      # Label groups
      labels <- label_smm_groups(
        base_save_dir = base_save_dir,
        outcome = outcome,
        centering = centering,
        winsorized = winsorized,
        rolling_avg = rolling_avg,
        g = g,
        date_folder = date_folder,
        interactive = interactive
      )
      
      # Save labels
      save_group_labels(
        group_labels = labels,
        base_save_dir = base_save_dir,
        outcome = outcome,
        centering = centering,
        winsorized = winsorized,
        rolling_avg = rolling_avg,
        g = g,
        date_folder = date_folder
      )
      
      all_labels[[glue("{outcome}_g{g}")]] <- labels
    }
  }
  
  cat("\n")
  cat("================================================================================\n")
  cat("BATCH LABELING COMPLETE\n")
  cat("================================================================================\n")
  cat(glue("Labeled {task_num} analyses successfully.\n"))
  cat("All labels saved to respective analysis directories.\n")
  cat("================================================================================\n\n")
  
  return(all_labels)
}


## ============================================================================
## FUNCTION 6: Generate Label Template CSV
## ============================================================================
## Creates a CSV template file that users can fill out manually instead of
## using interactive mode. Useful for sharing labeling tasks or batch processing.
##
## @param base_save_dir Base directory where SMM results are saved
## @param outcomes Vector of outcome names
## @param centering Centering method: "menses" or "ovulation"
## @param winsorized Logical: TRUE for winsorized, FALSE for unwinsorized
## @param g_values Vector of group numbers
## @param date_folder Date folder (YYYYMMDD format)
## @param output_file Path where template CSV should be saved
## @return Path to saved template file
## ============================================================================
generate_label_template <- function(
    base_save_dir,
    outcomes,
    centering = "menses",
    winsorized = TRUE,
    g_values = 2:5,
    date_folder = format(Sys.Date(), "%Y%m%d"),
    output_file = NULL
) {
  
  # Default output file location
  if (is.null(output_file)) {
    centering_folder <- if (centering == "ovulation") "ovulation_centered" else "menses_centered"
    winsorization_folder <- if (winsorized) "winsorized" else "unwinsorized"
    output_dir <- file.path(base_save_dir, "smm", date_folder, "group_labeling_templates")
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    output_file <- file.path(output_dir, glue("labeling_template_{centering}_{winsorization_folder}.csv"))
  }
  
  # Build template dataframe
  template_rows <- list()
  
  for (outcome in outcomes) {
    for (g in g_values) {
      for (group_num in 1:g) {
        template_rows[[length(template_rows) + 1]] <- data.frame(
          outcome = outcome,
          centering = centering,
          winsorized = winsorized,
          g = g,
          group = group_num,
          group_label = "",
          notes = "",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  template_df <- do.call(rbind, template_rows)
  
  # Save template
  write.csv(template_df, output_file, row.names = FALSE)
  
  cat("================================================================================\n")
  cat("LABEL TEMPLATE GENERATED\n")
  cat("================================================================================\n")
  cat(glue("Template saved to: {output_file}\n\n"))
  cat("Instructions:\n")
  cat("1. Open the CSV file in Excel or a text editor\n")
  cat("2. Fill in the 'group_label' column for each row\n")
  cat("3. Optionally add notes in the 'notes' column\n")
  cat("4. Save the file\n")
  cat("5. Use import_labels_from_template() to apply the labels\n")
  cat("================================================================================\n\n")
  
  return(output_file)
}


## ============================================================================
## FUNCTION 7: Import Labels from Template
## ============================================================================
## Reads a completed label template CSV and saves individual label files for
## each analysis.
##
## @param template_file Path to completed template CSV
## @param base_save_dir Base directory where SMM results are saved
## @param date_folder Date folder (YYYYMMDD format)
## @return List of imported labels
## ============================================================================
import_labels_from_template <- function(
    template_file,
    base_save_dir,
    date_folder = format(Sys.Date(), "%Y%m%d")
) {
  
  if (!file.exists(template_file)) {
    stop(glue("Template file not found: {template_file}"))
  }
  
  # Read template
  template_df <- read.csv(template_file, stringsAsFactors = FALSE)
  
  # Validate required columns
  required_cols <- c("outcome", "centering", "winsorized", "g", "group", "group_label")
  missing_cols <- setdiff(required_cols, names(template_df))
  if (length(missing_cols) > 0) {
    stop(glue("Template file is missing required columns: {paste(missing_cols, collapse=', ')}"))
  }
  
  # Check for empty labels
  empty_labels <- template_df %>% filter(trimws(group_label) == "")
  if (nrow(empty_labels) > 0) {
    cat("⚠️  Warning: Found rows with empty labels:\n")
    print(empty_labels[, c("outcome", "g", "group", "group_label")])
    cat("\n")
    proceed <- readline(prompt = "Continue anyway? (y/n): ")
    if (tolower(trimws(proceed)) != "y") {
      cat("Import cancelled.\n")
      return(invisible(NULL))
    }
  }
  
  # Process each unique combination
  unique_analyses <- template_df %>%
    select(outcome, centering, winsorized, g) %>%
    distinct()
  
  imported_labels <- list()
  
  cat("\n")
  cat("================================================================================\n")
  cat("IMPORTING LABELS FROM TEMPLATE\n")
  cat("================================================================================\n")
  cat(glue("Template file: {template_file}\n"))
  cat(glue("Analyses to import: {nrow(unique_analyses)}\n"))
  cat("================================================================================\n\n")
  
  for (i in 1:nrow(unique_analyses)) {
    outcome <- unique_analyses$outcome[i]
    centering <- unique_analyses$centering[i]
    winsorized <- unique_analyses$winsorized[i]
    g <- unique_analyses$g[i]
    
    # Extract labels for this analysis
    labels_for_analysis <- template_df %>%
      filter(outcome == !!outcome,
             centering == !!centering,
             winsorized == !!winsorized,
             g == !!g) %>%
      select(group, group_label) %>%
      arrange(group)
    
    # Save labels
    save_group_labels(
      group_labels = labels_for_analysis,
      base_save_dir = base_save_dir,
      outcome = outcome,
      centering = centering,
      winsorized = winsorized,
      g = g,
      date_folder = date_folder
    )
    
    imported_labels[[glue("{outcome}_g{g}")]] <- labels_for_analysis
  }
  
  cat("\n")
  cat("================================================================================\n")
  cat("IMPORT COMPLETE\n")
  cat("================================================================================\n")
  cat(glue("Imported labels for {nrow(unique_analyses)} analyses.\n"))
  cat("================================================================================\n\n")
  
  return(imported_labels)
}
