library(tidyverse)  # Includes dplyr, ggplot2, tidyr
library(mgcv)
library(gamm4)
library(zoo)
library(glue)


compare_bic_cyclic <- function(
    data,
    outcome,
    time_var,
    centering = "menses",
    smm_results,
    k_smooth = 10,
    save_dir = NULL,
    winsorized = TRUE,
    rolling_avg = "5day"
) {

  # Extract base outcome name (remove _win suffix if present) for folder/file naming
  base_outcome <- sub("_win$", "", outcome)

  ## --------- choose subfolder by centering for all saves ----------
  if (!is.null(save_dir)) {
    date_folder <- format(Sys.Date(), "%Y%m%d")
    centering_folder <- if (centering == "ovulation") "ovulation_centered" else "menses_centered"
    winsorization_folder <- if (winsorized) "winsorized" else "unwinsorized"
    rolling_folder <- paste0("roll", rolling_avg)
    
    # Final path: save_dir/smm/YYYYMMDD/base_outcome/winsorization_folder/rolling_folder/centering_folder
    sub_dir <- file.path(save_dir, "smm", date_folder, base_outcome, winsorization_folder, rolling_folder, centering_folder)
    
    dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)
  } else {
    sub_dir <- NULL
  }
  
  
  
  if (!time_var %in% names(data)) {
    stop(glue("Time variable '{time_var}' not found in data!"))
  }
  
  message(glue(">> Computing baseline (g=1) model for outcome = {base_outcome}, centering = {centering}"))
  
  ## knots for cyclic spline (assumes time spans -1..1)
  knots_list <- setNames(list(c(-1, 1)), time_var)
  
  ## Fit single-group model
  formula_g1 <- as.formula(glue("{outcome}_log.d ~ s({time_var},  bs = 'cc', k = {k_smooth})"))
  
  gamm_fit_g1 <- gamm4(
    formula_g1,
    random = as.formula(glue("~ (1 + {time_var} | id)")),
    data = data,
    na.action = na.omit, 
    knots = knots_list
  )
  
  # Compute log-likelihood, edf, and BIC
  llk_1group <- logLik(gamm_fit_g1$mer)
  df_1group <- sum(summary(gamm_fit_g1$gam)$edf) + 1
  n <- nrow(data)
  bic_1group <- -2 * llk_1group + log(n) * df_1group
  bic_1group_val <- as.numeric(bic_1group)
  message(glue(">> Baseline BIC (g=1): {round(bic_1group_val, 2)}"))
  
  ## Extract BIC values from smm_results
  if (is.null(smm_results$bic_table)) {
    stop("smm_results does not include bic_table. Did you pass the correct object?")
  }
  
  smm_table <- smm_results$bic_table %>% arrange(groups)
  
  ## Combine into one comparison table
  all_bic_table <- data.frame(
    groups = c(1, smm_table$groups),
    BIC = c(bic_1group_val, smm_table$BIC)
  )
  
  ## Plot
  bic_plot <- ggplot(all_bic_table, aes(x = groups, y = BIC)) +
    geom_line() + 
    geom_point(size = 2) +
    labs(
      title = glue("{base_outcome} SMM ({centering}-centered) Comparison by BIC"),
      x = "Number of Groups",
      y = "Minimum BIC"
    ) +
    scale_x_continuous(
      breaks = seq(min(all_bic_table$groups), max(all_bic_table$groups), by = 1)
    ) +
    theme_minimal()
  
  print(bic_plot)
  
  ## Save outputs if directory specified
  if (!is.null(sub_dir)) {
    
    # Build base filename
    base_name <- glue("{base_outcome}_{centering}_bic_comparison")
    
    # Save CSV
    csv_path <- file.path(sub_dir, paste0(base_name, ".csv"))
    write.csv(all_bic_table, csv_path, row.names = FALSE)
    message(glue("✓ Saved BIC comparison table: {csv_path}"))
    
    # Save plot
    png_path <- file.path(sub_dir, paste0(base_name, ".png"))
    ggsave(png_path, plot = bic_plot, width = 7, height = 5)
    message(glue("✓ Saved BIC comparison plot: {png_path}"))
  }
  
  ## Return both
  return(list(
    bic_table = all_bic_table,
    bic_plot = bic_plot
  ))
}
