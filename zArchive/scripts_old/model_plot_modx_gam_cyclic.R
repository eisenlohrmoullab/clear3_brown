library(tidyverse)  # Includes dplyr, ggplot2
library(mgcv)
library(gamm4)
library(zoo)



model_plot_modx_gam_cyclic <- function(
    data,
    outcome,
    time_var,
    smm_result,
    centering = "menses",
    save_dir,
    k_smooth = 10, 
    log_var = TRUE,
    show_CI = TRUE,
    winsorized = TRUE,
    rolling_avg = "5day"
) {
  library(glue)
  library(dplyr)
  library(ggplot2)
  library(mgcv)
  
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  
  if (!time_var %in% names(data)) {
    stop(glue("Time variable '{time_var}' not found in data!"))
  }
  
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
  
  
  ## Determine variable suffix
  var_suffix <- if (log_var) "_log" else ""
  full_outcome <- paste0(outcome, var_suffix)
  
  ## ð¹ Baseline model without group
  base_formula <- as.formula(glue(
    "{full_outcome} ~ s(id, bs = 're') + s({time_var}, id, bs = c('re', 'cc')) + s({time_var}, bs = 'cc')"
  ))
  
  base_fit <- mgcv::gam(
    formula = base_formula,
    data = data,
    method = "REML"
  )
  
  # Save baseline model and summary
  if (!is.null(sub_dir)) {
    saveRDS(base_fit, file = file.path(sub_dir, glue("{full_outcome}_noGroup_GAM_model.rds")))
    writeLines(capture.output(summary(base_fit)), file.path(sub_dir, glue("{full_outcome}_noGroup_GAM_summary.txt")))
  }
  
  # Iterate over all group sizes in smm result
  all_g <- names(smm_result$all_results)
  
  for (g in all_g) {
    message(glue(">> Processing grouping for g = {g}"))
    
    # 1ï¸✓£ Extract class assignments
    class_df <- smm_result$all_results[[g]]$class
    class_df <- unique(class_df)
    
    # 2ï¸✓£ Merge onto main data
    data_with_group <- data %>%
      dplyr::select(id, all_of(c(outcome, paste0(outcome, "_log"), full_outcome, time_var))) %>%
      left_join(class_df, by = "id") %>%
      filter(!is.na(group)) %>%
      rename(smm_group = group)
    
    # Make sure smm_group is a factor
    data_with_group$smm_group <- as.factor(data_with_group$smm_group)
    
    # 3ï¸✓£ Fit GAM
    gam_formula <- as.formula(glue(
      "{full_outcome} ~ s(id, bs = 're') + s({time_var}, id, bs = c('re', 'cc')) + smm_group + s({time_var}, by = smm_group, bs = c('cc'))"
    ))
    
    gam_fit <- mgcv::gam(
      formula = gam_formula,
      data = data_with_group,
      method = "REML"
    )
    
    # 4ï¸✓£ Save model as RDS
    if (!is.null(sub_dir)){
      saveRDS(gam_fit, file = file.path(sub_dir, glue("{full_outcome}_g{g}_GAM_model.rds")))
    }
    
    # 5ï¸✓£ Save summary as TXT
    summ_text <- capture.output(summary(gam_fit))
    if (!is.null(sub_dir)){
      writeLines(summ_text, con = file.path(sub_dir, glue("{full_outcome}_g{g}_GAM_summary.txt")))
    }
    
    # 6ï¸✓£ Build prediction grid
    group_levels = sort(unique(data_with_group$smm_group))
    pred_grid <- expand.grid(
      temp_time = seq(-1, 1, length.out = 100),
      smm_group = factor(group_levels, levels = group_levels),
      id = 0
    )
    names(pred_grid)[names(pred_grid) == "temp_time"] <- time_var
    
    # 7ï¸✓£ Predictions
    pred <- marginaleffects::predictions(
      gam_fit,
      newdata = pred_grid,
      type = "response",
      transform = if (log_var) function(x) exp(x) - 1 else NULL
    )
    
    pred_grid$estimate <- pred$estimate
    pred_grid$conf.low <- pred$conf.low
    pred_grid$conf.high <- pred$conf.high
    
   
    # 8ï¸✓£ Build prediction plot
    x_breaks <- seq(-1, 1, by = 0.5)
    x_labels <- if (centering == "menses") {
      c("Ovulation", "50%L", "Menses Onset", "50%F", "Ovulation")
    } else {
      c("Menses Onset", "50%F", "Ovulation", "50%L", "Menses Onset")
    }
    
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
    
    group_n <- data_with_group %>%
      group_by(smm_group) %>%
      summarise(n_id = n_distinct(id), .groups = "drop") %>%
      mutate(
        group_label = paste0("Group ", smm_group, " (N=", n_id, ")"),
        smm_group = as.factor(smm_group)
      )
    
    # Merge into prediction grid
    pred_grid <- pred_grid %>%
      left_join(group_n, by = "smm_group")
    
    p <- ggplot(pred_grid, aes_string(x = time_var, y = "estimate", color = "group_label")) +
      # Shading rectangles ✓ fix fill by setting it outside aes()
      geom_rect(
        data = rect_data,
        inherit.aes = FALSE,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        fill = rect_data$fill,
        alpha = 0.2,
        color = "white",
        show.legend = FALSE
      ) +
      # Confidence ribbons ✓ match group color or set distinct color scale
      geom_ribbon(
        aes(ymin = conf.low, ymax = conf.high, fill = group_label),
        alpha = if (show_CI) 0.2 else 0,
        color = NA,
        show.legend = FALSE
      ) +
      geom_line(linewidth = 0.9) +
      # scale_color_manual(values = group_colors) +
      # scale_fill_manual(values = group_colors) +
      scale_x_continuous(
        limits = c(-1, 1),
        breaks = x_breaks,
        labels = x_labels
      ) +
      labs(
        x = "",
        y = glue("{base_outcome}"),
        title = glue("Model-Implied Curves for {base_outcome} with Group Moderator (g={g})"),
        color = "Group",
        fill = "Group"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    
    # 9ï¸✓£ Save prediction plot
    if (!is.null(sub_dir)){
      ggsave(
        filename = file.path(sub_dir, glue("{full_outcome}_g{g}_GAM_plot.png")),
        plot = p,
        width = 8,
        height = 6,
        dpi = 300
      )
    }
    
    if (!is.null(sub_dir)){
      ggsave(
        filename = file.path(sub_dir, glue("{full_outcome}_g{g}_GAM_plot.jpeg")),
        plot = p,
        width = 8,
        height = 6,
        dpi = 300
      )
    }
    
    if (!is.null(sub_dir)){
      ggsave(
        filename = file.path(sub_dir, glue("{full_outcome}_g{g}_GAM_plot.svg")),
        plot = p,
        width = 8,
        height = 6,
        dpi = 300
      )
    }
    message(glue("✓ Done with g = {g}. Files saved in {save_dir}"))
  }
  
  message("✓✓ All group GAMs processed and saved!")
  
  ## Return
  return(invisible(NULL))
}
