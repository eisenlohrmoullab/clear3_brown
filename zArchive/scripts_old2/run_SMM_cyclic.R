library(tidyverse)  # Includes dplyr, ggplot2, tidyr
library(mgcv)
library(gamm4)
library(zoo)
library(glue)


run_smm_cyclic <- function(
    data,
    outcome,
    time_var,
    g = 2,
    d_inter = 20,
    plot = TRUE,
    seed = 123,
    centering = "menses", 
    save_dir = NULL,
    k_smooth = 10, 
    log_var = T,
    winsorized = TRUE,
    rolling_avg = "5day"
) {
  
  set.seed(seed)
  `%>%` <- dplyr::`%>%`
  
  ## --------- create nested folder structure: smm / date / outcome / winsorization / rolling_avg / centering ----------
  if (!is.null(save_dir)) {
    date_folder <- format(Sys.Date(), "%Y%m%d")
    centering_folder <- if (centering == "ovulation") "ovulation_centered" else "menses_centered"
    winsorization_folder <- if (winsorized) "winsorized" else "unwinsorized"
    rolling_folder <- paste0("roll", rolling_avg)
    
    # Extract base outcome name (remove _win suffix if present)
    base_outcome <- sub("_win$", "", outcome)
    
    # Final path: save_dir/smm/YYYYMMDD/base_outcome/winsorization_folder/rolling_folder/centering_folder
    sub_dir <- file.path(save_dir, "smm", date_folder, base_outcome, winsorization_folder, rolling_folder, centering_folder)
    
    dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)
  } else {
    sub_dir <- NULL
  }
  
  
  ## Determine variable suffix
  var_suffix <- if (log_var) "_log.d" else ".d"
  full_outcome <- paste0(outcome, var_suffix)
  
  ## ========= MULTI-g WRAPPER ========= ##
  if (length(g) > 1) {
    message(glue(">> Running multiple group sizes: {paste(g, collapse=', ')}"))
    all_results <- list()
    bic_table <- data.frame(groups = integer(), BIC = numeric())
    for (gval in g) {
      message(glue(">>> Fitting SMM for g = {gval} <<<"))
      res <- run_smm_cyclic(
        data = data, outcome = outcome, time_var = time_var, g = gval, d_inter = d_inter,
        plot = plot, seed = seed, centering = centering, save_dir = save_dir,
        k_smooth = k_smooth, log_var = log_var, winsorized = winsorized, rolling_avg = rolling_avg
      )
      all_results[[as.character(gval)]] <- res
      best_BIC <- min(res$BIC, na.rm = TRUE)
      bic_table <- rbind(bic_table, data.frame(groups = gval, BIC = best_BIC))
      
      ## save outputs for this g
      if (!is.null(sub_dir)) {
        write.csv(unique(res$class),
                  file = file.path(sub_dir, glue("{outcome}_g{gval}_class.csv")),
                  row.names = FALSE)
        plots_list <- res$plots
        if (!is.null(plots_list$plot_roll)) {
          ggsave(file.path(sub_dir, glue("{outcome}_g{gval}_roll.png")),
                 plots_list$plot_roll, width = 7, height = 5)
        }
        if (!is.null(plots_list$plot_centered)) {
          ggsave(file.path(sub_dir, glue("{outcome}_g{gval}_centered.png")),
                 plots_list$plot_centered, width = 7, height = 5)
        }
        if (!is.null(plots_list$plot_mean)) {
          ggsave(file.path(sub_dir, glue("{outcome}_g{gval}_mean.png")),
                 plots_list$plot_mean, width = 7, height = 5)
        }
      }
    }
    if (!is.null(sub_dir)) {
      write.csv(bic_table,
                file = file.path(sub_dir, glue("{outcome}_bic_table.csv")),
                row.names = FALSE)
    }
    return(list(all_results = all_results, bic_table = bic_table))
  }
  
  ## ========= SINGLE-g CASE ========= ##
  message(glue(">>> Fitting SMM for g = {g} <<<"))
  
  if (!time_var %in% names(data)) {
    stop(glue("Time variable {time_var} not found in data!"))
  }
  
  ## Subset columns
  # Include both log-transformed and non-log-transformed versions
  outcome_cols <- c(outcome, 
                    paste0(outcome, ".d"), 
                    paste0(outcome, ".d.roll"), 
                    paste0(outcome, "_log"), 
                    paste0(outcome, "_log.d"),
                    paste0(outcome, "_log.d.roll"))
  keep_cols <- intersect(c("id", time_var, outcome_cols), names(data))
  df <- dplyr::select(data, dplyr::all_of(keep_cols))
  
  ## knots for cyclic spline (assumes time spans -1..1)
  knots_list <- setNames(list(c(-1, 1)), time_var)
  
  ## K-means initialization on individual smooths
  grid <- setNames(data.frame(seq(-1, 1, length.out = 20)), time_var)
  id_vec <- unique(df$id)
  trajectory_matrix <- matrix(NA, nrow = length(id_vec), ncol = 20)
  
  for (i in seq_along(id_vec)) {
    person_data <- df %>% dplyr::filter(id == id_vec[i])
    if (nrow(person_data) >= 10) {
      fit <- tryCatch(
        mgcv::gam(
          as.formula(glue("{full_outcome} ~ s({time_var}, k = {k_smooth})")),
          data = person_data
        ),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        preds <- predict(fit, newdata = grid)
        trajectory_matrix[i, ] <- preds
      }
    }
  }
  
  ## Valid IDs
  valid_rows <- which(rowSums(is.na(trajectory_matrix)) < ncol(trajectory_matrix))
  trajectory_matrix <- trajectory_matrix[valid_rows, ]
  id_vec <- id_vec[valid_rows]
  
  if (length(id_vec) < g) {
    stop(glue("Not enough participants with valid trajectories for g = {g}. Needed {g}, got {length(id_vec)}."))
  }
  
  # Use raw trajectory_matrix (already person-centered from GAM predictions)
  # No additional transformations - cluster based on within-person trajectory shapes only
  km_res <- kmeans(trajectory_matrix, centers = g, nstart = 25)
  cluster_map <- data.frame(id = id_vec, group = as.character(km_res$cluster))
  df <- dplyr::left_join(df, cluster_map, by = "id")
  
  ## ========================================================================
  ## EXPECTATION-MAXIMIZATION (EM) ALGORITHM
  ## ========================================================================
  ## This iteratively assigns participants to groups and refits models until
  ## group memberships stabilize. We track log-likelihood (LLK) and BIC to
  ## assess model fit, and save final RSS values and models for calculating
  ## group membership probabilities.
  ## ========================================================================
  
  LLK <- numeric(d_inter)               # Log-likelihood for each iteration
  BIC <- numeric(d_inter)               # Bayesian Information Criterion for each iteration
  final_resid_df <- NULL                # Final RSS values (used for probability calculation)
  final_models <- NULL                  # Final fitted models (used for variance calculation)
  
  for (i in 1:d_inter) {
    ## --------------------------------------------------------------------
    ## E-STEP: Fit group-specific models
    ## --------------------------------------------------------------------
    ## For each group, fit a GAMM to model the cyclical pattern.
    ## Then predict all participants' data using each group's model.
    ## --------------------------------------------------------------------
    
    models <- list()
    preds <- matrix(NA, nrow = nrow(df), ncol = g)
    
    for (k in 1:g) {
      dat_k <- df %>% dplyr::filter(group == k)
      
      # Safety check: Need at least 2 participants per group
      if (dplyr::n_distinct(dat_k$id) < 2) {
        warning(glue("Group {k} has <2 participants. Stopping."))
        return(NULL)
      }
      
      # Fit GAMM: cyclic smooth + random effects for participants
      models[[k]] <- gamm4::gamm4(
        as.formula(glue("{full_outcome} ~ s({time_var}, bs = 'cc', k = {k_smooth})")),
        random = as.formula(glue("~ (1 + {time_var} | id)")),
        data = dat_k,
        na.action = na.omit,
        knots = knots_list
      )
      
      # Predict all participants' data using this group's model
      preds[, k] <- predict(models[[k]]$gam, newdata = df)
    }
    
    ## --------------------------------------------------------------------
    ## M-STEP: Reassign participants based on model fit
    ## --------------------------------------------------------------------
    ## Calculate RSS (residual sum of squares) for each participant with
    ## each group's model. Assign each participant to the group with the
    ## lowest RSS (best fit).
    ## --------------------------------------------------------------------
    
    # Calculate squared residuals for each participant-group combination
    resids <- (df[[full_outcome]] - preds)^2
    
    # Sum residuals by participant for each group
    resid_sums <- lapply(1:g, function(k) {
      stats::aggregate(resids[, k], by = list(id = df$id), FUN = sum, na.rm = TRUE)
    })
    resid_df <- Reduce(function(x, y) merge(x, y, by = "id"), resid_sums)
    colnames(resid_df)[-1] <- paste0("RSS_group", 1:g)
    
    # Assign each participant to group with lowest RSS
    group_map <- data.frame(
      id = resid_df$id,
      group = apply(resid_df[, -1], 1, which.min)
    )
    df <- dplyr::left_join(df %>% dplyr::select(-group), group_map, by = "id")
    
    ## --------------------------------------------------------------------
    ## Calculate model fit metrics
    ## --------------------------------------------------------------------
    
    # Log-likelihood and BIC for this iteration
    logliks <- sapply(models, function(m) logLik(m$mer))
    LLK[i] <- sum(logliks)
    n_params <- sum(sapply(models, function(m) sum(summary(m$gam)$edf) + 1))
    BIC[i] <- -2 * LLK[i] + log(nrow(df)) * n_params
    
    # Store final values for probability calculation (after last iteration)
    final_resid_df <- resid_df
    final_models <- models
    
    ## --------------------------------------------------------------------
    ## Early stopping condition
    ## --------------------------------------------------------------------
    
    if (any(table(df$group) <= 20)) {
      warning("At least one group has <=20 observations. Stopping early.")
      break
    }
  }
  
  ## ========================================================================
  ## GROUP MEMBERSHIP PROBABILITY CALCULATION
  ## ========================================================================
  ## Uses log-likelihood + softmax to calculate statistically rigorous
  ## probabilities of group membership. This method incorporates both:
  ## 1) How well each participant's data fits each group's model (RSS)
  ## 2) How variable each group is (σ²)
  ##
  ## This is more accurate than simple inverse RSS because it accounts for
  ## the fact that a more variable group shouldn't be penalized just for
  ## being more heterogeneous.
  ## ========================================================================
  
  rss_cols <- paste0("RSS_group", 1:g)
  rss_matrix <- as.matrix(final_resid_df[, rss_cols])
  
  ## ------------------------------------------------------------------------
  ## Step 1: Calculate group-specific variance (σ²)
  ## ------------------------------------------------------------------------
  ## For each group, calculate how much variability there is around the
  ## group's trajectory. This represents within-group heterogeneity.
  ## ------------------------------------------------------------------------
  
  group_variances <- numeric(g)
  n_obs_per_group <- numeric(g)
  
  for (k in 1:g) {
    # Get all observations from participants currently in this group
    dat_k <- df %>% dplyr::filter(group == k)
    
    # Predict their data using this group's model
    pred_k <- predict(final_models[[k]]$gam, newdata = dat_k)
    
    # Calculate residuals (observed - predicted)
    resid_k <- dat_k[[full_outcome]] - pred_k
    
    # Calculate variance: σ²_k = Σ(residuals²) / n
    n_obs <- length(resid_k)
    group_variances[k] <- sum(resid_k^2, na.rm = TRUE) / n_obs
    n_obs_per_group[k] <- n_obs
  }
  
  ## ------------------------------------------------------------------------
  ## Step 2: Calculate person-specific log-likelihood
  ## ------------------------------------------------------------------------
  ## For each participant, calculate how likely their data is under each
  ## group's model. This uses the statistical formula:
  ##
  ##   log L_ik = -n_i/2 * log(σ²_k) - RSS_ik / (2 * σ²_k)
  ##
  ## Where:
  ##   - n_i = number of observations for person i
  ##   - σ²_k = variance of group k (calculated above)
  ##   - RSS_ik = residual sum of squares for person i in group k
  ##
  ## Higher log-likelihood = better fit to that group
  ## ------------------------------------------------------------------------
  
  # Count observations per participant
  n_obs_per_person <- df %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(n_obs = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(id)
  
  # Ensure alignment with RSS dataframe
  n_obs_per_person <- final_resid_df %>%
    dplyr::select(id) %>%
    dplyr::left_join(n_obs_per_person, by = "id")
  
  # Calculate log-likelihood for each person-group pair
  log_lik_matrix <- matrix(NA, nrow = nrow(rss_matrix), ncol = g)
  
  for (k in 1:g) {
    # Use max() to avoid issues with very small variances
    sigma2_k <- max(group_variances[k], 1e-10)
    
    # Log-likelihood formula (constant term omitted as it cancels in softmax)
    log_lik_matrix[, k] <- -0.5 * n_obs_per_person$n_obs * log(sigma2_k) - 
                           rss_matrix[, k] / (2 * sigma2_k)
  }
  
  ## ------------------------------------------------------------------------
  ## Step 3: Convert log-likelihoods to probabilities (softmax)
  ## ------------------------------------------------------------------------
  ## The softmax function converts log-likelihoods to probabilities that
  ## sum to 1 for each participant. Formula:
  ##
  ##   P(group k | person i) = exp(log L_ik) / Σ_j exp(log L_ij)
  ##
  ## For numerical stability, we use the "log-sum-exp trick": subtract the
  ## maximum log-likelihood before exponentiating. This prevents overflow
  ## without changing the relative probabilities.
  ## ------------------------------------------------------------------------
  
  # Subtract max log-likelihood per person (numerical stability)
  log_lik_max <- apply(log_lik_matrix, 1, max)
  log_lik_centered <- log_lik_matrix - log_lik_max
  
  # Apply softmax: exponentiate and normalize
  exp_log_lik <- exp(log_lik_centered)
  prob_matrix <- exp_log_lik / rowSums(exp_log_lik)
  colnames(prob_matrix) <- paste0("prob_group", 1:g)
  
  # Create output dataframe with group assignments and probabilities
  # - id: participant identifier
  # - group: assigned group (highest probability)
  # - prob_group1, prob_group2, ...: probability of belonging to each group
  final_class <- data.frame(
    id = final_resid_df$id,
    group = apply(prob_matrix, 1, which.max)
  )
  final_class <- cbind(final_class, prob_matrix)
  
  ## ---------- Legend labels with group sizes ----------
  grp_counts <- df %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      n = dplyr::n(),
      N = dplyr::n_distinct(id),
      .groups = "drop"
    )
  # merge counts so we can build a labeled factor
  df <- df %>%
    dplyr::left_join(grp_counts, by = "group") %>%
    dplyr::mutate(
      group_lab = factor(
        paste0(group, " (N=", N, ", n=", n, ")"),
        levels = paste0(
          sort(unique(group)),
          " (N=", grp_counts$N[match(sort(unique(group)), grp_counts$group)],
          ", n=", grp_counts$n[match(sort(unique(group)), grp_counts$group)],
          ")"
        )
      )
    )
  
  
  ## Plotting
  p_roll <- p_centered <- p_mean <- NULL
  if (plot) {
    df <- df %>%
      dplyr::mutate(
        cycleday_perc = (.data[[time_var]] + 1) / 2,
        cycleday_5perc = round(cycleday_perc / 0.05) * 0.05
      )
    
    x_breaks <- seq(0, 1, by = 0.05)
    x_labels <- if (centering == "menses") {
      c("Ovulation", rep("", 4), "50%L", rep("", 4), "Menses Onset", rep("", 4), "50%F", rep("", 4), "Ovulation")
    } else {
      c("Menses Onset", rep("", 4), "50%F", rep("", 4), "Ovulation", rep("", 4), "50%L", rep("", 4), "Menses Onset")
    }
    
    # Determine the correct rolling variable name based on what's available
    roll_var <- if (paste0(outcome, "_log.d.roll") %in% names(df)) {
      paste0(outcome, "_log.d.roll")
    } else if (paste0(outcome, ".d.roll") %in% names(df)) {
      paste0(outcome, ".d.roll")
    } else {
      stop(glue("Neither {outcome}_log.d.roll nor {outcome}.d.roll found in data"))
    }
    
    summary_roll <- df %>%
      dplyr::group_by(group_lab, cycleday_5perc) %>%
      dplyr::summarise(
        mean_roll = mean(.data[[roll_var]], na.rm = TRUE),
        se = sd(.data[[roll_var]], na.rm = TRUE) /
          sqrt(sum(!is.na(.data[[roll_var]]))),
        .groups = "drop"
      ) 
    
    summary_roll <- dplyr::bind_rows(
      summary_roll,
      summary_roll %>%
        dplyr::filter(cycleday_5perc == 1) %>%
        dplyr::mutate(cycleday_5perc = 0)
    )
    
    # Determine the correct centered variable name based on what's available
    centered_var <- if (paste0(outcome, "_log.d") %in% names(df)) {
      paste0(outcome, "_log.d")
    } else if (paste0(outcome, ".d") %in% names(df)) {
      paste0(outcome, ".d")
    } else {
      stop(glue("Neither {outcome}_log.d nor {outcome}.d found in data"))
    }
    
    summary_centered <- df %>%
      dplyr::group_by(group_lab, cycleday_5perc) %>%
      dplyr::summarise(
        mean_centered = mean(.data[[centered_var]], na.rm = TRUE),
        se = sd(.data[[centered_var]], na.rm = TRUE) /
          sqrt(sum(!is.na(.data[[centered_var]]))),
        .groups = "drop"
      ) 
    
    summary_centered <- dplyr::bind_rows(
      summary_centered,
      summary_centered %>%
        dplyr::filter(cycleday_5perc == 1) %>%
        dplyr::mutate(cycleday_5perc = 0)
    )
    
    summary_mean <- df %>%
      dplyr::group_by(group_lab, cycleday_5perc) %>%
      dplyr::summarise(
        mean_outcome = mean(.data[[outcome]], na.rm = TRUE),
        se = sd(.data[[outcome]], na.rm = TRUE) /
          sqrt(sum(!is.na(.data[[outcome]]))),
        .groups = "drop"
      ) 
    
    summary_mean <- dplyr::bind_rows(
      summary_mean,
      summary_mean %>%
        dplyr::filter(cycleday_5perc == 1) %>%
        dplyr::mutate(cycleday_5perc = 0)
    )
    
    p_roll <- ggplot(summary_roll, aes(x = cycleday_5perc, y = mean_roll, color = group_lab, fill = group_lab)) +
      geom_line(linewidth = 0.9) +
      geom_ribbon(aes(ymin = mean_roll - se, ymax = mean_roll + se), alpha = 0.2, color = NA, show.legend = FALSE) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      labs(title = paste(outcome, "Rolling Avg (G =", g, ")"), x = "Percentage of Phase Elapsed", y = "Rolling Mean Centered", color = "Group") +
      theme_minimal()
    
    p_centered <- ggplot(summary_centered, aes(x = cycleday_5perc, y = mean_centered, color = group_lab, fill = group_lab)) +
      geom_line(linewidth = 0.9) +
      geom_ribbon(aes(ymin = mean_centered - se, ymax = mean_centered + se), alpha = 0.2, color = NA, show.legend = FALSE) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      labs(title = paste(outcome, ": Person-Centered (G =", g, ")"), x = "Percentage of Phase Elapsed", y = "Mean Centered", color = "Group") +
      theme_minimal()
    
    p_mean <- ggplot(summary_mean, aes(x = cycleday_5perc, y = mean_outcome, color = group_lab, fill = group_lab)) +
      geom_line(linewidth = 0.9) +
      geom_ribbon(aes(ymin = mean_outcome - se, ymax = mean_outcome + se), alpha = 0.2, color = NA, show.legend = FALSE) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      labs(title = paste(outcome, ": Mean (G =", g, ")"), x = "Percentage of Phase Elapsed", y = "Mean Outcome", color = "Group") +
      theme_minimal()
    
    print(p_roll); print(p_centered); print(p_mean)
    
    ## Save plots if requested
    if (!is.null(sub_dir)) {
      ggsave(file.path(sub_dir, glue("{outcome}_g{g}_roll.png")),     p_roll,     width = 7, height = 5)
      ggsave(file.path(sub_dir, glue("{outcome}_g{g}_centered.png")), p_centered, width = 7, height = 5)
      ggsave(file.path(sub_dir, glue("{outcome}_g{g}_mean.png")),     p_mean,     width = 7, height = 5)
      # also save class assignments
      write.csv(unique(final_class), file = file.path(sub_dir, glue("{outcome}_g{g}_class.csv")), row.names = FALSE)
      # and BIC trace for this g
      write.csv(data.frame(iter = seq_along(BIC), BIC = BIC),
                file = file.path(sub_dir, glue("{outcome}_g{g}_BIC_trace.csv")),
                row.names = FALSE)
    }
  }
  
  return(list(
    class = final_class,
    probabilities = data.frame(id = final_resid_df$id, prob_matrix),
    LLK = LLK,
    BIC = BIC,
    summary = df %>%
      dplyr::distinct(id, .keep_all = TRUE) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(
        n = dplyr::n(),
        mean_outcome = mean(.data[[outcome]], na.rm = TRUE),
        sd_outcome = sd(.data[[outcome]], na.rm = TRUE),
        .groups = "drop"
      ),
    plots = list(
      plot_roll = p_roll,
      plot_centered = p_centered,
      plot_mean = p_mean
    )
  ))
}