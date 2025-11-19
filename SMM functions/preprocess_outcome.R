library(tidyverse)
library(dplyr)


preprocess_outcome <- function(data, outcome) {
  outcome_log <- paste0(outcome, "_log")
  outcome_log_d <- paste0(outcome, "_log.d")
  
  data %>%
    mutate(id = as.factor(id)) %>%
    mutate(!!outcome_log := log(.data[[outcome]] + 1)) %>%
    group_by(id) %>%
    mutate(!!outcome_log_d := .data[[outcome_log]] - mean(.data[[outcome_log]], na.rm = TRUE)) %>%
    ungroup()
}