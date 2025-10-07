library(dplyr)

data <- readRDS("C:/Users/robin/OneDrive/Desktop/clear3daily_oura.rds")



#make variable groups
impulsivity_vars <- c(
  "actedwothink",
  "imp_trouble",
  "imp_regret",
  "DRSP12_appoverate",
  "eat_binge",
  "eat_purge",
  "NSSIyn"
)

irrangag_vars <- c(
  "BITe1",
  "BITe2",
  "BITe3",
  "BITe4",
  "BITe5",
  "DRSP5_moodswings",
  "DRSP7b_irritable",
  "DRSP7a_angry",
  "DRSP17_outofcontrol",
  "BAM1",
  "BAM2_stirredupscream",
  "BAM3",
  "DRSP8_intconflict",
  "stress_1"
)

happy_vars <- c(
  "PANAS_happy",
  "belonging_",
  "mastery"
)

cog_vars <- c(
  "forgetful",
  "distractable",
  "DRSP10_diffconc",
  "RNT_FEEL",
  "RNT_repetitiveness",
  "RNT_uncontrollable",
  "angrum"
)

si_vars <- c(
  "ASIQ9_wishdead",
  "wishsleep",
  "ASIQ2_thoughtkill",
  "ASIQ16_thoughtways",
  "ASIQ4_thoughtwhen",
  "ASIQ3_thoughthow",
  "wantedkill",
  "wantedNSSI",
  "ASIQ19_lifenotworth",
  "ASIQ1_betternotalive",
  "ASIQ25_notbetterkill",
  "ASIQ17_thoughtkillnotdo",
  "urgekill",
  "immrisk_concplan",
  "immrisk_intendkill",
  "immrisk_capablesafe",
  "immrisk_someoneknows"
)

move_vars <- c(
  "cal_active",
  "cal_total",
  "daily_movement",
  "inactivity_alerts",
  "Oura_Activity_steps",
  "non_wear",
  "rest",
  "inactive",
  "low",
  "medium",
  "high",
  "average_met",
  "met_min_inactive",
  "met_min_low",
  "met_min_medium",
  "met_min_high"
)


# move_vars <- c(
#   "cal_active",
#   "cal_total",
#   "daily_movement",
#   "inactivity_alerts",
#   "Oura_Activity_steps",
#   "non_wear",
#   "rest",
#   "inactive",
#   "low",
#   "medium",
#   "high",
#   "average_met",
#   "met_min_inactive",
#   "met_min_low",
#   "met_min_medium",
#   "met_min_high",
#   "target_calories",
#   "target_km",
#   "target_miles",
#   "score.activity",
#   "score_stay_active",
#   "score_move_every_hour",
#   "score_meet_daily_targets",
#   "score_training_frequency",
#   "score_training_volume",
#   "score_recovery_time"
# )



# Custom mode function
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Helper function to make one tidy table per variable group
summarize_vars <- function(data, vars) {
  data %>%
    select(all_of(vars)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    summarise(
      mean   = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      mode   = get_mode(value),
      sd     = sd(value, na.rm = TRUE),
      min    = min(value, na.rm = TRUE),
      max    = max(value, na.rm = TRUE),
      range  = max(value, na.rm = TRUE) - min(value, na.rm = TRUE),
      .groups = "drop"
    )
}

# Generate one table per variable group
impulsivity_summary <- summarize_vars(data, impulsivity_vars)
irrangag_summary <- summarize_vars(data, irrangag_vars)
happy_summary <- summarize_vars(data, happy_vars)
si_summary <- summarize_vars(data, si_vars)
move_summary <- summarize_vars(data, move_vars)




#CORELLOGRAMS
library(ggcorrplot)

##IRRMOVE

# Combine the two variable groups
irr_move_vars <- c(irrangag_vars, move_vars)

# Select those columns from your dataset
irr_move_data <- data %>%
  select(all_of(irr_move_vars))

# Compute correlation matrix (pairwise to handle missing data)
corr_matrix <- cor(irr_move_data, use = "pairwise.complete.obs")

# Plot correlogram
ggcorrplot(
  corr_matrix,
  method = "circle",        # "square" also nice
  type = "lower",           # show lower triangle
  lab = TRUE,               # add correlation values
  lab_size = 3,
  colors = c("red", "white", "blue"),
  title = "Correlogram: Irritability and Movement Variables",
  tl.cex = 10
)


# Combine the two variable groups
irr_move_vars <- c(irrangag_vars, move_vars)

# Select those columns from your dataset
irr_move_data <- data %>%
  select(all_of(irr_move_vars))

# Compute correlation matrix (pairwise to handle missing data)
corr_matrix <- cor(irr_move_data, use = "pairwise.complete.obs")

# Plot correlogram
ggcorrplot(
  corr_matrix,
  method = "circle",        # "square" also nice
  type = "lower",           # show lower triangle
  lab = TRUE,               # add correlation values
  lab_size = 3,
  colors = c("red", "white", "blue"),
  title = "Correlogram: Irritability and Movement Variables",
  tl.cex = 10
)

##impmove

# Combine the two variable groups
imp_move_vars <- c(impulsivity_vars, move_vars)

# Select those columns from your dataset
imp_move_data <- data %>%
  select(all_of(imp_move_vars))

# Compute correlation matrix (pairwise to handle missing data)
corr_matriximp <- cor(imp_move_data, use = "pairwise.complete.obs")

# Plot correlogram
ggcorrplot(
  corr_matriximp,
  method = "circle",        # "square" also nice
  type = "lower",           # show lower triangle
  lab = TRUE,               # add correlation values
  lab_size = 3,
  colors = c("red", "white", "blue"),
  title = "Correlogram: Impulsivity and Movement Variables",
  tl.cex = 10
)


##simove

# Combine the two variable groups
si_move_vars <- c(si_vars, move_vars)

# Select those columns from your dataset
si_move_data <- data %>%
  select(all_of(si_move_vars))

# Compute correlation matrix (pairwise to handle missing data)
corr_matrixsi <- cor(si_move_data, use = "pairwise.complete.obs")

# Plot correlogram
ggcorrplot(
  corr_matrixsi,
  method = "circle",        # "square" also nice
  type = "lower",           # show lower triangle
  lab = TRUE,               # add correlation values
  lab_size = 3,
  colors = c("red", "white", "blue"),
  title = "Correlogram: SI and Movement Variables",
  tl.cex = 10
)






##cogmove

# Combine the two variable groups
cog_move_vars <- c(cog_vars, move_vars)

# Select those columns from your dataset
cog_move_data <- data %>%
  select(all_of(cog_move_vars))

# Compute correlation matrix (pairwise to handle missing data)
corr_matrixcog <- cor(cog_move_data, use = "pairwise.complete.obs")

# Plot correlogram
ggcorrplot(
  corr_matrixcog,
  method = "circle",        # "square" also nice
  type = "lower",           # show lower triangle
  lab = TRUE,               # add correlation values
  lab_size = 3,
  colors = c("red", "white", "blue"),
  title = "Correlogram: Cog and Movement Variables",
  tl.cex = 10
)



# Combine the two variable groups
hap_move_vars <- c(happy_vars, move_vars)

# Select those columns from your dataset
hap_move_data <- data %>%
  select(all_of(hap_move_vars))

# Compute correlation matrix (pairwise to handle missing data)
corr_matrixhap <- cor(hap_move_data, use = "pairwise.complete.obs")

# Plot correlogram
ggcorrplot(
  corr_matrixhap,
  method = "circle",        # "square" also nice
  type = "lower",           # show lower triangle
  lab = TRUE,               # add correlation values
  lab_size = 3,
  colors = c("red", "white", "blue"),
  title = "Correlogram: Happy and Movement Variables",
  tl.cex = 10
)


