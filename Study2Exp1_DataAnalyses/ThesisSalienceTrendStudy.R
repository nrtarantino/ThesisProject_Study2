library(tidyverse)
library(dplyr)
library(tidyr)
library(lme4)

# ---------------- Load ----------------
df <- read_csv(
  file.path(
    path.expand("~"),
    "Desktop",
    "ThesisProject_Study2",
    "DataExp1d",
    "study2d_combined.csv"
  )
)

# ---------------- Clean / recode ----------------
df <- df %>%
  mutate(
    slope = as.numeric(as.character(slope)),
    n     = as.numeric(as.character(n)),
    
    math_exp = case_when(
      math_level %in% c("Geometry", "Algebra", "Pre-calculus") ~ "below_calc",
      math_level == "1 semester of calculus"                   ~ "calc_1",
      math_level == "2 semesters of calculus"                  ~ "calc_2",
      math_level == "Beyond 2 semesters of calculus"           ~ "calc_2plus",
      TRUE ~ NA_character_
    ),
    
    math_exp = factor(
      math_exp,
      levels = c("below_calc", "calc_1", "calc_2", "calc_2plus"),
      ordered = TRUE
    ),
    
    math_num = as.numeric(math_exp),
    math_z   = as.numeric(scale(math_num))
  )

# ---------------- Training dataframe ----------------
df_train <- df %>%
  filter(trialType == "training") %>%
  mutate(
    acc = case_when(
      as.character(accuracy) == "TRUE"  ~ 1L,
      as.character(accuracy) == "FALSE" ~ 0L,
      TRUE ~ suppressWarnings(as.integer(accuracy))
    )
  )

# ---------------- Find participants below 60% on training ----------------
bad_ids <- df_train %>%
  filter(!is.na(acc)) %>%
  group_by(sonaId) %>%
  summarise(
    mean_training_acc = mean(acc, na.rm = TRUE),
    n_training_trials = n(),
    .groups = "drop"
  ) %>%
  filter(mean_training_acc < 0.60) %>%
  pull(sonaId)

bad_ids

# optional: see who got excluded
df_train %>%
  filter(!is.na(acc)) %>%
  group_by(sonaId) %>%
  summarise(
    mean_training_acc = mean(acc, na.rm = TRUE),
    n_training_trials = n(),
    .groups = "drop"
  ) %>%
  filter(mean_training_acc < 0.60) %>%
  arrange(mean_training_acc)

# ---------------- Remove excluded participants from full dataframe ----------------
df <- df %>%
  filter(!sonaId %in% bad_ids)

# ---------------- Create choice variable ----------------
df <- df %>%
  mutate(
    chose_A = case_when(
      responseCode == 1 & responseKey == "Q" ~ 1,
      responseCode == 1 & responseKey == "P" ~ 0,
      responseCode == 2 & responseKey == "P" ~ 1,
      responseCode == 2 & responseKey == "Q" ~ 0,
      TRUE ~ NA_real_
    )
  )

# ---------------- Non-training model dataframe ----------------
df_model <- df %>%
  filter(trialType != "training") %>%
  mutate(
    sonaId = factor(sonaId),
    stimulusPairing = factor(stimulusPairing),
    
    n_z = as.numeric(scale(n)),
    slope_z = as.numeric(scale(slope)),
    
    # flip slope for pairing 2 so slope meaning is aligned across pairings
    slope_z_aligned = case_when(
      stimulusPairing == "1" ~ slope_z,
      stimulusPairing == "2" ~ -slope_z,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(
    !is.na(chose_A),
    !is.na(n_z),
    !is.na(slope_z_aligned),
    !is.na(sonaId)
  )

# quick sanity checks
table(df_model$stimulusPairing, useNA = "ifany")
with(df_model, tapply(slope_z, stimulusPairing, mean, na.rm = TRUE))
with(df_model, tapply(slope_z_aligned, stimulusPairing, mean, na.rm = TRUE))

# optional sanity check: how many participants remain
length(unique(df_model$sonaId))

# ---------------- One combined model ----------------
model_combined <- glmer(
  chose_A ~ n_z + slope_z_aligned + (1 + n_z + slope_z_aligned || sonaId),
  data = df_model,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

summary(model_combined)