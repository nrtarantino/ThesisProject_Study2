library(tidyverse)
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)

# ---------------- Load ----------------
df2 <- read_csv(
  file.path(
    path.expand("~"),
    "Desktop",
    "ThesisProject_Study2",
    "DataExp1c",
    "study2c_combined.csv"
  )
)

# ---------------- Clean / recode ----------------
df2 <- df2 %>%
  mutate(
    slope = as.numeric(as.character(slope)),
    n = as.numeric(as.character(n)),
    reactionTime = as.numeric(as.character(reactionTime)),
    acc = case_when(
      accuracy == TRUE  ~ 1L,
      accuracy == FALSE ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    math_exp = case_when(
      math_level %in% c("Geometry", "Algebra", "Pre-calculus") ~ "below_calc",
      math_level == "1 semester of calculus"                   ~ "calc_1",
      math_level == "2 semesters of calculus"                  ~ "calc_2",
      math_level == "Beyond 2 semesters of calculus"           ~ "calc_2plus",
      TRUE ~ NA_character_
    ),
    
    math_group = case_when(
      math_exp %in% c("below_calc", "calc_1") ~ "Low Math",
      math_exp %in% c("calc_2", "calc_2plus") ~ "High Math",
      TRUE ~ NA_character_
    )
  )

df2 <- df2 %>%
  mutate(
    math_group = factor(math_group, levels = c("Low Math", "High Math")),
    math_num = as.numeric(math_group),
    math_z = as.numeric(scale(math_num))
  )

df2 <- df2 %>%
  mutate(
    press_P = case_when(
      responseCode == 1 & responseKey == "P" ~ 1,
      responseCode == 1 & responseKey == "Q" ~ 0,
      responseCode == 2 & responseKey == "Q" ~ 1,  # flipped
      responseCode == 2 & responseKey == "P" ~ 0,
      TRUE ~ NA_real_
    )
  )

## FILTER PARTICIPANTS

good_trend_ids <- df2 %>%
  filter(blockName == "Trend", trialType != "training", !is.na(acc)) %>%
  group_by(sonaId) %>%
  summarise(acc = mean(acc), .groups = "drop") %>%
  filter(acc >= 0.53) %>%
  pull(sonaId)

good_number_ids <- df2 %>%
  filter(blockName == "Number", trialType != "training", !is.na(acc)) %>%
  group_by(sonaId) %>%
  summarise(acc = mean(acc), .groups = "drop") %>%
  filter(acc >= 0.53) %>%
  pull(sonaId)

##CHECK PARTICIPANTS

# Trend
df2 %>% filter(blockName == "Trend") %>% distinct(sonaId) %>% nrow()                     
length(good_trend_ids)                                                                 

# Number
df2 %>% filter(blockName == "Number") %>% distinct(sonaId) %>% nrow()                    
length(good_number_ids)                                                                 

## TREND ANALYSES


## ---------------- TREND DATASET ----------------
df_trend <- df2 %>%
  filter(
    trialType != "training",
    blockName == "Trend",
    sonaId %in% good_trend_ids,
    !is.na(press_P),
    !is.na(slope),
    !is.na(n),
    n != 15
  ) %>%
  mutate(
    n_c = scale(n, center = TRUE, scale = FALSE)[, 1],
    slope_c = scale(slope, center = TRUE, scale = FALSE)[, 1]
  )

## ---------------- TREND RT DATASET ----------------
df_trend_rt <- df_trend %>%
  filter(
    !is.na(reactionTime),
    acc == 1,
    reactionTime > 200,
    reactionTime < 3000
  ) %>%
  mutate(
    log_rt = log(reactionTime)
  )

## ---------------- NUMBER DATASET ----------------
df_number <- df2 %>%
  filter(
    trialType != "training",
    blockName == "Number",
    sonaId %in% good_number_ids,
    !is.na(press_P),
    !is.na(slope),
    !is.na(n),
    n != 15
  ) %>%
  mutate(
    n_c = scale(n, center = TRUE, scale = FALSE)[, 1],
    slope_c = scale(slope, center = TRUE, scale = FALSE)[, 1]
  )

## ---------------- NUMBER RT DATASET ----------------
df_number_rt <- df_number %>%
  filter(
    !is.na(reactionTime),
    acc == 1,
    reactionTime > 200,
    reactionTime < 3000
  ) %>%
  mutate(
    log_rt = log(reactionTime)
  )

# ---------------- Accuracy 4-cell summary ----------------
trend_4means_acc <- df_trend %>%
  mutate(
    slope_group = factor(
      if_else(slope > 0, "Positive slope", "Negative slope"),
      levels = c("Negative slope", "Positive slope")
    ),
    n_group = factor(
      if_else(n > 15, "n > 15", "n < 15"),
      levels = c("n < 15", "n > 15")
    )
  ) %>%
  group_by(sonaId, slope_group, n_group) %>%
  summarise(acc_person = mean(acc, na.rm = TRUE), .groups = "drop") %>%
  group_by(slope_group, n_group) %>%
  summarise(
    accuracy = mean(acc_person, na.rm = TRUE),
    se = sd(acc_person, na.rm = TRUE) / sqrt(n()),
    n_participants = n(),
    .groups = "drop"
  )

trend_4means_acc

# ---------------- Accuracy graph ----------------
ggplot(trend_4means_acc, aes(x = slope_group, y = accuracy, fill = n_group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = accuracy - se, ymax = accuracy + se),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Trend Block Accuracy",
    x = "Slope",
    y = "Mean Accuracy",
    fill = "Number"
  ) +
  theme_classic()

# ---------------- Accuracy model ----------------

m_choice_trend <- glmer(
  press_P ~ n_c * slope_c * math_group + (1 + n_c + slope_c || sonaId),
  data = df_trend,
  family = binomial
)

summary(m_acc_full)

# ---------------- RT 4-cell summary ----------------
trend_4means_rt <- df_trend_rt %>%
  mutate(
    slope_group = factor(
      if_else(slope > 0, "Positive slope", "Negative slope"),
      levels = c("Negative slope", "Positive slope")
    ),
    n_group = factor(
      if_else(n > 15, "n > 15", "n < 15"),
      levels = c("n < 15", "n > 15")
    )
  ) %>%
  group_by(sonaId, slope_group, n_group) %>%
  summarise(rt_person = mean(reactionTime, na.rm = TRUE), .groups = "drop") %>%
  group_by(slope_group, n_group) %>%
  summarise(
    reactionTime = mean(rt_person, na.rm = TRUE),
    se = sd(rt_person, na.rm = TRUE) / sqrt(n()),
    n_participants = n(),
    .groups = "drop"
  )

trend_4means_rt

# ---------------- RT graph ----------------
ggplot(trend_4means_rt, aes(x = slope_group, y = reactionTime, fill = n_group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = reactionTime - se, ymax = reactionTime + se),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    title = "Trend Block Reaction Time",
    x = "Slope",
    y = "Mean Reaction Time (ms)",
    fill = "Number"
  ) +
  theme_classic()

# ---------------- RT model ----------------
m_rt_ns <- lmer(
  log_rt ~ (n_centered_c + n_dist_c) * (slope_c + slope_dist_c) * math_group + (1 | sonaId),
  data = df_trend_rt
)

summary(m_rt_ns)


## NUMBER ANALYSES

# ---------------- Accuracy 4-cell summary ----------------
number_4means_acc <- df_number %>%
  mutate(
    slope_group = factor(
      if_else(slope > 0, "Positive slope", "Negative slope"),
      levels = c("Negative slope", "Positive slope")
    ),
    n_group = factor(
      if_else(n > 15, "n > 15", "n < 15"),
      levels = c("n < 15", "n > 15")
    )
  ) %>%
  group_by(sonaId, slope_group, n_group) %>%
  summarise(acc_person = mean(acc, na.rm = TRUE), .groups = "drop") %>%
  group_by(slope_group, n_group) %>%
  summarise(
    accuracy = mean(acc_person, na.rm = TRUE),
    se = sd(acc_person, na.rm = TRUE) / sqrt(n()),
    n_participants = n(),
    .groups = "drop"
  )

number_4means_acc

# ---------------- Accuracy graph ----------------
ggplot(number_4means_acc, aes(x = slope_group, y = accuracy, fill = n_group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = accuracy - se, ymax = accuracy + se),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Number Block Accuracy",
    x = "Slope",
    y = "Mean Accuracy",
    fill = "Number"
  ) +
  theme_classic()

# ---------------- Accuracy model dataset ----------------
df_number_acc_model <- df_number %>%
  mutate(
    n_c = scale(n, center = TRUE, scale = FALSE)[, 1],
    slope_c = scale(slope, center = TRUE, scale = FALSE)[, 1]
  )

# ---------------- Accuracy model ----------------
m_choice_number <- glmer(
  press_P ~ n_c * slope_c * math_group + (1 + n_c + slope_c || sonaId),
  data = df_number,
  family = binomial
)

summary(m_choice_number)

# ---------------- RT dataset for graph + model ----------------
df_number_rt <- df_number %>%
  filter(
    !is.na(reactionTime),
    acc == 1,
    reactionTime > 200,
    reactionTime < 3000
  ) %>%
  mutate(
    log_rt = log(reactionTime),
    n_c = scale(n, center = TRUE, scale = FALSE)[, 1],
    slope_c = scale(slope, center = TRUE, scale = FALSE)[, 1]
  )

# ---------------- RT 4-cell summary ----------------
number_4means_rt <- df_number_rt %>%
  mutate(
    slope_group = factor(
      if_else(slope > 0, "Positive slope", "Negative slope"),
      levels = c("Negative slope", "Positive slope")
    ),
    n_group = factor(
      if_else(n > 15, "n > 15", "n < 15"),
      levels = c("n < 15", "n > 15")
    )
  ) %>%
  group_by(sonaId, slope_group, n_group) %>%
  summarise(rt_person = mean(reactionTime, na.rm = TRUE), .groups = "drop") %>%
  group_by(slope_group, n_group) %>%
  summarise(
    reactionTime = mean(rt_person, na.rm = TRUE),
    se = sd(rt_person, na.rm = TRUE) / sqrt(n()),
    n_participants = n(),
    .groups = "drop"
  )

number_4means_rt

# ---------------- RT graph ----------------
ggplot(number_4means_rt, aes(x = slope_group, y = reactionTime, fill = n_group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = reactionTime - se, ymax = reactionTime + se),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    title = "Number Block Reaction Time",
    x = "Slope",
    y = "Mean Reaction Time (ms)",
    fill = "Number"
  ) +
  theme_classic()

# ---------------- RT model ----------------
m_rt_number <- lmer(
  log_rt ~ n_c * slope_c * math_group + (1 + n_c + slope_c || sonaId),
  data = df_number_rt
)

summary(m_rt_number)

## RT AND ACC TEST

## 4 LEVELS RT GRAPHED BY TASK

plot_4_rt <- df %>%
  filter(
    !is.na(reactionTime),
    !is.na(level),
    acc == 1,
    reactionTime > 200,
    reactionTime < 3000
  ) %>%
  group_by(blockName, level) %>%
  summarise(
    reactionTime = mean(reactionTime, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(plot_4_rt, aes(x = level, y = reactionTime, color = blockName)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(breaks = 1:4, limits = c(1, 4)) +
  theme_classic() +
  labs(
    x = "Difficulty Level (1 = hardest, 4 = easiest)",
    y = "Reaction Time (ms)",
    color = "Task"
  )