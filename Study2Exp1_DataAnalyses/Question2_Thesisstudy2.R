library(tidyverse)
library(dplyr)
library(ggplot2)

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
    n     = as.numeric(as.character(n)),
    acc   = case_when(
      accuracy == TRUE  ~ 1L,
      accuracy == FALSE ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# ---------------- Filter training + Trend only ----------------
df_trend <- df2 %>%
  filter(
    trialType != "training",
    blockName == "Trend",
    !is.na(acc),
    !is.na(slope),
    !is.na(n),
    n != 15
  )

# ---------------- Remove participants below 60% training accuracy ----------------
good_ids <- df2 %>%
  mutate(
    acc = case_when(
      accuracy == TRUE  ~ 1L,
      accuracy == FALSE ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(trialType == "training", !is.na(acc)) %>%
  group_by(sonaId) %>%
  summarise(training_acc = mean(acc, na.rm = TRUE), .groups = "drop") %>%
  filter(training_acc >= 0.60) %>%
  pull(sonaId)

df_trend <- df_trend %>%
  filter(sonaId %in% good_ids)

# ---------------- Make 4 conditions ----------------
trend_4means <- df_trend %>%
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

trend_4means

# ---------------- Graph ----------------
ggplot(trend_4means, aes(x = slope_group, y = accuracy, fill = n_group)) +
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