library(tidyverse)
library(readr)
library(dplyr)
library(purrr)

# ---------------- Load ----------------
df <- read_csv(
  file.path(
    path.expand("~"),
    "Desktop",
    "ThesisProject_Study2",
    "DataExp1a",
    "study2_combined.csv"
  )
)

# ---------------- Remove training trials ----------------
df <- df %>%
  filter(trialType != "training")

# ---------------- Clean / recode ----------------
df <- df %>%
  mutate(
    slope = as.numeric(as.character(slope)),
    n     = as.numeric(as.character(n)),
    
    # math experience coding (from the existing math_level column)
    math_exp = case_when(
      math_level %in% c("Geometry", "Algebra", "Pre-calculus") ~ "below_calc",
      math_level == "1 semester of calculus"                  ~ "calc_1",
      math_level == "2 semesters of calculus"                 ~ "calc_2",
      math_level == "Beyond 2 semesters of calculus"          ~ "calc_2plus",
      TRUE ~ NA_character_
    ),
    
    math_exp = factor(
      math_exp,
      levels  = c("below_calc", "calc_1", "calc_2", "calc_2plus"),
      ordered = TRUE
    ),
    
    math_num = as.numeric(math_exp),
    
    # effect coding + centering
    math_c = case_when(
      math_exp == "below_calc" ~ -1.5,
      math_exp == "calc_1"     ~ -0.5,
      math_exp == "calc_2"     ~  0.5,
      math_exp == "calc_2plus" ~  1.5,
      TRUE ~ NA_real_
    ),
    
    math_c = math_c - mean(math_c, na.rm = TRUE),
    
    acc = case_when(
      as.character(accuracy) == "TRUE"  ~ 1L,
      as.character(accuracy) == "FALSE" ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  group_by(blockName) %>%
  mutate(
    level_8 = if_else(
      blockName == "Trend",
      as.numeric(factor(slope, levels = sort(unique(slope)))),
      as.numeric(factor(n,     levels = sort(unique(n))))
    ),
    level = 5 - pmin(level_8, 9 - level_8)
  ) %>%
  ungroup()

# ---------------- Drop participants below chance on any block ----------------
ids_keep <- df %>%
  filter(!is.na(acc)) %>%
  group_by(sonaId, blockName) %>%
  summarise(
    n_correct = sum(acc),
    n_trials  = n(),
    above_chance = ifelse(
      n_trials > 0,
      binom.test(n_correct, n_trials, p = 0.5, alternative = "greater")$p.value < 0.05,
      FALSE
    ),
    .groups = "drop"
  ) %>%
  group_by(sonaId) %>%
  summarise(keep = all(above_chance), .groups = "drop") %>%
  filter(keep) %>%
  pull(sonaId)

df_cleaned <- df %>%
  filter(sonaId %in% ids_keep)

df_cleaned %>%
  summarise(n_participants = n_distinct(sonaId))
# ---------------- Descriptive mean comparison ----------------
df_cleaned %>%
  group_by(blockName) %>%
  summarise(mean_accuracy = mean(acc, na.rm = TRUE), .groups = "drop")

# ---------------- Discriminability model ----------------
m_discriminability <- glm(acc ~ blockName * level, data = df_cleaned, family = binomial)
anova(m_discriminability, test = "Chisq")

# ---------------- Psychometric curve: Trend ----------------
trend <- df_cleaned %>%
  filter(blockName == "Trend", !is.na(acc), !is.na(slope), slope != 0) %>%
  mutate(
    level = as.numeric(factor(slope, levels = sort(unique(slope)))),
    resp  = if_else((slope > 0 & acc == 1) | (slope < 0 & acc == 0), 1L, 0L)
  )

fit_t <- glm(resp ~ level, data = trend, family = binomial)
pred_t_levels <- seq(1, 8, length.out = 300)
pred_t <- tibble(
  level = pred_t_levels,
  p = predict(fit_t, newdata = tibble(level = pred_t_levels), type = "response")
)
pts_t <- trend %>% group_by(level) %>% summarise(p = mean(resp), .groups = "drop")

ggplot() +
  geom_point(data = pts_t, aes(level, p), size = 3) +
  geom_line(data = pred_t, aes(level, p)) +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic() +
  labs(title = "Trend", x = "Level (1–8)", y = "P(positive)")

# ---------------- Psychometric curve: Big/Small ----------------
bs <- df_cleaned %>%
  filter(blockName == "Big/Small", !is.na(acc), !is.na(n), n != 15) %>%
  mutate(
    level = as.numeric(factor(n, levels = sort(unique(n)))),
    resp  = if_else((n > 15 & acc == 1) | (n < 15 & acc == 0), 1L, 0L)
  )

fit_b <- glm(resp ~ level, data = bs, family = binomial)
pred_b_levels <- seq(1, 8, length.out = 300)
pred_b <- tibble(
  level = pred_b_levels,
  p = predict(fit_b, newdata = tibble(level = pred_b_levels), type = "response")
)
pts_b <- bs %>% group_by(level) %>% summarise(p = mean(resp), .groups = "drop")

ggplot() +
  geom_point(data = pts_b, aes(level, p), size = 3) +
  geom_line(data = pred_b, aes(level, p)) +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic() +
  labs(title = "Big/Small", x = "Level (1–8)", y = "P(big)")

# ---------------- 8-level Trend vs Big/Small accuracy ----------------
bs_plot <- df_cleaned %>%
  filter(blockName == "Big/Small", !is.na(acc), !is.na(n), n != 15) %>%
  mutate(level = as.numeric(factor(n, levels = sort(unique(n))))) %>%
  group_by(level) %>%
  summarise(
    x = level,
    accuracy = mean(acc, na.rm = TRUE),
    task = "Big/Small",
    .groups = "drop"
  )

trend_plot <- df_cleaned %>%
  filter(blockName == "Trend", !is.na(acc), !is.na(slope), slope != 0) %>%
  mutate(level = as.numeric(factor(slope, levels = sort(unique(slope))))) %>%
  group_by(level) %>%
  summarise(
    x = level,
    accuracy = mean(acc, na.rm = TRUE),
    task = "Trend",
    .groups = "drop"
  )

plot_df <- bind_rows(bs_plot, trend_plot)

ggplot(plot_df, aes(x, accuracy, color = task)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(breaks = 1:8, limits = c(1, 8)) +
  scale_y_continuous(limits = c(0.4, 1), breaks = seq(0.4, 1, by = 0.05)) +
  theme_classic() +
  labs(x = "Level (1–8)", y = "Accuracy", color = NULL)


# ---------------- 16-bin Trend vs Big/Small accuracy ----------------
bs_plot <- df_cleaned %>%
  filter(blockName == "Big/Small", !is.na(acc), !is.na(n), n != 15) %>%
  mutate(level = as.numeric(factor(n, levels = sort(unique(n))))) %>%
  group_by(level) %>%
  summarise(
    x = level,
    accuracy = mean(acc, na.rm = TRUE),
    task = "Big/Small",
    .groups = "drop"
  )

trend_plot <- df_cleaned %>%
  filter(blockName == "Trend", !is.na(observed_slope), !is.na(acc)) %>%
  mutate(bin = ntile(observed_slope, 16)) %>%
  group_by(bin) %>%
  summarise(
    accuracy = mean(acc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    x = 1 + (bin - 1) * (7 / 15),   # map 1..16 -> 1..8
    task = "Trend (16 bins)"
  )

plot_df <- bind_rows(bs_plot, trend_plot)

ggplot(plot_df, aes(x, accuracy, color = task)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(breaks = 1:8, limits = c(1, 8)) +
  scale_y_continuous(limits = c(0.4, 1), breaks = seq(0.4, 1, by = 0.05)) +
  theme_classic() +
  labs(x = "Level (1–8)", y = "Accuracy", color = NULL)

## Table for the observed slope bins and respective accuracies ##

df_cleaned %>%
  filter(blockName == "Trend", !is.na(observed_slope), !is.na(acc)) %>%
  mutate(bin = ntile(observed_slope, 16)) %>%
  group_by(bin) %>%
  summarise(
    min_slope = min(observed_slope),
    mean_slope = mean(observed_slope),
    max_slope = max(observed_slope),
    mean_accuracy = mean(acc)
  )