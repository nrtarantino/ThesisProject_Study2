library(tidyverse)

# ---------------- Load ----------------
df <- read_csv(
  file.path(
    path.expand("~"),
    "Desktop",
    "ThesisProject_Study2",
    "DataExp1c",
    "study2c_combined.csv"
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
    
    # math experience coding
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
      as.numeric(factor(n, levels = sort(unique(n))))
    ),
    level = 5 - pmin(level_8, 9 - level_8)
  ) %>%
  ungroup()

df %>%
  summarise(n_participants = n_distinct(sonaId))

## ACC by participant ##

df %>%
  group_by(sonaId) %>%
  summarise(mean_accuracy = mean(acc, na.rm = TRUE))


# ---------------- Remove participants below chance (p < .05 vs 50%) ----------------
threshold <- 275 / 512   # ≈ 0.537

keep_ids <- df %>%
  group_by(sonaId) %>%
  summarise(mean_acc = mean(acc, na.rm = TRUE), .groups = "drop") %>%
  filter(mean_acc >= threshold) %>%
  pull(sonaId)

df <- df %>%
  filter(sonaId %in% keep_ids)

# ---------------- Descriptive mean comparison ----------------
df %>%
  group_by(blockName) %>%
  summarise(mean_accuracy = mean(acc, na.rm = TRUE), .groups = "drop")

# ---------------- Psychometric curve: Trend ----------------
trend <- df %>%
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

# ---------------- P3sychometric curve: Number ----------------
num <- df %>%
  filter(blockName == "Number", !is.na(acc), !is.na(n), n != 15) %>%
  mutate(
    level = as.numeric(factor(n, levels = sort(unique(n)))),
    resp  = if_else((n > 15 & acc == 1) | (n < 15 & acc == 0), 1L, 0L)
  )

fit_n <- glm(resp ~ level, data = num, family = binomial)

pred_n_levels <- seq(1, 8, length.out = 300)

pred_n <- tibble(
  level = pred_n_levels,
  p = predict(fit_n, newdata = tibble(level = pred_n_levels), type = "response")
)

pts_n <- num %>%
  group_by(level) %>%
  summarise(p = mean(resp), .groups = "drop")

ggplot() +
  geom_point(data = pts_n, aes(level, p), size = 3) +
  geom_line(data = pred_n, aes(level, p)) +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic() +
  labs(title = "Number", x = "Level (1–8)", y = "P(big)")

# ---------------- 8-level Trend vs Number accuracy ----------------

num_plot <- df %>%
  filter(blockName == "Number", !is.na(acc), !is.na(n), n != 15) %>%
  mutate(level = as.numeric(factor(n, levels = sort(unique(n))))) %>%
  group_by(level) %>%
  summarise(
    accuracy = mean(acc, na.rm = TRUE),
    task = "Number",
    .groups = "drop"
  )

trend_plot <- df %>%
  filter(blockName == "Trend", !is.na(acc), !is.na(slope), slope != 0) %>%
  mutate(level = as.numeric(factor(slope, levels = sort(unique(slope))))) %>%
  group_by(level) %>%
  summarise(
    accuracy = mean(acc, na.rm = TRUE),
    task = "Trend",
    .groups = "drop"
  )

plot_df <- bind_rows(num_plot, trend_plot)

ggplot(plot_df, aes(x = level, y = accuracy, color = task)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(breaks = 1:8, limits = c(1, 8)) +
  scale_y_continuous(limits = c(0.4, 1), breaks = seq(0.4, 1, by = 0.05)) +
  theme_classic() +
  labs(x = "Level (1–8)", y = "Accuracy", color = NULL)

# ---------------- Participant accuracy at each 4-level difficulty ----------------
participant_acc_4levels <- df %>%
  filter(!is.na(acc), !is.na(level)) %>%
  group_by(sonaId, level) %>%
  summarise(mean_accuracy = mean(acc, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = level,
    values_from = mean_accuracy,
    names_prefix = "level_"
  ) %>%
  arrange(sonaId)

participant_acc_4levels

##to delete participants
df <- df %>%
  filter(!sonaId %in% c(91618, 96305, 12345))

## 4 levels graphed by task

plot_4 <- df %>%
  filter(!is.na(acc), !is.na(level)) %>%
  group_by(blockName, level) %>%
  summarise(
    accuracy = mean(acc, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(plot_4, aes(x = level, y = accuracy, color = blockName)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(breaks = 1:4, limits = c(1, 4)) +
  scale_y_continuous(limits = c(0.4, 1), breaks = seq(0.4, 1, by = 0.05)) +
  theme_classic() +
  labs(
    x = "Difficulty Level (1 = hardest, 4 = easiest)",
    y = "Accuracy",
    color = "Task"
  )

## Model to test

m_4level <- glm(acc ~ blockName * level,
                data = df,
                family = binomial)

anova(m_4level, test = "Chisq")

## Participants per block

df %>%
  distinct(sonaId, blockName) %>%
  count(blockName)