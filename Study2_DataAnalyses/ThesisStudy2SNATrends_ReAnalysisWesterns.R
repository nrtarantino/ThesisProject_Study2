library(dplyr)
library(readr)
library(ggplot2)
library(lme4)

df3 <- read_csv(
  file.path(
    path.expand("~"),
    "Desktop",
    "ThesisProject_Study2",
    "graphicacy_4112_participants.csv"
  )
)

df3 <- df3 %>%
  mutate(
    slope_var = as.numeric(OLS_slope),
    noise = as.numeric(standard_deviation),
    
    choose_increasing = case_when(
      dir == "down" & correct == TRUE  ~ 0L,
      dir == "down" & correct == FALSE ~ 1L,
      dir == "up"   & correct == TRUE  ~ 1L,
      dir == "up"   & correct == FALSE ~ 0L,
      TRUE ~ NA_integer_
    ),
    slope_magnitude = abs(steepness),
    n_size = factor(
      ifelse(number_points > median(number_points, na.rm = TRUE), "Many", "Few"),
      levels = c("Few", "Many")
    ),
    
    # linear centered number-of-points predictor
    n_c = scale(number_points, center = TRUE, scale = FALSE)[, 1],
    
    slope_sign = factor(
      ifelse(OLS_slope > 0, "Positive", "Negative"),
      levels = c("Negative", "Positive")
    ),
    
    slope_sign_c = ifelse(slope_sign == "Negative", -0.5, 0.5),
    
    acc = case_when(
      correct == TRUE  ~ 1L,
      correct == FALSE ~ 0L,
      TRUE ~ NA_integer_
    )
  )

## ---------------- TREND RT DATASET ----------------
df3_rt <- df3 %>%
  filter(
    !is.na(rt),
    acc == 1,
    rt > 250,
    rt < 3000
  ) %>%
  mutate(
    log_rt = log(rt)
  )

## ---------------- ACCURACY MODEL ----------------
m_acc_trend_sign <- glmer(
  acc ~ n_c * slope_sign_c + 
    (1 + n_c * slope_sign_c || ID),
  data = df3,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

summary(m_acc_trend_sign)

## ---------------- LOG RT MODEL ----------------
m_rt_trend_sign <- lmer(
  log_rt ~ n_c * slope_sign_c +
    (1 + n_c * slope_sign_c || ID),
  data = df3_rt,
  control = lmerControl(optimizer = "bobyqa")
)

summary(m_rt_trend_sign)


## Accuracy Graph
## ---------------- ACCURACY SUMMARY FOR GRAPH ----------------
trend_acc_means <- df3 %>%
  group_by(ID, slope_sign, n_size) %>%
  summarise(
    acc_person = mean(acc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(slope_sign, n_size) %>%
  summarise(
    accuracy = mean(acc_person, na.rm = TRUE),
    se = sd(acc_person, na.rm = TRUE) / sqrt(n()),
    n_participants = n(),
    .groups = "drop"
  )

trend_acc_means

## ---------------- ACCURACY GRAPH ----------------
ggplot(
  trend_acc_means,
  aes(
    x = slope_sign,
    y = accuracy,
    fill = n_size
  )
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = accuracy - se, ymax = accuracy + se),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  coord_cartesian(ylim = c(0.5, 1)) +
  labs(
    title = "Trend Accuracy",
    x = "Slope Direction",
    y = "Mean Accuracy",
    fill = "Number of Points"
  ) +
  theme_classic()

##RT Graph

## ---------------- LOG RT SUMMARY FOR GRAPH ----------------
trend_rt_means <- df3_rt %>%
  group_by(ID, slope_sign, n_size) %>%
  summarise(
    log_rt_person = mean(log_rt, na.rm = TRUE),
    rt_person = mean(rt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(slope_sign, n_size) %>%
  summarise(
    log_rt = mean(log_rt_person, na.rm = TRUE),
    rt = mean(rt_person, na.rm = TRUE),
    se_log_rt = sd(log_rt_person, na.rm = TRUE) / sqrt(n()),
    se_rt = sd(rt_person, na.rm = TRUE) / sqrt(n()),
    n_participants = n(),
    .groups = "drop"
  )

trend_rt_means

## ---------------- LOG RT GRAPH ----------------
ggplot(
  trend_rt_means,
  aes(
    x = slope_sign,
    y = log_rt,
    fill = n_size
  )
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = log_rt - se_log_rt, ymax = log_rt + se_log_rt),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    title = "Trend Reaction Time",
    x = "Slope Direction",
    y = "Mean log RT",
    fill = "Number of Points"
  ) +
  theme_classic()