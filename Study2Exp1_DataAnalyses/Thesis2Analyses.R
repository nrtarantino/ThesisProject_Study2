library(tidyverse)

df <- df %>%
  mutate(
    slope = as.numeric(as.character(slope)),
    n     = as.numeric(as.character(n)),
    acc   = as.integer(accuracy)   # TRUE/FALSE -> 1/0
  )

# ---------------- Trend ----------------
trend <- df %>%
  filter(blockName == "Trend") %>%
  mutate(
    level = as.numeric(factor(slope, levels = sort(unique(slope)))),
    resp  = if_else((slope > 0 & acc == 1) | (slope < 0 & acc == 0), 1L, 0L)
  )

fit_t <- glm(resp ~ level, data = trend, family = binomial)
pred_t <- tibble(level = seq(1, 8, length.out = 300),
                 p = predict(fit_t, newdata = tibble(level = seq(1, 8, length.out = 300)), type = "response"))

pts_t <- trend %>% group_by(level) %>% summarise(p = mean(resp), .groups="drop")

ggplot() +
  geom_point(data = pts_t, aes(level, p), size = 3) +
  geom_line(data = pred_t, aes(level, p)) +
  scale_x_continuous(breaks = 1:8) + ylim(0,1) +
  theme_classic() +
  labs(title="Trend", x="Level (1–8)", y="P(positive)")

# -------------- Big/Small --------------
bs <- df %>%
  filter(blockName == "Big/Small") %>%
  mutate(
    level = as.numeric(factor(n, levels = sort(unique(n)))),
    resp  = if_else((n > 15 & acc == 1) | (n < 15 & acc == 0), 1L, 0L)
  )

fit_b <- glm(resp ~ level, data = bs, family = binomial)
pred_b_levels <- seq(1, 8, length.out = 300)
pred_b <- tibble(level = pred_b_levels,
                 p = predict(fit_b, newdata = tibble(level = pred_b_levels), type = "response"))

pts_b <- bs %>% group_by(level) %>% summarise(p = mean(resp), .groups="drop")

ggplot() +
  geom_point(data = pts_b, aes(level, p), size = 3) +
  geom_line(data = pred_b, aes(level, p)) +
  scale_x_continuous(breaks = 1:8) + ylim(0,1) +
  theme_classic() +
  labs(title="Big/Small", x="Level (1–8)", y="P(big)")