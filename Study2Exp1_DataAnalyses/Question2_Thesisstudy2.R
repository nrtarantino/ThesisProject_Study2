library(tidyverse)

# ---------------- Load ----------------
df2 <- read_csv(
  file.path(
    path.expand("~"),
    "Desktop",
    "ThesisProject_Study2",
    "study2cb_combined.csv"
  )
)

# ---------------- Remove training trials ----------------
df2 <- df2 %>%
  filter(trialType != "training")

# ---------------- Clean / recode ----------------
df2 <- df2 %>%
  mutate(
    slope = as.numeric(as.character(slope)),
    n     = as.numeric(as.character(n)),
    
    # math experience coding
    math_exp = case_when(
      math_level %in% c("Algebra", "Pre-calculus")            ~ "below_calc",
      math_level == "1 semester of calculus"                  ~ "calc_1",
      math_level == "2 semesters of calculus"                 ~ "calc_2",
      math_level == "Beyond 2 semesters of calculus"          ~ "calc_2plus",
      TRUE                                                    ~ NA_character_
    ),
    
    math_exp = factor(
      math_exp,
      levels = c("below_calc", "calc_1", "calc_2", "calc_2plus"),
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
    
    acc = case_when(
      accuracy == TRUE  ~ 1L,
      accuracy == FALSE ~ 0L,
      TRUE ~ NA_integer_
    )
  )

library(dplyr)
library(ggplot2)

acc_by_block_math <- df2 %>%
  filter(blockName %in% c("Trend", "Number"), !is.na(math_exp)) %>%
  
  # participant-level accuracy
  group_by(sonaId, math_exp, blockName) %>%
  summarise(acc_person = mean(acc, na.rm = TRUE), .groups = "drop") %>%
  
  # average across participants
  group_by(math_exp, blockName) %>%
  summarise(
    accuracy = mean(acc_person, na.rm = TRUE),
    se = sd(acc_person, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )