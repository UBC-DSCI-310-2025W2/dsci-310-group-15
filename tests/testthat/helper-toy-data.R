# Synthetic helper data for tests only.
# These fixtures intentionally avoid the original dataset to keep tests
# deterministic, lightweight, and rubric-compliant.

make_toy_class_df <- function() {
  data.frame(
    is_free = factor(c("Free", "Paid", "Free"), levels = c("Free", "Paid"))
  )
}

make_toy_class_counts <- function() {
  data.frame(
    target_class = factor(c("Free", "Paid"), levels = c("Free", "Paid")),
    n = c(10, 5),
    pct = c(2 / 3, 1 / 3),
    label = c("10 (66.7%)", "5 (33.3%)")
  )
}

make_toy_numeric_df <- function() {
  data.frame(
    is_free = factor(c("Free", "Paid", "Free"), levels = c("Free", "Paid")),
    required_age = c(0, 18, 13),
    release_year = c(2020, 2019, -1),
    platform_count = c(3, 1, 2),
    n_categories = c(6, 2, 4)
  )
}

make_toy_numeric_long <- function() {
  data.frame(
    target_class = factor(c("Free", "Free", "Paid", "Paid")),
    predictor = c("required_age", "platform_count", "required_age", "platform_count"),
    value = c(0, 3, 18, 1)
  )
}

make_toy_target_summary_df <- function() {
  data.frame(
    is_free = factor(c("Free", "Paid", "Free", "Paid", "Free", "Paid"), levels = c("Free", "Paid")),
    release_year = c(2020, 2020, 2021, 2021, -1, 2022),
    has_dlc = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE),
    has_demo = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  )
}

make_toy_category_df <- function() {
  data.frame(
    game_id = 1:6,
    is_free = factor(c("Free", "Paid", "Free", "Paid", "Free", "Paid"), levels = c("Free", "Paid")),
    cat_single_player = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE),
    cat_multi_player = c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE)
  )
}

make_toy_model_df <- function() {
  data.frame(
    row_id = seq_len(20),
    is_free = factor(rep(c("Free", "Paid"), each = 10), levels = c("Free", "Paid")),
    required_age = rep(c(0, 13), times = 10),
    release_year = rep(2015:2019, times = 4),
    game_type = factor(rep("game", 20)),
    windows_support = TRUE,
    mac_support = rep(c(TRUE, FALSE), times = 10),
    linux_support = FALSE,
    platform_count = rep(c(1, 2), times = 10),
    has_dlc = rep(c(TRUE, FALSE), times = 10),
    has_demo = rep(c(FALSE, TRUE), times = 10),
    n_categories = rep(1:4, times = 5),
    cat_single_player = rep(c(TRUE, FALSE), times = 10)
  )
}
