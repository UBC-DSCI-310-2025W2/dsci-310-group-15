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
