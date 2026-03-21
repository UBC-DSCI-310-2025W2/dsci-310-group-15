"
Preprocesses the downloaded sample file. Handles the nested data types, renames certain variables for better readability, and removes NAs.

Usage: 02_data-preprocessing.R <output_location_from_01> <output_to_location_02> <processed_table_storage>
" -> doc

library(docopt)
required_packages <- c(
  "tidyverse", "lubridate",
  "scales", "patchwork", "purrr", "janitor", "knitr"
)

invisible(lapply(required_packages, library, character.only = TRUE))

opt <- docopt(doc)

preprocess <- function(output_location_from_01, output_to_location_02, processed_table_storage){
  df <- readRDS(paste(output_location_from_01, 'games_sample.RDS', sep = ''))

  # ---- Cleaning and feature engineering ----
  extract_categories <- function(cat_obj) {
    if (is.null(cat_obj) || length(cat_obj) == 0) return(character(0))

    if (is.data.frame(cat_obj) && "description" %in% names(cat_obj)) {
      return(as.character(cat_obj$description))
    }

    if (is.list(cat_obj)) {
      vals <- purrr::map_chr(cat_obj, function(x) {
        if (is.list(x) && "description" %in% names(x)) {
          as.character(x[["description"]])
        } else {
          NA_character_
        }
      })
      return(vals[!is.na(vals)])
    }

    character(0)
  }

  flat_data <- df |>
    rename(
      game_id = appid,
      game_name = name_from_applist,
      required_age = app_details.data.required_age,
      is_free = app_details.data.is_free,
      game_type = app_details.data.type,
      release_date = app_details.data.release_date.date,
      windows_support = app_details.data.platforms.windows,
      mac_support = app_details.data.platforms.mac,
      linux_support = app_details.data.platforms.linux,
      categories = app_details.data.categories,
      dlc = app_details.data.dlc,
      demos = app_details.data.demos,
      developers = app_details.data.developers,
      publishers = app_details.data.publishers
    ) |>
    mutate(
      is_free = factor(is_free, levels = c(TRUE, FALSE), labels = c("Free", "Paid")),
      required_age = suppressWarnings(as.integer(required_age)),
      game_type = as.factor(game_type),
      release_date_parsed = suppressWarnings(mdy(release_date)),
      release_year = year(release_date_parsed),
      windows_support = as.logical(windows_support),
      mac_support = as.logical(mac_support),
      linux_support = as.logical(linux_support),
      platform_count = as.integer(replace_na(windows_support, FALSE)) +
        as.integer(replace_na(mac_support, FALSE)) +
        as.integer(replace_na(linux_support, FALSE)),
      has_dlc = purrr::map_lgl(dlc, ~ length(.x) > 0),
      has_demo = purrr::map_lgl(demos, ~ length(.x) > 0),
      category_list = purrr::map(categories, extract_categories),
      n_categories = purrr::map_int(category_list, length),
      developer_name = purrr::map_chr(
        developers,
        ~ if (length(.x) > 0) as.character(.x[[1]]) else NA_character_
      ),
      publisher_name = purrr::map_chr(
        publishers,
        ~ if (length(.x) > 0) as.character(.x[[1]]) else NA_character_
      )
    )

  all_cats <- flat_data |>
    select(game_id, category_list) |>
    unnest(category_list) |>
    rename(category = category_list)

  top_categories <- all_cats |>
    count(category, sort = TRUE) |>
    slice_head(n = 15) |>
    pull(category)

  for (cat_name in top_categories) {
    col_name <- paste0("cat_", janitor::make_clean_names(cat_name))
    flat_data[[col_name]] <- purrr::map_lgl(flat_data$category_list, ~ cat_name %in% .x)
  }

  cat_cols <- grep("^cat_", names(flat_data), value = TRUE)

  df_model <- flat_data |>
    transmute(
      game_id,
      game_name,
      is_free,
      required_age = replace_na(required_age, 0L),
      release_year = replace_na(release_year, -1L),
      game_type = fct_na_value_to_level(game_type, level = "unknown"),
      windows_support = replace_na(windows_support, FALSE),
      mac_support = replace_na(mac_support, FALSE),
      linux_support = replace_na(linux_support, FALSE),
      platform_count = replace_na(platform_count, 0L),
      has_dlc = replace_na(has_dlc, FALSE),
      has_demo = replace_na(has_demo, FALSE),
      n_categories = replace_na(n_categories, 0L),
      developer_name,
      publisher_name,
      across(all_of(cat_cols), ~ replace_na(.x, FALSE))
    ) |>
    filter(!is.na(is_free)) |>
    distinct(game_id, .keep_all = TRUE)

  cat("Modeling table:", nrow(df_model), "rows x", ncol(df_model), "columns
  ")

  write.csv(df_model, paste(processed_table_storage, 'wrangled_table.csv', sep = ''), row.names = FALSE)

  saveRDS(df_model, paste(output_to_location_02, 'wrangled_table.RDS', sep = ''))

}

preprocess(opt$output_location_from_01, opt$output_to_location_02, opt$processed_table_storage)