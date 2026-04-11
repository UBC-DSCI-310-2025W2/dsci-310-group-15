#' Build a model-ready Steam games table
#'
#' Flattens selected nested Steam metadata fields, engineers the target and
#' predictor columns used in the analysis, one-hot encodes the most common
#' category labels, and removes rows without a target label.
#'
#' @param games_data Raw games data frame loaded from `games_sample.RDS`.
#' @param top_n_categories Number of category indicators to create.
#'
#' @return A data frame suitable for modeling.
build_modeling_table <- function(games_data, top_n_categories = 15L) {
  if (!is.data.frame(games_data)) {
    stop("`games_data` must be a data frame.", call. = FALSE)
  }

  if (!is.numeric(top_n_categories) || length(top_n_categories) != 1L ||
      is.na(top_n_categories) || top_n_categories < 1) {
    stop("`top_n_categories` must be a single positive number.", call. = FALSE)
  }

  required_columns <- c(
    "appid",
    "name_from_applist",
    "app_details.data.required_age",
    "app_details.data.is_free",
    "app_details.data.type",
    "app_details.data.release_date.date",
    "app_details.data.platforms.windows",
    "app_details.data.platforms.mac",
    "app_details.data.platforms.linux",
    "app_details.data.categories",
    "app_details.data.dlc",
    "app_details.data.demos",
    "app_details.data.developers",
    "app_details.data.publishers"
  )
  validate_required_columns(games_data, required_columns, "games_data")

  flat_data <- games_data |>
    dplyr::rename(
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
    dplyr::mutate(
      is_free = factor(is_free, levels = c(TRUE, FALSE), labels = c("Free", "Paid")),
      required_age = suppressWarnings(as.integer(required_age)),
      game_type = as.factor(game_type),
      release_date_parsed = suppressWarnings(lubridate::mdy(release_date)),
      release_year = lubridate::year(release_date_parsed),
      windows_support = as.logical(windows_support),
      mac_support = as.logical(mac_support),
      linux_support = as.logical(linux_support),
      platform_count = as.integer(tidyr::replace_na(windows_support, FALSE)) +
        as.integer(tidyr::replace_na(mac_support, FALSE)) +
        as.integer(tidyr::replace_na(linux_support, FALSE)),
      has_dlc = purrr::map_lgl(dlc, ~ length(.x) > 0L),
      has_demo = purrr::map_lgl(demos, ~ length(.x) > 0L),
      category_list = purrr::map(categories, extract_values, field_name = "description"),
      n_categories = purrr::map_int(category_list, length),
      developer_name = purrr::map_chr(
        developers,
        ~ if (length(.x) > 0L) as.character(.x[[1]]) else NA_character_
      ),
      publisher_name = purrr::map_chr(
        publishers,
        ~ if (length(.x) > 0L) as.character(.x[[1]]) else NA_character_
      )
    )

  all_categories <- flat_data |>
    dplyr::select(game_id, category_list) |>
    tidyr::unnest(category_list) |>
    dplyr::rename(category = category_list)

  top_categories <- all_categories |>
    dplyr::count(category, sort = TRUE) |>
    dplyr::slice_head(n = as.integer(top_n_categories)) |>
    dplyr::pull(category)

  for (category_name in top_categories) {
    column_name <- paste0("cat_", janitor::make_clean_names(category_name))
    flat_data[[column_name]] <- purrr::map_lgl(
      flat_data$category_list,
      ~ category_name %in% .x
    )
  }

  category_columns <- grep("^cat_", names(flat_data), value = TRUE)

  flat_data |>
    dplyr::transmute(
      game_id,
      game_name,
      is_free,
      required_age = tidyr::replace_na(required_age, 0L),
      release_year = tidyr::replace_na(release_year, -1L),
      game_type = forcats::fct_na_value_to_level(game_type, level = "unknown"),
      windows_support = tidyr::replace_na(windows_support, FALSE),
      mac_support = tidyr::replace_na(mac_support, FALSE),
      linux_support = tidyr::replace_na(linux_support, FALSE),
      platform_count = tidyr::replace_na(platform_count, 0L),
      has_dlc = tidyr::replace_na(has_dlc, FALSE),
      has_demo = tidyr::replace_na(has_demo, FALSE),
      n_categories = tidyr::replace_na(n_categories, 0L),
      developer_name,
      publisher_name,
      dplyr::across(dplyr::all_of(category_columns), ~ tidyr::replace_na(.x, FALSE))
    ) |>
    dplyr::filter(!is.na(is_free)) |>
    dplyr::distinct(game_id, .keep_all = TRUE)
}

#' Run the Steam data preprocessing workflow
#'
#' @param input_data_dir Directory containing `games_sample.RDS`.
#' @param output_data_dir Directory where `wrangled_table.RDS` should be saved.
#' @param table_output_dir Directory where `wrangled_table.csv` should be saved.
#' @param input_filename Input RDS filename under `input_data_dir`.
#' @param top_n_categories Number of category indicators to create.
#' @param force Whether to rebuild and overwrite outputs even when they already
#' exist and are newer than the input file.
#'
#' @return A named list containing the modeling table and output paths.
run_data_preprocessing <- function(
    input_data_dir,
    output_data_dir,
    table_output_dir,
    input_filename = "games_sample.RDS",
    top_n_categories = 15L,
    force = TRUE) {
  assert_single_string(input_data_dir, "input_data_dir")
  assert_single_string(output_data_dir, "output_data_dir")
  assert_single_string(table_output_dir, "table_output_dir")
  assert_single_string(input_filename, "input_filename")

  ensure_directory_exists(output_data_dir, "output_data_dir")
  ensure_directory_exists(table_output_dir, "table_output_dir")

  input_path <- build_file_path(input_data_dir, input_filename)
  if (!file.exists(input_path)) {
    stop(sprintf("Input file not found: %s", input_path), call. = FALSE)
  }

  rds_path <- build_file_path(output_data_dir, "wrangled_table.RDS")
  csv_path <- build_file_path(table_output_dir, "wrangled_table.csv")
  games_data <- load_validated_rds_data_frame(
    input_path,
    data_name = "games_sample"
  )

  validate_raw_games_data(games_data, stage = "raw_source")

  needs_refresh <- force ||
    !file.exists(rds_path) ||
    !file.exists(csv_path) ||
    file.info(input_path)$mtime > file.info(rds_path)$mtime ||
    file.info(input_path)$mtime > file.info(csv_path)$mtime

  if (needs_refresh) {
    modeling_table <- build_modeling_table(games_data, top_n_categories = top_n_categories)
  } else {
    modeling_table <- load_validated_rds_data_frame(
      rds_path,
      data_name = "wrangled_table"
    )
  }
  validate_modeling_table(
    modeling_table,
    stage = "modeling_table",
    expected_category_count = top_n_categories
  )

  if (needs_refresh) {
    saveRDS(modeling_table, rds_path)
    utils::write.csv(modeling_table, csv_path, row.names = FALSE)
  }

  message("Modeling table: ", nrow(modeling_table), " rows x ", ncol(modeling_table), " columns")
  
  list(
    modeling_table = modeling_table,
    rds_path = rds_path,
    csv_path = csv_path
  )
}
