library(testthat)

source("../../R/extract_values.R")

# ================================================================
# Test fixtures (required inputs)
# ================================================================

# 1 — single column dataframe
extract_values_exp_1_df_1col <- data.frame(
  categories = "Action"
)

# 2 — dataframe with extra columns + different field name
extract_values_exp_2_df_2col <- data.frame(
  id = 1:3,
  genre = c("RPG", "Action", "Strategy")
)

# 3 — list of lists (all contain field)
extract_values_exp_3_nested_list <- list(
  list(categories = "Action"),
  list(categories = "RPG"),
  list(categories = "Strategy")
)

# 4 — list of lists with missing entries
extract_values_exp_4_nested_list_some_missing <- list(
  list(categories = "Action"),
  list(),                         # empty list
  list(categories = "RPG"),
  list(other_field = "Ignore")    # wrong field
)

# 5 — invalid input
extract_values_null_input <- NULL

# ================================================================
# Expected outputs
# ================================================================

ev_exp_output_1 <- c("Action")

ev_exp_output_2 <- c("RPG", "Action", "Strategy")

# when using non-existent column → should return empty vector
ev_exp_output_2_game_name <- character(0)

ev_exp_output_3 <- c("Action", "RPG", "Strategy")

ev_exp_output_4 <- c("Action", "RPG")

ev_exp_output_empty <- character(0)


# ================================================================
# Edge-case inputs
# ================================================================

# vector input (invalid)
extract_values_edge_vector_input <- c("a", "b", "c")

# empty list
extract_values_edge_0_list <- list()

# dataframe without category column
extract_values_edge_df_no_category <- data.frame(
  id = 1:3,
  name = c("a", "b", "c")
)

# list of lists without category field
extract_values_edge_list_no_category <- list(
  list(name = "Game1"),
  list(name = "Game2")
)

test_that(
    "extract_values returns a character vector of the values
    when the input is a dataframe with the specified field as a column, and 
    there is only one value",
        expect_equal(
            extract_values(extract_values_exp_1_df_1col),
            ev_exp_output_1
        )
)

test_that(
  "extract_values works for dataframe with additional columns and custom field",
{
    # default field does not exist → empty
    expect_equal(
        extract_values(extract_values_exp_2_df_2col),
        character(0)
    )

    # correct field supplied
    expect_equal(
        extract_values(extract_values_exp_2_df_2col, field_name = "genre"),
        ev_exp_output_2
    )
})

test_that(
    "when the input is a list of lists, and the specified field is contained
    within the list, and each value only appears once",
    expect_equal(
        extract_values(extract_values_exp_3_nested_list),
        ev_exp_output_3
    )
)

test_that(
    "when the input is a list of lists, but some lists do not have the
    field name, or are empty, the vector only includes the values under the correct field names",
    expect_equal(
        extract_values(extract_values_exp_4_nested_list_some_missing),
        ev_exp_output_4
        )
)

test_that(
    "when the input is not a dataframe or list of lists, is null, 
    is length 0, or does not have the category field name,
    extract_values returns character(0)",
    {
        expect_equal(
            extract_values(extract_values_null_input),
            ev_exp_output_empty
            )
        expect_equal(
                extract_values(extract_values_edge_vector_input),
                ev_exp_output_empty
            )
        expect_equal(
            extract_values(extract_values_edge_0_list),
            ev_exp_output_empty
            )
        expect_equal(
            extract_values(extract_values_edge_df_no_category),
            ev_exp_output_empty
            )
        expect_equal(
            extract_values(extract_values_edge_list_no_category),
            ev_exp_output_empty
            )
    }
)

test_that(
    "when the field_name parameter is not a string, 
    extract_values throws an error",
    expect_error(
        extract_values(extract_values_exp_1_df_1col,
        field_name = 123)
    )
)
