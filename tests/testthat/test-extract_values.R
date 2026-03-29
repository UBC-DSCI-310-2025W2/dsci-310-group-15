library(testthat)

source("../../R/extract_values.R")

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
    "extract_values returns the vector of the values when the input is a dataframe
    with additional columns, and that the function works 
    with a different than default field_name",
    {
        expect_equal(
            extract_values(extract_values_exp_2_df_2col),
            ev_exp_output_2
        )
        expect_equal(
            extract_values(extract_values_exp_2_df_2col, field_name = "game_name"),
            ev_exp_output_2_game_name
        )
    }
)

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
