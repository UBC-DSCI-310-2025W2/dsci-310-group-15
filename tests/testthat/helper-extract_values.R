# reproducible code to generate simple test data 
# for the extract values function
library(purrr)
library(tibble)

#expected case 1, dataframe with one column of field name, with one value
extract_values_exp_1_df_1col <- data.frame(
    description = "Co-op"
)

ev_exp_output_1 <- c("Co-op")

#expected case 2, dataframe with two columns, one of field name, duplicated categories
extract_values_exp_2_df_2col <- data.frame(
    description = c("Co-op", "Singleplayer", "Singleplayer"),
    game_name = c("Game1", "Game2", "Game3")
)

ev_exp_output_2 <- c("Co-op", "Singleplayer", "Singleplayer")

#expected case 3, nested object, contains 'description', duplicate category
extract_values_exp_3_nested_list_some_missing <- list(
    list(game_name = "Game1", description = "Singleplayer"),
    list(game_name = "Game2", description = "Co-op"),
    list(game_name = "Game3", description = "Singleplayer"),
    list(game_name = "Game4", description = "Multiplayer")

)

ev_exp_output_3 <- c("Singleplayer", "Co-op", "Singleplayer", "Multiplayer")

#expected case 4, nested lists, some have description and some dont
extract_values_exp_4_nested_list <- list(
    list(game_name = "Game1", description = "Singleplayer"),
    list(game_name = "Game2", genre = "Competitive"),
    list(game_name = "Game3", description = "Co-op"),
    list()

)

ev_exp_output_4 <- c("Singleplayer", "Co-op")

#expected empty 

#null
extract_values_null_input <- NULL

#edge case 1: list of list 0 long 
extract_values_edge_0_list <- list()

#edge case 2: dataframe without category column 
extract_values_edge_df_no_category <- data.frame(
     game_name = c("Game1", "Game2", "Game3"),
     game_genre = c("Singleplayer", "Co-op", "Multiplayer")

)

#edge case 3: list of lists without category
extract_values_edge_list_no_category <- list(
    list(game_name = "Game1", game_genre = "Singleplayer"),
    list(game_name = "Game2", game_genre = "Multiplayer"),
    list(game_name = "Game3", game_genre = "Co-op")
)

#edge case 4: vector rather than df or list of lists
extract_values_edge_vector_input <- c("Singleplayer", "Co-op", "Multiplayer")

ev_exp_output_empty <- character(0)

