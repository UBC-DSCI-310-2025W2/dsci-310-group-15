#' Extract values from a certain named field 
#' 
#' Returns a vector with the values under a certain field name
#' 
#' @param object can be either a data frame, 
#'   a list of sub lists, NULL, or a zero-length object
#' @param field_name the field name to extract values from, 
#'   the default is 'description'. Must be a string.
#' 
#' @return A vector of the extracted values. Returns character(0) if
#'   the input is null, empty, or does not contain the specified field name
#' 
#' @export 
#' 
#' @examples 
#' extract_values(data.frame(description = c("Co-op", "Singleplayer")))
#' extract_values(
#'  list(
#'      list(category = "Co-op", release_year = 2025),
#'      list(category = "Singleplayer", release_year = 2023)
#'      ),
#'  field_name = 'category'
#' )
#' 
extract_values <- function(object, field_name = 'description' ) {
    if (!is.character(field_name)) {
        stop("field_name must be a string.")
    }

    if (is.null(object) || length(object) == 0) return(character(0))

    if (is.data.frame(object) && field_name %in% names(object)) {
      return(as.character(object[[field_name]]))
    }
    
    if (is.data.frame(object) && (!field_name %in% names(object))) {
      return(character(0))
    }

    if (is.list(object)) {
      vals <- purrr::map_chr(object, function(x) {
        if (is.list(x) && field_name %in% names(x)) {
          as.character(x[[field_name]])
        } else {
          NA_character_
        }
      })
      return(vals[!is.na(vals)])
    }

    character(0)
}
