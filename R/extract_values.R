#' Extract values under a specified field name
#'
#' Returns a character vector of values stored under a given field name
#' from either:
#'  • a dataframe column
#'  • a list of lists
#'
#' @param data A dataframe OR list of lists
#' @param field_name A string specifying the field to extract.
#'        Default = "categories"
#'
#' @return A character vector of extracted values.
#'         Returns character(0) for invalid inputs or if field not found.
#'
#' @examples
#' df <- data.frame(categories = c("Action","RPG"))
#' extract_values(df)
#'
#' lst <- list(list(categories="Action"), list(categories="RPG"))
#' extract_values(lst)

extract_values <- function(data, field_name = "categories") {

  # error if field_name not string
  if (!is.character(field_name) || length(field_name) != 1) {
    stop("field_name must be a string")
  }

  # return empty for NULL or empty input
  if (is.null(data) || length(data) == 0) {
    return(character(0))
  }

  # ---------------------------
  # CASE 1 — DATA FRAME INPUT
  # ---------------------------
  if (is.data.frame(data)) {

    # field must exist as column
    if (!(field_name %in% colnames(data))) {
      return(character(0))
    }

    # extract + coerce to character
    return(as.character(data[[field_name]]))
  }

  # ---------------------------
  # CASE 2 — LIST OF LISTS INPUT
  # ---------------------------
  if (is.list(data)) {

    values <- sapply(data, function(x) {
      if (is.list(x) && field_name %in% names(x)) {
        return(as.character(x[[field_name]]))
      }
      return(NULL)
    })

    # remove NULLs
    values <- values[!sapply(values, is.null)]

    return(as.character(values))
  }

  # ---------------------------
  # OTHERWISE INVALID INPUT
  # ---------------------------
  return(character(0))
}