#' Extract values from a certain named field 
#' 
#' Returns a vector with the values under a certain field name
#' 
#' @param object can be either a data frame, 
#' a list of sub lists, NULL, or a zero-length object
#' @param field_name the field name to extract values from, 
#' default is 'description'
#' 
#' @return A vector of the extracted values. Returns character(0) if
#' the input is null, empty, or does not contain the specified field name
#' 
#' @export 
#' 
#' @examples 
#' 
extract_values <- function(object, field_name = 'description' ) {
    #Returns a vector with the values under a certain field name
}