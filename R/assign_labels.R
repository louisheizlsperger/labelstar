#' Assigns Custom Variable Labels to a Data Frame
#'
#' This function assigns custom variable labels to a data frame. If a custom label is provided
#' for a variable, it assigns that label; otherwise, it assigns the variable name as the label.
#'
#' @param data A data frame to which labels will be applied.
#' @param variable_labels A named list of custom labels where the names are the variable names
#' in the data frame, and the values are the labels to be assigned.
#' @export
assign_labels <- function(data, variable_labels) {

  for (var_name in names(data)) {
    # Check if a custom label exists for the variable
    if (var_name %in% names(variable_labels)) {
      # Assign the custom label
      attr(data[[var_name]], "label") <- variable_labels[[var_name]]
    } else {
      # Assign the variable name as the label if no custom label exists
      attr(data[[var_name]], "label") <- var_name
    }
  }

  return(data)

}
