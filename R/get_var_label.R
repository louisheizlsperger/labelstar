#' Retrieve Variable Label from Term
#'
#' This function retrieves the label of a variable from a given term and data frame. It handles interaction terms,
#' logarithmic transformations, and standard variable names. The function returns the appropriate label for the term.
#'
#' @param term A character string representing the term for which the label is to be retrieved. This can be a variable name,
#' an interaction term (e.g., 'var1:var2' or 'var1*var2'), or a logarithmic transformation (e.g., 'log(var)' or 'log(1 + var)').
#' @param data The data frame containing the variables. Each variable can have a 'label' attribute that provides a descriptive label.
#' @return A character string representing the label for the given term. If the term is an interaction, the labels of the
#' interacting variables are combined with ' : '. If the term involves a logarithmic transformation, the label is formatted accordingly.
get_var_label <- function(term, data) {

  if (grepl('[:*]', term)) {

    term <- gsub("\\s*[:*]\\s*", ":", term)
    interaction_vars <- unlist(strsplit(term, '[:*]'))
    interaction_labels <- sapply(interaction_vars, get_var_label, data = data)
    interaction_labels_fmt <- paste(interaction_labels, collapse = ' : ')

    return(interaction_labels_fmt)
  }

  if (grepl('^log\\(1 \\+ .+\\)$', term)) {
    var_name <- sub('log\\(1 \\+ (.+)\\)', '\\1', term)
  } else if (grepl('^log\\(.+\\)$', term)) {
    var_name <- sub('log\\((.+)\\)', '\\1', term)
  } else {
    var_name <- term
  }

  var_label <- if (!is.null(attr(data[[var_name]], 'label'))) attr(data[[var_name]], 'label') else var_name

  if (grepl('^log\\(1 \\+ .+\\)$', term)) {
    var_label_formatted <- sprintf('log(1 + %s)', var_label)
  } else if (grepl('^log\\(.+\\)$', term)) {
    var_label_formatted <- sprintf('log(%s)', var_label)
  } else {
    var_label_formatted <- var_label
  }


  return(var_label_formatted)

}
