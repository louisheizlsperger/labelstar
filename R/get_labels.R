#' Retrieve Labels for Outcome Variable, Covariates, Fixed Effects, and Clustering Variables from Formulas
#'
#' This function retrieves the label of the outcome variable, the labels of the covariates,
#' the fixed effects, and the clustering variables from given formulas and a data frame.
#' The user can specify the preferred symbols for interaction terms and fixed effects.
#'
#' @param formulas A list of model formulas.
#' @param data The data frame containing the variables.
#' @param interaction_symbol A character string representing the preferred symbol for interaction terms (e.g., ":" or "x").
#' @param fe_symbol A character string representing the preferred symbol for the inclusion of fixed effects (e.g., "x" or "Yes").
#' @return A list with four elements: `dep_var_label` for the outcome variable label,
#' `covariate_labels` for the covariate labels (only for the longest specification),
#' `fe_labels` for the fixed effects labels, and `clustering_labels` for the clustering variable labels.
#' @import stringr
#' @import dplyr
#' @import purrr
#' @export
#'
get_labels <- function(formulas, data,
                       interaction_symbol = " : ", fe_symbol = "X") {

  # formulas <- formulas
  # interaction_symbol = " : "
  # fe_symbol <- "X"
  # data <- diamonds

  # Ensure formulas is a list
  if (!is.list(formulas)) {
    formulas <- list(formulas)
  }

  #=#=#=#=#=#=#=#=#=#=#=#
  ## Dependent variable
  #=#=#=#=#=#=#=#=#=#=#=#

  # Extract the outcome variable from the first formula
  # (assuming all formulas have the same outcome variable)
  dep_var <- all.vars(formulas[[1]])[1]

  # Get the label for the outcome variable
  dep_var_label <- get_var_label(dep_var, data)

  #=#=#=#=#=#=#=#=#=#=#=#
  ## Covariates
  #=#=#=#=#=#=#=#=#=#=#=#

  covariates <- extract_unique_covariates(formulas)

  covariate_labels <- sapply(covariates, get_var_label, data = data,
                             interaction_symbol = interaction_symbol)

  #=#=#=#=#=#=#=#=#=#=#=#
  ## Fixed effects
  #=#=#=#=#=#=#=#=#=#=#=#

  add_lines <- detect_included_fes(formulas, data, fe_symbol)

  #=#=#=#=#=#=#=#=#=#=#=#
  ## Clustering
  #=#=#=#=#=#=#=#=#=#=#=#

  # Extract clustering variables from the first formula
  # (assuming all formulas have the same clustering-level)
  formula_str <- as.character(formulas[[1]])
  rhs <- formula_str[3]
  rhs_parts <- stringr::str_split(rhs, "\\|")[[1]]
  clustering_labels <- if (length(rhs_parts) > 3) {
    clustering_vars <- all.vars(as.formula(paste0("~", rhs_parts[4])))
    sapply(clustering_vars, get_var_label, data = data)
  } else {
    NULL
  }

  # Table notes
  table_notes_signif <- "*** 1 percent level; ** 5 percent level; * 10 percent level"

  # Join clustering labels for table note
  if (!is.null(clustering_labels)) {
    cluster_label_str <- paste(clustering_labels, collapse = "-")
    table_notes_se <- paste0("Standard errors are clustered by ", cluster_label_str, ".")
  } else {
    table_notes_se <- ""
  }

  table_notes <- c(table_notes_signif, table_notes_se)

  #=#=#=#=#=#=#=#=#=#=#=#
  ## Return named list
  #=#=#=#=#=#=#=#=#=#=#=#

  # Return a list with all labels
  list_labels <- list(
    dep_var_label = dep_var_label,
    covariate_labels = covariate_labels,
    fe_labels = add_lines,
    table_notes = table_notes
  )

  return(list_labels)

}
