#' Retrieve Labels for Outcome Variable, Covariates, Fixed Effects, and Clustering Variables from a Formula
#'
#' This function retrieves the label of the outcome variable, the labels of the covariates,
#' the fixed effects, and the clustering variables from a given formula and data frame.
#' The user can specify the preferred symbols for interaction terms and fixed effects.
#'
#' @param formula The model formula.
#' @param data The data frame containing the variables.
#' @param interaction_symbol A character string representing the preferred symbol for interaction terms (e.g., ":" or "x").
#' @param fe_symbol A character string representing the preferred symbol for the inclusion of fixed effects (e.g., "x" or "Yes").
#' @return A list with four elements: `dep_var_label` for the outcome variable label,
#' `covariate_labels` for the covariate labels, `fe_labels` for the fixed effects labels,
#' and `clustering_labels` for the clustering variable labels.
#' @import stringr
#' @import dplyr
#' @export
#'
get_labels <- function(formula, data,
                       interaction_symbol = " : ", fe_symbol = "X") {

  #=#=#=#=#=#=#=#=#=#=#=#
  ## Dependent variable
  #=#=#=#=#=#=#=#=#=#=#=#

  # Extract the outcome variable from the formula
  dep_var <- all.vars(formula)[1]

  # Get the label for the outcome variable
  dep_var_label <- get_var_label(dep_var, data)

  #=#=#=#=#=#=#=#=#=#=#=#
  ## Covariates
  #=#=#=#=#=#=#=#=#=#=#=#

  # Extract parts of the formula
  formula_str <- as.character(formula)
  rhs <- formula_str[3]

  # Split RHS on '|' and extract covariates
  rhs_parts <- str_split(rhs, "\\|")[[1]]
  covariate_str <- rhs_parts[1]

  # Extract covariates
  covariates <- stringr::str_split(covariate_str, pattern = "\\+") %>%
    unlist() %>% stringr::str_trim()
  covariate_labels <- sapply(covariates, get_var_label, data = data,
                             interaction_symbol = interaction_symbol)

  #=#=#=#=#=#=#=#=#=#=#=#
  ## Fixed effects
  #=#=#=#=#=#=#=#=#=#=#=#

  # Options
  if (str_detect(fe_symbol, "check")) {
    fe_symbol <- "\u2713"
  }

  # Extract fixed effects if present
  fe_labels <- if (length(rhs_parts) > 2) {
    fixed_effects <- all.vars(as.formula(paste0("~", rhs_parts[2])))
    sapply(fixed_effects, get_var_label, data = data)
  } else {
    c("")
  }

  # Create the add_lines for fixed effects
  add_lines <- if (!is.null(fe_labels)) {
    lapply(fe_labels, function(label) c(paste0(label, " - FE"), fe_symbol))
  } else {
    list()
  }

  add_lines$space <- c("", "")

  #=#=#=#=#=#=#=#=#=#=#=#
  ## Clustering
  #=#=#=#=#=#=#=#=#=#=#=#

  # Extract clustering variables if present
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
