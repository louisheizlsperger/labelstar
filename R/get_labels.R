#' Retrieve Labels for Outcome Variable, Covariates, Fixed Effects, and Clustering Variables from Formulas
#'
#' Now accommodates a regression list with potentially different outcome variables.
#'
#' @param formulas A list or vector of model formulas.
#' @param data The data frame containing the variables.
#' @param interaction_symbol A character string representing the preferred symbol for interaction terms (e.g., ":" or "x").
#' @param fe_symbol A character string representing the preferred symbol for the inclusion of fixed effects (e.g., "x" or "Yes").
#' @param dep_var_means Either "no", "raw", or "transformed". If not "no", the mean of each dependent variable is computed.
#' @return A list with elements: `dep_var_label` (a vector of outcome labels),
#' `covariate_labels`, `add_lines` (which now includes the row with dependent variable means), and `table_notes`.
#' @import stringr dplyr purrr
#' @export
#'
get_labels <- function(formulas, data,
                       interaction_symbol = " : ", fe_symbol = "X",
                       dep_var_means = "no") {     # other options: "raw", "transformed"
  
  # Ensure formulas is a list (this prevents issues when formulas are combined via c())
  formulas <- as.list(formulas)
  
  #=#=#=#=#=#=#=#=#=#=#=#
  ## Dependent variable
  #=#=#=#=#=#=#=#=#=#=#=#
  # Extract the dependent variable (LHS) from each formula and get their labels.
  # Using trimws(deparse(...)) ensures that extra whitespace is removed.
  dep_var_strs <- sapply(formulas, function(f) trimws(deparse(f[[2]])))
  dep_var_labels <- sapply(dep_var_strs, get_var_label, data = data) %>% unique()
  
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
  ## Add Mean of Dependent Variable (if requested)
  #=#=#=#=#=#=#=#=#=#=#=#
  if (dep_var_means %in% c("raw", "transformed")) {
    dep_var_mean_lines <- compute_dep_var_mean(formulas = formulas,
                                               type = dep_var_means,
                                               data = data)
    add_lines <- c(add_lines, dep_var_mean_lines)
  }
  
  #=#=#=#=#=#=#=#=#=#=#=#
  ## Clustering
  #=#=#=#=#=#=#=#=#=#=#=#
  # Extract clustering variables from the first formula (assuming clustering is consistent)
  formula_str <- as.character(formulas[[1]])
  rhs <- formula_str[3]
  rhs_parts <- stringr::str_split(rhs, "\\|")[[1]]
  clustering_labels <- if (length(rhs_parts) > 3) {
    clustering_vars <- all.vars(as.formula(paste0("~", rhs_parts[4])))
    sapply(clustering_vars, get_var_label, data = data)
  } else {
    NULL
  }
  
  table_notes_signif <- "*** 1 percent level; ** 5 percent level; * 10 percent level"
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
  list_labels <- list(
    dep_var_labels = dep_var_labels,
    covariate_labels = covariate_labels,
    add_lines = add_lines,
    table_notes = table_notes
  )
  
  return(list_labels)
}