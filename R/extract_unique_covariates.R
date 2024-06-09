#' Extract Unique Covariates from a List of Formulas
#'
#' This function extracts all unique covariates from a list of formulas, preserving the order in which they appear.
#' It handles interaction terms by converting them to a readable format.
#'
#' @param formulas A list of model formulas.
#' @return A character vector containing all unique covariates in the order they appear across the formulas.
#' @import stringr
#' @import purrr
#' @import dplyr
#' @export
#'
extract_unique_covariates <- function(formulas) {

  covariates_list <- purrr::map(formulas, function(formula) {

    formula_str <- as.character(formula)
    rhs <- formula_str[3]

    # Split RHS on '|' to get the part with covariates
    rhs_parts <- stringr::str_split(rhs, "\\|")[[1]]
    covariate_str <- rhs_parts[1]

    # Extract covariates and interaction terms
    covariates <- stringr::str_split(covariate_str, pattern = "\\+") %>%
      unlist() %>%
      stringr::str_trim()

    # Handle interaction terms
    covariates <- gsub(":", " * ", covariates)
    return(covariates)

  })

  # Flatten the list of covariates and get unique values
  unique_covariates <- unique(unlist(covariates_list))

  # Flatten the list of covariates and get unique values
  unique_covariates <- unique(unlist(covariates_list))

  # Reorder covariates: move interaction terms (containing ':') to the end
  main_effects <- unique_covariates[!stringr::str_detect(unique_covariates, "\\*")]
  interaction_terms <- unique_covariates[stringr::str_detect(unique_covariates, "\\*")]

  ordered_covariates <- c(main_effects, interaction_terms)

  return(ordered_covariates)

}
