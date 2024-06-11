#' Extract Unique Covariates from a List of Formulas
#'
#' This function extracts all unique covariates from a list of formulas, preserving the order in which they appear.
#' It handles interaction terms by converting them to a readable format.
#'
#' @param formulas A list of model formulas.
#' @return A character vector containing all unique covariates in the order they appear across the formulas.
#' @import stringr
#' @import purrr
#' @export
#'
extract_unique_covariates <- function(formulas) {

  covariates_list <- purrr::map(formulas, extract_covariates)

  # Flatten the list of covariates and get unique values
  unique_covariates <- unique(unlist(covariates_list))

  # Reorder covariates: move interaction terms (containing '*') to the end
  main_effects <- unique_covariates[!str_detect(unique_covariates, "\\*")]
  interaction_terms <- unique_covariates[str_detect(unique_covariates, "\\*")]

  ordered_covariates <- c(main_effects, interaction_terms)

  return(ordered_covariates)

}

# Extract covariates from given formula
extract_covariates <- function(formula) {

  # Convert formula to character string
  formula_str <- as.character(formula)
  rhs <- formula_str[3]

  # Split RHS on '|' to get the part with covariates
  rhs_parts <- str_split(rhs, "\\|")[[1]]
  covariate_str <- rhs_parts[1]

  covariates <- c()
  open_parens <- 0
  current_covariate <- ""

  for (char in str_split(covariate_str, "")[[1]]) {
    if (char == "(") {
      open_parens <- open_parens + 1
    } else if (char == ")") {
      open_parens <- open_parens - 1
    } else if (char == "+" && open_parens == 0) {
      covariates <- c(covariates, str_trim(current_covariate))
      current_covariate <- ""
      next
    }

    current_covariate <- paste0(current_covariate, char)
  }

  # Add the last covariate
  covariates <- c(covariates, str_trim(current_covariate))

  return(covariates)

}
