#' Detect Included Fixed Effects in Multiple Formulas
#'
#' This function processes multiple formulas to determine the unique fixed effects and tracks their inclusion in each specification.
#' It returns a list indicating which fixed effects are included in which specifications and a corresponding symbol.
#'
#' @param formulas A list of model formulas.
#' @param data The data frame containing the variables.
#' @param fe_symbol A character string representing the preferred symbol for the inclusion of fixed effects (e.g., "X" or "check").
#' @param get_var_label A function to retrieve the variable label.
#' @return A list where each element corresponds to a fixed effect, containing the fixed effect label and symbols indicating inclusion in each specification.
#' @import stringr
#' @import purrr
#' @export
#'
detect_included_fes <- function(formulas, data, fe_symbol = "X") {

  # Options
  if (stringr::str_detect(fe_symbol, "check")) {
    fe_symbol <- "\u2713"
  }

  # Extract fixed effects
  fixed_effects_list <- purrr::map(formulas, function(formula) {

    formula_str <- as.character(formula)

    rhs <- formula_str[3]
    rhs_parts <- stringr::str_split(rhs, "\\|")[[1]]

    if (length(rhs_parts) > 1) {
      fixed_effects <- all.vars(as.formula(paste0("~", rhs_parts[2])))
      return(fixed_effects)
    } else {
      return(character(0))
    }

  })

  # Determine unique fixed effects
  unique_fixed_effects <- unique(purrr::flatten_chr(fixed_effects_list))

  # Create a list to track the inclusion of each fixed effect in each specification
  fe_inclusion_list <- purrr::map(unique_fixed_effects, function(fe) {

    included_in <- purrr::map_lgl(fixed_effects_list, ~ fe %in% .x)
    symbols <- ifelse(included_in, fe_symbol, "")
    c(paste0(get_var_label(fe, data), " - FE"), symbols[symbols != ""])

  })

  names(fe_inclusion_list) <- unique_fixed_effects

  # Add space element
  fe_inclusion_list$space <- c("", "")

  return(fe_inclusion_list)

}
