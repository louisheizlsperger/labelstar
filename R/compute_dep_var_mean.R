#' Compute the Mean of the Dependent Variable Across Specifications
#'
#' This version loops over the list of formulas and computes the mean
#' (raw or transformed) for each outcome variable as specified in the formula.
#' It then returns a formatted list of lines for the regression table.
#'
#' @param formulas A list of model formulas.
#' @param type A character string indicating whether to compute the mean of the raw or transformed dependent variable. Options are "raw" or "transformed".
#' @param data The data frame containing the variables.
#' @return A list of lines to be added to a regression table, with one mean per formula.
#' @import stringr
#' @export
compute_dep_var_mean <- function(formulas, type = c("raw", "transformed"), data) {
  type <- match.arg(type)
  
  # For each formula, extract the LHS as a string (e.g., "log(wage)" or "wage")
  dep_var_strs <- sapply(formulas, function(f) deparse(f[[2]]))
  
  if(length(unique(dep_var_strs)) == 1) {
    # All formulas share the same outcome. Compute once and repeat.
    dep_var <- dep_var_strs[1]
    dep_var_parts <- stringr::str_split(dep_var, pattern = "([()\\+\\-*/ ])", simplify = TRUE)
    raw_var <- dep_var_parts[dep_var_parts %in% names(data)]
    
    if (type == "raw") {
      m <- mean(data[[raw_var]], na.rm = TRUE)
    } else if (type == "transformed") {
      tf <- stringr::str_replace(dep_var, raw_var, "xxx")
      tf_str <- stringr::str_replace(tf, "xxx", paste0("data[['", raw_var, "']]"))
      tf_dv <- eval(parse(text = tf_str))
      m <- mean(tf_dv, na.rm = TRUE)
    }
    
    dep_var_mean_fmt <- sprintf("%.2f", m)
    # If the original string is not simply the raw variable, indicate the transformation.
    dv_str <- if(dep_var == raw_var) "" else paste0(" ", type)
    dv_line_str <- stringr::str_glue("Mean of{dv_str} DV")
    mean_line <- c(dv_line_str, rep(dep_var_mean_fmt, length(formulas)))
    
  } else {
    # Different outcomes across specifications: compute each mean individually.
    means <- sapply(dep_var_strs, function(current_dep) {
      dep_var_parts <- stringr::str_split(current_dep, pattern = "([()\\+\\-*/ ])", simplify = TRUE)
      raw_var <- dep_var_parts[dep_var_parts %in% names(data)]
      if (type == "raw") {
        m <- mean(data[[raw_var]], na.rm = TRUE)
      } else if (type == "transformed") {
        tf <- stringr::str_replace(current_dep, raw_var, "xxx")
        tf_str <- stringr::str_replace(tf, "xxx", paste0("data[['", raw_var, "']]"))
        tf_dv <- eval(parse(text = tf_str))
        m <- mean(tf_dv, na.rm = TRUE)
      }
      sprintf("%.2f", m)
    })
    # Use a generic header since the outcomes differ.
    dv_line_str <- "Mean of DV"
    mean_line <- c(dv_line_str, means)
  }
  
  dv_mean_added_lines <- list(
    mean = mean_line,
    space = rep("", length(formulas) + 1)
  )
  
  return(dv_mean_added_lines)
  
}