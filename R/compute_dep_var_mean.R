#' Compute the Mean of the Dependent Variable
#'
#' This function calculates the mean of the dependent variable, either in its raw form
#' or after applying the specified transformation. It returns a formatted list of lines
#' to be added to a regression table, indicating the mean value of the dependent variable.
#'
#' @param dep_var A character string representing the dependent variable, possibly with transformations.
#' @param formulas A list of model formulas.
#' @param type A character string indicating whether to compute the mean of the raw or transformed dependent variable. Options are "raw" or "transformed".
#' @param data The data frame containing the variables.
#' @return A list of lines to be added to a regression table, indicating the mean of the dependent variable.
#' @import stringr
#' @export
#'
compute_dep_var_mean <- function(dep_var, formulas,
                                 type = c("raw", "transformed"), data) {

  # Identify raw version of dependent variable
  dep_var_parts <- stringr::str_split(dep_var, pattern = "([()\\+\\-*/ ])", simplify = TRUE)

  raw_var <- dep_var_parts[dep_var_parts %in% names(data)]

  # Identify the transformation
  tf <- stringr::str_replace(dep_var, raw_var, "xxx")
  tf_str <- stringr::str_replace(tf, "xxx", paste0("data[['", raw_var, "']]"))

  # Raw mean
  if (type == "raw") {

    dep_var_mean <- mean(data[[raw_var]], na.rm = TRUE)

  # Transformed mean
  } else if (type == "transformed") {

    tf_dv <- eval(parse(text = tf_str))

    dep_var_mean <- mean(tf_dv, na.rm = TRUE)

  }

  # Format mean
  dep_var_mean_fmt <- sprintf(fmt = "%.2f", dep_var_mean)

  # Specify added lines
  dv_line_str <- stringr::str_glue("Mean of {type} DV")
  mean_line <- c(dv_line_str, rep(dep_var_mean_fmt, length(formulas)))
  dv_mean_added_lines <- c(
                           mean = list(mean_line),
                           space = list(rep("", length(formulas) + 1))
                           )


  return(dv_mean_added_lines)

}
