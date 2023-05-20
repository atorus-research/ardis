#' Set Summaries
#'
#' If ardis on only being used to summarize numeric data, then layer formatting
#' components of the settings become unnecessary. As such, a second interface is
#' necessary to specify which summaries should actually be performed. the
#' `set_summaries()` function provided an interface to each layer type to
#' establish the numbers that must be calculated.
#'
#' @param e Layer upon which summaries should be bound
#' @param ... Named parameters containing lists of quosures created using `dplyr::vars()`
#'
#' @return The layer environment with the summary variables binding applied
#' @export
#' @md
#'
#' @rdname set_summaries
#'
#' @examples
#' # This is the desired API for count layers
#' t <- ardis(adsl, TRT01P) %>%
#'   add_layer(
#'     group_desc(AGE, by = "Age (years)", where= SAFFL=="Y") %>%
#'       set_summaries(
#'         "n"        = vars(n),
#'         "Mean (SD)"= vars(mean, sd),
#'         "Median"   = vars(median),
#'         "Q1, Q3"   = vars(q1, q3),
#'         "Min, Max" = vars(min, max),
#'         "Missing"  = vars(missing)
#'       )
#'   )
#'
set_summaries  <- function(e, ...) {
  UseMethod("set_summaries")
}

#' Set summaries for descriptive stats layer
#'
#' @export
#' @rdname set_summaries
#'
set_summaries.ardis_layer <- function(e, ...) {

  # Create the summary_vars object
  summaries <- list(...)
  summary_vars <- flatten(summaries)

  env_bind(e,
           summary_grps = summaries,
           summary_vars = vars(!!!summary_vars)
  )

  e
}

#' Check if summaries have been set on a ardis layer
#'
#' @param e A layer environment
#'
#' @return Boolean
has_summaries <- function(e) {
  all(c("summary_grps", "summary_vars") %in% ls(e))
}

#' Extract a translation vector for f_str or summary object
#'
#' This provides the row labels in preparation for the numeric data output
#'
#' @param x The format strings list or a list of summaries
#'
#' @return A named character vector with the flipping applied
#' @noRd
name_translator_numeric <- function(x) {

  # This needs to be a for loop for clarity because a map
  # function would be too unreadable and overly complex
  out <- character(length(flatten(x)))
  i <- 1
  # Loop the labels
  for (l in names(x)) {
    # Loop the variable names
    for (n in x[[l]]) {
      out[i] <- l
      names(out)[i] <- as_name(n)
      i <- i + 1
    }
  }

  out
}
