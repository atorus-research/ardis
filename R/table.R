### Table Constructor


#' Create a ardis table object
#'
#' The \code{ardis} object is the main container upon which a ardis table is constructed. ardis tables are made up of
#' one or more layers. Each layer contains an instruction for a summary to be performed. The \code{ardis} object contains
#' those layers, and the general data, metadata, and logic necessary.
#'
#' @details
#' When a \code{ardis} is created, it will contain the following bindings:
#' \itemize{
#' \item{target - The dataset upon which summaries will be performed}
#' \item{pop_data - The data containing population information. This defaults to the target dataset}
#' \item{cols - A categorical variable to present summaries grouped by column (in addition to treat_var)}
#' \item{table_where - The \code{where} parameter provided, used to subset the target data}
#' \item{treat_var - Variable used to distinguish treatment groups.}
#' \item{header_n - Default header N values based on \code{treat_var}}
#' \item{pop_treat_var - The treatment variable for \code{pop_data} (if different)}
#' \item{layers - The container for individual layers of a \code{ardis}}
#' \item{treat_grps - Additional treatment groups to be added to the summary (i.e. Total)}
#' }
#'
#' \code{ardis} allows you a basic interface to instantiate the object. Modifier functions are available to change
#' individual parameters catered to your analysis. For example, to add a total group, you can use the
#' \code{\link{add_total_group}}.
#'
#' In future releases, we will provide vignettes to fully demonstrate these capabilities.
#'
#' @param target Dataset upon which summaries will be performed
#' @param treat_var Variable containing treatment group assignments. Supply unquoted.
#' @param where A general subset to be applied to all layers. Supply as programming logic (i.e. x < 5 & y == 10)
#' @param cols A grouping variable to summarize data by column (in addition to treat_var). Provide multiple
#' column variables by using \code{\link[dplyr]{vars}}
#'
#' @return A \code{ardis} object
#' @export
#'
#' @examples
#'
#' tab <- ardis(iris, Species, where = Sepal.Length < 5.8)
#'
ardis <- function(target, treat_var, where = TRUE, cols = vars()) {

  if(missing(target)){
    # return a blank environment if no table information is passed. This can be
    # used as a placeholder when creating a table if the dataset is not available.
    return(structure(rlang::env(),
                     class = c("ardis", "environment")))
  }

  target_name <- enexpr(target)

  new_ardis(target, enquo(treat_var), enquo(where), enquos(cols), target_name)
}

#' Construct new ardis
#'
#' @inheritParams ardis
#' @noRd
new_ardis <- function(target, treat_var, where, cols, target_name) {
  cols <- unpack_vars(cols)

  validate_ardis(target, cols)

  # Create table object with default bindings and class of `ardis`
  table_ <- structure(rlang::env(
    target = target,
    treat_grps = list(),
    cols = cols,
    layers = structure(list(),
                       class = c("ardis_layer_container", "list"))
  ), class = c("ardis", "environment"))
  attr(table_, "target_name") <- target_name

  table_ <- table_ %>%
    # Set default bindings with standard setter methods
    set_treat_var(!!treat_var) %>%
    set_pop_data(target) %>%
    set_pop_treat_var(!!treat_var) %>%
    set_where(!!where) %>%
    set_pop_where(!!where) %>%
    set_desc_layer_summaries() %>%
    set_count_layer_summaries() %>%
    set_shift_layer_formats()



  table_
}

#' Validate ardis target dataset
#'
#' Most validation is done in the binding functions to reduce code duplication
#'
#' @param target target dataset passed from new_ardis
#' @param cols cols argument passed from new_ardis
#'
#' @noRd
validate_ardis <- function(target, cols) {

  # table should be a data.frame
  assertthat::assert_that(inherits(target, "data.frame"),
                          msg = paste0("'pop_data' argument passed to ardis must be a data.frame,",
                                       "\n",
                                       "instead a class of: '",
                                       class(target),
                                       "' was passed."))

  assert_quo_var_present(cols, names(target), allow_character = FALSE)
}


