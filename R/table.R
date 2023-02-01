### Table Constructor


#' Create a tardis table object
#'
#' The \code{tardis_table} object is the main container upon which a tardis table is constructed. tardis tables are made up of
#' one or more layers. Each layer contains an instruction for a summary to be performed. The \code{tardis_table} object contains
#' those layers, and the general data, metadata, and logic necessary.
#'
#' @details
#' When a \code{tardis_table} is created, it will contain the following bindings:
#' \itemize{
#' \item{target - The dataset upon which summaries will be performed}
#' \item{pop_data - The data containing population information. This defaults to the target dataset}
#' \item{cols - A categorical variable to present summaries grouped by column (in addition to treat_var)}
#' \item{table_where - The \code{where} parameter provided, used to subset the target data}
#' \item{treat_var - Variable used to distinguish treatment groups.}
#' \item{header_n - Default header N values based on \code{treat_var}}
#' \item{pop_treat_var - The treatment variable for \code{pop_data} (if different)}
#' \item{layers - The container for individual layers of a \code{tardis_table}}
#' \item{treat_grps - Additional treatment groups to be added to the summary (i.e. Total)}
#' }
#'
#' \code{tardis_table} allows you a basic interface to instantiate the object. Modifier functions are available to change
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
#' @return A \code{tardis_table} object
#' @export
#'
#' @examples
#'
#' tab <- tardis_table(iris, Species, where = Sepal.Length < 5.8)
#'
tardis_table <- function(target, treat_var, where = TRUE, cols = vars()) {

  if(missing(target)){
    # return a blank environment if no table information is passed. This can be
    # used as a placeholder when creating a table if the dataset is not available.
    return(structure(rlang::env(),
                     class = c("tardis_table", "environment")))
  }

  target_name <- enexpr(target)

  new_tardis_table(target, enquo(treat_var), enquo(where), enquos(cols), target_name)
}

#' Construct new tardis_table
#'
#' @inheritParams tardis_table
#' @noRd
new_tardis_table <- function(target, treat_var, where, cols, target_name) {
  cols <- unpack_vars(cols)

  validate_tardis_table(target, cols)

  # Create table object with default bindings and class of `tardis_table`
  table_ <- structure(rlang::env(
    target = target,
    treat_grps = list(),
    cols = cols,
    layers = structure(list(),
                       class = c("tardis_layer_container", "list"))
  ), class = c("tardis_table", "environment"))
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

#' Validate tardis_table target dataset
#'
#' Most validation is done in the binding functions to reduce code duplication
#'
#' @param target target dataset passed from new_tardis_table
#' @param cols cols argument passed from new_tardis_table
#'
#' @noRd
validate_tardis_table <- function(target, cols) {

  # table should be a data.frame
  assertthat::assert_that(inherits(target, "data.frame"),
                          msg = paste0("'pop_data' argument passed to tardis_table must be a data.frame,",
                                       "\n",
                                       "instead a class of: '",
                                       class(target),
                                       "' was passed."))

  assert_quo_var_present(cols, names(target), allow_character = FALSE)
}


