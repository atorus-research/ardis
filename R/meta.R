#' tardis Metadata Object
#'
#' If a tardis table is built with the `metadata=TRUE` option specified, then
#' metadata is assembled behind the scenes to provide traceability on each
#' result cell derived. The functions `get_meta_result()` and
#' `get_meta_subset()` allow you to access that metadata by using an ID provided
#' in the row_id column and the column name of the result you'd like to access.
#' The purpose is of the row_id variable instead of a simple row index is to
#' provide a sort resistant reference of the originating column, so the output
#' tardis table can be sorted in any order but the metadata are still easily
#' accessible.
#'
#' The `tardis_meta` object provided a list with two elements - names and
#' filters. The names contain every column from the target data.frame of the
#' tardis table that factored into the specified result cell, and the filters
#' contains all the necessary filters to subset the target data to create the
#' specified result cell. `get_meta_subset()` additionally provides a parameter to
#' specify any additional columns you would like to include in the returned
#' subset data frame.
#'
#' @param names List of symbols
#' @param filters  List of expressions
#'
#' @return tardis_meta object
#' @export
#'
#' @examples
#'
#' tardis_meta(
#'    names = rlang::quos(x, y, z),
#'    filters = rlang::quos(x == 1, y==2, z==3)
#'  )
#'
tardis_meta <- function(names=list(), filters=exprs()) {
  meta <- new_tardis_meta()
  meta <- add_variables(meta, names)
  meta <- add_filters(meta, filters)
  meta
}

#' Create a tardis_meta object
#'
#' @return tardis_meta object
#' @noRd
new_tardis_meta <- function(names = list(), filters=exprs()) {
  structure(
    list(
      names = names,
      filters = filters
    ),
    class = 'tardis_meta'
  )
}

#' Add variables to a tardis_meta object
#'
#' Add additional variable names to a `tardis_meta()` object.
#'
#' @param meta A tardis_meta object
#' @param names A list of names, providing variable names of interest. Provide
#'   as a list of quosures using `rlang::quos()`
#'
#' @return tardis_meta object
#' @md
#'
#' @family Metadata additions
#' @rdname metadata_additions
#'
#' @export
#'
#' @examples
#'
#' m <- tardis_meta()
#' m <- add_variables(m, rlang::quos(a, b, c))
#' m <- add_filters(m, rlang::quos(a==1, b==2, c==3))
#' m
add_variables <- function(meta, names) {

  if (!all(map_lgl(names, ~ is_quosure(.) && quo_is_symbol(.)))) {
    stop("Names must be provided as a list of names", call.=FALSE)
  }

  if (!inherits(meta, 'tardis_meta')) {
    stop("meta must be a tardis_meta object", call.=FALSE)
  }

  add_variables_(meta, names)
}

#' Internal application of variables onto tardis_meta object
#' @noRd
add_variables_ <- function(meta, names) {
  meta$names <- append(meta$names, names)
  meta
}

#' @param filters A list of symbols, providing variable names of interest. Provide
#'   as a list of quosures using `rlang::quos()`
#'
#' @family Metadata additions
#' @rdname metadata_additions
#'
#' @export
add_filters <- function(meta, filters) {

  if (!all(map_lgl(filters, ~ is_quosure(.) && quo_is_call(.)))) {
    stop("Filters must be provided as a list of calls", call.=FALSE)
  }

  if (!inherits(meta, 'tardis_meta')) {
    stop("meta must be a tardis_meta object", call.=FALSE)
  }

  add_filters_(meta, filters)
}

#' Internal application of filters onto tardis_meta object
#' @noRd
add_filters_ <- function(meta, filters) {
  meta$filters <- append(meta$filters, filters)
  meta
}

#' Get the metadata dataframe from a tardis_table
#'
#' Pull out the metadata dataframe from a tardis_table to work with it directly
#'
#' @param t A tardis table with metadata built
#'
#' @return tardis metadata dataframe
#' @export
#'
#' @examples
#' t <- tardis_table(mtcars, gear) %>%
#'   add_layer(
#'     group_desc(wt)
#'   )
#'
#' t %>%
#'   build(metadata=TRUE)
#'
#' get_metadata(t)
get_metadata <- function(t) {

  if (!inherits(t, 'tardis_table')) {
    stop("t must be a tardis_table object", call.=FALSE)
  }

  if (is.null(t$metadata)){
    stop(paste(
      "t does not contain a metadata dataframe.",
      "Make sure the tardis_table was built with `build(metadata=TRUE)`"))
  }

  return(t$metadata)
}

#' Append the tardis table metadata dataframe
#'
#' `append_metadata()` allows a user to extend the tardis metadata data frame
#' with user provided data. In some tables, tardis may be able to provided most
#' of the data, but a user may have to extend the table with other summaries,
#' statistics, etc. This function allows the user to extend the tardis_table's
#' metadata with their own metadata content using custom data frames created
#' using the `tardis_meta` object.
#'
#' As this is an advanced feature of tardis, ownership is on the user to make
#' sure the metadata data frame is assembled properly. The only restrictions
#' applied by `append_metadata()` are that `meta` must have a column named
#' `row_id`, and the values in `row_id` cannot be duplicates of any `row_id`
#' value already present in the tardis metadata dataframe. `tardis_meta()` objects
#' align with constructed dataframes using the `row_id` and output dataset
#' column name. As such, `tardis_meta()` objects should be inserted into a data
#' frame using a list column.
#'
#'
#' @param t A tardis_table object
#' @param meta A dataframe fitting the specifications of the details section of
#'   this function
#'
#' @return A tardis_table object
#' @export
#' @md
#'
#' @examples
#' t <- tardis_table(mtcars, gear) %>%
#'   add_layer(
#'     group_desc(wt)
#'   )
#'
#' t %>%
#'   build(metadata=TRUE)
#'
#' m <- tibble::tibble(
#'   row_id = c('x1_1'),
#'   var1_3 = list(tardis_meta(rlang::quos(a, b, c), rlang::quos(a==1, b==2, c==3)))
#' )
#'
#' append_metadata(t, m)
append_metadata <- function(t, meta) {

  if (!('row_id' %in% names(meta))) {
    stop("The provided metadata dataset must have a column named row_id", call.=FALSE)
  }

  if (any(meta$row_id %in% t$metadata$row_id)) {
    stop(
      paste("row_id values in the provided metadata dataset are duplicates of",
            "row_id values in the tardis metadata. All row_id values must be unique.",
            call.=FALSE)
    )
  }

  t$metadata <- bind_rows(t$metadata, meta)
  t
}

#' @export
print.tardis_meta <- function(x, ...) {
  cat(sprintf("tardis_meta: %d names, %d filters\n", length(x$names), length(x$filters)))
  cat("Names:\n")
  names <- map_chr(x$names, as_label)
  filters <- map_chr(x$filters, as_label)
  cat("   ", paste(names, collapse = ", "), "\n")
  cat("Filters:\n")
  cat("   ", paste(filters, collapse = ", "), "\n")
  invisible()
}
