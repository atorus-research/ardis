#' ardis Metadata Object
#'
#' If a ardis table is built with the `metadata=TRUE` option specified, then
#' metadata is assembled behind the scenes to provide traceability on each
#' result cell derived. The functions `get_meta_result()` and
#' `get_meta_subset()` allow you to access that metadata by using an ID provided
#' in the row_id column and the column name of the result you'd like to access.
#' The purpose is of the row_id variable instead of a simple row index is to
#' provide a sort resistant reference of the originating column, so the output
#' ardis table can be sorted in any order but the metadata are still easily
#' accessible.
#'
#' The `ardis_meta` object provided a list with two elements - names and
#' filters. The names contain every column from the target data.frame of the
#' ardis table that factored into the specified result cell, and the filters
#' contains all the necessary filters to subset the target data to create the
#' specified result cell. `get_meta_subset()` additionally provides a parameter to
#' specify any additional columns you would like to include in the returned
#' subset data frame.
#'
#' @param names List of symbols
#' @param filters  List of expressions
#'
#' @return ardis_meta object
#' @export
#'
#' @examples
#'
#' ardis_meta(
#'    names = rlang::quos(x, y, z),
#'    filters = rlang::quos(x == 1, y==2, z==3)
#'  )
#'
ardis_meta <- function(names=list(), filters=exprs()) {
  meta <- new_ardis_meta()
  meta <- add_variables(meta, names)
  meta <- add_filters(meta, filters)
  meta
}

#' Create a ardis_meta object
#'
#' @return ardis_meta object
#' @noRd
new_ardis_meta <- function(names = list(), filters=exprs()) {
  structure(
    list(
      names = names,
      filters = filters
    ),
    class = 'ardis_meta'
  )
}

#' Add variables to a ardis_meta object
#'
#' Add additional variable names to a `ardis_meta()` object.
#'
#' @param meta A ardis_meta object
#' @param names A list of names, providing variable names of interest. Provide
#'   as a list of quosures using `rlang::quos()`
#'
#' @return ardis_meta object
#' @md
#'
#' @family Metadata additions
#' @rdname metadata_additions
#'
#' @export
#'
#' @examples
#'
#' m <- ardis_meta()
#' m <- add_variables(m, rlang::quos(a, b, c))
#' m <- add_filters(m, rlang::quos(a==1, b==2, c==3))
#' m
add_variables <- function(meta, names) {

  if (!all(map_lgl(names, ~ is_quosure(.) && quo_is_symbol(.)))) {
    stop("Names must be provided as a list of names", call.=FALSE)
  }

  if (!inherits(meta, 'ardis_meta')) {
    stop("meta must be a ardis_meta object", call.=FALSE)
  }

  add_variables_(meta, names)
}

#' Internal application of variables onto ardis_meta object
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

  if (!inherits(meta, 'ardis_meta')) {
    stop("meta must be a ardis_meta object", call.=FALSE)
  }

  add_filters_(meta, filters)
}

#' Internal application of filters onto ardis_meta object
#' @noRd
add_filters_ <- function(meta, filters) {
  meta$filters <- append(meta$filters, filters)
  meta
}

#' Get the metadata dataframe from a ardis
#'
#' Pull out the metadata dataframe from a ardis to work with it directly
#'
#' @param t A ardis table with metadata built
#'
#' @return ardis metadata dataframe
#' @export
#'
#' @examples
#' t <- ardis(mtcars, gear) %>%
#'   add_layer(
#'     group_desc(wt)
#'   )
#'
#' t %>%
#'   build(metadata=TRUE)
#'
#' get_metadata(t)
get_metadata <- function(t) {

  if (!inherits(t, 'ardis')) {
    stop("t must be a ardis object", call.=FALSE)
  }

  if (is.null(t$metadata)){
    stop(paste(
      "t does not contain a metadata dataframe.",
      "Make sure the ardis was built with `build(metadata=TRUE)`"))
  }

  return(t$metadata)
}

#' Append the ardis table metadata dataframe
#'
#' `append_metadata()` allows a user to extend the ardis metadata data frame
#' with user provided data. In some tables, ardis may be able to provided most
#' of the data, but a user may have to extend the table with other summaries,
#' statistics, etc. This function allows the user to extend the ardis's
#' metadata with their own metadata content using custom data frames created
#' using the `ardis_meta` object.
#'
#' As this is an advanced feature of ardis, ownership is on the user to make
#' sure the metadata data frame is assembled properly. The only restrictions
#' applied by `append_metadata()` are that `meta` must have a column named
#' `row_id`, and the values in `row_id` cannot be duplicates of any `row_id`
#' value already present in the ardis metadata dataframe. `ardis_meta()` objects
#' align with constructed dataframes using the `row_id` and output dataset
#' column name. As such, `ardis_meta()` objects should be inserted into a data
#' frame using a list column.
#'
#'
#' @param t A ardis object
#' @param meta A dataframe fitting the specifications of the details section of
#'   this function
#'
#' @return A ardis object
#' @export
#' @md
#'
#' @examples
#' t <- ardis(mtcars, gear) %>%
#'   add_layer(
#'     group_desc(wt)
#'   )
#'
#' t %>%
#'   build(metadata=TRUE)
#'
#' m <- tibble::tibble(
#'   row_id = c('x1_1'),
#'   var1_3 = list(ardis_meta(rlang::quos(a, b, c), rlang::quos(a==1, b==2, c==3)))
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
            "row_id values in the ardis metadata. All row_id values must be unique.",
            call.=FALSE)
    )
  }

  t$metadata <- bind_rows(t$metadata, meta)
  t
}

#' @export
print.ardis_meta <- function(x, ...) {
  cat(sprintf("ardis_meta: %d names, %d filters\n", length(x$names), length(x$filters)))
  cat("Names:\n")
  names <- map_chr(x$names, as_label)
  filters <- map_chr(x$filters, as_label)
  cat("   ", paste(names, collapse = ", "), "\n")
  cat("Filters:\n")
  cat("   ", paste(filters, collapse = ", "), "\n")
  invisible()
}
