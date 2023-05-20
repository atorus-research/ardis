#' Retrieve the numeric data from a ardis objects
#'
#' \code{build} provides access to the un-formatted numeric data for
#' each of the layers within a \code{ardis}, with options to allow you to
#' extract distinct layers and filter as desired.
#'
#' When used on a \code{ardis} object, this method will aggregate the
#' numeric data from all ardis layers. The data will be returned to the user in
#' a list of data frames. If the data has already been processed (i.e.
#' \code{build} has been run), the numeric data is already available and will be
#' returned without reprocessing. Otherwise, the numeric portion of the layer
#' will be processed.
#'
#' Using the layer and where parameters, data for a specific layer can be
#' extracted and subset. This is most clear when layers are given text names
#' instead of using a layer index, but a numeric index works as well.
#'
#' @param x A ardis or ardis_layer object
#' @param layer Layer name or index to select out specifically
#' @param where Subset criteria passed to dplyr::filter
#' @param ... Additional arguments to pass forward
#'
#' @return Numeric data from the ardis layer
#' @export
#'
#' @examples
#'
#' t <- ardis(mtcars, gear) %>%
#'  add_layer(name='drat',
#'            group_desc(drat)
#'  ) %>%
#'  add_layer(name='cyl',
#'            group_count(cyl)
#'  )
#'
#'  # Return a list of the numeric data frames
#'  build(t)
#'
#'  # Get the data from a specific layer
#'  build(t, layer='drat')
#'  build(t, layer=1)
#'
#'  # Choose multiple layers by name or index
#'  build(t, layer=c('cyl', 'drat'))
#'  build(t, layer=c(2, 1))
#'
#'  # Get the data and filter it
#'  build(t, layer='drat', where = gear==3)
#'
build <- function(x, layer=NULL, where=TRUE, ...) {
  UseMethod("build")
}


#' Get numeric data from a ardis object
#' @export
#' @noRd
build.ardis <- function(x, layer=NULL, where=TRUE, ...) {

  where <- enquo(where)

  # If where is provided then a layer must be specified
  assert_that(!(quo_get_expr(where) != TRUE && (is.null(layer) || length(layer)!=1)),
              msg="If `where` is provided, a single `layer` value must be specified")

  # If layer is a numeric value then it must be in range
  if (is.numeric(layer)) {
    assert_that(all(between(layer, 1, length(x$layers))), msg="Provided layer index is out of range")
  } else if (!is.null(layer) && !is.null(names(x$layers))) {
    # The provided name must exist
    missing_layers <- layer %in% names(x$layers)
    assert_that(all(missing_layers), msg=paste0("Layer(s) ", paste0(layer[!missing_layers], collapse=", "), " do(es) not exist"))
  }

  # If the pre-build wasn't executed then execute it
  if (!'built_target' %in% ls(x)) {
    treatment_group_build(x)
    x <- build_header_n(x)
  }

  # If not picking a specific layer, then get all the numeric data
  if (is.null(layer)) {
    df_ls <- map(x$layers, build)
    layer_type <- map(x$layers, ~class(.)[[2]])

    out <- map2_dfr(df_ls, layer_type, function(data, type){
      if(type == "desc_layer"){
        max_label <- names(data)[str_detect(names(data), "row_label")] %>%
          str_remove_all("row_label") %>%
          as.numeric() %>%
          max() %>%
          paste0("row_label", .)
        data <- data %>%
          select(-row_label1,
                 row_label1 = max_label)
      }
      data %>%
        mutate(across(starts_with("row_label"), str_trim))
    })
    return(out)
  } else if (length(layer) > 1) {
    # If the layer variable was multiple elements then grap all of them
    df_ls <-map(x$layers[layer], build)
    layer_type <- map(x$layers[layer], ~class(.)[[2]])

    map2_dfr(df_ls, layer_type, function(data, type){
      if(type == "desc_layer"){
        max_label <- names(data)[str_detect(names(data), "row_label")] %>%
          str_remove_all("row_label") %>%
          as.numeric() %>%
          max() %>%
          paste0("row_label", .)
        data <- data %>%
          select(-row_label1,
                 row_label1 = max_label)
      }
      data %>%
        mutate(across(starts_with("row_label"), str_trim))
    })
    return(out)
  } else {
    # Otherwise, pick it out and filter
    build(x$layers[[layer]]) %>%
      filter(!!where) %>%
      select(starts_with('row_label'), everything())
  }
}


#' Get numeric data from a ardis_layer object
#' @export
#' @noRd
build.ardis_layer <- function(x, layer=NULL, where=TRUE, ...) {

  # If the numeric data doesn't exist in the layer then process it
  if (!'numeric_data' %in% ls(x)) {
    process_summaries(x)
  }

  # Return the object
  env_get(x, 'numeric_data')
}


#' Process layers to get numeric results of layer
#'
#' This is an internal method, but is exported to support S3 dispatch. Not intended for direct use by a user.
#' @param x a ardis_layer object
#' @param ... arguments passed to dispatch
#'
#' @return The ardis_layer object with a 'built_table' binding
#' @export
#' @keywords internal
process_summaries <- function(x, ...) {
  UseMethod("process_summaries")
}
