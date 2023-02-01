### Utility Functions

#' Extract the top function from a nested call and insert desired arguments
#'
#' @param c An R expression
#' @param allowable_calls A character vector of function names allowed to be called within a piping sequence
#' @param ... Parameters to insert into topmost call
#'
#' @return The original call object with
#'
#' @noRd
modify_nested_call <- function(c, examine_only=FALSE, ...) {

  # Get exports from tardis
  allowable_calls = getNamespaceExports("tardis")

  # Only allow the user to use `tardis` functions
  assert_that(
    call_name(c) %in% allowable_calls,
    msg = "Functions called within `add_layer` must be part of `tardis`"
    )

  # Process the magrittr pipe
  if (call_name(c) == "%>%") {
    # Only allow the user to use `tardis` functions on both sides of the pipe
    assert_that(all(map_chr(call_args(c), call_name) %in% allowable_calls),
                msg="Functions called within `add_layer` must be part of `tardis`")

    # Recursively extract the left side of the magrittr call to work your way up
    e <- call_standardise(c)
    c <- modify_nested_call(call_args(e)$lhs, examine_only, ...)
    if (!examine_only) {
      # Modify the magittr call by inserting the call retrieved from recursive command back in
      c <- call_modify(e, lhs=c)
      c
    }
  }
  # Process the 'native' pipe (arguments logically insert as first parameter)
  else if (!str_starts(call_name(c), "group_[cds]|use_template")) {

    # Standardize the call to get argument names and pull out the literal first argument
    # Save the call to a new variable in the process
    e <- call_standardise(c)
    args <- call_args(e)[1]

    # Send the first parameter back down recursively through modify_nested_call and
    # save it back to the arguments list
    c <- modify_nested_call(call_args(c)[[1]], ...)

    if (!examine_only) {
      args[[1]] <- c

      # Modify the standardized call with the modified first parameter and send it up
      c <- call_modify(e, !!!args)
      c
    }
  }
  # If the call is not from magrittr or the pipe, then modify the contents and return the call
  else if (!examine_only) {
    c <- call_modify(.call=c, ...)
  }

}

#' Find depth of a layer object
#'
#' This function returns the number of containers "above" a layer object. As
#' layers can be nested layers may contain layers and so on. This uses
#' recursion to find the table environment
#'
#' @param layer A layer object
#' @param i The current index
#'
#' @return the number of containers a layer is in
#' @noRd
depth_from_table <- function(layer, i){
  if(class(env_parent(layer))[1] == "tardis_table") return(i + 1)
  else {
    return(depth_from_table(env_parent(layer), i+1))
  }
}

#' Convert a list of quosures to character strings
#'
#' Intended for use in a tidyselect context. Pivots take arguments as character
#' strings or indices. Tidyselect tools return those indices. This allows you to
#' pass a list of quosures (which tardis carries a lot of) without explicitly
#' converting types
#'
#' @param var_list List of quosures containing variables
#'
#' @return Character string of labels
#'
#' @noRd
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#' library(dplyr)
#' library(tidyr)
#'
#' iris %>%
#'   group_by(Species) %>%
#'   summarize(mean=mean(Sepal.Length), median = median(Sepal.Length)) %>%
#'   pivot_longer(cols = match_exact(vars(mean, median)))
#'
match_exact <- function(var_list) {
  # Return the variable names as a character string in appropriate tidyselect format
  out <- map_chr(var_list, as_label) # as_label is needed here vs as_name
  unname(out[out != 'NULL']) # Exclude NULL quosures and remove names
}

#' Organize row labels within a layer output
#'
#' @param dat A data.frame/tibble to have row labels renamed
#' @param by The \code{by} object within a layer
#' @param treat_var treatment variable quosure for use when stats_as_columns is true
#' @param lab Column prefix to replace
#'
#' @return A tibble with renamed variables and row labels re-ordered to the front of the tibble
#' @noRd
replace_by_string_names <- function(dat, by, treat_var = NULL, lab = "row_label") {
  # By must be a list of quosures
  assert_that(is_quosures(by), msg = "`by` must be a list of quosures")

  by <- append(by, treat_var)

  # If there were character strings in the by variables then rename them
  # with an index, starting at 1
  for (i in seq_along(by)) {
    # If stats are present in a table and there are character values in the by variables
    # The name may be `value` or `"value"` this check catches those scenerios
    if(as_label(by[[i]]) %in% names(dat)) {
      dat <- rename(dat, !!paste0(lab, i) := as_label(by[[i]]))
    } else if(as_name(by[[i]]) %in% names(dat)) {
      dat <- rename(dat, !!paste0(lab, i) := as_name(by[[i]]))
    }

  }

  # If i iterated above, it will be have a value. Otherwise it's null, so set it to 0
  i <- ifelse(is.null(i), 0, i)

  # If there was a column named `row_label` the index it
  if ('row_label' %in% names(dat)) {
    dat <- rename(dat, !!paste0('row_label', i + 1) := row_label)
  }

  # Sort the row labels by index
  row_labels <- names(dat)[str_detect(names(dat), 'row_label')]

  # Insert row labels to the front of the tibble
  select(dat, all_of(sort(row_labels)), everything()) %>%
    ungroup() %>%
    mutate_at(row_labels, ~ as.character(.x)) # Coerce all row labels into character
}

#' Get the unique levels/factors of a dataset
#'
#' @param e An environment, generally a table or a layer object
#' @param x A target variable to get the levels/unique values of
#'
#' @return Unique target values
#' @noRd
get_target_levels <- function(e, x) {
  # If its a factor just return the levels
  if(is.factor(env_get(e, "target", inherit = TRUE)[, as_name(x)])) levels(env_get(e, "built_target", inherit = TRUE)[, as_name(x)])
  # Otherwise return the unique values
  else {
    unique(env_get(e, "built_target", inherit = TRUE)[, as_name(x)])
  }
}

#' Take a list of quosures and pull out things that aren't symbols
#'
#' @param var_list List of quosures
#'
#' @return Quosures that aren't symbols
#' @noRd
extract_character_from_quo <- function(var_list) {

  is_symbol_ <- map_lgl(var_list, quo_is_symbol)

  var_list[!is_symbol_]
}

#' Clean variable attributes
#'
#' @param dat Dataframe to strip of variable attributes
#'
#' @return Dataframe with variable attributes removed, except for factor levels
#' @noRd
clean_attr <- function(dat) {
  for (n in names(dat)) {
    for (a in names(attributes(dat[[n]]))) {
      if (!a  %in% c('levels', 'class', 'names', 'row.names', 'groups')) {
        attr(dat[[n]], a) <- NULL
      }
    }
  }
  dat
}

#' Simulate IBM rounding
#'
#' This logic is from the github issue
#' https://github.com/atorus-research/tardis/issues/9
#'
#' @param x The numeric values to round
#' @param n The number of decimal rounding points
#'
#' @return The rounded value
#' @noRd
ut_round <- function(x, n=0)
{
  # x is the value to be rounded
  # n is the precision of the rounding
  scale <- 10^n
  y <- trunc(x * scale + sign(x) * 0.5) / scale
  # Return the rounded number
  return(y)
}

#' Assign a row identifier to a layer
#'
#' To link with the metadata we need an row identifier to link
#' the metadata post sort with built data
#'
#' @param dat Input data that should be ordered identically to the metadata
#' @param layer_type First character of the layer type
#'
#' @return Data with row_id assigned
#' @noRd
assign_row_id <- function(dat, layer_type) {
  dat %>%
    mutate(
      row_id = paste0(layer_type, row_number())
    )
}
