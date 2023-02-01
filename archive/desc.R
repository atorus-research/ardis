#' Format processing for desc layers
#'
#' @param x layer object
#'
#' @return Formatted and processed data
#' @noRd
#' @export
process_formatting.desc_layer <- function(x, ...) {

  # Execute in the layer environment
  evalq({
    # Initialize list for formatted, transposed outputs
    form_sums <- vector("list", length(target_var))

    if (need_prec_table) {
      if ('prec' %in% ls()) {
        # If precision data was manually specified, grab it
        prec <- get_prec_data(built_target, prec, precision_by, precision_on, cap, prec_error)
      } else {
        # Otherwise create it
        prec <- make_prec_data(built_target, precision_by, precision_on, cap)
      }
    }

    for (i in seq_along(trans_sums)) {
      # Format the display strings - this is just applying construct_desc_string to each row of
      # the data.frame

      if (need_prec_table) {
        # Merge the precision data on
        trans_sums[[i]] <- left_join(trans_sums[[i]], prec, by=c(match_exact(precision_by), 'precision_on'))
      }

      # Reset the scientific notation presentation settings temporarily
      trans_sums[[i]]['display_string'] <- pmap_chr(trans_sums[[i]],
                                                    function(...) construct_desc_string(...,
                                                                                        .fmt_str = format_strings),
                                                    format_strings=format_strings)

      # Now do one more transpose to split the columns out
      # Default is to use the treatment variable, but if `cols` was provided
      # then also transpose by cols.
      if (stats_as_columns) {
        form_sums[[i]] <- trans_sums[[i]] %>%
          pivot_wider(id_cols=c(!!treat_var, match_exact(by)), # Keep row_label and the by variables
                      names_from = match_exact(vars(row_label, !!!cols)), # Pull the names from treatment and cols argument
                      names_prefix = paste0('var', i, "_"), # Prefix with the name of the target variable
                      values_from = display_string # Use the created display_string variable for values
          )

      } else {
        form_sums[[i]] <- trans_sums[[i]] %>%
          pivot_wider(id_cols=c('row_label', match_exact(by)), # Keep row_label and the by variables
                      names_from = match_exact(vars(!!treat_var, !!!cols)), # Pull the names from treatment and cols argument
                      names_prefix = paste0('var', i, "_"), # Prefix with the name of the target variable
                      values_from = display_string # Use the created display_string variable for values
          )

      }
    }

    # Join the final outputs
    if (stats_as_columns) {
      formatted_data <- reduce(form_sums, full_join, by=c(as_label(treat_var), match_exact(by)))

      # Replace row label names
      formatted_data <- replace_by_string_names(formatted_data, by, treat_var)
    } else {
      formatted_data <- reduce(form_sums, full_join, by=c('row_label', match_exact(by)))

      # Replace row label names
      formatted_data <- replace_by_string_names(formatted_data, by)
    }


    # Don't want to delete this until I'm absolutely sure it's not necessary
    # formatted_data <- formatted_data %>%
    #   rowwise() %>%
    #   # Replace NA values with the proper empty strings
    #   mutate_at(vars(starts_with('var')), ~ replace_na(.x, format_strings[[row_label]]$empty))


    # Clean up
    rm(form_sums, i)

    formatted_data <- assign_row_id(formatted_data, 'd')

  }, envir=x)

  add_order_columns(x)

  env_get(x, "formatted_data")
}

#' Format a descriptive statistics display string
#'
#' Intended to be applied through a map_chr call
#'
#' @param ... A row of a data frame - captured through the ellipsis argument
#' @param .fmt_str The format strings container with all f_str objects and row labels
#'
#' @return A character vector of display formatted strings
#' @noRd
construct_desc_string <- function(..., .fmt_str=NULL) {
  # Unpack names into current namespace for ease
  list2env(list(...), envir=environment())

  # Get the current format to be applied
  fmt <- .fmt_str[[row_label]]

  # If all the values summarized are NA then return the empty string
  if (all(is.na(append(map(fmt$vars[-1], eval, envir=environment()), value)))) {
    if ('.overall' %in% names(fmt$empty)) {
      return(fmt$empty['.overall'])
    }
  }

  # Make the autos argument
  if (fmt$auto_precision) {
    autos <- c('int'=max_int, 'dec'=max_dec)
  } else {
    autos <- c('int'=0, 'dec'=0)
  }
  # Format the transposed value
  fmt_args <- list(fmt = fmt$repl_str, num_fmt(value, 1, fmt, autos))


  # Now evaluate any additional numbers that must be formatted
  # Exclude the initial variable because it's already been evaluated
  # i is intended to start on the second argument so +1 in the num_fmt call
  fmt_args <- append(fmt_args,
                     imap(fmt$vars[-1],
                          function(val, i, fmt, autos) num_fmt(eval(val), i+1, fmt, autos),
                          fmt=fmt,
                          autos=autos)
  )

  # Apply the call to sprintf
  do.call(sprintf, fmt_args)
}
