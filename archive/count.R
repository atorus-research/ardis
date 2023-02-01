
#' Prepare metadata for table
#'
#' @param x count_layer object
#' @noRd
prepare_format_metadata.count_layer <- function(x) {
  evalq({

    # Get formatting metadata prepared
    if (is.null(format_strings)) {
      format_strings <- gather_defaults(environment())
    } else if (!'n_counts' %in% names(format_strings)) {
      format_strings[['n_counts']] <- gather_defaults(environment())[['n_counts']]
    }


    # If there is both n & distinct, or pct and distinct_pct there has to be a
    # distinct_by
    # If both distinct and n
    if (((("distinct_n" %in% map(format_strings$n_counts$vars, as_name) &
           "n" %in% map(format_strings$n_counts$vars, as_name)) |
          # or both distinct_pct and pct
          ("distinct_pct" %in% map(format_strings$n_counts$vars, as_name) &
           "pct" %in% map(format_strings$n_counts$vars, as_name))) &
         # AND distinct_by is null
         is.null(distinct_by))) {
      stop("You can't use distinct and non-distinct parameters without specifying a distinct_by")
    }

    # If distinct_by isn't there, change distinct and distinct_pct
    if (is.null(distinct_by) & "distinct_n" %in% map(format_strings$n_counts$vars, as_name)) {
      distinct_ind <- which(map(format_strings$n_counts$vars, as_name) %in% "distinct_n")
      format_strings$n_counts$vars[[distinct_ind]] <- expr(n)
    }
    if (is.null(distinct_by) & "distinct_pct" %in% map(format_strings$n_counts$vars, as_name)) {
      distinct_ind <- which(map(format_strings$n_counts$vars, as_name) %in% "distinct_pct")
      format_strings$n_counts$vars[[distinct_ind]] <- expr(pct)
    }

    # Pull max character length from counts. Should be at least 1
    n_width <- max(c(nchar(numeric_data$n), 1L), na.rm = TRUE)

    # If a layer_width flag is present, edit the formatting string to display the maximum
    # character length
    if (str_detect(format_strings[['n_counts']]$format_string, "a|A")) {
      # Replace 'a' with appropriate 'x'
      replaced_string <- str_replace(format_strings[['n_counts']]$format_string, "a",
                                     paste(rep("x", n_width), collapse = ""))
      # Replace 'A' with appropriate 'X'
      replaced_string <- str_replace(replaced_string, "A",
                                     paste(rep("X", n_width), collapse = ""))

      # Make a new f_str and replace the old one
      format_strings[['n_counts']] <- f_str(replaced_string, !!!format_strings$n_counts$vars)
    }
    max_length <- format_strings[['n_counts']]$size
  }, envir = x)
}

#' @noRd
#' @export
process_formatting.count_layer <- function(x, ...) {
  evalq({

    # Calculate the indentation length. This is needed if there are missing
    #values in a nested count layer. Length is sent to string construction and
    #used to split the string.
    indentation_length <- ifelse(is.null(indentation), 0, nchar(encodeString(indentation)))

    formatted_data <- numeric_data %>%
      filter_numeric(numeric_cutoff,
                     numeric_cutoff_stat,
                     numeric_cutoff_column,
                     treat_var) %>%
      # Mutate value based on if there is a distinct_by
      mutate(n = {
        construct_count_string(.n = n, .total = total,
                               .distinct_n = distinct_n,
                               .distinct_total = distinct_total,
                               count_fmt = format_strings[['n_counts']],
                               max_layer_length = max_layer_length,
                               max_n_width = max_n_width,
                               missing_string = missing_string,
                               missing_f_str = missing_count_string,
                               summary_var = summary_var,
                               indentation_length = indentation_length,
                               total_count_format = total_count_format,
                               total_row_label = total_row_label,
                               has_missing_count = has_missing_count)
      }) %>%
      # Pivot table
      pivot_wider(id_cols = c(match_exact(by), "summary_var"),
                  names_from = c(!!treat_var, match_exact(cols)), values_from = n,
                  names_prefix = "var1_") %>%
      # Replace the by variables and target variable names with `row_label<n>`
      replace_by_string_names(quos(!!!by, summary_var))

    if (is_built_nest) {
      # I had trouble doing this in a 'tidy' way so I just did it here.
      # First column is always the outer target variable.
      # Last row label is always the inner target variable
      row_labels <- vars_select(names(formatted_data), starts_with("row_label"))
      # Replace the missing 'outer' with the original target
      # The indexing looks weird but the idea is to get rid of the matrix with the '[, 1]'
      formatted_data[is.na(formatted_data[[1]]), 1] <- formatted_data[is.na(formatted_data[[1]]),
                                                                      tail(row_labels, 1)]
    }

    if (!is_empty(stats)) {
      # Process the statistical data formatting
      formatted_stats_data <- map(stats, process_statistic_formatting) %>%
        reduce(full_join, by = c('summary_var', match_exact(c(by, head(target_var, -1))))) %>%
        # Replace the by variables and target variable names with `row_label<n>`
        replace_by_string_names(quos(!!!by, summary_var))

      formatted_data <- full_join(formatted_data, formatted_stats_data,
                                  by = vars_select(names(formatted_data), starts_with("row_label")))
    }

    # Attach the row identifier
    formatted_data <- assign_row_id(formatted_data, 'c')
  }, envir = x)

  add_order_columns(x)

  env_get(x, "formatted_data")

}

#' Format n counts for display in count_layer
#'
#' left padding = (maximum_n_width - this_n_width)
#' right padding = (maximum_layer_width - this_layer_width[after left padding])
#'
#' @param .n Vector of counts for each cell
#' @param .total  Vector of totals. Should be the same length as .n and be the
#'   denominator that column is based off of.
#' @param count_fmt The f_str object the strings are formatted around.
#' @param max_layer_length The maximum layer length of the whole table
#' @param max_n_width The maximum length of the actual numeric counts
#' @param .distinct_n Vector of distinct counts
#' @param .distinct_total Vector of total counts for distinct
#' @param missing_string The value of the string used to note missing. Usually NA
#' @param missing_f_str The f_str object used to display missing values
#' @param summary_var The summary_var values that contain the values of the
#'   target variable.
#' @param indentation_length If this is a nested count layer. The row prefixes
#'   must be removed
#'
#' @return A tibble replacing the original counts
#' @noRd
construct_count_string <- function(.n, .total, .distinct_n = NULL, .distinct_total = NULL,
                                   count_fmt = NULL, max_layer_length, max_n_width, missing_string,
                                   missing_f_str, summary_var, indentation_length, total_count_format,
                                   total_row_label, has_missing_count) {

  ## Added this for processing formatting in nested count layers where this won't be processed yet
  if (is.null(max_layer_length)) max_layer_length <- 0
  if (is.null(max_n_width)) max_n_width <- 0
  missing_rows <- FALSE
  total_rows <- FALSE

  # Add in the missing format if its null and there are missing counts
  if (has_missing_count && is.null(missing_f_str)) {
    missing_f_str <- count_fmt
  }

  if (!is.null(missing_f_str)) {

    # This subsets the indentation length for nested count layers. The 'outer'
    # values will be cut off but they will never be "missing" so that shouldn't
    # be an issue.
    summary_var <- str_sub(summary_var, indentation_length)

    missing_rows <- summary_var %in% missing_string
    missing_vars_ord <- map_chr(missing_f_str$vars, as_name)
  }

  ## Pull out string information for total rows
  if (!is.null(total_count_format)) {
    total_rows <- summary_var %in% total_row_label
    total_vars_ord <- map_chr(total_count_format$vars, as_name)
  }

  vars_ord <- map_chr(count_fmt$vars, as_name)

  # str_all is a list that contains character vectors for each parameter that might be calculated
  str_all <- vector("list", 5)
  # Append the repl_str to be passed to do.call
  str_all[1] <- count_fmt$repl_str
  # Iterate over every variable
  for (i in seq_along(vars_ord)) {
    str_all[[i + 1]] <-  count_string_switch_help(vars_ord[i], count_fmt, .n[!missing_rows & !total_rows], .total[!missing_rows & !total_rows],
                                                  .distinct_n[!missing_rows & !total_rows], .distinct_total[!missing_rows & !total_rows], vars_ord)
  }


  # Logic for missing
  # Same logic as above, just add for missing
  missing_str_all <- vector("list", 5)
  missing_str_all[1] <- missing_f_str$repl_str
  for (i in seq_along(missing_vars_ord)) {
    missing_str_all[[i + 1]] <- count_string_switch_help(missing_vars_ord[i],
                                                         missing_f_str,
                                                         .n[missing_rows],
                                                         .total[missing_rows],
                                                         .distinct_n[missing_rows],
                                                         .distinct_total[missing_rows],
                                                         missing_vars_ord)
  }

  total_str_all <- vector("list", 5)
  total_str_all[1] <- total_count_format$repl_str
  for (i in seq_along(total_vars_ord)) {
    total_str_all[[i + 1]] <- count_string_switch_help(total_vars_ord[i],
                                                       total_count_format,
                                                       .n[total_rows],
                                                       .total[total_rows],
                                                       .distinct_n[total_rows],
                                                       .distinct_total[total_rows],
                                                       total_vars_ord)
  }

  # Put the vector strings together. Only include parts of str_all that aren't null
  # nm is non-missing, m is mising, and t is total.
  string_nm <- do.call(sprintf, str_all[!map_lgl(str_all, is.null)])
  if (!is.null(missing_vars_ord)) string_m <-  do.call(sprintf, missing_str_all[!map_lgl(missing_str_all, is.null)])
  if (!is.null(total_vars_ord)) string_t <- do.call(sprintf, total_str_all[!map_lgl(total_str_all, is.null)])
  # string_ is the final string to return. Merge the missing, non-missing, and others together
  string_ <- character(length(string_nm) + length(string_m) + length(string_t))
  string_[!missing_rows & !total_rows] <- string_nm
  string_[total_rows] <-   string_t
  string_[missing_rows] <-  string_m




  # Left pad set to 0 meaning it won't pad to the left at all
  # right pad is set to the maximum n count in the table
  string_ <- pad_formatted_data(string_, 0, max_n_width)

  string_
}

#' Switch statement used in processing
#'
#' @param x Current parameter to format
#' @param count_fmt f_str object used to format
#' @param .n values used in 'n'
#' @param .total values used in pct calculations
#' @param .distinct_n values used in 'distinct_n'
#' @param vars_ord values used in distinct pct
#'
#' @noRd
count_string_switch_help <- function(x, count_fmt, .n, .total,
                                     .distinct_n, .distinct_total, vars_ord){

  switch(x,
         "n" = map_chr(.n, num_fmt, which(vars_ord == "n"), fmt = count_fmt),
         "pct" = {
           # Makea vector of ratios between n and total. Replace na values with 0
           pcts <- replace(.n/.total, is.na(.n/.total), 0)
           # Make a vector of percentages
           map_chr(pcts*100, num_fmt, which(vars_ord == "pct"), fmt = count_fmt)
         },
         "distinct_n" =  map_chr(.distinct_n, num_fmt, which(vars_ord == "distinct_n"), fmt = count_fmt),
         "distinct_pct" = {
           # Same as pct
           pcts <- replace(.distinct_n/.distinct_total, is.na(.distinct_n/.distinct_total), 0)

           map_chr(pcts*100, num_fmt, which(vars_ord == "distinct_pct"), fmt = count_fmt)
         },
         "total" = {
           map_chr(.total, num_fmt, which(vars_ord == "total"), fmt = count_fmt)
         },
         "distinct_total" = {
           map_chr(.distinct_total, num_fmt, which(vars_ord == "distinct_total"), fmt = count_fmt)
         }
  )


}
