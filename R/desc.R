#' Process numeric data for a layer of type \code{desc}
#'
#' @param x Layer object
#'
#' @return Nothing
#' @export
#' @noRd
process_summaries.desc_layer <- function(x, ...) {

  # If format strings weren't provided, then grab the defaults
  if (!has_summaries(x)) {
    # Grab the defaults available at the table or option level
    params <- gather_defaults(x)
    # Place the formats
    x <- do.call('set_summaries', append(x, params))
  }

  # Execute in the layer environment
  evalq({
    # trans_sums is the data that will pass forward to be formatted
    trans_sums <- vector("list", length(target_var))
    # num_sums is the data that will be bound together and returned to provide
    # the numeric internal values
    # num_sums_raw is kept separate to better facililate use for prep of metadata
    num_sums_raw <- vector("list", length(target_var))
    num_sums <- vector("list", length(target_var))

    # Get the row labels out the summaries
    row_labels_num <- name_translator_numeric(summary_grps)
    row_labels <- row_labels_num

    # Subset the local built_target based on where
    # Catch errors
    tryCatch({
      built_target <- built_target %>%
        filter(!!where)
    }, error = function(e) {
      abort(paste0("group_desc `where` condition `",
                   as_label(where),
                   "` is invalid. Filter error:\n", e))
    })

    # Extract the list of summaries that need to be performed
    for (i in seq_along(target_var)) {

      # Pull out the target variable being iterated
      cur_var <- target_var[[i]]

      # Get the summaries that need to be performed for this layer
      summaries <- get_summaries()[match_exact(summary_vars)]

      # Create the numeric summary data
      num_sums[[i]] <- built_target %>%
        # Rename the current variable to make each iteration use a generic name
        rename(.var = !!cur_var) %>%
        # Group by treatment, provided by variable, and provided column variables
        group_by(!!treat_var, !!!by, !!!cols) %>%
        # Execute the summaries
        summarize(!!!summaries) %>%
        ungroup() %>%
        # Fill in any missing treat/col combinations
        complete(!!treat_var, !!!by, !!!cols)

      # Numeric data needs the variable names replaced and add summary variable name
      num_sums[[i]] <- num_sums[[i]] %>%
        mutate(summary_var = as_name(cur_var)) %>%
        select(summary_var, everything())

      # Clean up loop
      rm(cur_var, summaries, i)
    }

    # Bind the numeric data together within the layer
    numeric_data <- pivot_longer(bind_rows(num_sums), cols = match_exact(summary_vars), names_to = "stat")

    numeric_data <- numeric_data %>%
      rowwise() %>%
      # Add in the row labels
      mutate(
        row_label = row_labels_num[[stat]]
      ) %>%
      ungroup() %>%
      # Replace by variable names
      replace_by_string_names(quos(summary_var, !!!by, row_label)) %>%
      # Replace column names
      replace_by_string_names(quos(!!treat_var, !!!cols), lab="col") %>%
      rename(param=stat)

  }, envir=x)
}

#' Get the summaries to be passed forward into \code{dplyr::summarize()}
#'
#' @param e the environment summaries are stored in.
#'
#' @return A list of expressions to be unpacked in \code{dplyr::summarize}
#' @noRd
get_summaries <- function(e = caller_env()) {

  # Define the default list of summaries
  summaries <- exprs(
    n       = n(),
    mean    = mean(.var, na.rm=TRUE),
    sd      = sd(.var, na.rm=TRUE),
    median  = median(.var, na.rm=TRUE),
    var     = var(.var, na.rm=TRUE),
    min     = min(.var, na.rm=TRUE),
    max     = max(.var, na.rm=TRUE),
    iqr     = IQR(.var, na.rm=TRUE, type=getOption('tardis.quantile_type')),
    q1      = quantile(.var, na.rm=TRUE, type=getOption('tardis.quantile_type'))[[2]],
    q3      = quantile(.var, na.rm=TRUE, type=getOption('tardis.quantile_type'))[[4]],
    missing = sum(is.na(.var))
  )

  append(summaries, get_custom_summaries(e), after=0)
}
