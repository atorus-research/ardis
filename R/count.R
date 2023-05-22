
#' @noRd
#' @export
process_summaries.count_layer <- function(x, prepare_numeric = TRUE, ...) {

  if (env_get(x, "is_built_nest", default = FALSE)) {
    refresh_nest(x)
  }

  # If format strings weren't provided, then grab the defaults
  if (!has_summaries(x)) {
    # Grab the defaults available at the table or option level
    params <- gather_defaults(x)
    # Place the formats
    x <- do.call('set_summaries', append(x, params$n_counts))
  }

  # Subset the local built_target based on where
  # Catch errors
  evalq({
    tryCatch({
      # Check 'kept_levels' and stop if they're not in the target dataset
      #Logic to check for keep_levels
      # If this is not a built nest
      if (!("ardis_layer" %in% class(env_parent()))) {
        keep_levels_logic <- expr(!is.null(levels_to_keep))
        # If this is a built nest and we're begining to process
      } else if ("ardis_layer" %in% class(env_parent()) && length(target_var) == 2) {
        keep_levels_logic <- expr(!is.null(levels_to_keep) && quo_is_symbol(target_var[[1]]))
        # If this is a built nest and we are processing the "sub" layers
      } else {
        keep_levels_logic <- expr(FALSE)
      }

      # Check that all values in 'keep levels' are present in the data
      if (eval_tidy(keep_levels_logic)) {
        if (is.factor(target[[as_name(tail(target_var, 1)[[1]])]])) {
          target_levels <- levels(target[[as_name(tail(target_var, 1)[[1]])]])
        } else {
          target_levels <- unique(target[[as_name(tail(target_var, 1)[[1]])]])
        }
        kept_levels_found <- unlist(levels_to_keep) %in% target_levels
        assert_that(all(kept_levels_found),
                    msg = paste0("level passed to `kept_levels` not found: ",
                                 paste0(levels_to_keep[!kept_levels_found],
                                        collapse = "",
                                        sep = " ")))
      }

      # Save this for the denominator where, but only if it hasn't been saved yet.
      if (is.null(built_target_pre_where)) built_target_pre_where <- built_target

      built_target <- built_target %>%
        filter(!!where) %>%
        filter(!!kept_levels)

      ## Drop levels if target var is factor and kept levels used
      if (eval_tidy(keep_levels_logic) &&
         is.factor(built_target[[as_name(tail(target_var, 1)[[1]])]])) {
        # Pull out the levels that weren't in keep levels.
        target_levels <- levels(built_target[[as_name(tail(target_var, 1)[[1]])]])
        drop_levels_ind <- !(target_levels %in% levels_to_keep)
        drop_these_levels <- target_levels[drop_levels_ind]
        # Use forcats to remove the levels that weren't in the "keep levels"
        built_target <- built_target %>%
          mutate(!!tail(target_var,1)[[1]] := fct_drop(!!tail(target_var,1)[[1]], only = drop_these_levels))
      }

    }, error = function(e) {
      abort(paste0("group_count `where` condition `",
                   as_label(where),
                   "` is invalid. Filter error:\n", e))
    })

    if (!quo_is_symbol(target_var[[1]]) && as_name(target_var[[1]]) %in% names(target)) {
      warning(paste0("The first target variable has been coerced into a symbol.",
                     " You should pass variable names unquoted."), immediate. = TRUE)

      target_var[[1]] <- quo(!!sym(as_name(target_var[[1]])))
    }

    if (length(target_var) == 2 && !quo_is_symbol(target_var[[2]]) &&
                as_name(target_var[[2]]) %in% names(target)) {
      warning(paste0("The second target variable has been coerced into a symbol.",
                     "You should pass variable names unquoted."), immediate. = TRUE)

      target_var[[2]] <- quo(!!sym(as_name(target_var[[2]])))
    }

  }, envir = x)

  rename_missing_values(x)

  # Preprocssing in the case of two target_variables
  if (length(env_get(x, "target_var")) > 2) abort("Only up too two target_variables can be used in a count_layer")

  else if (length(env_get(x, "target_var")) == 2) {

    # Change treat_var to factor so all combinations appear in nest
    factor_treat_var(x)

    # If the nest_sort_index isn't null, reset it
    # This happens if the layer is reloaded
    if (!is.null(env_get(x, "nest_sort_index", default = NULL))) env_bind(x, nest_sort_index = NULL)

    process_nested_count_target(x)

  } else {

    process_count_denoms(x)

    process_single_count_target(x)

  }

  # Trigger any derivation of additional statistics
  map(x$stats, process_statistic_data)

  if (prepare_numeric) {
    prepare_numeric_data(x)
  }

  x
}

#' @param x A count layer with a single target_var
#'
#' This function uses dplyr to filter out the where call, pull out the distinct
#' rows if applicable, and tallies the different target_var values.
#'
#' If include_total_row is true a row will be added with a total row labeled
#' with total_row_label.
#'
#' Complete is used to complete the combinaions of by, treat_var, and target_var
#'
#' @noRd
process_single_count_target <- function(x) {
  evalq({

    if (is.null(include_total_row)) include_total_row <- FALSE
    if (is.null(total_row_label)) total_row_label <- "Total"

    # The current environment should be the layer itself
    process_count_n(current_env())

    if (include_total_row) {
      process_count_total_row(current_env())

      # Used to temporarily check summaries
      if (count_missings && !(is.null(denom_ignore) || length(denom_ignore) == 0) &&
         (("pct" %in% summary_vars || "distinct_pct" %in% summary_vars$vars))
         ) {
           warning("Your total row is ignoring certain values. The 'pct' in this row may not be 100%",
                   immediate. = TRUE)
         }
    }

    if (is.null(count_row_prefix)) count_row_prefix <- ""

    if (is.null(denoms_by)) denoms_by <- c(treat_var, cols)

    # rbind tables together
    numeric_data <- summary_stat %>%
      bind_rows(total_stat) %>%
      rename("summary_var" = !!target_var[[1]]) %>%
      group_by(!!!denoms_by) %>%
      do(get_denom_total(., denoms_by, denoms_df, "n")) %>%
      mutate(summary_var = prefix_count_row(summary_var, count_row_prefix)) %>%
      ungroup()


    if (!is.null(distinct_stat)) {
      if (include_total_row) {
        distinct_stat <- distinct_stat %>%
          bind_rows(total_stat_denom) %>%
          group_by(!!!denoms_by) %>%
          do(get_denom_total(., denoms_by, denoms_df, "distinct_n"))
      }
      numeric_data <- bind_cols(numeric_data,
                                distinct_stat[, c("distinct_n", "distinct_total")])
    }

  }, envir = x)
}

#' Process the n count data and put into summary_stat
#'
#' @param x Count layer
#' @noRd
process_count_n <- function(x) {

  evalq({

    if (is.null(denoms_by)) denoms_by <- c(treat_var, cols)
    denoms_by_ <- map(denoms_by, function(x) {
      if (as_name(x) == "summary_var") quo(!!target_var[[1]])
      else x
    })

    summary_stat <- built_target %>%
      mutate(
        across(
          .cols = any_of(map_chr(c(denoms_by, target_var, by), ~as_name(.))),
          .fns = function(x) if (is.factor(x)) x else as.factor(x)
        )
      ) %>%
      # Group by variables including target variables and count them
      group_by(!!treat_var, !!!by, !!!target_var, !!!cols) %>%
      summarize(
        n = n(),
        distinct_n = n_distinct(!!!distinct_by, !!treat_var, !!!target_var)
      ) %>%
      mutate(
        n = as.double(n),
        distinct_n = as.double(distinct_n)
      ) %>%
      ungroup()

    # If there is a missing_count_string, but its not in the dataset
    if (has_missing_count &&

       !((any(unname(unlist(missing_count_list)) %in% unique(built_target[, as_name(target_var[[1]])]))) ||
        any(is.na(built_target[, as_name(target_var[[1]])])))) {
      # This adds the missing string as a factor to the tallies. This is needed
      # to make sure the missing row is added even if there are no missing values.
      summary_stat <- summary_stat %>%
        mutate(!!target_var[[1]] := fct_expand(.data[[as_name(target_var[[1]])]],
                                               names(missing_count_list)))
    }

    summary_stat <- summary_stat %>%
      # complete all combinations of factors to include combinations that don't exist.
      # add 0 for combinations that don't exist
      complete(!!treat_var, !!!by, !!!target_var, !!!cols,
               fill = list(n = 0, total = 0, distinct_n = 0, distinct_total = 0)) %>%
      # Change the treat_var and first target_var to characters to resolve any
      # issues if there are total rows and the original column is numeric
      mutate(!!treat_var := as.character(!!treat_var)) %>%
      mutate(!!as_name(target_var[[1]]) := as.character(!!target_var[[1]])) %>%
      group_by(!!!denoms_by_) %>%
      ungroup()

    rm(denoms_by_)
    # If there is no values in summary_stat, which can happen depending on where. Return nothing
    if (nrow(summary_stat) == 0) return()
  }, envir = x)

}

#' Process the amounts for a total row
#'
#' @param x A Count layer
#' @noRd
process_count_total_row <- function(x) {
  evalq({

    # Check if denoms_by wasn't passed and by was passed.
    if (exists("include_total_row") && include_total_row &&
        identical(denoms_by, c(treat_var, cols)) && any(map_lgl(by, quo_is_symbol)) > 0) {
      warning("A total row was added in addition to non-text by variables, but
no denoms_by variable was set. This may cause unexpected results. If you wish to
change this behavior, use `set_denoms_by()`.", immediate. = TRUE)
    }

    # Make sure the denoms_by is stripped
    # Stripped of cols and treat_var variables, otherwise it will error out in the group_by
    # I thought of replacing the group by with !!!unique(c(treat_var, cols, denoms_by))
    # but that doesn't work due to the denoms_by having an environment set

    # Logical vector that is used to remove the treat_var and cols
    needed_denoms_by <- map_lgl(denoms_by, function(x, treat_var, cols) {
      all(as_name(x) != as_name(treat_var),
          as_name(x) != map_chr(cols, as_name))
    }, treat_var, cols)

    #Create an expression to evaluate filter
    if (!count_missings) {
      filter_logic <- expr(!(!!target_var[[1]] %in% names(missing_count_list)))
    } else {
      filter_logic <- expr(TRUE)
    }

    # create a data.frame to create total counts
    total_stat <- summary_stat %>%
      #Filter out any ignored denoms
      filter(!!filter_logic) %>%
      # Use distinct if this is a distinct total row
      # Group by all column variables
      group_by(!!treat_var, !!!cols, !!!denoms_by[needed_denoms_by]) %>%
      summarize(
        n = sum(n),
        distinct_n = sum(distinct_n)
      ) %>%
      ungroup() %>%
      # Create a variable to label the totals when it is merged in.
      mutate(!!as_name(target_var[[1]]) := total_row_label) %>%
      # Create variables to carry forward 'by'. Only pull out the ones that
      # aren't symbols
      group_by(!!!extract_character_from_quo(by)) %>%
      # ungroup right away to make sure the complete works
      ungroup() %>%
      # complete based on missing groupings
      complete(!!treat_var, !!!cols, fill = list(n = 0, total = 0))
  }, envir = x)
}


#' @param x Count Layer
#'
#' When nesting a count layer in some cases a treatment group will not apear in one of the
#' groups so this will turn the variable into a factor to force it to complete in the
#' complete logic
#'
#' @noRd
factor_treat_var <- function(x) {
  evalq({

    built_target[, as_name(treat_var)] <- as.factor(unlist(built_target[, as_name(treat_var)]))

  }, envir = env_parent(x))
}


#' Prefix a row with a specifed character
#'
#' @param row_i The row to prefix
#' @param count_row_prefix The prefix
#'
#' @return The modified row
#' @noRd
prefix_count_row <- function(row_i, count_row_prefix) {

  paste0(count_row_prefix, row_i)

}

#' @noRd
process_count_denoms <- function(x) {

  evalq({

    # This used in case there is a character passed to the layer
    layer_params <- c(target_var, treat_var, by, cols)
    # Logical vector indicating if the param appears in the target dataset.
    param_apears <- map_lgl(layer_params, function(x) {
      as_name(x) %in% names(target)
    })

    # Raise errors if a denom is ignored but there isn't a missing count string
    if (!is.null(denom_ignore) && is.null(missing_count_string)) {
      abort("A value(s) were set with 'denom_ignore' but no missing count was set. Your percentages/totals may not have meaning.")
    }

    # Logic to determine how to subset target for denominators.
    if (is.null(denom_where)) {
      # If a pop_data was passed change the denom_where to the pop_data_where
      if (!isTRUE(try(identical(pop_data, target)))) {
        denom_where <- quo(TRUE)
      } else {
        # Otherwise make denom_where equal to table where
        denom_where <- where
      }
    }

    # Because the missing strings haven't replaced the missing strings, it has to happen here.
    # Expand denoms contains the
    if (!is.null(missing_count_list)) {
      expand_denoms <- names(missing_count_list) %in% unlist(denom_ignore)
      denom_ignore <- c(denom_ignore, unname(missing_count_list[expand_denoms]))
    }


    # Subset the local built_target based on where
    # Catch errors
    tryCatch({
      denom_target <- built_target_pre_where %>%
        filter(!!denom_where) %>%
        filter(!(!!target_var[[1]] %in% unlist(denom_ignore)))
    }, error = function(e) {
      abort(paste0("group_count `where` condition `",
                   as_label(denom_where),
                   "` is invalid. Filter error:\n", e))
    })

      # For distinct counts, we want to defer back to the
      # population dataset. Trigger this by identifying that
    # the population dataset was overridden
    if (!isTRUE(try(identical(pop_data, target)))) {
      # If the denom_where doesn't match the where AND the denom_where isn't true
      # than the user passed a custom denom_where
      if (deparse(denom_where) != deparse(where) && !isTRUE(quo_get_expr(denom_where))) {
        warning(paste0("A `denom_where` has been set with a pop_data. The `denom_where` has been ignored.",
                       "You should use `set_pop_where` instead of `set_denom_where`.", sep = "\n"),
                immediate. = TRUE)
      }
    }

    denoms_df_n <- denom_target %>%
      group_by(!!!layer_params[param_apears]) %>%
      summarize(
        n = n()
        ) %>%
      ungroup()

    # Use cached header N's if they're available
    if (exists("cached_header_n")) {
      denoms_df_dist <- header_n %>%
        rename(distinct_n = n)

      # If there was a treatment variable remap provided, add it
      if (!is.null(header_treat_var)) {
        by_join <- as_name(header_treat_var)
      }
    } else {
      # Calculate header N's from pop data if it wasn't available
      denoms_df_dist <- built_pop_data %>%
        filter(!!denom_where) %>%
        group_by(!!pop_treat_var) %>%
        summarize(
          distinct_n = n_distinct(!!!distinct_by, !!pop_treat_var)
        ) %>%
        ungroup()

      by_join <- as_name(pop_treat_var)
    }

    names(by_join) <- as_name(treat_var)

    denoms_df <- denoms_df_n %>%
      complete(!!!layer_params[param_apears],
               fill = list(n = 0)) %>%
      left_join(denoms_df_dist, by = by_join)

    if (as_name(target_var[[1]]) %in% names(target)) {
      denoms_df <- denoms_df %>%
        rename("summary_var" := !!target_var[[1]])
    }

  }, envir = x)

}

rename_missing_values <- function(x) {
  evalq({
    # Rename missing values
    if (!is.null(missing_count_list)) {
      missing_count_list_ <- missing_count_list
      # If the target variable isn't a character or a factor. Coerse it as a
      # character. This can happen if the target var is numeric
      if (!(class(built_target[, as_name(target_var[[1]])][[1]]) %in% c("factor", "character"))) {
        built_target <- built_target %>%
          mutate(!!target_var[[1]] := as.character(!!target_var[[1]]))
      }
      # Collapse the factors that were missing.
      for (i in seq_along(missing_count_list)) {

        # Logic if the missing_count_list contains an implicit NA
        if (any(is.nan(missing_count_list[[i]]))) {
          ## Repalce the NA in the missing_count list with an explicit value
          missing_count_list_[[i]] <- ifelse(missing_count_list[[i]] == "NaN", "(Missing_NAN)", as.character(missing_count_list[[i]]))
          # Replace the implicit values in built_target
          built_target <- built_target %>%
            mutate(!!target_var[[1]] := fct_expand(!!target_var[[1]], "(Missing_NAN)")) %>%
            mutate(!!target_var[[1]] := ifelse(is.nan(!!target_var[[1]]), "(Missing_NAN)", as.character(!!target_var[[1]])))

        } else if (any(is.na(missing_count_list[[i]]))) {
          ## Repalce the NA in the missing_count list with an explicit value
          missing_count_list_[[i]] <- ifelse(is.na(as.character(missing_count_list[[i]])) , "(Missing)", as.character(missing_count_list[[i]]))
          # Replace the implicit values in built_target
          built_target <- built_target %>%
            mutate(!!target_var[[1]] := fct_expand(!!target_var[[1]], "(Missing)")) %>%
            mutate(!!target_var[[1]] := fct_explicit_na(!!target_var[[1]]))

        }
        built_target <- built_target %>%
          mutate(
            # Warnings suppressed here. They can happen if something is called missing
            # That isn't in the data, that isn't something to warn about in this context
            !!target_var[[1]] := suppressWarnings(fct_collapse(!!target_var[[1]], !!names(missing_count_list)[i] := missing_count_list_[[i]]))
          )
      }
    }
  }, envir = x)
}

filter_numeric <- function(.data,
                           numeric_cutoff,
                           numeric_cutoff_stat,
                           numeric_cutoff_column,
                           treat_var,
                           by = NULL) {

  if (is.null(numeric_cutoff)) {
    return(.data)
  }

    vals <- .data %>%
      {if (is.null(numeric_cutoff_column)) . else filter(., !!treat_var == numeric_cutoff_column)} %>%
      mutate(
        pct = n/total,
        distinct_pct = distinct_n/distinct_total
      ) %>%
      filter(!!sym(numeric_cutoff_stat) >= !!numeric_cutoff) %>%
      extract2("summary_var")

    .data %>%
      filter(summary_var %in% vals)
}

prepare_numeric_data <- function(x) {
  evalq({

    # TODO: Update processing for this later - this is some ugly renaming
    # just to prevent downstream impacts
    num_data <- numeric_data

    # if (!('summary_vars' %in% ls())) {
    #   summary_vars <- format_strings$n_counts$vars
    # }

    numeric_data <- numeric_data %>%
      mutate(
        pct = n / total,
        distinct_pct = distinct_n / distinct_total,
      ) %>%
      pivot_longer(cols = match_exact(summary_vars), names_to = "stat")

    # Define the row labels established in set_summaries()
    # and apply them to the numeric data
    if (is_named(summary_grps)){
      row_labels <- name_translator_numeric(summary_grps)
      numeric_data <- numeric_data %>%
        rowwise() %>%
        # Add in the row labels
        mutate(
          row_label = row_labels[[stat]]
        ) %>%
        ungroup() %>%
        select(!!treat_var, !!!cols, summary_var, match_exact(by), stat, row_label, value)
    } else {
      numeric_data <- numeric_data %>%
        select(!!treat_var, !!!cols, summary_var, match_exact(by), stat, value)
    }

    if (length(target_var_saved) == 2) {
      numeric_data <- numeric_data %>%
        # Reset by var to the inner layer value
        mutate(
          # Sort helper for interleaving outer layers
          sort_helper = if_else(
            is.na(!!target_var_saved[[1]]),
            1,
            2
          ),
          !!target_var_saved[[1]] := if_else(
            sort_helper == 1,
            as.character(summary_var),
            as.character(!!target_var_saved[[1]])
          )
        ) %>%
        arrange(!!!by, sort_helper, summary_var) %>%
        select(-sort_helper)
    }

    # Rename remaining vars
    if ('row_label' %in% names(numeric_data)) {
      numeric_data <- numeric_data %>%
        # Replace by variable names
        replace_by_string_names(quos(summary_var, !!!by, row_label))
    } else {
      numeric_data <- numeric_data %>%
        # Replace by variable names
        replace_by_string_names(quos(summary_var, !!!by))
    }

    numeric_data <- numeric_data %>%
      # Replace column names
      replace_by_string_names(quos(!!treat_var, !!!cols), lab="col") %>%
      rename(param=stat)

  }, envir=x)
}
