df_targets_to_list = function(df, target_map = NULL) {
  # First, how do we map the information we're interested in to a target?

  # - If it's a table with a target map: follow target map
  # - If variable, level, proportion are columns: use those
  # - If it's a 3 column table and we have no target map: 1, 2, 3
  # - If it's a 3 column table with a busted target map: warn and 1, 2, 3
  # - If it's a non-3 column table with no target map: Error
  if(all(c("variable", "level", "proportion") %in% names(target_map))) {
    variable_ptr = target_map[["variable"]]
    level_ptr = target_map[["level"]]
    proportion_ptr = target_map[["proportion"]]
  } else if(all(c("variable", "level", "proportion") %in% names(df))) {
    variable_ptr = which(colnames(df) == "variable")
    level_ptr = which(colnames(df) == "level")
    proportion_ptr = which(colnames(df) == "proportion")
  } else if(ncol(df) == 3 &&
            all(!c("variable", "level", "proportion") %in% names(target_map))) {
    variable_ptr = 1
    level_ptr = 2
    proportion_ptr = 3
  } else if(ncol(df) == 3) {
    warning("Some but not all columns specified in target weight data frame. ",
            "Proceding assuming `variable` is the first column, `level` is ",
            "the second column, and `proportion` is the third column.")
    variable_ptr = 1
    level_ptr = 2
    proportion_ptr = 3
  }  else {
    stop("Target weights expressed as data frame must have three columns: ",
         "`variable`, `level`, and `proportion`, or else `target_map` must ",
         "be specified to map variables to targets.")
  }

  # Convert a data frame of targets to a list.
  # Preserve the order of the variables in the data frame
  fact_id = factor(df[[variable_ptr]], levels = unique(df[[variable_ptr]]))

  lapply(split(df, fact_id), function(single_var) {
    x = single_var[[proportion_ptr]]
    names(x) = single_var[[level_ptr]]
    x
  })
}

list_targets_to_df = function(target) {
  results = lapply(names(target), function(x) {
    data = target[[x]]
    data.frame(
      variable = x,
      level = names(data),
      proportion = unname(data),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}
