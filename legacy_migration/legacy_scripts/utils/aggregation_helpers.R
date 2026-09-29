get_most_likely <- function(x) {
  x_clean <- x[!is.na(x) & x != "NA" & x != ""]
  if (length(x_clean) == 0) return(NA_character_)
  tab <- table(x_clean)
  names(tab)[which.max(tab)]
}

aggregate_attributes <- function(df, group_col, attr_cols) {
  df %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      across(all_of(attr_cols), get_most_likely),
      .groups = "drop"
    )
}
