
#' Remove Test Observation
#'
#' This function filters a dataframe to remove rows where 'test' or 'testing'
#' (case-insensitive) is found in the Site_ID or Plot_ID columns.
#'
#' @param df The input dataframe to be cleaned.
#' @return A dataframe with the test rows removed.

remove_test_observations <- function(df) {
  # Store the original number of rows for comparison
  original_rows <- nrow(df)

  # Define the pattern to search for ("test" or "testing", case-insensitive)
  test_pattern <- regex("test|testing", ignore_case = TRUE)

  # Filter using if_all and any_of to handle missing columns safely
  cleaned_df <- df %>%
    filter(
      if_all(
        any_of(c("site_name", "plot_name")),
        ~ !str_detect(replace_na(.x, ""), test_pattern)
      )
    )

  # Calculate the number of rows that were removed
  rows_removed <- original_rows - nrow(cleaned_df)

  # Print a summary message to the console
  message(paste0(rows_removed, " test observation(s) removed. ", nrow(cleaned_df), " rows remaining."))

  # Return the cleaned dataframe
  return(cleaned_df)
}

#' Remove Test Observations
#'
#' This function filters a dataframe to remove rows where 'test' or 'testing'
#' (case-insensitive) is found in the Site_ID or Plot_ID columns.
#'
#' @param df The input dataframe to be cleaned.
#' @return A dataframe with the test rows removed.

#' Apply Cleaning Log
#' This function applies corrections or deletions from a cleaning log to a given dataframe.
#' @param df The input dataframe to be cleaned.
#' @param log_file_path The file path to the Excel file containing the cleaning log.
#' @param sheet_name The name of the sheet in the Excel file that contains the cleaning log.
#' @param key_col The name of the composite key column to be created for matching.
#' @param key_components A vector of column names that will be combined to create the composite key for matching.
#' @param is_deletion A boolean indicating if the sheet is meant to drop matching rows entirely.
#' @return A cleaned dataframe with corrections or deletions applied.

apply_cleaning_log <- function(
  df,
  df_name,
  log_file_path,
  sheet_name,
  key_col,
  key_components,
  is_deletion = FALSE
  ) {

  # Read cleaning log and create composite keys
  cleaning_log <- read_xlsx(log_file_path, sheet = sheet_name)

  # Create composite key only if key_components are specified
  if (!is.null(key_components)) {
    cleaning_log <- cleaning_log %>%
      mutate(across(all_of(key_components), ~trimws(as.character(.)))) %>%
      unite(!!sym(key_col), all_of(key_components), sep = "_", remove = FALSE)
  }
  
  # Check missing keys
  if (!is_deletion) {

    # Select columns to display for unmatched keys
    unmatched_cols <- if (!is.null(key_components)) c(key_components, key_col) else key_col
    
    unmatched_cleaning_keys <- cleaning_log %>%
      filter(!(!!sym(key_col) %in% df[[key_col]])) %>%
      select(all_of(unmatched_cols)) %>%
      distinct()
    
    # Warn about unmatched keys in the cleaning log
    if (nrow(unmatched_cleaning_keys) != 0) {
      message(paste0(
        "\n",
        nrow(unmatched_cleaning_keys),
        " keys from the ", sheet_name, "-level cleaning log were not found."
      ))
      print(unmatched_cleaning_keys, n=Inf)
      cat("\n")
    }
  }

  # Execute deletion logic and return early if deletion flag is TRUE
  if (is_deletion) {
    original_rows <- nrow(df)
    
    df <- df %>%
      anti_join(cleaning_log, by = key_col)
    
    rows_removed <- original_rows - nrow(df)
    message(sprintf(
      "\n%d observation(s) dropped based on the '%s' log", 
      rows_removed, 
      sheet_name
    ))
    return(df)
  }
  
  # Filter cleaning log to only include entries that match keys in the dataframe
  corrections_wide <- cleaning_log %>%
    filter(!is.na(correct_values) & trimws(correct_values) != "") %>%
    select(!!sym(key_col), correct_values) %>%
    separate_rows(correct_values, sep = ";") %>%
    separate(
      correct_values,
      into = c("target_col", "new_val"),
      sep = "=",
      extra = "merge"
    ) %>%
    mutate(
      target_col = trimws(target_col),
      new_val = trimws(new_val),
      new_val = str_remove_all(new_val, '^"|"$|^""|""$')
    ) %>%

    # Deduplicate to prevent list-cols during pivot
    group_by(!!sym(key_col), target_col) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    pivot_wider(names_from = target_col, values_from = new_val)
  
  # Identify relevant correction columns
  correction_columns <- setdiff(names(corrections_wide), key_col)
  
  # Capture the original column names before joining
  original_cols <- names(df)
  
  # Add correction columns to original dataframe
  df <- df %>%
    left_join(corrections_wide, by = key_col, suffix = c("", "_new"))

  # Apply corrections using logic that respects explicit NAs
  for (col in correction_columns) {
    corr_col_name <- paste0(col, "_new")
    
    # Only process columns that actually exist in the current target dataframe
    if (col %in% original_cols) {
      df <- df %>%
        mutate(
          !!sym(col) := case_when(
            !is.na(.data[[corr_col_name]]) & .data[[corr_col_name]] %in% c("NA", "na", "NA_character_", "NA_integer_", "NA_real_", "") ~ NA_character_,
            !is.na(.data[[corr_col_name]]) ~ .data[[corr_col_name]],
            TRUE ~ as.character(.data[[col]])
          )
        )
      # Remove the temporary correction column
      df[[corr_col_name]] <- NULL
    } else {
      # Remove unneeded columns added directly by the join
      df[[col]] <- NULL
    }
  }
  
  return(df)
}

#' Clean tree_type Columnw
#'
#' This function cleans the 'tree_type' column in the provided dataframe by
#' standardizing the entries to 'Seedling', 'Sapling', or 'Tree'.
#' It also removes any rows with unrecognized or missing 'tree_type' values.
#' @param df The input dataframe containing a 'tree_type' column.
#' @return A dataframe with cleaned 'tree_type' values.

clean_tree_type <- function(df) {
  df <- df %>%

    # Standardize tree_type values based on common patterns
    mutate(
      tree_type = case_when(
        str_detect(tree_type, regex("planted", ignore_case = TRUE)) ~ "planted",
        str_detect(tree_type, regex("present", ignore_case = TRUE)) ~ "present",
        str_detect(tree_type, regex("regenerating", ignore_case = TRUE)) ~ "regenerating",
        str_detect(tree_type, regex("know", ignore_case = TRUE)) ~ "unknown",
        TRUE ~ tree_type
      )
    )  %>%

    # Apply rule to change 'Naturally regenerating' into 'Already present'
    mutate(
      tree_type = case_when(
        tree_type == "regenerating" &
          timeframe == "y0" &
          size_class == ">10cm" &
          plot_size == "30x30" ~
          "present",
        TRUE ~ tree_type
      )
    ) %>%
    # Apply rule to change 'Unknown' into 'Already present'
    mutate(
      tree_type = case_when(
        tree_type == "unknown" &
          timeframe == "y0" &
          size_class == ">10cm" &
          plot_size == "30x30" ~
          "present",
        TRUE ~ tree_type
      )
    ) %>%
    # Fix for tree type
    mutate(
      tree_type = case_when(
        tree_type == "present" &
          timeframe != "y0" ~
          "unknown",
        TRUE ~ tree_type
      )
    )

  return(df)
}

#' Clean resampling data for projects with bare land
#' This function removes resampling entries for projects that are located in the USA.
#' It sets `Resample_Main_Plot` and `Resample_3x3_Subplot` to 0 for all plots
#' within the USA.
#' @param df The input dataframe containing plot data.
#' @return A dataframe with resampling entries for USA projects removed.

clean_resample_info <- function(df) {

  bare_land_org_names <- c(
    "ACT",
    "CHI",
    "MAD",
    "UAE",
    "GFW",
    "GAU",
    "RCH",
    "TCA"
  )

  df <- df %>%

  # For all plots

  # Wipe resampling variables for control plots OR bare land organizations
  mutate(
    resampling_30x30 = case_when(
      tolower(plot_type) == "control" ~ NA_character_,
      org_name %in% bare_land_org_names ~ NA_character_,
      TRUE ~ resampling_30x30
    ),
    resampling_3x3 = case_when(
      tolower(plot_type) == "control" ~ NA_character_,
      org_name %in% bare_land_org_names ~ NA_character_,
      TRUE ~ resampling_3x3
    ),
    large_trees_present = case_when(
      tolower(plot_type) == "control" ~ NA_character_,
      org_name %in% bare_land_org_names ~ NA_character_,
      TRUE ~ large_trees_present
    ),
    little_trees_present = case_when(
      tolower(plot_type) == "control" ~ NA_character_,
      org_name %in% bare_land_org_names ~ NA_character_,
      TRUE ~ little_trees_present  
    )
  )

  # Resampling from kobo_tree_data

  if (all(c(
    "plot_size", 
    "tree_type", 
    "size_class"
  ) %in% names(df))) {
    df <- df %>%
    mutate(
      # Update Main Plot Resampling (30x30)
      resampling_30x30 = case_when(
        plot_size == "30x30" & tree_type == "present" & size_class == ">10cm" & resampling_30x30 == "2" ~ "0",
        TRUE ~ resampling_30x30
      ),
      
      # Update Subplot Resampling (3x3)
      resampling_3x3 = case_when(
        plot_size == "3x3" & tree_type == "present" & size_class == "1 - 9.9cm" ~ "0",
        TRUE ~ resampling_3x3
      ),
      
      # Update Large Tree Indicator
      large_trees_present = case_when(
        plot_size == "30x30" & tree_type == "present" & size_class == ">10cm" & resampling_30x30 == "2" ~ NA_character_,
        TRUE ~ large_trees_present
      ),
      
      # Update Small Tree Indicator (Change name to little_trees_present if matching plot master schema)
      little_trees_present = case_when(
        plot_size == "3x3" & tree_type == "present" & size_class == "1 - 9.9cm" & resampling_3x3 == "2" ~ NA_character_,
        TRUE ~ little_trees_present
      )
    )
  }    
  
  return(df)
}

#' Clean Invisible Spaces from Character Columns
#'
#' This function iterates over all character columns in a dataframe and 
#' removes leading, trailing, and repeated whitespace as well as any hidden 
#' line break characters (\\r, \\n) that often come from KoboToolbox text fields.
#'
#' @param df The input dataframe to clean.
#' @return A dataframe with cleaned character columns.
clean_invisible_spaces <- function(df) {
  df <- df %>%
    mutate(across(where(is.character), ~ stringr::str_squish(.)))
  return(df)
}

#region Execute cleaning routines

#' Master Orchestrator for Data Cleaning
#'
#' This function detects the available columns in a given dataframe and seamlessly 
#' applies the appropriate cleaning routines (logs, timeframe, tree type, and resampling).
#'
#' @param df The input dataframe to clean.
#' @param cleaning_file_path Path to the external Excel database cleaning log.
#' @return The completely cleaned dataframe.

execute_cleaning_pipeline <- function(df, cleaning_file_path, df_name = "df") {
  
  # 0. Specify cleaning object
  message(paste0(
    "\n",
    "----------------------------------------------------------",
    "\nCLEANING ", toupper(df_name), "\n"
  ))

  # 0.5 Clean Invisible Spaces
  # This fixes the line breaks and spaces imported from Kobo across all text columns
  df <- clean_invisible_spaces(df)

  # 1. Clean Timeframe
  # if (any(c("timeframe", "Timeframe") %in% names(df))) {
  #   df <- clean_timeframe(df)
  #  }

  # 2. Remove Test Observations
  df <- remove_test_observations(df)

  # 3.0. Apply Site Deletion Log
  if (all(c("country_name", "org_name", "site_name") %in% names(df))) {
      df <- apply_cleaning_log(
        df = df,
        log_file_path = cleaning_file_path,
        sheet_name = "site_deletion",
        key_col = "temp_site_key",
        key_components = c("country_name", "org_name", "site_name"),
        is_deletion = TRUE,
        df_name = df_name
      )
    }

  # 3.1. Apply Site-Level External Cleaning Log
  if (all(c("country_name", "org_name", "site_name") %in% names(df))) {
    df <- apply_cleaning_log(
      df = df,
      log_file_path = cleaning_file_path,
      sheet_name = "site",
      key_col = "temp_site_key",
      key_components = c("country_name", "org_name", "site_name"),
      df_name = df_name
    )
  }

  # 3.2. Apply Plot-Level Deletion Log
  if (all(c("plot_name", "uuid") %in% names(df))) {
    df <- apply_cleaning_log(
      df = df,
      log_file_path = cleaning_file_path,
      sheet_name = "plot_deletion",
      key_col = "uuid",
      key_components = NULL,
      is_deletion = TRUE,
      df_name = df_name
    )
  }

  # 3.3. Apply Tree-Level Deletion Log
  if (all(c("uuid", "species", "tree_type", "size_class") %in% names(df))) {
    df <- df %>%
      mutate(temp_tree_composite_key = paste(uuid, species, tree_type, size_class, sep = "_"))
      
    df <- apply_cleaning_log(
      df = df,
      log_file_path = cleaning_file_path,
      sheet_name = "tree_deletion",
      key_col = "temp_tree_composite_key",
      key_components = c("uuid", "species", "tree_type", "size_class"),
      is_deletion = TRUE,
      df_name = df_name
    )
    
    df <- df %>% select(-temp_tree_composite_key)
  }

  # 3.2. Apply Plot-Level External Cleaning Log
  if (all(c("plot_name", "uuid") %in% names(df))) {
    df <- apply_cleaning_log(
      df = df,
      log_file_path = cleaning_file_path,
      sheet_name = "plot",
      key_col = "uuid",
      key_components = NULL,
      df_name = df_name
    )
  }

  # 3.3. Apply Tree-Level External Cleaning Log
  if (all(c("uuid", "species", "tree_type", "size_class") %in% names(df))) {
    
    # Create a composite key on the fly for matching
    df <- df %>%
      mutate(temp_tree_composite_key = paste(uuid, species, tree_type, size_class, sep = "_"))
      
    df <- apply_cleaning_log(
      df = df,
      log_file_path = cleaning_file_path,
      sheet_name = "tree",
      key_col = "temp_tree_composite_key",
      key_components = c("uuid", "species", "tree_type", "size_class"),
      df_name = df_name
    )
    
    # Clean up the temporary key
    df <- df %>% select(-temp_tree_composite_key)
  }
  
  # 4. Clean Tree Type
  # Checks for all variables required by the internal logic of clean_tree_type()
  if (all(c("tree_type", "timeframe", "size_class", "plot_size") %in% names(df))) {
    df <- clean_tree_type(df)
  }
  
  # 5. Clean Resampling Info
  if (all(c(
    "resampling_30x30", 
    "resampling_3x3", 
    "large_trees_present", 
    "little_trees_present"
  ) %in% names(df))) {
    df <- clean_resample_info(df)
  }

  df <- df %>% mutate(across(where(is.character), ~ na_if(., "na")))
  
  return(df)
}