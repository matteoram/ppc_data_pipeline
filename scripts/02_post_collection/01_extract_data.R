# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Johannes Nelson, Leon Xing, Matteo Ramina (raminamatteo@me.com)
# Date: 2025-09-15
# Version: 2.0

# Description: This code forms the data extraction and preprocessing component
# of PPC's data analysis pipeline. The script leverages the package 
# robotoolbox, which simplifies the data download compared to a solution that 
# uses Kobo's native API only. The script outputs information about each 
# submission/monitoring plot (the responses to the Kobo form questions that are 
# not records of tree species observed or planted), as well as information 
# about all the trees observed during monitoring surveys, besides additional 
# outputs. 
# Because the Colombian data misses key geolocation information, this script
# modifies the 'extract_geopoint_' function from robotoolbox to prevent the
# script from failing when geolocation data is missing.
# The tree data file contains the word "uncorrected", which refers to the fact 
# that the species names have not been cleaned. This will be addressed in a
# later script.

# The script's inputs are:
#   - ".Renviron", a file storing the user's Kobo API token for secure
#                  authentication.

# The script's output is:
#   - "Plot_Data_yyyy-mm-dd.csv", the main data table with submission-level
#                                 information.
#   - "Tree_Data_Uncorrected_yyyy-mm-dd.csv", the combined tree data table
#                                             with all tree records.
#   - "Geo_Data_yyyy-mm-dd.csv", a table with geolocation data for each plot.
#   - "Photo_Data_yyyy-mm-dd.csv", a table with photo attachment data for each
#                                  plot.

# The script uses VS Code minimap's regions.
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 0. Load packages

# Load centralized paths
source(here::here("scripts/utils/config.R"))

# Load packages into session
library(here)
library(dplyr)
library(tidyr)
library(robotoolbox)
library(labelled)
library(readr)

# Load Mapping Files
map_orgs <- read_csv(file_map_orgs, show_col_types = FALSE)
map_sites <- read_csv(file_map_sites, show_col_types = FALSE)

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 1. Utility functions for harmonization

#' Standardize identifiers using mapping files
harmonize_data <- function(df) {
  # 1. Standardize Country names
  df <- df %>%
    left_join(map_countries, by = c("Country" = "kobo_country")) %>%
    mutate(Country = coalesce(standard_country, Country)) %>%
    select(-standard_country)

  # 2. Standardize Organization names
  df <- df %>%
    left_join(map_orgs, by = c("Organization_Name" = "kobo_name")) %>%
    mutate(Organization_Name = coalesce(standard_name, Organization_Name)) %>%
    select(-standard_name)

  # 3. Standardize Site IDs
  df <- df %>%
    left_join(map_sites, by = c("Site_ID" = "kobo_site_id", "Country" = "country")) %>%
    mutate(Site_ID = coalesce(standard_site_id, Site_ID)) %>%
    select(-standard_site_id)

  # 4. Generate Temporary Key
  df <- df %>%
    mutate(temp_key = paste(
      tolower(trimws(Country)), 
      tolower(trimws(Organization_Name)), 
      tolower(trimws(Site_ID)), 
      tolower(trimws(Plot_ID)), 
      sep = "_"
    ))

  return(df)
}

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 0. Retrieve data from Kobo

#' 1. Retrieve Kobo Data
#'
#' This function takes the user's Kobo API token stored in the .Renviron file 
#' to extract data from Kobo with minor wrangling and renaming. The function 
#' does not uses any parameters other than the API token and asset name, which 
#' are hardcoded.
#'
#' @return List of data.frames that correspond to different data tables.

retrieve_kobo_data <- function(asset_name = "Tree Monitoring") {

  # Set up Kobo envinronment
  kobo_setup(
    url = "https://kf.kobotoolbox.org",
    token = Sys.getenv("KOBO_API_TOKEN")
  )
  kobo_settings()

  # Get Kobo assets
  l <- kobo_asset_list()

  # Get "Tree Monitoring" asset
  uid <- l %>%
    filter(name == asset_name) %>%
    pull(uid) %>%
    first()
  asset <- kobo_asset(uid)

  # Monkey-patch extract_geopoint_ by adding `too_few = "align_start"`
  my_extract_geopoint <- function (x, form) 
  {
      cond <- form$type %in% "geopoint"
      nm <- unique(form$name[cond])
      nm <- intersect(names(x), nm)
      if (any(cond) && length(nm) > 0) {
          wkt_geopoint <- function(x) {
              pattern <- "^(\\s*-?\\d+(?:\\.\\d+)?)\\s+(-?\\d+(?:\\.\\d+)?)\\s+(-?\\d+(?:\\.\\d+)?)(\\s+-?\\d+(?:\\.\\d+)?)$"
              replacement <- "POINT (\\2 \\1 \\3)"
              gsub(pattern, replacement, x)
          }
          separate_geopoint <- function(x, col) {
              lbl <- var_label(select(x, all_of(col)))
              nm <- c("latitude", "longitude", "altitude", "precision", 
                  "wkt")
              lbl <- lapply(
                seq_along(lbl), function(i) setNames(paste0(lbl[[i]], 
                  "::", nm), paste0(names(lbl)[i], "_", nm))
                )
              lbl <- lapply(lbl, as.list)
              lbl <- unlist(lbl, recursive = FALSE)
              x <- set_variable_labels(
                rename_with(separate_wider_delim(
                  mutate(x, 
                  across(.cols = all_of(col), .fns = ~wkt_geopoint(.x), 
                    .names = "{.col}_wkt")
                  ), {
                  {col}
                  }, names = c(
                    "latitude", "longitude", "altitude", 
                  "precision"
                  ), delim = " ",names_sep = "_", cols_remove = FALSE,
                  too_few = "align_start"
                  ), 
                  ~gsub("^(.+)_(\\1)$", "\\1", .x), starts_with(col)), 
                  .labels = lbl, .strict = FALSE
                  )
              x
          }
          x <- separate_geopoint(x, nm)
      }
      x
  }

  # Replace extract_geopoint_ with my_extract_geopoint
  unlockBinding("extract_geopoint_", asNamespace("robotoolbox"))
  assign(
    "extract_geopoint_", my_extract_geopoint,
    envir = asNamespace("robotoolbox")
  )
  lockBinding("extract_geopoint_", asNamespace("robotoolbox"))

  # Download submissions with monkey-patched extract_geopoint_
  df <- kobo_submissions(asset, all_versions = T, progress = T)
  main_list <- as.list(df)

  # Separate out the main submission data
  plot_table <- main_list$main

  # This is a dataframe that maps the names in Kobo with more clear, informative
  # names designating what the data represents. If new tables appear, this will
  # need to be edited.
  table_name_mappings <- data.frame(
    original_name = c(
      "main", "begin_repeat_ztsNCjoPm", "begin_repeat_ELDOiv5Dr", 
      "PlantedTrees3", "begin_repeat_YDvKUvA32", "begin_repeat_Wq3dUfnDG", 
      "group_an2yk58", "group_ka7vj63", "group_qr1fe53", "group_ai86m63", 
      "group_oq8nt56", "group_ql6qg69", "group_qs7yn71", "group_wj4lz16"
    ),
    clarified_name = c(
      "main", "Normal_30x30", "Nested_3x3_within_30x30", "Planted_30x30",
      "Control_10x10", "Nested_3x3_within_10x10_Control", "Normal_30x30_2",
      "Nested_3x3_within_30x30_2", "Nested_1x1", "Small_3x3", "Control_10x10_2",
      "Census_30x30", "Planted_30x30_2", "Nested_3x3_within_10x10_Control_2"
    )
  )
  for (i in seq_len(nrow(table_name_mappings))) {
    if (table_name_mappings$original_name[i] %in% names(main_list)) {
      names(main_list)[
        names(main_list) == table_name_mappings$original_name[i]
        ] <- table_name_mappings$clarified_name[i]
    }
  }

  # Separate out tree tables
  tree_tables <- main_list[setdiff(names(main_list), "main")]

  # Get the number of tables and print table count
  num_tables <- length(main_list) 
  cat(paste0("Number of tables downloaded: ", num_tables, "\n"))

  # Print table names
  cat("Names of tables downloaded (Cleaned Name [Original Name]):\n")

  # Loop through the table mappings to print the vertical list
  for (i in seq_len(nrow(table_name_mappings))) {
    original <- table_name_mappings$original_name[i]
    clarified <- table_name_mappings$clarified_name[i]
    
    # Check if the original table name was actually present in the data
    # This prevents printing names that were expected but not downloaded
    if (clarified %in% names(main_list)) {
      cat(paste0("  - ", clarified, " [", original, "]\n"))
    }
  }

  return(list(plot_table = plot_table, tree_tables = tree_tables))
}

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2. Data preprocessing

#' 2. Prepare Plot Table
#'
#' This function preprocesses the plot table by renaming columns, handling NAs
#' within the 'resample' column--the assumption here is that no response meant 
#' no resampling occurred--and organizing the columns into a more helpful 
#' order. It also prepares and separates out a table for geolocation data and 
#' photo attachment data.
#'
#' @param plot_table The original plot table extracted from Kobo.
#' @return List of dataframes: Plot_Data, Geo_Data, Photo_Data.

process_plot_table <- function(plot_table) {
  
  # Rename generic _index column to distinct value for later joins.
  colnames(plot_table)[colnames(plot_table) == "_index"] <- "main_index"
  colnames(plot_table)[colnames(plot_table) == "Enter_a_date"] <- "Date"

  # Rename SiteType, SiteSize and Planting_Pattern for clarity and consistency
  colnames(plot_table)[colnames(plot_table) == "SiteType"] <- "Site_Type"
  colnames(plot_table)[colnames(plot_table) == "SiteSize"] <- "Site_Size"
  colnames(plot_table)[
    colnames(plot_table) == "PlantingPattern"] <- "Planting_Pattern"

  # Rename and process sampling columns; add plot size based on Site_Size answer
  plot_table <- plot_table %>%
    mutate(Resample_Plot = ifelse(is.na(Resampling1), 0, Resampling1)) %>%
    mutate(
      Resample_Subplot = ifelse(is.na(Resampling2), 0, Resampling2)) %>%
    mutate(Monitoring_Plot_Size = case_when(
      Site_Size == "Yes" ~ "30x30",
      Site_Size == "No" ~ "3x3",
      Plot_Type == "Control" ~ "10x10"
    )) %>%
    select(-Resampling1, -Resampling2)

  # Separate out geolocation data based on pattern in column names
  geo_columns <- names(plot_table)[
    grep("Corner|Centroid", names(plot_table), ignore.case = TRUE)]
  geo_data <- plot_table %>%
    select(
      Organization_Name, Site_ID, Plot_ID,
      Site_Type, Plot_Type, Coordinate_System_Used,
      all_of(geo_columns), `_uuid`
    ) %>%
    select(-names(.)[grep("Photo", names(.), ignore.case = TRUE)])

  # Separate out photo attachment data and join with relevant plot data
  photo_attachments <- plot_table$`_attachments`
  full_attachments_df <- bind_rows(photo_attachments)
  final_attachments <- plot_table %>%
    select(
      Organization_Name, Plot_ID, Site_ID, `_id`, `_attachments`, `_uuid`
    ) %>%
    tidyr::unnest(`_attachments`, keep_empty = FALSE) %>%
    select(-media_file_basename, -is_deleted, -question_xpath) %>%
    arrange(Site_ID, Plot_ID)

  # Simple reordering for ease of viewing; drops geolocation and photo columns
  plot_table_shortened <- plot_table %>%
    select(
      Plot_ID,
      Site_ID,
      Site_Type,
      Plot_Type,
      Country,
      Organization_Name,
      Plot_Permanence,
      Monitoring_Plot_Size,
      Strata_Number,
      Timeframe,
      Date,
      Resample_Plot,
      Resample_Subplot,
      everything(),
      -all_of(geo_columns),
      -`_attachments`
    )

  return(list(Plot_Data = plot_table_shortened, Geo_Data = geo_data,
  Photo_Data = final_attachments))
}

#' 3. Extract Misplaced Data
#'
#' For mysterious reasons that no mortal mind can comprehend, some of the tree
#' data was output into the main data table with submission data. The columns
#' containing this data have patterns '1', '2', '0011', and '0012' in their 
#' names.
#' These patterns are used to find the misplaced data. This lengthy function's
#' sole purpose is to grab this data and put it where it belongs based on
#' hard-coded, clunky rules. If there ever appear new data in the plot table 
#' that do not have the above mentioned patterns,it will need to be examined 
#' and code will need to be added to this function to accommodate it.
#'
#' @param plot_table The plot table after it has undergone the above 
#'                   preprocessing.
#' @param tree_tables The tree tables straight from extraction.
#' @return List with a dataframe called plot_table and a list of dataframes 
#'         called tree tables.

extract_misplaced_data <- function(plot_table, tree_tables) {
  tree_table_names <- names(tree_tables)

  # This renames all '_index' columns to 'tree_index', and all '_parent_index'
  # columns to 'main_index' to be able to match the entries in the plot table.
  tree_tables <- lapply(tree_tables, function(df) {
    col_names <- names(df)
    col_names[col_names == "_index"] <- "tree_index"
    col_names[col_names == "_parent_index"] <- "main_index"
    names(df) <- col_names
    return(df)
  })

  # Select columns in plot table where there is tree data based on known
  # patterns, group by data type, and append to appropriate tree table.
  # Destination tables were chosen based on patterns in the 'Kobo_Key' document.
  misplaced_tree_data <- plot_table %>%
    filter(
      !is.na(Tree_Species1) |
        !is.na(Tree_Species2) |
        !is.na(Tree_Species_0011) |
        !is.na(Tree_Species_0012)
    ) %>%
    select(
      main_index,
      contains("Number_of_Trees"),
      contains("Tree_Type"),
      contains("Tree_Species")
    )

  df_type_1 <- misplaced_tree_data %>%
    select(main_index, ends_with("1")) %>%
    select(-ends_with("0011")) %>%
    filter(
      !is.na(Tree_Species1) |
        !is.na(Tree_Type1) |
        !is.na(Number_of_Trees_of_this_Species1)
    ) %>%
    mutate(
      destination_table = "Control_10x10"
    )

  df_type_2 <- misplaced_tree_data %>%
    select(main_index, ends_with("2")) %>%
    select(-ends_with("0012")) %>%
    filter(
      !is.na(Tree_Species2) |
        !is.na(Tree_Type2) |
        !is.na(Number_of_Trees_of_this_Species2)
    ) %>%
    mutate(
      destination_table = "Normal_30x30"
    )

  df_type_0011 <- misplaced_tree_data %>%
    select(main_index, ends_with("0011")) %>%
    filter(
      !is.na(Tree_Species_0011) |
        !is.na(Tree_Type_0011) |
        !is.na(Number_of_Trees_of_this_Species_0011)
    ) %>%
    mutate(
      destination_table = "Nested_3x3_within_10x10_Control"
    )

  df_type_0012 <- misplaced_tree_data %>%
    select(main_index, ends_with("0012")) %>%
    filter(
      !is.na(Tree_Species_0012) |
        !is.na(Tree_Type_0012) |
        !is.na(Number_of_Trees_of_this_Species_0012)
    ) %>%
    mutate(
      destination_table = "Nested_3x3_within_30x30"
    )

  # Helper function to bind data to correct tree table
  bind_to_tree_table <- function(df, tree_tables) {
    destination <- unique(df$destination_table)
    corresponding_tree_table <- tree_tables[[destination]]
    updated_table <- bind_rows(corresponding_tree_table, df)
    return(updated_table)
  }

  # Call the binding function for each table, putting the data in correct place
  tree_tables$Control_10x10 <- bind_to_tree_table(df_type_1, tree_tables)
  tree_tables$Normal_30x30 <- bind_to_tree_table(df_type_2, tree_tables)
  tree_tables$Nested_3x3_within_10x10_Control <- bind_to_tree_table(
    df_type_0011, tree_tables
  )
  tree_tables$Nested_3x3_within_30x30 <- bind_to_tree_table(
    df_type_0012, tree_tables
  )

  # Remove data from plot table after it has been put in place
  plot_table <- plot_table %>%
    select(
      -contains("Tree_Species"),
      -contains("Number_of_Trees"),
      -contains("Tree_Type")
    )

  return(list(plot_table = plot_table, tree_tables = tree_tables))
}

#' 4. Clean Tree Tables
#'
#' This function renames columns based on known naming conventions so that the
#' combined data has common variable names, adds columns and values for
#' Plot_Size and origin_table, and joins the tree data with the plot data so
#' that information like Plot_ID, Site_ID, etc. becomes available in the tree
#' data as well.
#'
#' @param tree_tables The tree_tables after they have been 'repaired' by the 
#'                    above extract_misplaced_data function.
#' @param plot_table The preprocessed and repaired plot table, output by 
#'                   previous function
#' @return List with a dataframe of all tree data combined, as well as a nested
#'         list with tree data still separated by origin table.

clean_tree_tables <- function(tree_tables, plot_table) {
  
  # Extract table names to iterate over
  tree_table_names <- names(tree_tables)

  tree_tables_modified <- lapply(tree_table_names, function(name) {

    # Extract single table from list
    df <- tree_tables[[name]]

    # Check column names for certain patterns and standardize across tables
    names(df)[grep(
      "number|numer", names(df), ignore.case = TRUE)] <- "Tree_Count"
    names(df)[grep("species", names(df), ignore.case = TRUE)] <- "Species"
    names(df)[grep("type", names(df), ignore.case = TRUE)] <- "Tree_Type"

    # Extract plot dimensions from the table name and add Plot_Size column. 
    # Assumes table names do not change. Unexpected new tables may cause 
    # unwanted behavior.
    plot_dims <- strsplit(name, "_")[[1]][2]
    df$Plot_Size <- plot_dims

    # Record origin table for future reference
    if ("origin_table" %in% names(df)) {
      # 'origin_table' column exists, fill NA values with 'name'
      df$origin_table <- ifelse(is.na(df$origin_table), name, df$origin_table)
    } else {
      # 'origin_table' column doesn't exist, create it and set all values to 
      # 'name'
      df$origin_table <- name
    }

    # Checks the tables with planted tree data. These need Tree_Type columns
    # that can automatically be populated with 'planted'
    if (
      grepl(
        "planted", name, ignore.case = TRUE) && !"Tree_Type" %in% names(df)) {
      df$Tree_Type <- "planted"
    }

    # This joins the tree data with essential elements in the plot data that
    # will be used in down stream analyses.
    df <- df %>%
      left_join(
        select(
          plot_table,
          main_index,
          Plot_ID,
          Organization_Name,
          Country,
          Site_ID,
          Site_Type,
          Plot_Type,
          Plot_Permanence,
          Resample_Plot,
          Resample_Subplot,
          Planting_Pattern,
          Timeframe,
          `_uuid`
        ),
        by = "main_index"
      )

    df <- df %>%
      mutate(
        size_class = case_when(
          grepl("Nested_3x3", origin_table, ignore.case = TRUE) ~ "1 - 9.9cm",
          grepl("census", origin_table, ignore.case = TRUE) ~ "1 - 9.9cm",
          grepl("planted", origin_table, ignore.case = TRUE) ~ "<10cm planted",
          origin_table == "Small_3x3" ~ ">1cm",
          grepl("Nested_1x1", origin_table, ignore.case = TRUE) ~ "<1cm",
          TRUE ~ ">10cm"
        )
      )

    # Convert all plot and site IDs to character values.
    df$Plot_ID <- as.character(df$Plot_ID)
    df$Site_ID <- as.character(df$Site_ID)

    # Reordering columns -- relevant first. Can easily be customized.
    df <- df %>% select(
      Species,
      Tree_Type,
      Tree_Count,
      size_class,
      Site_ID,
      Plot_ID,
      Country,
      Organization_Name,
      Site_Type,
      Plot_Type,
      Plot_Size,
      everything()
    ) 

    return(df)
  })
  names(tree_tables_modified) <- tree_table_names

  combined_tree_tables <- bind_rows(tree_tables_modified)


  # Remove unnecessary columns
  combined_tree_tables <- combined_tree_tables %>% select(
    -Note111, -tree_index, -`_parent_table_name`, -destination_table, -Note3, 
    -Note24
  )

  return(list(Tree_Tables = tree_tables_modified, Full_Tree_Data = combined_tree_tables))
}

#' 5. Remove Columns with Only NAs
#'
#' This function processes a list of tables and removes any columns within
#' these tables that contain only NA values. If this is not desired, exclude
#' this function from main script below. The assumption here is that if every
#' value is NA, it is likely not helpful.
#'
#' @param tables_list A list of tables (dataframes) from which columns
#'                    containing only NAs should be removed.
#' @return A list of cleaned tables with columns containing only NAs removed.

remove_NA_columns <- function(tables_list) {
  cleaned_tables <- lapply(tables_list, function(df) {
    df <- df %>% select_if(~ !all(is.na(.)))
    return(df)
  })
  return(cleaned_tables)
}

#' 6. Write CSVs to disk
#'
#' These are some simple helper functions to make writing lists of dataframes
#' and single dataframes to the disk. They add date stamps and assume no
#' sub-directory by default.

write_to_csv <- function(data, prefix, date_stamp = TRUE, sub_dir = NULL) {

  # 1. Define the base path using the centralized raw data directory
  current_date_folder <- format(Sys.Date(), "%Y_%m_%d")
  main_dir <- file.path(dir_raw_data, current_date_folder)

  # Check if the main directory exists, if not, create it recursively
  if (!dir.exists(main_dir)) {
    dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # 2. If a subdirectory is provided, ensure it's created inside the date folder
  if (!is.null(sub_dir)) {
    sub_path <- file.path(main_dir, sub_dir)
    if (!dir.exists(sub_path)) {
      dir.create(sub_path, recursive = TRUE, showWarnings = FALSE)
    }
    path_prefix <- file.path(sub_path, prefix)
  } else {
    path_prefix <- file.path(main_dir, prefix)
  }

  # 3. Determine filename with optional date stamp appended to the file itself
  if (date_stamp) {
    current_date_file <- format(Sys.Date(), "%Y_%m_%d") # e.g., "2023_10_10"
    filename <- paste0(path_prefix, "_", current_date_file, ".csv")
  } else {
    filename <- paste0(path_prefix, ".csv")
  }

  # 4. Write to file and print message
  write.csv(data, filename, row.names = FALSE)
  cat(paste("Data written to:", filename), "\n")
}

write_list_to_csv <- function(
  data_list, prefix_list, date_stamp = TRUE, sub_dir = NULL) {
  if (length(data_list) != length(prefix_list)) {
    stop("The number of data items does not match the number of prefixes.")
  }

  for (i in seq_along(data_list)) {
    write_to_csv(data_list[[i]], prefix_list[i], date_stamp, sub_dir)
  }
}

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 3 Execution block

#' Pipeline Execution
#'
#' This function serves as the orchestrator for the entire data pipeline. It
#' calls a sequence of functions to retrieve raw data from KoBoToolbox, clean 
#' and restructure the tables, and finally export the processed data frames 
#' (Plot, Tree, Geolocation, and Photo tables) to disk to disk as CSV files. 
#' The print() statements output at each step in the console can help locate 
#' where things went wrong, and the relevant function above will be a good 
#' starting point for debugging.
#'
#' @param None
#' @return A named list containing the final processed data frames:
#'         - `Plot`: The cleaned Plot data frame.
#'         - `Trees_Split`: The cleaned list of tree tables, split by plot type.
#'         - `Trees_Full`: The full, cleaned tree data combined into one table.
#'         - `Geo_Photo`: List containing Geolocation and Photo data tables.
#'         The return value is invisible, meaning it won't be auto-printed
#'         to the console unless explicitly assigned to a variable.

extract_main_data <- function() {

  # 1. Retrieve Kobo Data
  print(
    "Retrieving data from KoboToolbox. This requires internet connection and may take a moment."
  )
  all_data <- retrieve_kobo_data()

  # 2. Prepare Plot Table and Harmonize
  print("Processing Plot Table, Extracting Geolocation and Photo Data.")
  main_geo_photo <- process_plot_table(all_data$plot_table)
  
  # 3. Extract misplaced data
  print("Extracting and correcting misplaced tree data from main dataset.")
  all_data_fixed <- extract_misplaced_data(
    plot_table = main_geo_photo$Plot_Data, tree_tables = all_data$tree_tables
  )

  # 4. Clean Tree Tables
  print("Cleaning tree tables.")
  cleaned_tree_tables <- clean_tree_tables(
    all_data_fixed$tree_tables, all_data_fixed$plot_table
  )

  # 5. Remove columns that are entirely NA (optional)
  print("Removing columns that are entirely NA.")
  final_tree_tables <- remove_NA_columns(cleaned_tree_tables$Tree_Tables)
  final_full_tree_table <- remove_NA_columns(
    list(cleaned_tree_tables$Full_Tree_Data))[[1]]
  final_plot_table <- remove_NA_columns(list(all_data_fixed$plot_table))[[1]]

  # 8. Write Data to Disk
  print("Writing data to disk.")
  write_to_csv(final_plot_table, "plot_data")
  write_to_csv(final_full_tree_table, "tree_data")
  write_list_to_csv(main_geo_photo[2:3], names(main_geo_photo[2:3]))

  message("Data processing and export complete!\n")
  
  # Return a list of final tables for use in the current session
  invisible(list(
    Main = final_plot_table,
    Trees_Split = final_tree_tables,
    Trees_Full = final_full_tree_table,
    Geo_Photo = main_geo_photo[2:3]
  ))
}

# Execution Call
extract_main_data()

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# END