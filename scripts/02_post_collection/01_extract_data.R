# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Johannes Nelson, Leon Xing, Matteo Ramina (raminamatteo@me.com)
# Date: 2026-10-29
# Version: 3.0
# 
# Description: This script forms the data extraction and preprocessing component
# of PPC's data analysis pipeline. The script leverages the package robotoolbox
# to download data from the new KoBoToolbox monitoring form ("NEW Tree Monitoring Form").
# It extracts and standardizes:
#   1. Plot data (plot-level attributes and survey metadata)
#   2. Tree data (individual tree counts, size classes, and plot types)
#   3. Geolocation data (corner coordinates and centroids)
#   4. Photo attachments (media links and metadata)
#
# Inputs:
#   - ".Renviron" storing KOBO_API_TOKEN
#   - "config.R" defining paths and asset settings
#
# Outputs (written to data/raw/<YYYY_MM_DD>/):
#   - "plot_data_<YYYY_MM_DD>.csv"
#   - "tree_data_<YYYY_MM_DD>.csv"
#   - "Geo_Data_<YYYY_MM_DD>.csv"
#   - "Photo_Data_<YYYY_MM_DD>.csv"
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 0. Load packages and configuration

# Load centralized paths and configurations
source(here::here("scripts/utils/config.R"))

# Load packages into session
library(here)
library(dplyr)
library(tidyr)
library(robotoolbox)
library(readr)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 1 Retrieve data from Kobo

#' Retrieve Kobo Data
#'
#' Connects to KoBoToolbox using the token in .Renviron and downloads all submissions
#' for the target asset.
#'
#' @param asset_name Name of the KoBo asset (default from config or "NEW Tree Monitoring Form")
#' @param asset_uid Optional UID of the KoBo asset (default from config or "aLXWzgVxSSHnzEGhtbi2sH")
#' @return Named list with `plot_table` and `tree_tables`.

retrieve_kobo_data <- function(
  asset_name = if (exists("kobo_asset_name")) kobo_asset_name else "NEW Tree Monitoring Form",
  asset_uid  = if (exists("kobo_asset_uid"))  kobo_asset_uid  else "aLXWzgVxSSHnzEGhtbi2sH"
) {

  # Set up Kobo environment
  kobo_setup(
    url = "https://kf.kobotoolbox.org",
    token = Sys.getenv("KOBO_API_TOKEN")
  )
  kobo_settings()

  # Retrieve asset by UID if provided, otherwise locate by name
  asset <- if (!is.null(asset_uid) && nzchar(asset_uid)) {
    kobo_asset(asset_uid)
  } else {
    l <- kobo_asset_list()
    target_uid <- l %>%
      filter(name == asset_name) %>%
      pull(uid) %>%
      first()
    if (is.null(target_uid) || is.na(target_uid)) {
      stop(sprintf("Asset '%s' not found in KoBo account.", asset_name))
    }
    kobo_asset(target_uid)
  }

  message(sprintf("--> Downloading submissions for asset: '%s' (%s)...", asset$name, asset$uid))

  # Download all submissions directly without monkey-patching
  df <- kobo_submissions(asset, all_versions = TRUE, progress = TRUE)
  main_list <- as.list(df)

  # Separate plot-level submissions from tree repeat tables
  plot_table <- main_list$main
  tree_tables <- main_list[setdiff(names(main_list), "main")]

  message(sprintf("--> Download complete. Found %d plot submission(s) and %d tree repeat table(s):", nrow(plot_table), length(tree_tables)))
  for (tname in names(tree_tables)) {
    message(sprintf("    • %s: %d record(s)", tname, nrow(tree_tables[[tname]])))
  }

  return(list(plot_table = plot_table, tree_tables = tree_tables))
}

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2 Prepare plot table, geolocation, and photos

#' Prepare Plot Table, Geolocation Data, and Photo Attachments
#'
#' @param plot_table The raw plot table from Kobo submissions
#' @return Named list with `Plot_Data`, `Geo_Data`, and `Photo_Data`

process_plot_table <- function(plot_table, tree_tables) {

  plot_table$main_index <- plot_table$`_index`

  # Standardize core columns
  plot_table <- plot_table %>%
    mutate(
      plot_size = case_when(
        (site_size_control == "yes" & plot_type == "control") ~ "10x10",
        (site_size_control == "no" & plot_type == "control") ~ "30x30",
        (site_size_restoration == "yes" & plot_type == "restoration") ~ "30x30",
        (site_size_restoration == "no" & plot_type == "restoration") ~ "3x3",
      ),
      stratum = strata_number
    )

  # Geolocation columns
  geo_cols <- grep(
    "Corner|Centroid", names(plot_table), ignore.case = TRUE, value = TRUE
  )
  photo_col_names <- grep("Photo", geo_cols, ignore.case = TRUE, value = TRUE)
  geo_cols_clean <- setdiff(geo_cols, photo_col_names)

  geo_data <- plot_table %>%
    select(
      plot_id, site_id, organization_name, country_name, site_type, plot_type, 
      all_of(geo_cols_clean), `_uuid`
    )

  # Photo attachments

  # 1. Extract photo attachments from the main plot table  
  plot_photos <- if ("_attachments" %in% names(plot_table)) {
    plot_table %>%
      select(
        plot_id, site_id, organization_name, country_name, `_id`, 
        `_attachments`, `_uuid`
      ) %>%
      tidyr::unnest(`_attachments`, keep_empty = FALSE) %>%
      mutate(
        photo_type = if ("question_xpath" %in% names(.)) {
          basename(as.character(question_xpath))
        } else {
          NA_character_
        },
        source_table = "main"
      ) %>%
      select(
        -any_of(c("media_file_basename", "is_deleted"))
      ) %>%
      arrange(site_id, plot_id)
  } else {
    tibble()
  }

  # 2. Extract and format photo URLs from the tree tables
  tree_photos_list <- lapply(names(tree_tables), function(table_name) {
    df <- tree_tables[[table_name]]
    
    # Identify the two photo columns dynamically based on their naming pattern
    photo_cols <- grep("_photo_1$|_photo_2$", names(df), value = TRUE)
    
    if (length(photo_cols) > 0) {
      df %>%
        select(`_parent_index`, `_index`, all_of(photo_cols)) %>%
        # Pivot the wide photo columns into a long format
        pivot_longer(
          cols = all_of(photo_cols), 
          names_to = "photo_type", 
          values_to = "download_url", 
          values_transform = list(download_url = as.character),
          values_drop_na = TRUE
        ) %>%
        # Link back to the main plot table to get IDs and metadata
        inner_join(
          plot_table %>% select(`_index`, plot_id, site_id, organization_name, 
          country_name, `_uuid`, `_id`),
          by = c("_parent_index" = "_index")
        ) %>%
        select(-`_parent_index`) %>%
        mutate(
          source_table = table_name,
          question_xpath = paste0(table_name, "/", photo_type)
        )
    } else {
      tibble()
    }
  })
  
  # Combine all the extracted tree photos into a single dataframe
  tree_photos <- bind_rows(tree_photos_list)
  
  # 3. Bind plot and tree photos together into the final photo_data output
  photo_data <- bind_rows(plot_photos, tree_photos) %>%
    select(
      plot_id, site_id, organization_name, country_name,
      source_table, photo_type, question_xpath,
      everything()
    ) %>%
    arrange(site_id, plot_id)

  # Clean plot table
  plot_data_clean <- plot_table %>%
    select(
      plot_id, site_id, site_name, organization_id, organization_name, 
      country_id, country_name, timeframe, other_timeframe, date, start_time, 
      end_time, site_type, site_size_restoration, site_size_control, stratum, 
      plot_type, plot_permanence, plot_size, planting_pattern, 
      resampling_30x30, large_trees_present, resampling_3x3,
      little_trees_present, little_trees_census_present, tiny_trees_present,
      notes_trees_10cm_dbh_group, notes_trees_1_9_9cm_dbh_group,
      notes_trees_more_1cm_dbh_group, notes_trees_planted_group,
      notes_trees_less_1cm_dbh_group, end_note, main_index, `_uuid`, `_id`, 
      -all_of(geo_cols), -any_of(c("_attachments"))
    )

  return(
    list(
      plot_data = plot_data_clean, geo_data = geo_data, photo_data = photo_data
    )
  )
}

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 3 Clean tree tables

#' Standardize and Combine Tree Repeat Tables
#'
#' @param tree_tables Named list of repeat tables from Kobo
#' @param plot_table Processed plot table with submission metadata
#' @return Named list with `Tree_Tables` (per repeat) and `Full_Tree_Data` (combined)

clean_tree_tables <- function(tree_tables, plot_table) {

  meta_lookup <- plot_table %>%
    select(
      main_index, plot_id, site_id, site_name, organization_id, 
      organization_name, country_id, country_name, site_type, plot_type, 
      plot_permanence, plot_size, resampling_30x30, resampling_3x3, 
      planting_pattern, timeframe, `_uuid`
    )

  processed_tables <- list()

  for (tname in names(tree_tables)) {
    df <- tree_tables[[tname]]
    if (nrow(df) == 0) next

    tree_species_col  <- grep(
      "^tree_species_.*(?<!other|photo_1|photo_2)$", names(df), value = TRUE, 
      perl = TRUE
    )[1]
    tree_species_other_col <- grep("other$", names(df), value = TRUE)[1]
    ty_col  <- grep("^tree_type_", names(df), value = TRUE)[1]
    cnt_col <- grep("^number_tree_", names(df), value = TRUE)[1]

    # Standardize species columns
    df$tree_species <- as.character(df[[tree_species_col]])
    df$tree_species_other <- if (
      !is.na(tree_species_other_col) && tree_species_other_col %in% names(df)
    ) {
      as.character(df[[tree_species_other_col]])
    } else {
      NA_character_
    }

    # Clean tree type
    if (!is.na(ty_col) && ty_col %in% names(df)) {
      df$tree_type <- tolower(as.character(df[[ty_col]]))
    } else if (grepl("planted", tname, ignore.case = TRUE)) {
      df$tree_type <- "planted"
    } else {
      df$tree_type <- NA_character_
    }

    # Resolve tree count and index linkage
    df$tree_count <- as.numeric(df[[cnt_col]])
    df$main_index <- df$`_parent_index`
    df$tree_index <- df$`_index`

    # Join plot metadata
    df <- df %>% left_join(meta_lookup, by = "main_index")

    # Determine origin_table, size_class, and Plot_Size
    df <- df %>%
      mutate(
        origin_table = case_when(
          tname == "trees_10cm_dbh_repeat" & plot_type == "control" ~ "control_10x10",
          tname == "trees_10cm_dbh_repeat" ~ "normal_30x30",
          tname == "trees_1_9_9cm_dbh_repeat" & plot_type == "control" ~ "nested_3x3_within_10x10",
          tname == "trees_1_9_9cm_dbh_repeat" ~ "nested_3x3_within_30x30",
          tname == "trees_more_1cm_dbh_repeat" ~ "small_3x3",
          tname == "trees_planted_repeat" ~ "planted_30x30",
          tname == "trees_less_1cm_dbh_repeat" ~ "nested_1x1",
          TRUE ~ tname
        ),
        size_class = case_when(
          tname == "trees_10cm_dbh_repeat" ~ ">10cm",
          tname == "trees_1_9_9cm_dbh_repeat" ~ "1 - 9.9cm",
          tname == "trees_more_1cm_dbh_repeat" ~ ">1cm",
          tname == "trees_planted_repeat" ~ "<10cm planted",
          tname == "trees_less_1cm_dbh_repeat" ~ "<1cm",
          TRUE ~ NA_character_
        )
      )

    processed_tables[[tname]] <- df
  }

  full_trees <- bind_rows(processed_tables) %>%
    select(
      plot_id, site_id, site_name, organization_id, organization_name,
      country_id, country_name, timeframe, site_type, plot_type, 
      plot_permanence, plot_size, planting_pattern, resampling_30x30, 
      resampling_3x3, origin_table, tree_species, tree_species_other,
      tree_type, size_class, tree_count, tree_index, main_index, `_uuid`
    )

  return(list(tree_tables = processed_tables, full_tree_data = full_trees))
}

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 4 CSV export

#' Write dataframe to CSV with UTF-8 BOM
write_to_csv <- function(data, prefix, date_stamp = TRUE, sub_dir = NULL) {
  current_date_folder <- format(Sys.Date(), "%Y_%m_%d")
  main_dir <- file.path(dir_raw_data, current_date_folder)

  if (!dir.exists(main_dir)) {
    dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!is.null(sub_dir)) {
    sub_path <- file.path(main_dir, sub_dir)
    if (!dir.exists(sub_path)) {
      dir.create(sub_path, recursive = TRUE, showWarnings = FALSE)
    }
    path_prefix <- file.path(sub_path, prefix)
  } else {
    path_prefix <- file.path(main_dir, prefix)
  }

  filename <- if (date_stamp) {
    paste0(path_prefix, "_", current_date_folder, ".csv")
  } else {
    paste0(path_prefix, ".csv")
  }

  write_excel_csv(data, filename)
  message(paste("--> Data written to:", filename))
}

write_list_to_csv <- function(
  data_list, prefix_list, date_stamp = TRUE, sub_dir = NULL
) {
  if (length(data_list) != length(prefix_list)) {
    stop("The number of data items does not match the number of prefixes.")
  }
  for (i in seq_along(data_list)) {
    write_to_csv(data_list[[i]], prefix_list[i], date_stamp, sub_dir)
  }
}

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 5 Pipeline execution

#' Master Execution Orchestrator
extract_main_data <- function() {

  message("\n=== PPC Data Extraction Pipeline (New KoBo Form) ===")

  # 1. Retrieve Kobo Data
  message("\n--> [1/4] Retrieving data from KoBoToolbox...")
  all_data <- retrieve_kobo_data()

  # 2. Prepare Plot Table, Geolocation, and Photos
  message(
    "\n--> [2/4] Processing plot table, geolocation, and photo attachments..."
  )
  main_geo_photo <- process_plot_table(
    all_data$plot_table,
    all_data$tree_tables  
  )

  # 3. Clean Tree Tables
  message("\n--> [3/4] Standardizing and combining tree repeat tables...")
  cleaned_tree_tables <- clean_tree_tables(
    tree_tables = all_data$tree_tables,
    plot_table  = main_geo_photo$plot_data
  )

  # 4. Write data to disk
  message("\n--> [4/4] Writing extracted data to disk...")
  write_to_csv(main_geo_photo$plot_data, "plot_data")
  write_to_csv(cleaned_tree_tables$full_tree_data, "tree_data")
  write_list_to_csv(main_geo_photo[2:3], names(main_geo_photo[2:3]))

  message("\n[OK] Data extraction and export complete!\n")

  invisible(list(
    main = main_geo_photo$plot_data,
    trees_split = cleaned_tree_tables$tree_tables,
    trees_full = cleaned_tree_tables$full_tree_data,
    geo_photo = main_geo_photo[2:3]
  ))
}

# Execution Call
extract_main_data()

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# END