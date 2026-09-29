# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Matteo Ramina (raminamatteo@me.com)
# Date: 2025-12-08
# Version: 1.0

# Description: This script is part of the legacy migration pipeline (02_load_and_harmonize.R). Because source files lack 
# standardization, the pipeline  first harmonizes and cleans the datasets. This
# script runs on a per-organization basis, requiring the target organization
# and relevant timeframe(s) to be manually updated in the script prior to each 
# execution. The file uses a configuration file that defines paths to all 
# source files, mapping files, and output directories.

# Inputs:
#   • config.R - Centralized configuration file defining paths to source files, 
#     mapping files, and output directories
#   • sites-ppc.csv - Site-level data from TerraMatch
#   • site_size_data.csv - Data on site areas
#   • 2026 Active-NonActive Kobo Forms.xlsx (sheet: KoboBD) - Plot-level data 
#     downloaded from Kobotoolbox's "Data" tab
#   • Plot_Data_2026_05_12.csv - Plot-level data downloaded from Kobo's API
#   • Tree_Data_2026_05_12.csv - Tree-level data downloaded from Kobo's API
#   • tree_species_table.csv - Species lookup table for tree-level data
#   • database_cleaning_2026_08_09.xlsx - Cleaning log for data quality issues 
#     and correction instructions

# Parameters:
#   • target_org - Organization name to migrate
#   • target_org_code - 3-letter organization code
#   • target_time - Survey timeframes to migrate

# Outputs:
#   • Updated ppc_database.sqlite

# The script uses VS Code minimap's regions.
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 0 Packages and configuration

source(here::here("scripts/utils/config.R"))
library(dplyr)
library(countrycode)
library(here)
library(purrr)
library(tidyr)
library(readxl)
library(stringr)
library(readr)

# Define export directory for the target organization and timeframe(s)
export_dir <- file.path(dir_legacy, "legacy_tables", "temp")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 1 Utility functions

#' Find the most frequent non-NA value in a vector
#'
#' @param x A vector of values to analyze
#' @return Returns the most frequent non-NA value as a string, or NA_character_ 
#'         if no valid values exist
get_most_likely <- function(x) {

  # Filter out NA values and find the most frequent value
  valid_x <- x[!is.na(x)]

  # If there are valid values, return most frequent one; otherwise, return NA
  if (length(valid_x) > 0) {
    return(names(which.max(table(valid_x))))
  } else {
    return(NA_character_)
  }
}

#' Aggregate attributes by a grouping key using the most likely value
#'
#' @param df The data frame to aggregate
#' @param group_col The column name to group by (as a string)
#' @param attr_cols A character vector of column names to aggregate
#' @return Returns a summarized data frame with the grouping key and aggregated 
#'         attributes
aggregate_attributes <- function(df, group_col, attr_cols) {
  df %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      across(all_of(attr_cols), get_most_likely),
      .groups = "drop"
    )
}

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2 Load data

# Print section message
message("\n=== 1 Loading source data ===")

# Load source data
site_data <- read_csv(
  file.path(dir_legacy_data, file_site_data),
  show_col_types = FALSE
)
site_size_data <- read_csv(
  file.path(dir_legacy_data, file_site_size_data),
  show_col_types = FALSE
)
kobo_plot_data <- read_csv(
  file.path(dir_raw_data, gsub("plot_data_|\\.csv", "", file_kobo_plot), file_kobo_plot),
  show_col_types = FALSE
)
kobo_tree_data <- read_csv(
  file.path(dir_raw_data, gsub("tree_data_|\\.csv", "", file_kobo_tree), file_kobo_tree), 
  show_col_types = FALSE
)

# Load tree species table
tree_species_table <- read_csv(
  file_tree_species,
  show_col_types = FALSE
)

# Load mapping files
map_orgs <- read_xlsx(file_map_orgs, sheet="map_organizations")
map_sites <- read_xlsx(file_map_sites, sheet="map_sites")
map_plots <- read_xlsx(file_map_plots, sheet="map_plots")
map_columns <- read_xlsx(file_map_columns, sheet="map_columns")
map_species <- read_xlsx(file_map_species, sheet="Sheet1")

# Define path to cleaning log
cleaning_file_path <- file.path(
  dir_legacy_qa, file_cleaning_log
)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 3 Harmonize identifiers and columns

# Print section message
message("\n=== 2 Harmonizing data sources ===")
message("--> Applying mappings...")

# Define the unique arguments for each dataframe
configs <- list(
  site_data = list(
    df = site_data,
    country_col = NULL,
    org_col = "organization-name",
    site_col = "id",
    plot_col = NULL,
    time_col = NULL

  ),
  site_size_data = list(
    df = site_size_data,
    country_col = "Project Country (from projectUuid) (from siteUuid)",
    org_col = NULL,
    site_col = "ppcExternalId",
    plot_col = NULL,
    time_col = NULL 
  ),
  kobo_plot_data = list(
    df = kobo_plot_data,
    country_col = "Country",
    org_col = "Organization_Name",
    site_col = "Site_ID",
    plot_col = "Plot_ID",
    time_col = "Timeframe"
  ),  
  kobo_tree_data = list(
    df = kobo_tree_data,
    country_col = "Country",
    org_col = "Organization_Name",
    site_col = "Site_ID",
    plot_col = "Plot_ID",
    time_col = "Timeframe",
    species_col = "Species"
  )
)

# Loop through the configurations and apply the function
harmonized_dfs <- lapply(configs, function(args) {
  harmonize_identifiers(
    df = args$df,
    country_col = args$country_col,
    org_col = args$org_col,
    site_col = args$site_col,
    plot_col = args$plot_col,
    species_col = args$species_col,
    map_orgs = map_orgs,
    map_sites = map_sites,
    map_plots = map_plots,
    map_species = map_species,
    map_columns = map_columns
  )
})

# Extract the updated dataframes back into your workspace
site_data_harm <- harmonized_dfs$site_data
site_size_data_harm <- harmonized_dfs$site_size_data
kobo_plot_data_harm <- harmonized_dfs$kobo_plot_data
kobo_tree_data_harm <- harmonized_dfs$kobo_tree_data

# Harmonize column names across all dataframes using the mapping file
site_data_harm <- harmonize_columns(site_data_harm, map_columns)
site_size_data_harm <- harmonize_columns(site_size_data_harm, map_columns)
kobo_plot_data_harm <- harmonize_columns(kobo_plot_data_harm, map_columns)
kobo_tree_data_harm <- harmonize_columns(kobo_tree_data_harm, map_columns)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 4 Export 

# Create export directory if it doesn't exist
if (!dir.exists(export_dir)) dir.create(export_dir, recursive = TRUE)

# Save harmonized data and mapping files as RDS
saveRDS(list(
  site_data_harm=site_data_harm, 
  site_size_data_harm=site_size_data_harm, 
  kobo_plot_data_harm=kobo_plot_data_harm, 
  kobo_tree_data_harm=kobo_tree_data_harm,
  tree_species_table=tree_species_table
), file.path(export_dir, "harmonized_data.rds"))
saveRDS(list(
  map_orgs=map_orgs,
  map_sites=map_sites,
  map_plots=map_plots,
  map_columns=map_columns,
  map_species=map_species
), file.path(export_dir, "mapping_data.rds"))

message("--> Harmonized data exported to: ", export_dir)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# END