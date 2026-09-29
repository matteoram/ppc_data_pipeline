# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Matteo Ramina (raminamatteo@me.com)
# Date: 2025-12-08
# Version: 1.0

# Description: This script is part of the legacy migration pipeline (05_execute_sql_migration.R). Because source files lack 
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
export_dir_base <- file.path(
  dir_legacy,
  "legacy_tables",
  paste(
    target_org_code,
    paste(target_time, collapse = "_"),
    format(Sys.Date(), "%Y%m%d"),
    sep = "_"
  )
)
if (!dir.exists(export_dir_base)) stop(
  "Export directory not found. Run clean_and_transform.R first.")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 1 Load tables

countries_table <- read_csv(
  file.path(export_dir_base, "countries_table.csv"), show_col_types = FALSE
)
org_table_final <- read_csv(
  file.path(export_dir_base, "org_table.csv"), show_col_types = FALSE
)
sites_table_final <- read_csv(
  file.path(export_dir_base, "sites_table.csv"), show_col_types = FALSE
)
plots_table_final <- read_csv(
  file.path(export_dir_base, "plots_table.csv"), show_col_types = FALSE
)
surveys_table <- read_csv(
  file.path(export_dir_base, "surveys_table.csv"), show_col_types = FALSE
)
species_registry_final <- read_csv(
  file.path(export_dir_base, "species_registry.csv"), show_col_types = FALSE
)
org_species_lists_final <- read_csv(
  file.path(export_dir_base, "org_species_links.csv"), show_col_types = FALSE
)
tree_counts_table <- read_csv(
  file.path(export_dir_base, "tree_counts.csv"), show_col_types = FALSE
)

# Fix numeric casting issues from CSV read
sites_table_final <- sites_table_final %>% mutate(
  site_id = as.character(site_id),
  site_name = as.character(site_name)
)
plots_table_final <- plots_table_final %>% mutate(
  site_id = as.character(site_id)
)

# --- DYNAMIC CLEAN PLOT FILTER ---
# Read the list of approved (clean) UUIDs from the CSV
approved_plots_filename <- paste0(
  target_org_code, "_", 
  paste(target_time, collapse = "_"),
  format(Sys.Date(), "_%Y_%m_%d"), 
  "_approved_plots.csv"
)
approved_plots_file <- file.path(dir_legacy_qa, format(Sys.Date(), "%Y_%m_%d"), approved_plots_filename)
if (file.exists(approved_plots_file)) {
  approved_data <- read.csv(approved_plots_file, stringsAsFactors = FALSE)
  approved_uuids <- approved_data$uuid
  message(sprintf("--> Filtering data to ONLY include %d explicitly approved UUIDs...", length(approved_uuids)))
  
  # Filter surveys and trees by UUID
  surveys_table     <- surveys_table %>% filter(uuid %in% approved_uuids)
  tree_counts_table <- tree_counts_table %>% filter(uuid %in% approved_uuids)
  
  # Filter plots by the plot_names that remain in the filtered surveys
  plots_table_final <- plots_table_final %>% filter(plot_name %in% surveys_table$plot_name)
  
  # Cascade the filter up to parents to prevent orphaned records
  sites_table_final <- sites_table_final %>% filter(site_id %in% plots_table_final$site_id)
  org_table_final   <- org_table_final %>% filter(org_id %in% sites_table_final$org_id)
  countries_table   <- countries_table %>% filter(country_id %in% org_table_final$country_id)
  
  # Filter species to only those actually found in these specific plots
  species_registry_final  <- species_registry_final %>% filter(name %in% tree_counts_table$species | name == "Other or don't know")
  org_species_lists_final <- org_species_lists_final %>% filter(org_id %in% org_table_final$org_id & species_key %in% species_registry_final$species_key)
} else {
  stop("Approved plots CSV not found! Cannot proceed safely.")
}
# ---------------------------------

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2 Migration

# Print section message
message("\n=== 7 Migration ===")
message(
  sprintf(
    "--> Starting migration for: %s (%s) ---", target_org,
    paste(target_time, collapse = ", ")
  )
)

# Backup database before modification
db_path <- here::here("database", "ppc_database.sqlite")
backup_dir <- here::here("database", "backups")
if (file.exists(db_path)) {
  # Create daily subfolder
  date_folder <- format(Sys.Date(), "%Y_%m_%d")
  daily_backup_dir <- file.path(backup_dir, date_folder)
  if (!dir.exists(daily_backup_dir)) dir.create(daily_backup_dir, recursive = TRUE)
  
  # Copy and timestamp the file
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_path <- file.path(daily_backup_dir, paste0("ppc_database_", timestamp, ".sqlite"))
  file.copy(db_path, backup_path)
  message(sprintf("--> Database backed up to: %s", backup_path))
}

# Connect and enforce constraints
message("--> Connecting to database and inserting records...")
con <- dbConnect(
  RSQLite::SQLite(), here::here("database", "ppc_database.sqlite")
)
on.exit(dbDisconnect(con), add = TRUE)
dbExecute(con, "PRAGMA foreign_keys = ON;")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.1 Countries

existing_countries <- dbGetQuery(
  con, "SELECT country_id FROM countries")$country_id
new_countries <- countries_table %>% filter(!country_id %in% existing_countries)
if(nrow(new_countries) > 0) dbAppendTable(con, "countries", new_countries)

# Ask SQL what Integer Keys it assigned
db_countries <- dbGetQuery(con, "SELECT country_key, country_id FROM countries")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.2 Organizations

# Swap the text country_id for the integer country_key
orgs_ready <- org_table_final %>%
  left_join(db_countries, by = "country_id") %>%
  select(country_key, org_id, org_name)

existing_orgs <- dbGetQuery(con, "SELECT org_id FROM organizations")$org_id
new_orgs <- orgs_ready %>% filter(!org_id %in% existing_orgs)
if(nrow(new_orgs) > 0) dbAppendTable(con, "organizations", new_orgs)
db_orgs <- dbGetQuery(con, "SELECT org_key, org_id FROM organizations")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.3 Sites

# Swap the text org_id for the integer org_key
sites_ready <- sites_table_final %>%
  left_join(db_orgs, by = "org_id") %>%
  mutate(site_id = as.character(site_id)) %>% # SQL expects TEXT
  select(
    org_key, site_id, site_name, site_type, site_size_restoration, 
    site_size_control, bare_land
  )

existing_sites <- dbGetQuery(con, "SELECT site_name FROM sites")$site_name
new_sites <- sites_ready %>% filter(!site_name %in% existing_sites)
if(nrow(new_sites) > 0) dbAppendTable(con, "sites", new_sites)
db_sites <- dbGetQuery(con, "SELECT site_key, site_name FROM sites")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.4 Plots

# Bridge 'site_id' to 'site_name' first to get the site_key
plots_ready <- plots_table_final %>%
  left_join(
    sites_table_final %>% select(site_id, site_name), by = "site_id") %>%
  left_join(db_sites, by = "site_name") %>%
  select(site_key, plot_name, plot_type, plot_permanence)

existing_plots <- dbGetQuery(con, "SELECT plot_name FROM plots")$plot_name
new_plots <- plots_ready %>% filter(!plot_name %in% existing_plots)
if(nrow(new_plots) > 0) dbAppendTable(con, "plots", new_plots)
db_plots <- dbGetQuery(con, "SELECT plot_key, plot_name FROM plots")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.5 Surveys

# Swap plot_name for plot_key
surveys_ready <- surveys_table %>%
  left_join(db_plots, by = "plot_name") %>%
  mutate(date = as.character(date)) %>% # Convert R Date/Time to SQL TEXT
  select(
    plot_key, timeframe, date, start_time, end_time, stratum, 
    planting_pattern, planting_indicator, planting_group, anr_indicator, 
    resampling_30x30, large_trees_present, resampling_3x3, 
    little_trees_present, tiny_trees_present, uuid
  )

existing_surveys <- dbGetQuery(con, "SELECT uuid FROM surveys")$uuid
new_surveys <- surveys_ready %>% filter(!uuid %in% existing_surveys)
if(nrow(new_surveys) > 0) {
  dbAppendTable(con, "surveys", new_surveys)
  message(sprintf("    [OK] Inserted %d new surveys.", nrow(new_surveys)))
} else {
  message("    [SKIP] All surveys already exist in database.")
}
db_surveys <- dbGetQuery(
  con, "SELECT survey_key, plot_key, timeframe FROM surveys"
)

# Create composite lookup table for Trees (plot_name + timeframe -> survey_key)
survey_lookup <- db_surveys %>%
  left_join(db_plots, by = "plot_key") %>%
  select(survey_key, plot_name, timeframe)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.6 Species

# Rename R columns to match SQL species schema
species_ready <- species_registry_final %>%
  select(scientific_name = name, common_name = label)

existing_species <- dbGetQuery(
  con, "SELECT scientific_name FROM species")$scientific_name
new_species <- species_ready %>% filter(!scientific_name %in% existing_species)
if(nrow(new_species) > 0) dbAppendTable(con, "species", new_species)
db_species <- dbGetQuery(
  con, "SELECT species_key, scientific_name FROM species")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.7 Organization-species links

org_links_ready <- org_species_lists_final %>%
  left_join(db_orgs, by = "org_id") %>%
  left_join(species_registry_final, by = "species_key") %>%
  left_join(db_species, by = c("name" = "scientific_name")) %>%
  select(org_key, species_key = species_key.y)

existing_org_links <- dbGetQuery(con, "SELECT org_key, species_key FROM org_species_links")
new_org_links <- org_links_ready %>% anti_join(existing_org_links, by = c("org_key", "species_key"))
if(nrow(new_org_links) > 0) dbAppendTable(con, "org_species_links", new_org_links)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.8 Tree counts

trees_ready <- tree_counts_table %>%

  # Join to get the survey_key using plot_name and timeframe
  left_join(survey_lookup, by = c("plot_name", "timeframe")) %>%
  # Join to get the species_key
  left_join(db_species, by = c("species" = "scientific_name")) %>%
  select(survey_key, species_key, tree_type, size_class, tree_count)

# Only insert trees belonging to the surveys we JUST inserted
# To do this, we lookup the survey_keys for the newly inserted UUIDs
if(nrow(new_surveys) > 0) {
  new_survey_keys <- dbGetQuery(con, sprintf("SELECT survey_key, uuid FROM surveys WHERE uuid IN ('%s')", paste(new_surveys$uuid, collapse="','")))
  trees_to_insert <- trees_ready %>% filter(survey_key %in% new_survey_keys$survey_key)
  if(nrow(trees_to_insert) > 0) {
    dbAppendTable(con, "tree_counts", trees_to_insert)
    message(sprintf("    [OK] Inserted %d tree records.", nrow(trees_to_insert)))
  }
}

message("    [OK] Migration complete")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# END