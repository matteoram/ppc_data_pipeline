#!/usr/bin/env Rscript

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Matteo Ramina (raminamatteo@me.com)
# Date: 2026-10-01
# Version: 1.0

# Description: This script parses a new application form and updates the
# SQLite database with structural metadata (Countries, Organizations, Sites,
# Plots). Older version of the database is moved into the backup directory.
# This ensures Kobo attachments can be subsequently generated with the
# latest approved plots.

# Inputs:
#   • config.R - Centralized configuration file
#   • ApplicationForm_Template_XXX.xlsx - Target application form for XXX

# Outputs:
#   • ppc_database.sqlite - Updated SQLite database

# The script uses VS Code minimap's regions.
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 0 Packages and configuration

source(here::here("scripts/utils/config.R"))
library(here)
library(dplyr)
library(readxl)
library(DBI)
library(RSQLite)
library(openxlsx)

# NOTE: Specify the folder name inside database/update/ that contains the form
target_folder <- "CANTEIROS_Y0"
db_path <- file.path(dir_database, "ppc_database.sqlite")
folder_path <- file.path(dir_db_update, target_folder)

# Connect to the database
con <- dbConnect(RSQLite::SQLite(), db_path)

# Helper to export conflict reports
write_conflict_report <- function(df, error_type, base_path) {
  report_path <- file.path(
    base_path, paste0(
      "Conflict_Report_",
      error_type,
      "_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".xlsx"
    )
  )
  openxlsx::write.xlsx(df, file = report_path)
  return(report_path)
}

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 1 Load form

# Find the file automatically
form_files <- list.files(
  folder_path, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE
)
# Ignore open excel temp files
form_files <- form_files[
  !grepl("~\\$", basename(form_files))
]

# Check that exactly one Excel file is found
if (length(form_files) == 0) {
  stop(sprintf("No Excel files found in %s", folder_path))
} else if (length(form_files) > 1) {
  stop(
    sprintf(
      "Multiple Excel files found in %s. Please ensure only ONE application form is in the folder.", folder_path
    )
  )
}

# Select excel file and print success message
app_form_path <- form_files[1]
message(sprintf("Found application form: %s", basename(app_form_path)))

# Identify the actual column headers are on row 3 (skip = 2)
app_raw <- tryCatch({
  read_excel(app_form_path, sheet = "PlotInfo", skip = 2)
}, error = function(e) {
  stop(
    "Failed to read 'PlotInfo' sheet from the provided Excel file. Ensure the file path and sheet names are correct."
  )
})

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 1 Prepare form

# Standardize column names and values to match DB schema
app_clean <- app_raw |>

  # Drop 4 rows with English/Spanish/Portuguese/French header translations
  slice(-(1:4)) |>
  rename(plot_name = plot_id) |>
  mutate(
    country_id = str_trim(as.character(country_id)),
    org_id = str_trim(as.character(org_id)),
    org_name = str_trim(as.character(org_name)),
    site_id = str_trim(as.character(site_id)),
    site_type = str_trim(tolower(site_type)),
    plot_name = str_trim(as.character(plot_name)),
    plot_type = str_trim(tolower(plot_type)),
    plot_permanence = str_trim(tolower(plot_permanence)),
    stratum = str_trim(tolower(strata_number)),
    planting_pattern = str_trim(tolower(planting_pattern_tmp)),
    planting_indicator = str_trim(tolower(planting_indicator)),
    anr_indicator = str_trim(tolower(anr_indicator))
  ) |>
  
  # Replace plot_name with naming convention
  mutate(
    plot_name = {
      paste(org_id, site_id, plot_name, sep = "-")
    }
  ) |>
  filter(!is.na(plot_name))

# Check if any plot names are duplicated
internal_plot_dups <- app_clean |>
  group_by(plot_name) |>
  filter(n() > 1) |>
  ungroup()
if (nrow(internal_plot_dups) > 0) {
  report_file <- write_conflict_report(internal_plot_dups, "Internal_Duplicates", folder_path)
  stop(
    sprintf(
      "\n[FATAL ERROR] The application form contains duplicated plot IDs.\n--> A detailed report has been exported to: %s", 
      report_file
    )
  )
}

# Extract unique entity structures
new_countries <- app_clean |>
  select(country_id) |> distinct() |>
  filter(!is.na(country_id))
new_orgs <- app_clean |>
  select(country_id, org_id, org_name) |> distinct() |>
  filter(!is.na(org_id))
new_sites <- app_clean |>
  select(org_id, site_id, site_name, site_type) |> distinct() |>
 filter(!is.na(site_id))
new_plots <- app_clean |>
  select(
    site_id, plot_name, plot_type, plot_permanence, stratum, planting_pattern, 
    planting_indicator, anr_indicator
  ) |> distinct() |>
  filter(!is.na(plot_name))

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2 Validate form

message("Running pre-flight conflict checks against the database...")

# Extract existing sites and plots from the database for comparison
db_sites <- dbReadTable(con, "sites")
db_plots <- dbReadTable(con, "plots")

# Fatal check 1: Conflicting sites (same ID, different name or type)
site_conflicts <- new_sites |>
  inner_join(db_sites, by = "site_id") |>
  filter(site_name.x != site_name.y | site_type.x != site_type.y)
if (nrow(site_conflicts) > 0) {
  dbDisconnect(con)
  report_file <- write_conflict_report(
    site_conflicts, "Site_Conflicts", folder_path
  )
  stop(
    sprintf(
      "\n[FATAL ERROR] The application form contains %d site(s) with conflicting characteristics.\n--> A detailed report has been exported to: %s", 
      nrow(site_conflicts), report_file
    )
  )
}

# Fatal check 2: identical plots
plot_conflicts <- new_plots |>
  inner_join(db_plots, by = "plot_name")
if (nrow(plot_conflicts) > 0) {
  dbDisconnect(con)
  report_file <- write_conflict_report(
    plot_conflicts, "Plot_Conflicts", folder_path
  )
  stop(
    sprintf(
      "\n[FATAL ERROR] The application form contains %d plot(s) that already exist.\n--> A detailed report has been exported to: %s", 
      nrow(plot_conflicts), report_file
    )
  )
}

# Warning 1: identical sites (same ID, exactly matching name and type)
site_duplicates <- new_sites |>
  inner_join(db_sites, by = "site_id") |>
  filter(site_name.x == site_name.y & site_type.x == site_type.y)
if (nrow(site_duplicates) > 0) {
  warning(
    sprintf(
      "\n[WARNING] %d sites in the application form already exist in the database. They will be skipped safely.", nrow(site_duplicates)
    )
  )
}

# Print success message
message(
  "All checks passed successfully. Commencing safe inserts via anti-joins...\n"
)

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 3 Execute update

# Backup the database before modification
message("Backing up database before modification...")
backup_dir <- file.path(dir_database, "backups")
if (file.exists(db_path)) {

  # Create daily subfolder
  date_folder <- format(Sys.Date(), "%Y_%m_%d")
  daily_backup_dir <- file.path(backup_dir, date_folder)
  if (
    !dir.exists(daily_backup_dir)) dir.create(daily_backup_dir,
    recursive = TRUE
  )
  
  # Copy and timestamp the file
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_path <- file.path(
    daily_backup_dir, paste0("ppc_database_", timestamp, ".sqlite")
  )
  file.copy(db_path, backup_path)
  message(sprintf("--> Database backed up to: %s\n", backup_path))
}

# Begin transaction
dbBegin(con)

# Execute inserts with anti-joins to avoid duplicates
tryCatch({

  # -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-  
  #region 3.1 Countries
  db_countries <- dbReadTable(con, "countries")
  
  # -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  #region 3.2 Organizations
  
  # Map country_key
  orgs_to_insert <- new_orgs |>
    left_join(db_countries, by = "country_id") |>
    select(country_key, org_id, org_name) |>
    anti_join(dbReadTable(con, "organizations"), by = "org_id")
    
  if (nrow(orgs_to_insert) > 0) {
    dbWriteTable(
      con, "organizations", orgs_to_insert, append = TRUE, row.names = FALSE
    )
    message(sprintf(" -> Inserted %d new organizations.", nrow(orgs_to_insert)))
  }
  
  # -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  #region 3.3 Sites

  # Map org_key
  db_orgs_updated <- dbReadTable(con, "organizations")
  sites_to_insert <- new_sites |>
    left_join(db_orgs_updated, by = "org_id") |>
    select(org_key, site_id, site_name, site_type) |>
    anti_join(dbReadTable(con, "sites"), by = "site_id")
    
  if (nrow(sites_to_insert) > 0) {
    dbWriteTable(
      con, "sites", sites_to_insert, append = TRUE, row.names = FALSE
    )
    message(sprintf(" -> Inserted %d new sites.", nrow(sites_to_insert)))
  }
  
  # -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  #region 3.4 Plots

  # Map site_key
  db_sites_updated <- dbReadTable(con, "sites")
  plots_to_insert <- new_plots |>
    left_join(db_sites_updated, by = "site_id") |>
    select(
      site_key, plot_name, plot_type, plot_permanence, stratum, 
      planting_pattern, planting_indicator, anr_indicator
    ) |>
    anti_join(dbReadTable(con, "plots"), by = "plot_name")
    
  if (nrow(plots_to_insert) > 0) {
    dbWriteTable(
      con, "plots", plots_to_insert, append = TRUE, row.names = FALSE
    )
    message(sprintf(" -> Inserted %d new plots.", nrow(plots_to_insert)))
  }
  
  dbCommit(con)
  message(
    "\nDatabase successfully updated!"
  )
  
}, error = function(e) {
  dbRollback(con)
  stop(paste("Transaction failed and rolled back:", e$message))
})

# Cleanup
dbDisconnect(con)

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# END