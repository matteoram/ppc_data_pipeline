# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Matteo Ramina
# Date: 2026-05-14
# Description: Centralized path and environment management for the PPC data pipeline.
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Check for sync and missing packages
if (!renv::status()$synchronized) {
  stop(
    paste0(
      "Your library is out of sync with the lockfile.
",
      "Please run renv::restore() in the console before proceeding."
    )
  )
}

library(here)
library(readr)

# -----------------------------------------------------------------------------
# CORE DIRECTORIES
# -----------------------------------------------------------------------------
dir_data         <- here("data")
dir_raw_data     <- file.path(dir_data, "raw")
dir_cleaned_data <- file.path(dir_data, "clean")

dir_database     <- here("database")
dir_db_update    <- file.path(dir_database, "update")
dir_relational_db <- dir_database # Alias for legacy scripts

dir_outputs      <- here("outputs")
dir_analysis_results <- dir_outputs # Alias for legacy scripts

dir_scripts      <- here("scripts")
dir_utils        <- file.path(dir_scripts, "utils")

dir_kobo         <- here("kobo_management")
dir_legacy       <- here("legacy_migration")
dir_legacy_data  <- file.path(dir_legacy, "legacy_data")
dir_legacy_qa    <- file.path(dir_legacy, "legacy_qa_issues")
dir_legacy_utils <- file.path(dir_legacy, "legacy_scripts", "utils")

# -----------------------------------------------------------------------------
# TARGET FILES (Legacy & Active)
# -----------------------------------------------------------------------------
# Key Database Files
file_country_master <- file.path(dir_database, "country_master_file.csv")
file_org_master     <- file.path(dir_database, "organization_master_file.csv")
file_plot_master    <- file.path(dir_database, "plot_master_file.csv")
file_site_master    <- file.path(dir_database, "site_master_file.csv")

# Tree Species Registry
file_tree_species   <- here(
  "kobo_management", "attachments", "tree_species_table.csv"
)
file_master_species_excel <- file.path(dir_db_update, "Tree Species for Review 10.2026.xlsx")

# Legacy Migration Mapping Files
file_map_orgs    <- file.path(dir_legacy_data, "map_organizations.xlsx")
file_map_sites   <- file.path(dir_legacy_data, "map_sites.xlsx")
file_map_plots   <- file.path(dir_legacy_data, "map_plots.xlsx")
file_map_species <- file.path(dir_legacy_data, "map_species.xlsx")
file_map_columns <- file.path(dir_legacy_data, "map_columns.xlsx")

# Legacy Target Data Files (Dynamic)
file_site_data      <- "sites-ppc.csv"
file_site_size_data <- "site_size_data.csv"
file_kobo_forms     <- "2026 Active-NonActive Kobo Forms.xlsx"
file_kobo_plot      <- "plot_data_2026_09_23.csv"
file_kobo_tree      <- "tree_data_2026_09_23.csv"
file_dq_issues      <- "data_quality_issues_2026_05_12.xlsx"
file_cleaning_log   <- "database_cleaning_2026_09_10.xlsx"

# -----------------------------------------------------------------------------
# LOAD UTILITIES
# -----------------------------------------------------------------------------
source(file.path(dir_legacy_utils, "harmonize_sources.R"))
source(file.path(dir_legacy_utils, "add_missing_identifiers.R"))
source(file.path(dir_legacy_utils, "generate_temp_keys.R"))
source(file.path(dir_legacy_utils, "run_cleaning_routines.R"))
source(file.path(dir_legacy_utils, "aggregation_helpers.R"))


# Helper message
message("Configuration successfully loaded from scripts/utils/config.R")
