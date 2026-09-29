# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Matteo Ramina (raminamatteo@me.com)
# Date: 2026-08-26
# Version: 1.0

# Description: This script initializes the SQLite database and creates the core 
# relational schema (tables and triggers) for the PPC data pipeline. It creates 
# the fundamental tables: countries, organizations, sites, plots, surveys, 
# species, and tree_counts. It also builds in a robust series of SQL triggers 
# to enforce complex data integrity rules at the database level.

# Inputs:
#   • config.R - Centralized configuration file defining paths and environment

# Outputs:
#   • ppc_database.sqlite - The core SQLite database file.

# The script uses VS Code minimap's regions.
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 0 Packages and configuration

source(here::here("scripts/utils/config.R"))
library(DBI)
library(RSQLite)
library(here)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 1 Database connection

message("\n=== 1 Initializing Database ===")

# Define path to the new database
db_path <- file.path(dir_database, "ppc_database.sqlite")
message("--> Creating/connecting to database at: ", db_path)

# Connect to the database (this creates the file if it doesn't exist)
con <- dbConnect(RSQLite::SQLite(), db_path)

# Ensure disconnection on exit
on.exit(dbDisconnect(con), add = TRUE)

# Enforce Foreign Key constraints (SQLite requires this to be turned on per connection)
dbExecute(con, "PRAGMA foreign_keys = ON;")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2 Table creation

message("\n=== 2 Creating Tables ===")

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.1 Countries

message("--> [1/7] countries...")
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS countries (
    country_key INTEGER PRIMARY KEY AUTOINCREMENT,
    country_id TEXT UNIQUE NOT NULL,
    country_name TEXT UNIQUE NOT NULL
  );
")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.2 Organizations

message("--> [2/7] organizations...")
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS organizations (
    org_key INTEGER PRIMARY KEY AUTOINCREMENT,
    country_key INTEGER NOT NULL,
    org_id TEXT UNIQUE NOT NULL,
    org_name TEXT NOT NULL,
    FOREIGN KEY(country_key) REFERENCES countries(country_key) ON DELETE CASCADE,
    UNIQUE(country_key, org_name)
  );
")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.3 Sites

message("--> [3/7] sites...")
# We scope the uniqueness to the organization. (Site 274 can exist in RPA, but another Site 274 might exist in WRI)
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS sites (
    site_key INTEGER PRIMARY KEY AUTOINCREMENT,
    org_key INTEGER NOT NULL,
    site_id TEXT NOT NULL,
    site_name TEXT NOT NULL,
    site_type TEXT,
    site_size_restoration TEXT,
    site_size_control TEXT,
    bare_land TEXT,
    FOREIGN KEY(org_key) REFERENCES organizations(org_key) ON DELETE CASCADE,
    UNIQUE(org_key, site_name),
    UNIQUE(org_key, site_id)
  );
")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.4 Plots

message("--> [4/7] plots...")
# We scope the uniqueness to the site. 
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS plots (
    plot_key INTEGER PRIMARY KEY AUTOINCREMENT,
    site_key INTEGER NOT NULL,
    plot_name TEXT NOT NULL,
    plot_type TEXT,
    plot_permanence TEXT,
    FOREIGN KEY(site_key) REFERENCES sites(site_key) ON DELETE CASCADE,
    UNIQUE(site_key, plot_name)
  );
")

# Force the plots auto-increment counter to start at 100,000
dbExecute(con, "INSERT INTO sqlite_sequence (name, seq) VALUES ('plots', 99999);")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.5 Surveys

message("--> [5/7] surveys...")
# Surveys Table (Mutable Characteristics / Timeline)
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS surveys (
    survey_key INTEGER PRIMARY KEY AUTOINCREMENT,
    plot_key INTEGER NOT NULL,
    timeframe TEXT NOT NULL,
    date TEXT,
    start_time TEXT,
    end_time TEXT,
    stratum TEXT,
    planting_pattern TEXT,
    planting_indicator TEXT,
    planting_group TEXT,
    anr_indicator TEXT,
    resampling_30x30 TEXT,
    large_trees_present TEXT,
    resampling_3x3 TEXT,
    little_trees_present TEXT,
    tiny_trees_present TEXT,
    uuid TEXT,
    FOREIGN KEY(plot_key) REFERENCES plots(plot_key) ON DELETE CASCADE,
    UNIQUE(plot_key, timeframe)
  );
")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.6 Species

message("--> [6/7] species & org_species_links...")
# Species Registry Table (The Global Dictionary)
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS species (
    species_key INTEGER PRIMARY KEY AUTOINCREMENT,
    scientific_name TEXT UNIQUE NOT NULL,
    common_name TEXT
  );
")

# Organization Species Linking Table (For Kobo Dropdowns)
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS org_species_links (
    org_key INTEGER NOT NULL,
    species_key INTEGER NOT NULL,
    FOREIGN KEY(org_key) REFERENCES organizations(org_key) ON DELETE CASCADE,
    FOREIGN KEY(species_key) REFERENCES species(species_key) ON DELETE CASCADE,
    UNIQUE(org_key, species_key)
  );
")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.7 Tree counts

message("--> [7/7] tree counts...")
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS tree_counts (
    observation_key INTEGER PRIMARY KEY AUTOINCREMENT,
    survey_key INTEGER NOT NULL,
    species_key INTEGER NOT NULL,
    tree_type TEXT NOT NULL,
    size_class TEXT NOT NULL,
    tree_count REAL NOT NULL,
    FOREIGN KEY(survey_key) REFERENCES surveys(survey_key) ON DELETE CASCADE,
    FOREIGN KEY(species_key) REFERENCES species(species_key) ON DELETE RESTRICT
  );
")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 3 SQL Triggers

message("\n=== 3 Creating SQL Triggers ===")
message("--> Enforcing data integrity constraints...")

dbExecute(con, "
  CREATE TRIGGER IF NOT EXISTS prevent_plot_type_mismatch_insert
  BEFORE INSERT ON plots
  FOR EACH ROW
  WHEN EXISTS (
    SELECT 1 FROM sites WHERE site_key = NEW.site_key 
    AND site_type = 'control' AND NEW.plot_type = 'restoration'
  )
  BEGIN
      SELECT RAISE(ABORT, 'ERROR: A control site cannot have a restoration plot.');
  END;
")

dbExecute(con, "
  CREATE TRIGGER IF NOT EXISTS check_planting_pattern_consistency_insert
  BEFORE INSERT ON surveys
  FOR EACH ROW
  WHEN (NEW.planting_indicator = 'yes' AND NEW.planting_pattern IS NULL)
    OR (NEW.planting_indicator = 'no' AND NEW.planting_pattern IS NOT NULL)
  BEGIN
      SELECT RAISE(ABORT, 'ERROR: If planting_indicator is yes, planting_pattern cannot be null. If no, it must be null.');
  END;
")

dbExecute(con, "
  CREATE TRIGGER IF NOT EXISTS check_planted_tree_consistency_insert
  BEFORE INSERT ON tree_counts
  FOR EACH ROW
  WHEN NEW.tree_type = 'planted' AND EXISTS (
    SELECT 1 FROM surveys WHERE survey_key = NEW.survey_key AND planting_indicator = 'no'
  )
  BEGIN
      SELECT RAISE(ABORT, 'ERROR: Cannot insert a planted tree observation if the parent survey has planting_indicator = no.');
  END;
")

dbExecute(con, "
  CREATE TRIGGER IF NOT EXISTS check_anr_tree_consistency_insert
  BEFORE INSERT ON tree_counts
  FOR EACH ROW
  WHEN NEW.size_class = '<1cm' AND EXISTS (
    SELECT 1 FROM surveys WHERE survey_key = NEW.survey_key AND anr_indicator = 'no'
  )
  BEGIN
      SELECT RAISE(ABORT, 'ERROR: Cannot insert an ANR (<1cm) tree observation if the parent survey has anr_indicator = no.');
  END;
")

dbExecute(con, "
  CREATE TRIGGER IF NOT EXISTS check_large_trees_consistency_insert
  BEFORE INSERT ON tree_counts
  FOR EACH ROW
  WHEN NEW.size_class = '>10cm' AND EXISTS (
    SELECT 1 FROM surveys WHERE survey_key = NEW.survey_key AND large_trees_present = 'no'
  )
  BEGIN
      SELECT RAISE(ABORT, 'ERROR: Cannot insert a >10cm tree observation if the parent survey has large_trees_present = no.');
  END;
")

dbExecute(con, "
  CREATE TRIGGER IF NOT EXISTS check_tiny_trees_consistency_insert
  BEFORE INSERT ON tree_counts
  FOR EACH ROW
  WHEN NEW.size_class = '<1cm' AND EXISTS (
    SELECT 1 FROM surveys WHERE survey_key = NEW.survey_key AND tiny_trees_present = 'no'
  )
  BEGIN
      SELECT RAISE(ABORT, 'ERROR: Cannot insert a <1cm tree observation if the parent survey has tiny_trees_present = no.');
  END;
")

message("\n[OK] Database schema successfully initialized at: ", db_path)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# END
