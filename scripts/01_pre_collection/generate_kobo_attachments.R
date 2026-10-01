#!/usr/bin/env Rscript

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Matteo Ramina (raminamatteo@me.com)
# Date: 2026-10-01
# Version: 1.0

# Description: This script queries the SQLite database and generates the 
# CSV files required as external attachments for the Kobo survey form.
# This ensures that all form lookups, routing logic, and tree count reminders
# reflect the absolute latest ground truth from the central database.

# Inputs:
#   • config.R - Centralized configuration file
#   • ppc_database.sqlite - Target SQLite database

# Outputs:
#   • Multiple CSV attachments saved directly to kobo_management/attachments/
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 0 Packages and configuration


library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(here)

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 1 Backup and Load SQL Tables
db_path <- here::here("database/ppc_database.sqlite")
if(!file.exists(db_path)) stop("Database not found!")
con <- dbConnect(RSQLite::SQLite(), db_path)

out_dir <- here::here("kobo_management/attachments")

# Backup existing attachments before overwrite
backup_dir <- file.path(out_dir, "backups")
date_folder <- format(Sys.Date(), "%Y_%m_%d")
daily_backup_dir <- file.path(backup_dir, date_folder)

existing_csvs <- list.files(out_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(existing_csvs) > 0) {
  if (!dir.exists(daily_backup_dir)) dir.create(daily_backup_dir, recursive = TRUE)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  for (csv_path in existing_csvs) {
    base_name <- tools::file_path_sans_ext(basename(csv_path))
    new_name <- paste0(base_name, "_", timestamp, ".csv")
    file.copy(csv_path, file.path(daily_backup_dir, new_name))
  }
  message(sprintf("--> Backed up %d attachments to %s", length(existing_csvs), daily_backup_dir))
}

# Fetch full tables from SQL
db_countries <- dbGetQuery(con, "SELECT * FROM countries")
db_orgs <- dbGetQuery(con, "SELECT * FROM organizations")
db_sites <- dbGetQuery(con, "SELECT * FROM sites")
db_plots <- dbGetQuery(con, "SELECT * FROM plots")
db_surveys <- dbGetQuery(con, "SELECT * FROM surveys")
db_species <- dbGetQuery(con, "SELECT * FROM species")
db_org_species <- dbGetQuery(con, "SELECT * FROM org_species_links")
db_trees <- dbGetQuery(con, "SELECT * FROM tree_counts")

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2 Master Files (pulldata)
country_master <- db_countries %>% select(country_id, country_name)
write_excel_csv(country_master, file.path(out_dir, "attachment_country_master.csv"))

org_master <- db_orgs %>% select(org_id, org_name)
write_excel_csv(org_master, file.path(out_dir, "attachment_organization_master.csv"))

site_master <- db_sites %>% select(site_id, site_name, site_type, site_size_restoration, site_size_control)

# Inject Dummy Site for Training
dummy_site <- data.frame(
  site_id = "0000", site_name = "0000", site_type = "restoration", 
  site_size_restoration = "100", site_size_control = "100", stringsAsFactors = FALSE
)
site_master <- site_master %>% mutate(
  site_id = as.character(site_id),
  site_size_restoration = as.character(site_size_restoration),
  site_size_control = as.character(site_size_control)
)
site_master <- bind_rows(site_master, dummy_site)

write_excel_csv(site_master, file.path(out_dir, "attachment_site_master.csv"))

# plot_master must include mutable indicators from the surveys table.
# Most indicators come from the LATEST survey, but resampling info is only recorded in the FIRST survey!
surveys_processed <- db_surveys %>%
  mutate(
    time_numeric = suppressWarnings(as.numeric(str_extract(timeframe, "\\d+"))),
    time_numeric = case_when(
      timeframe == "APP" ~ -1,
      is.na(time_numeric) ~ 0,
      TRUE ~ time_numeric
    )
  )

latest_surveys <- surveys_processed %>%
  group_by(plot_key) %>%
  filter(time_numeric == max(time_numeric, na.rm = TRUE)) %>%
  ungroup() %>%
  select(
    plot_key, 
    last_timeframe = timeframe, 
    stratum, 
    planting_indicator, 
    planting_pattern, 
    anr_indicator, 
    large_trees_present, 
    little_trees_present, 
    tiny_trees_present
  )

first_surveys <- surveys_processed %>%
  group_by(plot_key) %>%
  filter(time_numeric == min(time_numeric, na.rm = TRUE)) %>%
  ungroup() %>%
  select(plot_key, resampling_30x30, resampling_3x3)

plot_master <- db_plots %>%
  left_join(db_sites %>% select(site_key, site_id), by="site_key") %>%
  left_join(latest_surveys, by="plot_key") %>%
  left_join(first_surveys, by="plot_key") %>%
  # Removed hardcoded NA override
  select(
    plot_id = plot_key, plot_name, # Kobo's pulldata explicitly looks for a column named 'plot_id'
    plot_type,
    plot_permanence,
    stratum,
    planting_pattern,
    planting_indicator,
    anr_indicator,
    site_id,
    resampling_30x30,
    resampling_3x3,
    large_trees_present,
    little_trees_present,
    tiny_trees_present,
    last_timeframe
  ) %>%
  mutate(
    last_timeframe = replace_na(last_timeframe, ''),
    # Blank out 'APP' so Kobo treats it as a first-time baseline survey
    last_timeframe = ifelse(last_timeframe == "APP", "", last_timeframe)
  )

# Inject Dummy Plots for Training
dummy_plots <- expand_grid(
  org_id = unique(db_orgs$org_id), 
  plot_type = c("restoration", "control")
) %>%
  mutate(
    plot_id = paste0(
      org_id, "-", ifelse(plot_type == "restoration", "test_restoration", "test_control")
    ),
    plot_name = paste0(
      org_id, "-", ifelse(plot_type == "restoration", "test_restoration", "test_control")
    ),
    site_id = "0000",
    plot_permanence = "permanent",
    stratum = ifelse(plot_type == "restoration", "none", "control"),
    planting_pattern = ifelse(plot_type == "restoration", "to be confirmed", "not applicable"),
    planting_indicator = ifelse(plot_type == "restoration", "yes", "no"),
    anr_indicator = "no",
    resampling_30x30 = NA_character_,
    resampling_3x3 = NA_character_,
    large_trees_present = NA_character_,
    little_trees_present = NA_character_,
    tiny_trees_present = NA_character_,
  last_timeframe = ''
) %>% select(-org_id)
plot_master <- plot_master %>% mutate(plot_id = as.character(plot_id))
plot_master <- bind_rows(plot_master, dummy_plots)

write_excel_csv(plot_master, file.path(out_dir, "attachment_plot_master.csv"))

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 3 Dropdown Files (Choices)
country_dropdown <- db_countries %>% select(name = country_id, label = country_name)
write_excel_csv(country_dropdown, file.path(out_dir, "attachment_country_dropdown.csv"))

org_dropdown <- db_orgs %>% 
  left_join(db_countries %>% select(country_key, country_id), by="country_key") %>%
  select(name = org_id, label = org_name, country_id)
write_excel_csv(org_dropdown, file.path(out_dir, "attachment_organization_dropdown.csv"))

plot_dropdown <- db_plots %>% 
  left_join(db_sites %>% select(site_key, org_key), by="site_key") %>%
  left_join(db_orgs %>% select(org_key, org_id), by="org_key") %>%
  mutate(label = paste0(plot_name, " (", plot_key, ")")) %>%
  select(name = plot_key, label, org_id)

# Inject Dummy Plots into Dropdown
dummy_dropdown <- expand_grid(
  org_id = unique(db_orgs$org_id), 
  plot_type = c("restoration", "control")
) %>%
  mutate(
    name = paste0(org_id, "-", ifelse(plot_type == "restoration", "test_restoration", "test_control")),
    label = paste0(
      "Test Plot ", 
      ifelse(plot_type == "restoration", "(Restoration)", "(Control)")
    )
  ) %>% select(name, label, org_id)
plot_dropdown <- plot_dropdown %>% mutate(name = as.character(name))
plot_dropdown <- bind_rows(plot_dropdown, dummy_dropdown)

write_excel_csv(plot_dropdown, file.path(out_dir, "attachment_plot_dropdown.csv"))

# Tree Species Dropdown (Filtered by Org_id)
tree_species_dropdown <- db_org_species %>%
  left_join(db_orgs %>% select(org_key, org_id), by="org_key") %>%
  left_join(
    db_species %>% select(species_key, scientific_name, common_name), 
    by="species_key"
  ) %>%
  mutate(
    label = ifelse(
      is.na(common_name), 
      scientific_name, 
      paste0(scientific_name, " (", common_name, ")")
    ),
    is_other = ifelse(scientific_name == "Other or don't know", 1, 0)
  ) %>%
  arrange(org_id, is_other, label) %>%
  select(name = scientific_name, label = label, org_id)

write_excel_csv(tree_species_dropdown, file.path(out_dir, "attachment_tree_species_dropdown.csv"))

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 4 Tree Count Reminders
aux_tree_prep <- db_trees %>%
  left_join(db_surveys %>% select(survey_key, plot_key, timeframe), by="survey_key") %>%
  left_join(db_species %>% select(species_key, scientific_name), by="species_key") %>%
  mutate(
    time_numeric = suppressWarnings(as.numeric(str_extract(timeframe, "\\d+"))),
    time_numeric = replace_na(time_numeric, 0)
  )

aux_tree_latest <- aux_tree_prep %>%
  group_by(plot_key) %>%
  filter(time_numeric == max(time_numeric, na.rm = TRUE)) %>%
  ungroup()

tree_summary <- aux_tree_latest %>%
  group_by(plot_key, timeframe, size_class) %>%
  summarise(
    message = paste(
      " -- ", tree_count, " ", scientific_name, " (", tree_type, ")", 
      sep = "", collapse = ""
    ),
    .groups = "drop"
  )

tree_count_reminders <- tree_summary %>%
  pivot_wider(
    id_cols = c(plot_key, timeframe),
    names_from = size_class,
    values_from = message,
    names_glue = "reminder_{size_class}"
  ) %>%
  rename(
    plot_id = plot_key, # Renamed so Kobo can look it up natively
    reminder_10cm_dbh = any_of("reminder_>10cm"),
    reminder_1_9cm_dbh = any_of("reminder_1 - 9.9cm"),
    reminder_planted = any_of("reminder_<10cm planted"),
    reminder_lt_1cm_dbh = any_of("reminder_<1cm"),
    reminder_ge_1cm_dbh = any_of("reminder_>1cm")
  )

write_excel_csv(tree_count_reminders, file.path(out_dir, "attachment_tree_count_reminders.csv"))

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 5 Cleanup

dbDisconnect(con)
message("All 9 Kobo attachments successfully generated in kobo_management/attachments/")
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# END
