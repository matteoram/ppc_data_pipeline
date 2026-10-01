#!/usr/bin/env Rscript

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Matteo Ramina (raminamatteo@me.com)
# Date: 2026-10-01
# Version: 1.1

# Description: Parses the org-specific Excel species tracking file in the update 
# directory and safely updates the SQLite database (species and org_species_links) 
# via anti-joins.

# Inputs:
#   • config.R - Centralized configuration file
#   • Tree Species for Review ... .xlsx (Inside target update folder)

# Outputs:
#   • Updated ppc_database.sqlite
#
# The script uses VS Code minimap's regions.
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

source(here::here("scripts/utils/config.R"))

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(RSQLite)
library(DBI)

# NOTE: Specify the folder name inside database/update/ that contains the file
target_folder <- "CANTEIROS_Y0"

folder_path <- file.path(dir_db_update, target_folder)

# Helper function to remove zero-width spaces
clean_invisible_spaces <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ str_replace_all(.x, "\\u200B", "")))
}

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 1 Load Excel

# Find the file automatically
form_files <- list.files(
  folder_path, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE
)

# Only keep files that contain "Species" in the name, ignore temps
species_files <- form_files[
  grepl("Species", basename(form_files), ignore.case = TRUE) & 
  !grepl("~\\$", basename(form_files))
]

if (length(species_files) == 0) {
  stop(sprintf("No Species Excel file found in %s", folder_path))
} else if (length(species_files) > 1) {
  stop(sprintf("Multiple Species Excel files found in %s.", folder_path))
}

species_file_path <- species_files[1]
message("--> Loading species from: ", basename(species_file_path))

clean_sheets <- c(
  "WRI (Clean Data)", "CI_Juliana (Clean Data)", "CI_Yulan (Clean Data)", 
  "Mada-New species", "Full List (Clean Data)"
)
master_list <- list()

for (sheet in clean_sheets) {

  # Catch errors if a specific sheet doesn't exist in a localized file
  tryCatch({
    df <- read_excel(species_file_path, sheet = sheet)
    if ("Species Names" %in% names(df)) df <- df %>% rename(
      scientific_name = `Species Names`
    )
    
    core_cols <- intersect(
      names(df),
      c("project_name", "scientific_name", "taxon_id", "Common names")
    )
    
    df <- df %>%
      select(all_of(core_cols)) %>%
      rename(org_name = project_name) %>%
      mutate(scientific_name = trimws(as.character(scientific_name)))
      
    if ("Common names" %in% names(df)) {
      df <- df %>% rename(common_name = `Common names`) %>%
        mutate(common_name = as.character(common_name))
    } else {
      df <- df %>% mutate(common_name = NA_character_)
    }
    if (!("taxon_id" %in% names(df))) df <- df %>%
      mutate(taxon_id = NA_character_)
    
    master_list[[sheet]] <- df %>%
      select(org_name, scientific_name, taxon_id, common_name)
  }, error = function(e) {
    # Sheet doesn't exist, skip silently
  })
}

if (length(master_list) == 0) {
  stop("None of the expected sheets were found in the file.")
}

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2 Clean and Transform

map_org_id <- function(org_name) {
  case_when(
    org_name == "Accion Andina" ~ "AND",
    org_name == "ACT" ~ "ACT",
    org_name == "Amadese" ~ "AMA",
    org_name == "AMBIO" ~ "AMB",
    org_name == "APRN La Frailescana" | org_name == "La Frailescana" ~ "FRA",
    org_name == "Café Capitan" | org_name == "Cafe Capitan" ~ "CAF",
    org_name == "CEPAN" ~ "CEP",
    org_name == "CERT" ~ "CER",
    org_name == "CI Cambodia" ~ "CAM",
    org_name == "CI China" ~ "CHI",
    org_name == "CI Colombia" ~ "COL",
    org_name == "CI Madagascar" ~ "MAD",
    org_name == "CI Philippines" ~ "PHI",
    org_name == "CICLOS" ~ "CIC",
    org_name == "COOPERATIVA CANTEIROS" ~ "CAN",
    org_name == "CUCOS" ~ "CUC",
    org_name == "EMA" ~ "EMA",
    org_name == "Emirates Nature WWF" ~ "UAE",
    org_name == "Fafiala" ~ "FAF",
    org_name == "Faja Lobi" ~ "FAL",
    org_name == "Fedecovera" ~ "FED",
    org_name == "GANB (2021)" | org_name == "GANB 2021" ~ "GAN",
    org_name == "GANB (Flagship)" | org_name == "GANB FS" ~ "GAF",
    org_name == "Green Belt Movement" | org_name == "GBM" ~ "GBM",
    org_name == "Green Forests Work" | org_name == "GFW" ~ "GFW",
    org_name == "Greening Australia" ~ "GAU",
    org_name == "Grow Trees" ~ "GTR",
    org_name == "ISA" ~ "ISA",
    org_name == "IUCN Thailand" | org_name == "IUCN" ~ "THA",
    org_name == "Macuiles" ~ "MAC",
    org_name == "MDPS" ~ "MDP",
    org_name == "MVGI" ~ "MVG",
    org_name == "PEROA" ~ "PER",
    org_name == "POLIMATA" ~ "POL",
    org_name == "PRIMAFLORA" ~ "PRI",
    org_name == "REBISE" ~ "BSE",
    org_name == "REBISO" ~ "BSO",
    org_name == "REBITRI" ~ "BTR",
    org_name == "REBIVTA" ~ "BVT",
    org_name == "Reforest Action (France)" | org_name == "Reforest Action France" ~ "RCH",
    org_name == "Reforest Action (Portugal)" | org_name == "Reforest Action Portugal" ~ "RPR",
    org_name == "Reforest Action (Spain)" | org_name == "Reforest Action Spain" ~ "RPA",
    org_name == "TERI" ~ "TER",
    org_name == "Toatsara" ~ "TOA",
    org_name == "Tree Canada" ~ "TCA",
    org_name == "UCIRI" ~ "UCI",
    org_name == "Wells for Zoe" ~ "WFZ",
    org_name == "WWF" ~ "WWF",
    org_name == "COOPERATIVA CANTEIROS" ~ "CAN",
    org_name == "FONCET" ~ "temp3", TRUE ~ NA_character_
  )
}

combined_df <- bind_rows(master_list) %>%
  filter(!is.na(scientific_name) & !is.na(org_name)) %>%
  mutate(org_id = map_org_id(org_name)) %>%
  filter(!is.na(org_id)) %>%
  group_by(org_id, scientific_name) %>%
  summarize(taxon_id = first(na.omit(taxon_id), default = NA_character_), common_name = first(na.omit(common_name), default = NA_character_), .groups = "drop") %>%
  distinct()

tree_species_table <- combined_df %>%
  mutate(
    common_name = ifelse(is.na(common_name) | trimws(common_name) == "" | tolower(common_name) == "nan", NA, trimws(common_name)),
    label = ifelse(!is.na(common_name), paste0(scientific_name, " (", common_name, ")"), scientific_name),
    name = scientific_name
  ) %>%
  select(name, label, org_id, taxon_id)

unique_orgs <- unique(tree_species_table$org_id)
other_df <- data.frame(name = "Other or don't know", label = "Other or don't know", org_id = unique_orgs, taxon_id = NA_character_, stringsAsFactors = FALSE)
tree_species_table <- bind_rows(tree_species_table, other_df) %>% arrange(org_id, name)

# Parse species registry
species_registry_final <- tree_species_table %>%
  clean_invisible_spaces() %>%
  distinct(name, .keep_all = TRUE) %>%
  arrange(name) %>%
  mutate(
    label = stringr::str_extract(label, "(?<=\\().*?(?=\\))")
  )

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 3 Database Insertion

db_path <- file.path(dir_database, "ppc_database.sqlite")
con <- dbConnect(RSQLite::SQLite(), db_path)

dbBegin(con)

tryCatch({
  
  # --- Insert Species ---
  species_ready <- species_registry_final %>%
    select(scientific_name = name, common_name = label)
  
  existing_species <- dbGetQuery(con, "SELECT scientific_name FROM species")$scientific_name
  new_species <- species_ready %>% filter(!scientific_name %in% existing_species)
  
  if(nrow(new_species) > 0) {
    dbAppendTable(con, "species", new_species)
    message(sprintf("--> Inserted %d new species into the database.", nrow(new_species)))
  } else {
    message("--> No new species to insert.")
  }
  
  # Fetch updated species table with auto-generated species_keys
  db_species <- dbGetQuery(con, "SELECT species_key, scientific_name FROM species")
  db_orgs <- dbGetQuery(con, "SELECT org_key, org_id FROM organizations")
  
  # --- Insert Org-Species Links ---
  org_links_ready <- tree_species_table %>%
    clean_invisible_spaces() %>%
    left_join(db_orgs, by = "org_id") %>%
    left_join(db_species, by = c("name" = "scientific_name")) %>%
    select(org_key, species_key) %>%
    distinct()
    
  existing_org_links <- dbGetQuery(con, "SELECT org_key, species_key FROM org_species_links")
  new_org_links <- org_links_ready %>% anti_join(existing_org_links, by = c("org_key", "species_key"))
  
  if(nrow(new_org_links) > 0) {
    dbAppendTable(con, "org_species_links", new_org_links)
    message(sprintf("--> Inserted %d new org-species links into the database.", nrow(new_org_links)))
  } else {
    message("--> No new org-species links to insert.")
  }

  dbCommit(con)
  message("\nDatabase successfully updated!")
  
}, error = function(e) {
  dbRollback(con)
  stop("\n[FATAL ERROR] Transaction failed and rolled back. Details: ", e$message)
}, finally = {
  dbDisconnect(con)
})
