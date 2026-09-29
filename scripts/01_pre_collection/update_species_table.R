#!/usr/bin/env Rscript

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Matteo Ramina (raminamatteo@me.com)
# Date: 2026-09-03
# Version: 1.0

# Description: This script parses the master Excel species tracking file to
# generate a machine-readable tree_species_table.csv mapping scientific names, 
# labels, and org_ids. This CSV is used both as a Kobo choices list and as the
# foundational species registry for the SQL database.

# Inputs:
#   • config.R - Centralized configuration file
#   • Tree Species for Review 6.2026.xlsx - Human-maintained master species
#     tracker

# Outputs:
#   • tree_species_table.csv - Standardized species mapping
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

source(here::here("scripts/utils/config.R"))

library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

source_file <- file_master_species_excel
output_file <- file_tree_species

# 1. Read all relevant sheets and combine them into a unified base
clean_sheets <- c("WRI (Clean Data)", "CI_Juliana (Clean Data)", "CI_Yulan (Clean Data)", "Mada-New species", "Full List (Clean Data)")
master_list <- list()

for (sheet in clean_sheets) {
  df <- read_excel(source_file, sheet = sheet)
  
  # Standardize "Species Names" to "scientific_name" if needed (Mada-New species)
  if ("Species Names" %in% names(df)) {
    df <- df %>% rename(scientific_name = `Species Names`)
  }
  
  # Extract core info safely
  core_cols <- intersect(names(df), c("project_name", "scientific_name", "taxon_id", "Common names"))
  
  df <- df %>%
    select(all_of(core_cols)) %>%
    rename(org_name = project_name) %>%
    mutate(scientific_name = trimws(as.character(scientific_name)))
    
  if ("Common names" %in% names(df)) {
    df <- df %>% rename(common_name = `Common names`) %>% mutate(common_name = as.character(common_name))
  } else {
    df <- df %>% mutate(common_name = NA_character_)
  }
  
  if (!("taxon_id" %in% names(df))) {
    df <- df %>% mutate(taxon_id = NA_character_)
  }
  
  master_list[[sheet]] <- df %>% select(org_name, scientific_name, taxon_id, common_name)
}

# 2. Map org_name to org_id early
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
    org_name == "FONCET" ~ "temp3",
    TRUE ~ NA_character_
  )
}

# 3. Combine and deduplicate BY org_id
combined_df <- bind_rows(master_list) %>%
  filter(!is.na(scientific_name) & !is.na(org_name)) %>%
  mutate(org_id = map_org_id(org_name)) %>%
  filter(!is.na(org_id)) %>%
  # Fill NA values by grouping by ORG_ID (not raw org_name) so variations of the same project resolve to one row
  group_by(org_id, scientific_name) %>%
  summarize(
    taxon_id = first(na.omit(taxon_id), default = NA_character_),
    common_name = first(na.omit(common_name), default = NA_character_),
    .groups = "drop"
  ) %>%
  distinct()

# 4. Format labels
processed_df <- combined_df %>%
  mutate(
    common_name = ifelse(is.na(common_name) | trimws(common_name) == "" | tolower(common_name) == "nan", NA, trimws(common_name)),
    label = ifelse(!is.na(common_name), paste0(scientific_name, " (", common_name, ")"), scientific_name),
    name = scientific_name
  ) %>%
  select(name, label, org_id, taxon_id)

# Create 'Other or don't know' for EVERY unique org_id
unique_orgs <- unique(processed_df$org_id)
other_df <- data.frame(
  name = "Other or don't know",
  label = "Other or don't know",
  org_id = unique_orgs,
  taxon_id = NA_character_,
  stringsAsFactors = FALSE
)

# Bind and sort
final_df <- bind_rows(processed_df, other_df) %>%
  arrange(org_id, name)

# Write output
write_excel_csv(final_df, output_file, na = "")

message(sprintf("Successfully generated %s with %d rows and added 'Other or don't know' for %d orgs.", output_file, nrow(final_df), length(unique_orgs)))
