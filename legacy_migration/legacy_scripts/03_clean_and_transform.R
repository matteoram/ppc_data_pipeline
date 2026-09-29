# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Matteo Ramina (raminamatteo@me.com)
# Date: 2025-12-08
# Version: 1.0

# Description: This script is part of the legacy migration pipeline (03_clean_and_transform.R). Because source files lack 
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

# Filtering parameters for the target organization and timeframe(s)
target_org <- "CEPAN"
target_org_code <- "CEP"
target_time <- c("y0")
target_year <- NULL #c(2023, 2024)

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
export_dir_temp <- file.path(dir_legacy, "legacy_tables", "temp")

if (!dir.exists(export_dir_temp)) stop(
  "Harmonized data not found. Run 02_load_and_harmonize.R first."
)
harm_data <- readRDS(file.path(export_dir_temp, "harmonized_data.rds"))
list2env(harm_data, envir = .GlobalEnv)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 1 Load data

map_data <- readRDS(file.path(export_dir_temp, "mapping_data.rds"))
list2env(map_data, envir = .GlobalEnv)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2 Identifiers

# Print section message
message("--> Adding common identifiers...")

# Add missing identifiers in site and plot data
harmonized_dfs <- add_missing_identifiers(
  site_df = site_data_harm,
  site_size_df = site_size_data_harm,
  kobo_plot_df = kobo_plot_data_harm
)

# Extract site and plot data with missing identifiers
site_data_harm <- harmonized_dfs$site_df
site_size_data_harm <- harmonized_dfs$site_size_df
kobo_plot_data_harm <- harmonized_dfs$kobo_plot_df

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 3 Temporary keys

# Print section message
message("--> Generating temporary keys...")

# Generate temporary keys for each dataframe
site_data_temp_key <- generate_temp_keys(site_data_harm)
site_size_data_temp_key <- generate_temp_keys(site_size_data_harm)
kobo_plot_data_temp_key <- generate_temp_keys(kobo_plot_data_harm)
kobo_tree_data_temp_key <- generate_temp_keys(kobo_tree_data_harm)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 4 Cleaning

# Print section message
message("\n=== 3 Cleaning source datasets ===")

# Define path to cleaning log (required for fresh R sessions)
cleaning_file_path <- file.path(dir_legacy_qa, file_cleaning_log)

# Gather all dataframes into a named list
dfs_with_key <- list(
  site_data = site_data_temp_key,
  site_size = site_size_data_temp_key,
  kobo_plot = kobo_plot_data_temp_key,
  kobo_tree = kobo_tree_data_temp_key
)

# Run the orchestrator over the entire list
cleaned_dfs <- imap(
  dfs_with_key, ~execute_cleaning_pipeline(.x, cleaning_file_path, df_name = .y)
)

# Extract the cleaned dataframes back into the global environment
site_data_clean <- cleaned_dfs$site_data
site_size_data_clean <- cleaned_dfs$site_size
kobo_plot_data_clean <- cleaned_dfs$kobo_plot
kobo_tree_data_clean <- cleaned_dfs$kobo_tree

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 5 Filtering

# Print section message
message("\n=== 4 Filtering data ===")
message("--> Target organization: ", target_org)
message("--> Target timeframe(s): ", paste(target_time, collapse = ", "))

# Filter target project data for database creation
site_data_clean <- filter(
  site_data_clean,
  org_name == target_org
)
site_size_data_clean <- filter(
  site_size_data_clean,
  org_name == target_org
)
kobo_plot_data_clean <- filter(
  kobo_plot_data_clean,
  org_name == target_org & timeframe %in% target_time
)
kobo_tree_data_clean <- filter(
  kobo_tree_data_clean,
  org_name == target_org & timeframe %in% target_time
)
tree_species_table <- filter(
  tree_species_table,
  org_id == target_org_code
)

# Optional target year filtering
if (!is.null(target_year) && !all(is.na(target_year))) {
  message("--> Target year(s): ", paste(target_year, collapse = ", "))
  kobo_plot_data_clean <- filter(
    kobo_plot_data_clean,
    lubridate::year(date) %in% target_year
  )
  
  # Cascade the filter to tree-level data based on the valid UUIDs that passed the plot-level date filter
  if ("uuid" %in% names(kobo_tree_data_clean)) {
    kobo_tree_data_clean <- filter(
      kobo_tree_data_clean,
      uuid %in% kobo_plot_data_clean$uuid
    )
  }
}

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 6 Countries table

# Print section message
message("\n=== 5 Creating database tables ===\n\n")
message("--> [1/8] Countries table...")

# Extract all country names
all_countries <- c(
  site_data_clean$country_name,
  site_size_data_clean$country_name,
  kobo_plot_data_clean$country_name
)

# Create the countries table dataframe
countries_table <- tibble(country_name = unique(all_countries)) %>%

  # Remove NAs and empty strings
  filter(!is.na(country_name) & country_name != "") %>%

  # Sort alphabetically
  arrange(country_name) %>%

  mutate(
    # Generate standard 2-letter ISO codes
    country_id = suppressWarnings(
      toupper(countrycode(country_name, "country.name", "iso2c"))
    ),
    
    # Fill in NA values (e.g., "Andes") with a custom 2-letter code
    country_id = case_when(
      country_name == "High Andes (Ecuador and Peru)" ~ "HA",
      country_name == "Scotland" ~ "SC",
      is.na(country_id) ~ paste0("X", toupper(substr(country_name, 1, 1))),
      TRUE ~ country_id
    )
  ) %>%

  # Keep only the requested columns
  select(country_id, country_name)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 7 Organizations table

# Print section message
message("--> [2/8] Organizations table...")

# Extract all country-org pairs
org_list <- list(
  site_data_clean %>% select(org_name, country_name),
  site_size_data_clean %>% select(org_name, country_name),
  kobo_plot_data_clean %>% select(org_name, country_name)
)

# Combine and consolidate
org_table_base <- bind_rows(org_list) %>%
  filter(!is.na(org_name) & org_name != "") %>%
  distinct() %>%
  
  # Keep unique pairs and drop an NA country if org has other valid country row
  group_by(org_name) %>%
  filter(!(is.na(country_name) & n() > 1)) %>%
  ungroup() %>%
  arrange(org_name)

# Generate unique IDs and apply manual overrides
unique_orgs <- org_table_base %>%
  distinct(org_name) %>%
  mutate(
    org_id = case_when(
      org_name == "Accion Andina" ~ "AND",
      org_name == "ACT" ~ "ACT",
      org_name == "AMBIO" ~ "AMB",
      org_name == "APRN La Frailescana" ~ "FRA",
      org_name == "Café Capitan" ~ "CAF",
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
      org_name == "Faja Lobi" ~ "FAL",
      org_name == "Fedecovera" ~ "FED",
      org_name == "GANB (2021)" ~ "GAN",
      org_name == "GANB (Flagship)" ~ "GAF",
      org_name == "Green Belt Movement" ~ "GBM",
      org_name == "Green Forests Work" ~ "GFW",
      org_name == "Greening Australia" ~ "GAU",
      org_name == "Grow Trees" ~ "GTR",
      org_name == "ISA" ~ "ISA",
      org_name == "IUCN Thailand" ~ "THA",
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
      org_name == "Reforest Action (France)" ~ "RCH",
      org_name == "Reforest Action (Portugal)" ~ "RPR",
      org_name == "Reforest Action (Spain)" ~ "RPA",
      org_name == "TERI" ~ "TER",
      org_name == "Tree Canada" ~ "TCA",
      org_name == "UCIRI" ~ "UCI",
      org_name == "Wells for Zoe" ~ "WFZ",
      org_name == "WWF" ~ "WWF",
      org_name == "Conservation International - México" ~ "temp1", 
      org_name == "Conservação Internacional do Brasil" ~ "temp2",
      org_name == "FONCET" ~ "temp3",
      org_name == "RA" ~ "temp4",
      TRUE ~ NA_character_
    )
  )

# Join the IDs back to the table list, rename to final column name
org_table_final <- org_table_base %>%
  left_join(unique_orgs, by = "org_name") %>%
  rename(org_name = org_name) %>%
  
  # Join with countries_table to get country_id
  left_join(
    countries_table %>% select(country_id, country_name),
    by = "country_name",
    relationship = "many-to-many"
  ) %>%
  select(org_id, org_name, country_id) %>%

  # Add missing countries
  mutate(
    country_id = case_when(
      org_name == "COOPERATIVA CANTEIROS" ~ "BR",
      org_name == "POLIMATA" ~ "BR",
      org_name == "PRIMAFLORA" ~ "BR",
      TRUE ~ country_id
    )
  )

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 8 Sites table

# Print section message
message("--> [3/8] Sites table...")

# Extract all org-site pairs
site_list <- list(
  site_data_clean %>% select(
    site_name, org_name, temp_site_key
  ),
  site_size_data_clean %>% select(
    site_name, org_name, temp_site_key
  ),
  kobo_plot_data_clean %>% select(
    site_name, org_name, temp_site_key
  )
)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 10.1 Base table

# Combine and consolidate without site characteristics
sites_table_base <- bind_rows(site_list) %>%
  filter(!is.na(site_name) & site_name != "") %>%
  distinct() %>%
  arrange(site_name) %>%
  
  # Create numeric site_id (coerces non-numeric text to NA automatically)
  mutate(site_id = suppressWarnings(as.numeric(site_name))) %>%
  
  # Join org_table_final to get org_id
  left_join(org_table_final %>% select(org_id, org_name), by = "org_name")

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 10.2 Attributes

# Define the columns we want to extract and coalesce
site_attr_cols <- c("site_type", "site_size_restoration", "site_size_control")

# Get site type and sizes from kobo_plot_data
site_attributes_kobo_plot <- aggregate_attributes(
  df = kobo_plot_data_clean, 
  group_col = "temp_site_key", 
  attr_cols = site_attr_cols
)


# Combine site type and sizes from kobo API
sites_table_with_attr <- sites_table_base %>%
  left_join(
    site_attributes_kobo_plot, by = "temp_site_key"
  ) %>%
  select(
    site_id,
    site_name,
    site_type,
    site_size_restoration,
    site_size_control,
    org_id,
    temp_site_key
  ) %>%
  arrange(
    site_id
  )

# Add bare land indicator
bare_land_projects <- c(
  "ACT",
  "CHI",
  "MAD",
  "UAE",
  "GFW",
  "GAU",
  "RCH",
  "TCA"
)
sites_table_with_attr <- sites_table_with_attr %>%
  mutate(
    bare_land = if_else(
      org_id %in% bare_land_projects, "yes", "no"
    )
  )

# Apply site-level cleaning log AGAIN here so it can patch attributes for sites that were missing in Kobo
sites_table_with_attr <- apply_cleaning_log(
  df = sites_table_with_attr,
  log_file_path = cleaning_file_path,
  sheet_name = "site",
  key_col = "temp_site_key",
  key_components = c("country_name", "org_name", "site_name"),
  df_name = "sites_table_with_attr"
)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 10.3 Final table

sites_table_final <- sites_table_with_attr %>%
  mutate(in_terramatch = site_name %in% site_data_clean$site_name) %>%
  left_join(site_data_clean %>% select(site_name, name) %>% distinct(site_name, .keep_all = TRUE), by = "site_name") %>%
  mutate(site_name = coalesce(as.character(name), as.character(site_name))) %>%
  select(
    site_id,
    site_name,
    site_type,
    site_size_restoration,
    site_size_control,
    bare_land,
    in_terramatch,
    org_id
  ) %>%
  arrange(site_id)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 11 Plots table

# Print section message
message("--> [4/8] Plots table...")

# Extract all org-site-plot combinations
plot_list <- list(
  kobo_plot_data_clean %>% 
    select(
      plot_name, temp_site_key, temp_plot_key
    )
)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 11.1 Base table

# Combine and consolidate without site attributes
plots_table_base <- bind_rows(plot_list) %>%
  filter(!is.na(temp_plot_key) & temp_plot_key != "") %>%
  distinct() %>%
  
  # Join with sites_table_with_attr to fetch site IDs
  left_join(
    sites_table_with_attr %>% select(
      site_id, temp_site_key
    ), by = "temp_site_key"
  )

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 11.2 Attributes

# Get plot type, permanence, strata, planting and tiny trees from kobo_plot_data
plot_attributes_kobo_plot <- kobo_plot_data_clean %>%
  mutate(form_anr_indicator = if("anr_indicator" %in% names(.)) anr_indicator else NA_character_) %>%
  select(
    temp_plot_key,
    timeframe,
    date,
    start_time,
    end_time,
    plot_type,
    plot_permanence,
    stratum,
    planting_pattern,
    tiny_trees_present,
    form_anr_indicator,
    uuid
  ) %>%

  # Keep all duplicate submissions (group by UUID)
  group_by(temp_plot_key, uuid) %>%
  slice(1) %>%
  ungroup()

# Get planting and ANR indicators from kobo_tree_data
attributes_kobo_tree <- kobo_tree_data_clean %>%
  select(temp_plot_key, uuid, tree_type, size_class, plot_size, origin_table) %>%
  group_by(temp_plot_key, uuid) %>%
  summarise(
    planting_indicator = if_else(
      any(tree_type == "planted", na.rm = TRUE), "yes", "no"
    ),
    planting_group = if_else(
      any(grepl("planted", origin_table, ignore.case = TRUE), na.rm = TRUE),
      "yes", "no"
    ),
    .groups = "drop"
  )


# Get resampling and large/little trees presence attributes from kobo_plot_data
resampling_plot_attributes_kobo_plot <- kobo_plot_data_clean %>%
  select(
    temp_plot_key, uuid,
    resampling_30x30,
    large_trees_present,
    resampling_3x3,
    little_trees_present
  ) %>%

  # Keep all duplicate submissions (group by UUID)
  group_by(temp_plot_key, uuid) %>%
  slice(1) %>%
  ungroup()

# Get resampling and large/little trees presence attributes from kobo_tree_data
resampling_attributes_kobo_tree <- kobo_tree_data_clean %>%
  select(
    temp_plot_key, uuid,
    plot_size,
    size_class,
    tree_type
  ) %>%

  # Add resampling info from kobo_plot_data
  left_join(
    resampling_plot_attributes_kobo_plot,
    by = c("temp_plot_key", "uuid"),
    relationship = "many-to-many"
  ) %>%

  # Keep all duplicate submissions (group by UUID)
  group_by(temp_plot_key, uuid) %>%
  slice(1) %>%
  ungroup()

# Combine all data streams and coalesce
plots_table_with_attr <- plots_table_base %>%
  left_join(
    plot_attributes_kobo_plot,
    by = "temp_plot_key"
  ) %>%
  left_join(
    attributes_kobo_tree, by = c("temp_plot_key", "uuid")
  ) %>%
  # Join the cleaned and flattened Kobo/Tree resampling data
  left_join(
    resampling_attributes_kobo_tree, 
    by = c("temp_plot_key", "uuid")
  ) %>%
  
  mutate(
    # Default indicator fallbacks
    planting_indicator = replace_na(planting_indicator, "no"),
    planting_group = replace_na(planting_group, "no")
  ) %>%
  select(
    -plot_size,
    -size_class,
    -tree_type
  )

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 10.3 Final table

# Plot ID generation and schema finalization
plots_table_with_attr_and_key <- plots_table_with_attr %>%
  
  left_join(
    sites_table_final %>% 
      select(site_id, org_id) %>% 
      distinct(site_id, .keep_all = TRUE),
    by = "site_id"
  ) %>%
  left_join(
    org_table_final %>% select(org_id, org_name, country_id) %>% distinct(org_id, .keep_all = TRUE),
    by = "org_id"
  ) %>%
  mutate(
    plot_name = paste(org_id, site_id, plot_name, sep = "-"),
    
    is_historical_anr = (country_id %in% c("PH", "FR") | 
                         org_name %in% c("CERT", "GANB", "CICLOS", "MDPS", "Café Capitán", "AMBIO", "Macuiles", "APRN Frailescana", "REBISE", "REBITRI", "REBISO", "REBIVTA", "WWF")) & 
                        plot_type == "restoration",
                        
    anr_indicator = case_when(
      tolower(plot_type) == "control" ~ NA_character_,
      !is.na(form_anr_indicator) ~ form_anr_indicator,
      is_historical_anr ~ "yes",
      TRUE ~ "no"
    ),
    
    # Wipe planting metrics for control plots
    planting_pattern = if_else(tolower(plot_type) == "control", NA_character_, planting_pattern),
    planting_indicator = if_else(tolower(plot_type) == "control", NA_character_, planting_indicator),
    planting_group = if_else(tolower(plot_type) == "control", NA_character_, planting_group)
  ) %>%
  select(
    temp_plot_key,
    plot_name,
    timeframe,
    date,
    start_time,
    end_time,
    plot_type,
    plot_permanence,
    stratum,
    planting_pattern,
    planting_indicator,
    planting_group,
    anr_indicator,
    resampling_30x30,
    large_trees_present,
    resampling_3x3,
    little_trees_present,
    tiny_trees_present,
    uuid,
    site_id
  ) %>%
  arrange(temp_plot_key)

# Store a copy of the plots table for QA purposes
plot_candidates <- plots_table_with_attr_and_key %>%
  select(temp_plot_key, plot_name, plot_type, plot_permanence, site_id, uuid) %>%
  left_join(kobo_plot_data_clean %>% select(uuid, site_size_restoration, site_size_control), by = "uuid")

# Deduplicate and officially create plots_table_final
plots_table_final <- plot_candidates %>%
  group_by(plot_name) %>%
  fill(everything(), .direction = "downup") %>%
  slice(1) %>%
  ungroup() %>%
  select(-temp_plot_key, -uuid)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 12 Surveys table

# Print section message
message("--> [5/8] Surveys table...")

# Create the surveys table
surveys_table <- plots_table_with_attr_and_key %>%
  select(
    plot_name,
    timeframe,
    date,
    start_time,
    end_time,
    stratum,
    planting_pattern,
    planting_indicator,
    planting_group,
    anr_indicator,
    resampling_30x30,
    large_trees_present,
    resampling_3x3,
    little_trees_present,
    tiny_trees_present,
    uuid,
    site_id
  ) %>%
  arrange(plot_name, timeframe)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 13 Tree counts table

# Print section message
message("--> [6/8] Tree counts table...")

# Create tree counts table
tree_counts_table <- kobo_tree_data_clean %>%

  # Add org_id and site_id by joining with sites_table_final
  left_join(
    sites_table_base %>% select(site_id, org_id, temp_site_key),
    by = "temp_site_key"
  ) %>%
  mutate(
    plot_name = paste(org_id, site_id, plot_name, sep = "-")
  ) %>%
  select(
    site_name,
    plot_name,
    timeframe,
    species,
    tree_type,
    size_class,
    tree_count,
    origin_table,
    uuid
  ) %>%
  arrange(plot_name, timeframe)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 14 Species table

# Print section message
message("--> [7/8] Species table...")

# Create unique species registry
species_registry_final <- tree_species_table %>%
  clean_invisible_spaces() %>%
  distinct(name, .keep_all = TRUE) %>%
  arrange(name) %>%
  mutate(
    label = stringr::str_extract(label, "(?<=\\().*?(?=\\))"),
    species_key = row_number()
  )

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 14 Organizations-species table

# Print section message
message("--> [8/8] Organizations-species table...")

# Create organization-species link table
org_species_lists_final <- tree_species_table %>%
  clean_invisible_spaces() %>%
  left_join(
    species_registry_final %>% select(name, species_key), by = "name") %>%
  left_join(org_table_final, by = c("org_id" = "org_name")) %>%
  select(org_id, species_key) %>%
  distinct() %>%
  arrange(org_id, species_key)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region Export 

message("\n=== Exporting Final Tables ===")

if (!dir.exists(export_dir_base)) {
  dir.create(export_dir_base, recursive = TRUE, showWarnings = FALSE)
}

write_csv(
  plot_candidates, file.path(export_dir_base, "plot_candidates.csv")
)
write_csv(
  tree_species_table, file.path(export_dir_base, "tree_species_table.csv")
)
write_csv(
  countries_table, file.path(export_dir_base, "countries_table.csv")
)
write_csv(
  org_table_final, file.path(export_dir_base, "org_table.csv")
)
write_csv(
  sites_table_final, file.path(export_dir_base, "sites_table.csv")
)
write_csv(
  plots_table_final, file.path(export_dir_base, "plots_table.csv")
)
write_csv(
  surveys_table, file.path(export_dir_base, "surveys_table.csv")
)
write_csv(
  species_registry_final, file.path(export_dir_base, "species_registry.csv")
)
write_csv(
  org_species_lists_final, file.path(export_dir_base, "org_species_links.csv")
)
write_csv(
  tree_counts_table, file.path(export_dir_base, "tree_counts.csv")
)

message("--> Final tables exported to: ", export_dir_base)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# END