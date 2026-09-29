# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Matteo Ramina (raminamatteo@me.com)
# Date: 2025-12-08
# Version: 1.0

# Description: This script is part of the legacy migration pipeline (04_run_qa_checks.R). Because source files lack 
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
library(openxlsx)

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

# Folder is just the date stamp
qa_log_dir <- file.path(dir_legacy_qa, format(Sys.Date(), "%Y_%m_%d"))
if (!dir.exists(qa_log_dir)) dir.create(qa_log_dir, recursive = TRUE)


#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 1 Load tables

plot_candidates <- read_csv(
  file.path(export_dir_base, "plot_candidates.csv"), show_col_types = FALSE)

tree_species_table <- read_csv(
  file.path(export_dir_base, "tree_species_table.csv"), show_col_types = FALSE
)
plots_table_final <- read_csv(
  file.path(export_dir_base, "plots_table.csv"), show_col_types = FALSE
)
surveys_table <- read_csv(
  file.path(export_dir_base, "surveys_table.csv"), show_col_types = FALSE
)
map_strata <- read.xlsx(file.path(dir_legacy_data, "map_strata.xlsx")) %>%
  separate_rows(stratum_name, sep = "\\s*;\\s*") %>%
  mutate(stratum_name = trimws(stratum_name))
tree_counts_table <- read_csv(
  file.path(export_dir_base, "tree_counts.csv"), show_col_types = FALSE
)
species_registry_final <- read_csv(
  file.path(export_dir_base, "species_registry.csv"), show_col_types = FALSE
)
sites_table <- read_csv(
  file.path(export_dir_base, "sites_table.csv"), show_col_types = FALSE
)
org_table <- read_csv(
  file.path(export_dir_base, "org_table.csv"), show_col_types = FALSE
)
countries_table <- read_csv(
  file.path(export_dir_base, "countries_table.csv"), show_col_types = FALSE
)

survey_metadata <- surveys_table %>%
  select(plot_name, timeframe, uuid) %>%
  left_join(plots_table_final, by = "plot_name") %>%
  left_join(sites_table, by = "site_id") %>%
  left_join(org_table, by = "org_id") %>%
  left_join(countries_table, by = "country_id") %>%
  mutate(site_name = as.character(site_name)) %>%
  select(plot_name, timeframe, uuid, site_id, org_name, country_name)


#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2 QA checks

# Print section message
message("\n=== 6 Quality assurance checks ===")

qa_issues_list <- list()

message("--> Tabulating planting_pattern:")
print(as.data.frame(table(surveys_table$planting_pattern, useNA = "ifany")))
message("--> Tabulating stratum:")
print(as.data.frame(table(surveys_table$stratum, useNA = "ifany")))

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.1 Attribute uniqueness

message("--> [1/5] Checking duplicate submissions...")
duplicate_surveys <- surveys_table %>%
  group_by(plot_name, timeframe) %>%
  filter(n() > 1) %>%
  summarise(
    duplicate_uuids = paste(unique(uuid), collapse = ", "),
    .groups = "drop"
  )

if (nrow(duplicate_surveys) > 0) {
  qa_issues_list[[1]] <- duplicate_surveys %>%
    left_join(
      survey_metadata %>% select(plot_name, timeframe, site_id, org_name, country_name) %>% distinct(plot_name, timeframe, .keep_all = TRUE), 
      by = c("plot_name", "timeframe")
    ) %>%
    mutate(
      issue = sprintf("• Duplicate form submission detected! Plot '%s' was surveyed multiple times during '%s'.", plot_name, timeframe),
      uuid = duplicate_uuids,
      org_name = coalesce(org_name, target_org),
      correct_value = NA_character_,
      resolved = NA_character_,
      note = "Please indicate UUID to DROP or specify correct plot name/timeframe"
    ) %>%
    select(-duplicate_uuids)
}

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.2 Species

message("--> [2/5] Checking species registry match...")
global_species_registry <- read_csv(file_tree_species, show_col_types = FALSE)
names(global_species_registry)[1] <- "name" # Clean BOM if present

unmapped_species <- setdiff(
  unique(tree_counts_table$species[!is.na(tree_counts_table$species) & trimws(tree_counts_table$species) != ""]), 
  unique(species_registry_final$name)
)

if (length(unmapped_species) > 0) {
  unmapped_df <- tree_counts_table %>%
    filter(species %in% unmapped_species) %>%
    group_by(species) %>%
    summarise(plots = paste(unique(plot_name), collapse = ", "), .groups = "drop") %>%
    mutate(
      is_in_global = species %in% global_species_registry$name,
      issue_text = if_else(
        is_in_global,
        sprintf("• Species '%s' is in the registry, but is not mapped to organization '%s'", species, target_org_code),
        sprintf("• Species '%s' is completely missing from the master species registry", species)
      )
    )
    
  qa_issues_list[[5]] <- data.frame(
    country_name = NA_character_,
    org_name = target_org,
    site_name = NA_character_,
    plot_name = unmapped_df$plots,
    timeframe = NA_character_,
    uuid = NA_character_,
    issue = unmapped_df$issue_text,
    correct_value = NA_character_,
    resolved = NA_character_,
    note = NA_character_
  )
}

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.3 Logical constraints
#region 2.3 Logical constraints

message("--> [3/5] Checking complex logical constraints...")

# Roll over the resampling data for timeframe logic checks
surveys_table_filled <- surveys_table %>%
  group_by(plot_name) %>%
  arrange(plot_name, timeframe) %>%
  mutate(
    resampling_30x30_orig = resampling_30x30,
    resampling_3x3_orig = resampling_3x3
  ) %>%
  mutate(across(starts_with("issue_"), ~replace_na(., FALSE))) %>%
  fill(resampling_30x30, resampling_3x3, large_trees_present, little_trees_present, .direction = "down") %>%
  ungroup()

map_strata_filtered <- map_strata %>%
  filter(org_id == "ALL" | org_id == target_org_code | org_id == target_org) %>%
  arrange(desc(org_id != "ALL")) %>%
  distinct(stratum_name, .keep_all = TRUE)

qa_logic_base <- surveys_table_filled %>%
  mutate(stratum_clean = trimws(stratum)) %>%
  left_join(map_strata_filtered %>% select(stratum_name, stratum_category), by = c("stratum_clean" = "stratum_name")) %>%
  left_join(plots_table_final %>% select(plot_name, plot_type) %>% distinct(), by = "plot_name") %>%
  mutate(
    stratum_category = replace_na(stratum_category, "unmapped"),
    plot_type = tolower(trimws(plot_type))
  ) %>%
  group_by(plot_name) %>%
  mutate(
    plot_has_any_resampling = any(
      !is.na(resampling_30x30) | !is.na(resampling_3x3), na.rm = TRUE
    )
  ) %>%
  mutate(across(starts_with("issue_"), ~replace_na(., FALSE))) %>%
  ungroup() %>%
  left_join(
    tree_counts_table %>% 
      group_by(plot_name, timeframe) %>%
      summarise(
        has_planted = any(tree_type == "planted", na.rm = TRUE),
        total_planted_in_group = sum(suppressWarnings(as.numeric(tree_count))[grepl("planted", origin_table, ignore.case=TRUE)], na.rm=TRUE),
        planted_tree_tables = paste(unique(origin_table[which(tree_type == "planted" & !is.na(tree_type))]), collapse = " and "),
        has_tiny = any(size_class == "<1cm", na.rm = TRUE),
        has_little_in_3x3 = any(size_class == "1 - 9.9cm" & grepl("Nested_3x3", origin_table, ignore.case=TRUE), na.rm = TRUE),
        has_little_in_census = any(size_class == "1 - 9.9cm" & grepl("Census", origin_table, ignore.case=TRUE), na.rm = TRUE),
        has_large = any(size_class == ">10cm" & grepl("Normal_30x30", origin_table, ignore.case=TRUE), na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("plot_name", "timeframe")
  ) %>%
  mutate(across(starts_with("issue_"), ~replace_na(., FALSE))) %>%
  replace_na(list(
    has_planted=FALSE, has_tiny=FALSE, has_little_in_3x3=FALSE, 
    has_large=FALSE, has_little_in_census=FALSE
  ))

logic_failures <- qa_logic_base %>%
  mutate(
    issue_Large_Present_NA = timeframe == "y0" & (is.na(resampling_30x30) | resampling_30x30 < 2) & !is.na(large_trees_present),
    issue_Large_Present_Missing = timeframe == "y0" & (!is.na(resampling_30x30) & resampling_30x30 == 2) & !(large_trees_present %in% c("yes", "no")),
    issue_Large_Trees_When_NA = timeframe == "y0" & is.na(resampling_30x30) & has_large == TRUE,
    issue_Missing_Large_Under_2 = timeframe == "y0" & (!is.na(resampling_30x30) & resampling_30x30 < 2) & has_large == FALSE,
    issue_Missing_Large_When_Yes = timeframe == "y0" & (!is.na(resampling_30x30) & resampling_30x30 == 2 & large_trees_present == "yes") & has_large == FALSE,
    issue_Unexpected_Large_When_No = timeframe == "y0" & (!is.na(resampling_30x30) & resampling_30x30 == 2 & large_trees_present == "no") & has_large == TRUE,
    
    issue_Little_Present_NA = timeframe == "y0" & (is.na(resampling_3x3) | resampling_3x3 < 2) & !is.na(little_trees_present),
    issue_Little_Present_Missing = timeframe == "y0" & (!is.na(resampling_3x3) & resampling_3x3 == 2) & !(little_trees_present %in% c("yes", "no")),
    issue_Little_Trees_When_NA = timeframe == "y0" & is.na(resampling_3x3) & has_little_in_3x3 == TRUE,
    issue_Missing_Little_Under_2 = timeframe == "y0" & (!is.na(resampling_3x3) & resampling_3x3 < 2) & has_little_in_3x3 == FALSE,
    issue_Missing_Little_When_Yes = timeframe == "y0" & (!is.na(resampling_3x3) & resampling_3x3 == 2 & little_trees_present == "yes") & has_little_in_3x3 == FALSE,
    issue_Unexpected_Little_When_No = timeframe == "y0" & (!is.na(resampling_3x3) & resampling_3x3 == 2 & little_trees_present == "no") & has_little_in_3x3 == TRUE,
    
    issue_Missing_Census = (has_little_in_census == FALSE) & !((!is.na(resampling_3x3) & resampling_3x3 < 2) | (!is.na(resampling_3x3) & resampling_3x3 == 2 & little_trees_present == "yes")),
    
    issue_Invalid_Census = (has_little_in_census == TRUE) & !(!is.na(resampling_3x3) & resampling_3x3 == 2 & little_trees_present == "no"),
    
    pattern_is_no_planting = !is.na(planting_pattern) & grepl("(no|not).*plant", planting_pattern, ignore.case=TRUE),
    has_actual_pattern = !is.na(planting_pattern) & !pattern_is_no_planting,
    
    issue_Pattern_Format_Invalid = has_actual_pattern & trimws(planting_pattern) != "" & !grepl("x|by|m|ft|feet|meter|metre|pour|para|\\d", planting_pattern, ignore.case=TRUE),
    
    # --- Consolidated Intervention Coherence ---
    class_type = case_when(
      plot_type == "control" ~ "control",
      plot_type %in% c("restoration", "planting", "anr", "planting_anr") ~ "restoration",
      TRUE ~ "unknown"
    ),
    class_strat = case_when(
      stratum_category == "control" ~ "control",
      stratum_category %in% c("planting", "anr", "planting_anr") ~ "restoration",
      TRUE ~ "unknown"
    ),
    is_anr_plot = (!is.na(plot_type) & plot_type == "anr") | (!is.na(stratum_category) & stratum_category == "anr") | (anr_indicator == "yes"),
    class_patt = case_when(
      has_actual_pattern ~ "restoration",
      (is.na(planting_pattern) | trimws(planting_pattern) == "" | pattern_is_no_planting) & is_anr_plot ~ "restoration",
      (is.na(planting_pattern) | trimws(planting_pattern) == "" | pattern_is_no_planting) & !is_anr_plot ~ "control",
      TRUE ~ "unknown"
    ),
    class_ind = case_when(
      planting_indicator == "yes" ~ "restoration",
      planting_indicator == "no" & !is_anr_plot ~ "control",
      TRUE ~ "unknown"
    ),
    class_group = case_when(
      has_actual_pattern & planting_group == "no" ~ "control",
      planting_group == "yes" ~ "restoration",
      TRUE ~ "unknown"
    ),
    restoration_count = (class_type == "restoration") + (class_strat == "restoration") + (class_patt == "restoration") + (class_ind == "restoration") + (class_group == "restoration"),
    control_count = (class_type == "control") + (class_strat == "control") + (class_patt == "control") + (class_ind == "control") + (class_group == "control"),
    
    issue_Intervention_Mismatch = (restoration_count > 0 & control_count > 0),
    
    pattern_area = purrr::map_dbl(stringr::str_extract_all(planting_pattern, "\\d+(\\.\\d+)?"), function(x) {
      nums = as.numeric(x)
      if(length(nums) == 0) return(NA_real_)
      if(length(nums) == 1) return(nums[1]^2)
      return(nums[1] * nums[2])
    }),
    expected_trees = if_else(class_type == "restoration" & has_actual_pattern & planting_group == "yes" & !is.na(pattern_area) & pattern_area > 0, 900 / pattern_area, NA_real_),
    issue_Tree_Count_Mismatch = !is.na(expected_trees) & (total_planted_in_group < (expected_trees / 2) | total_planted_in_group > (expected_trees * 2)),
    
    # Unmapped Stratum
    issue_Unmapped_Stratum     = !is.na(stratum) & stratum_category == "unmapped",
    
    issue_y0_Missing_Resampling = (timeframe == "y0") & tolower(plot_type) != "control" & (is.na(resampling_30x30_orig) | is.na(resampling_3x3_orig)),
    issue_y0_Extra_Resampling = (timeframe != "y0") & tolower(plot_type) != "control" & (!is.na(resampling_30x30_orig) | !is.na(resampling_3x3_orig))
  ) %>%
  mutate(across(starts_with("issue_"), ~replace_na(., FALSE))) %>%
  filter(
    issue_Large_Present_NA | issue_Large_Present_Missing | issue_Large_Trees_When_NA | 
    issue_Missing_Large_Under_2 | issue_Missing_Large_When_Yes | issue_Unexpected_Large_When_No |
    issue_Little_Present_NA | issue_Little_Present_Missing | issue_Little_Trees_When_NA | 
    issue_Missing_Little_Under_2 | issue_Missing_Little_When_Yes | issue_Unexpected_Little_When_No |
    issue_Missing_Census | issue_Invalid_Census | 
    issue_Pattern_Format_Invalid | issue_Intervention_Mismatch | issue_Unmapped_Stratum |
    issue_y0_Missing_Resampling | issue_y0_Extra_Resampling
  )

if (nrow(logic_failures) > 0) {
  qa_issues_list[[2]] <- logic_failures %>%
    rowwise() %>%
    mutate(
      issue_text = paste(c(
        if(issue_Large_Present_NA && issue_Missing_Large_Under_2 && large_trees_present == "no") sprintf("resampling_30x30 is NA or <2, but NO LARGE TREES found (large_trees_present is 'no')") else if(issue_Large_Present_NA) sprintf("resampling_30x30 is NA or <2, but large_trees_present is '%s'", large_trees_present) else NULL,
        if(issue_Large_Present_Missing) sprintf("resampling_30x30 is 2, but large_trees_present is '%s' (expected 'yes'/'no')", large_trees_present) else NULL,
        if(issue_Large_Trees_When_NA) "resampling_30x30 is NA, but large trees found" else NULL,
        if(issue_Missing_Large_Under_2 && issue_Missing_Large_When_Yes) "resampling_30x30 is <2 and large_trees_present is 'yes', but NO LARGE TREES found" else if(issue_Missing_Large_Under_2 && !(issue_Large_Present_NA && large_trees_present == "no")) "resampling_30x30 is <2, but NO LARGE TREES found" else if(issue_Missing_Large_When_Yes) "large_trees_present is 'yes', but NO LARGE TREES found" else NULL,
        if(issue_Unexpected_Large_When_No) "large_trees_present is 'no', but large trees WERE found" else NULL,
        
        if(issue_Little_Present_NA && issue_Missing_Little_Under_2 && little_trees_present == "no") sprintf("resampling_3x3 is NA or <2, but NO LITTLE TREES found (little_trees_present is 'no')") else if(issue_Little_Present_NA) sprintf("resampling_3x3 is NA or <2, but little_trees_present is '%s'", little_trees_present) else NULL,
        if(issue_Little_Present_Missing) sprintf("resampling_3x3 is 2, but little_trees_present is '%s' (expected 'yes'/'no')", little_trees_present) else NULL,
        if(issue_Little_Trees_When_NA) "resampling_3x3 is NA, but little trees found" else NULL,
        if(issue_Missing_Little_Under_2 && issue_Missing_Little_When_Yes) "resampling_3x3 is <2 and little_trees_present is 'yes', but NO LITTLE TREES found" else if(issue_Missing_Little_Under_2 && !(issue_Little_Present_NA && little_trees_present == "no")) "resampling_3x3 is <2, but NO LITTLE TREES found" else if(issue_Missing_Little_When_Yes) "little_trees_present is 'yes', but NO LITTLE TREES found" else NULL,
        if(issue_Unexpected_Little_When_No) "little_trees_present is 'no', but little trees WERE found" else NULL,
        if(issue_Missing_Census) sprintf("resampling_3x3 is '%s' and little_trees_present is '%s', so census trees SHOULD exist", resampling_3x3, little_trees_present) else NULL,
        if(issue_Invalid_Census) sprintf("Census trees found, but resampling_3x3 is '%s' and little_trees_present is '%s' (expected 2 and 'no')", resampling_3x3, little_trees_present) else NULL,
        if(issue_Pattern_Format_Invalid) sprintf("Plot type is '%s' and planting pattern '%s' appears to have an invalid format", plot_type, planting_pattern) else NULL,
        
        if(issue_Intervention_Mismatch) paste0(
            paste(c(
              if(class_type == "restoration") sprintf("plot_type is '%s'", plot_type) else NULL,
              if(class_strat == "restoration") sprintf("stratum is '%s'", stratum) else NULL,
              if(class_patt == "restoration") sprintf("planting pattern is '%s'", replace_na(planting_pattern, "missing")) else NULL,
              if(class_ind == "restoration") "ONE OR MORE PLANTED TREES were recorded (planting indicator is 'yes')" else NULL,
              if(class_group == "restoration") "TREES found in planted group (planting group = 'yes')" else NULL
            ), collapse = " and "),
            ", BUT ",
            paste(c(
              if(class_type == "control") sprintf("plot_type is '%s'", plot_type) else NULL,
              if(class_strat == "control") sprintf("stratum is '%s'", stratum) else NULL,
              if(class_patt == "control") sprintf("planting pattern is '%s'", replace_na(planting_pattern, "missing")) else NULL,
              if(class_ind == "control") "NO PLANTED TREES recorded (planting indicator is 'no')" else NULL,
              if(class_group == "control") paste0("NO TREE is found in planted group (planting_group = 'no')", if(class_ind == "restoration" && !is.na(planted_tree_tables) && planted_tree_tables != "") sprintf(" (planted trees are found in %s)", planted_tree_tables) else "") else NULL
            ), collapse = " and ")
          ) else NULL,
        
        if(issue_Tree_Count_Mismatch) sprintf("With planting pattern '%s' a total of %s planted trees should be observed, but only %s are actually entered", planting_pattern, round(expected_trees), total_planted_in_group) else NULL,
        
        if(issue_Unmapped_Stratum) sprintf("Stratum '%s' does not seem a valid stratum", stratum) else NULL,
        if(issue_y0_Missing_Resampling) sprintf("Timeframe is 'y0', but resampling_30x30 is '%s' and resampling_3x3 is '%s'", resampling_30x30_orig, resampling_3x3_orig) else NULL,
        if(issue_y0_Extra_Resampling) sprintf("Timeframe is '%s', but resampling_30x30 is '%s' and resampling_3x3 is '%s' (should be NA)", timeframe, resampling_30x30_orig, resampling_3x3_orig) else NULL
      ), collapse = "\n• ")
    ) %>%
  mutate(across(starts_with("issue_"), ~replace_na(., FALSE))) %>%
    ungroup() %>%
    select(-any_of("site_id")) %>%
    left_join(survey_metadata %>% select(plot_name, timeframe, site_id, org_name, country_name) %>% distinct(plot_name, timeframe, .keep_all = TRUE), by = c("plot_name", "timeframe")) %>%
    mutate(
      issue = paste0("• ", issue_text),
      correct_value = NA_character_,
      resolved = NA_character_,
      note = NA_character_
    ) %>%
    arrange(plot_name) %>%
    mutate(across(starts_with("issue_"), ~replace_na(., FALSE))) %>%
    select(-issue_text)
}


#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.4 Multiple resamplings

message("--> [4/5] Checking multiple resamplings...")
multiple_resamplings <- surveys_table %>%
  filter((!is.na(resampling_30x30) & trimws(resampling_30x30) != "") | 
         (!is.na(resampling_3x3) & trimws(resampling_3x3) != "")) %>%
  group_by(plot_name) %>%
  filter(n_distinct(timeframe) > 1) %>%
  summarise(
    timeframes_resampled = paste(unique(timeframe), collapse = ", "),
    .groups = "drop"
  )

if (nrow(multiple_resamplings) > 0) {
  qa_issues_list[[3]] <- multiple_resamplings %>%
    left_join(survey_metadata %>% select(plot_name, site_id, org_name, country_name) %>% distinct(plot_name, .keep_all = TRUE), by = "plot_name") %>%
    mutate(
      timeframe = NA_character_,
      uuid = NA_character_,
      org_name = coalesce(org_name, target_org),
      correct_value = NA_character_,
      resolved = NA_character_,
      issue = sprintf("• Multiple Resamplings Warning for plot '%s'. It was resampled in multiple timeframes: %s", plot_name, timeframes_resampled),
      note = NA_character_
    ) %>%
    select(-timeframes_resampled)
}

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.5 Tree counts validity

message("--> [5/5] Checking tree counts validity...")

invalid_tree_counts <- tree_counts_table %>%
  select(-any_of("site_id")) %>%
  left_join(survey_metadata %>% select(plot_name, timeframe, site_id, org_name, country_name) %>% distinct(), by = c("plot_name", "timeframe")) %>%
  mutate(
    numeric_count = suppressWarnings(as.numeric(tree_count)),
    is_slow_growth = grepl("Australia|Canada|USA|United States|France|Spain|Portugal|Scotland|UK|United Kingdom|UAE|United Arab Emirates|High Andes|Andina|Madagascar|China", paste(country_name, org_name, sep=" "), ignore.case=TRUE)
  ) %>%
  mutate(across(starts_with("issue_"), ~replace_na(., FALSE))) %>%
  filter(
    is.na(numeric_count) | numeric_count <= 0 |
    is.na(species) | trimws(species) == "" |
    is.na(size_class) | trimws(size_class) == "" |
    is.na(tree_type) | trimws(tree_type) == "" |
    (timeframe == "y0" & tree_type == "planted" & (size_class == ">10cm" | grepl("Normal_", origin_table, ignore.case=TRUE))) |
    (timeframe == "y0" & tree_type == "planted" & is_slow_growth & (size_class == "1 - 9.9cm" | grepl("Nested_3x3", origin_table, ignore.case=TRUE)))
  )

if (nrow(invalid_tree_counts) > 0) {
  tree_issues_log <- invalid_tree_counts %>%
    mutate(
      tmp_issue_1 = if_else(is.na(numeric_count) | numeric_count <= 0, sprintf("Invalid tree count (tree count value: %s)", tree_count), NA_character_),
      tmp_issue_2 = if_else(is.na(species) | trimws(species) == "", "Missing or empty species", NA_character_),
      tmp_issue_3 = if_else(is.na(size_class) | trimws(size_class) == "", "Missing or empty size_class", NA_character_),
      tmp_issue_4 = if_else(is.na(tree_type) | trimws(tree_type) == "", "Missing or empty tree_type", NA_character_),
      tmp_issue_5 = if_else((timeframe == "y0" & tree_type == "planted" & (size_class == ">10cm" | grepl("Normal_", origin_table, ignore.case=TRUE))), "One or more planted trees with DBH >10cm is reported at Y0", NA_character_),
      tmp_issue_6 = if_else((timeframe == "y0" & tree_type == "planted" & is_slow_growth & (size_class == "1 - 9.9cm" | grepl("Nested_3x3", origin_table, ignore.case=TRUE))), "One or more planted trees with DBH >1cm is reported at Y0 in slow growth region", NA_character_)
    ) %>%
    tidyr::unite(col = "issue", starts_with("tmp_issue_"), sep = "\n• ", na.rm = TRUE) %>%
    mutate(
      issue = paste0("• ", issue),
      issue = if_else(issue == "• ", "• Invalid tree record", issue),
      org_name = coalesce(org_name, target_org),
      correct_values = NA_character_,
      resolved = NA_character_,
      note_justification = NA_character_,
      answer = NA_character_
    ) %>%
    
    mutate(site_name = coalesce(as.character(site_id), as.character(site_name))) %>% select(plot_name, site_name, org_name, country_name, timeframe, uuid, species, origin_table, tree_type, size_class, tree_count, issue, correct_values, resolved, note_justification, answer)
} else {
  tree_issues_log <- NULL
}
#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.5b Site attributes validity

message("--> [6/6] Checking site attributes validity...")
if(!"in_terramatch" %in% names(sites_table)) sites_table$in_terramatch <- TRUE
invalid_sites <- sites_table %>%
  filter(
    is.na(site_type) | trimws(toupper(site_type)) %in% c("", "NA") |
    ( (is.na(site_size_restoration) | trimws(toupper(site_size_restoration)) %in% c("", "NA")) & 
      (is.na(site_size_control) | trimws(toupper(site_size_control)) %in% c("", "NA")) ) |
    !in_terramatch
  )

if (nrow(invalid_sites) > 0) {
  qa_issues_list[[6]] <- invalid_sites %>%
    left_join(org_table, by = "org_id") %>%
    left_join(countries_table, by = "country_id") %>%
    mutate(
      site_name = as.character(site_name),
      issue = paste0("• ", case_when(
        !in_terramatch ~ sprintf("Site ID '%s' is not found in IMP/TerraMatch database", site_id),
        is.na(site_type) | trimws(toupper(site_type)) %in% c("", "NA") ~ sprintf("Site ID '%s': site_type is missing", site_id),
        (is.na(site_size_restoration) | trimws(toupper(site_size_restoration)) %in% c("", "NA")) & (is.na(site_size_control) | trimws(toupper(site_size_control)) %in% c("", "NA")) ~ sprintf("Site ID '%s': Both site_size_restoration and site_size_control are missing", site_id),
        TRUE ~ "Invalid site attributes"
      )),
      plot_name = NA_character_,
      timeframe = NA_character_,
      uuid = NA_character_,
      correct_value = NA_character_,
      resolved = NA_character_,
      note = NA_character_
    )
}

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.5c Site attributes incongruency

message("--> [7/7] Checking site attributes incongruency across plots...")

if ("site_size_restoration" %in% names(plot_candidates)) {
  incongruent_sites <- plot_candidates %>%
    group_by(site_id) %>%
    summarise(
      n_restoration = n_distinct(site_size_restoration[!is.na(site_size_restoration)]),
      n_control = n_distinct(site_size_control[!is.na(site_size_control)])
    ) %>%
    filter(n_restoration > 1 | n_control > 1) %>%
    left_join(sites_table, by = "site_id") %>%
    filter(!is.na(site_id))

  if (nrow(incongruent_sites) > 0) {
    qa_issues_list[[7]] <- incongruent_sites %>%
      left_join(org_table, by = "org_id") %>%
      left_join(countries_table, by = "country_id") %>%
      mutate(
        site_name = as.character(site_name),
        tmp_issue_1 = if_else(n_restoration > 1, sprintf("Site ID '%s' has multiple values for site size (restoration)", site_id), NA_character_),
        tmp_issue_2 = if_else(n_control > 1, sprintf("Site ID '%s' has multiple values for site size (control)", site_id), NA_character_)
      ) %>%
      tidyr::unite(col = "issue", starts_with("tmp_issue_"), sep = "\n• ", na.rm = TRUE) %>%
      mutate(
        issue = paste0("• ", issue),
        plot_name = NA_character_,
        timeframe = NA_character_,
        uuid = NA_character_,
        correct_value = NA_character_,
        resolved = NA_character_,
        note = NA_character_
      )
  }
}

# Reorder qa_issues_list to prioritize site-level issues (indices 7 and 6)
qa_issues_list <- qa_issues_list[c(7, 6, 1, 2, 3, 4, 5)]

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2.6 Export Unified QA Log

if (length(qa_issues_list) > 0 || !is.null(tree_issues_log)) {
  log_filename <- paste0(
    target_org_code, "_", 
    paste(target_time, collapse = "_"),
    format(Sys.Date(), "_%Y_%m_%d"), ".xlsx"
  )
  log_path <- file.path(qa_log_dir, log_filename)
  wb <- createWorkbook()
  modifyBaseFont(wb, fontSize = 18)
  wrap_style <- createStyle(wrapText = TRUE)

  if (length(qa_issues_list) > 0) {
    std_cols <- c("plot_name", "site_id", "org_name", "country_name", "timeframe", "uuid", "issue", "correct_value", "resolved", "note")
    
    final_qa_log <- bind_rows(lapply(qa_issues_list[!sapply(qa_issues_list, is.null)], function(df) df %>% mutate(across(any_of(c("site_name", "site_id")), as.character)))) %>%
      bind_cols(
        as.data.frame(
          matrix(NA_character_, nrow = 1, ncol = length(setdiff(std_cols, names(.))), 
                 dimnames = list(NULL, setdiff(std_cols, names(.))))
        )[rep(1, max(1, nrow(.))), , drop = FALSE]
      ) %>%
      mutate(across(starts_with("issue_"), ~replace_na(., FALSE))) %>%
      select(all_of(std_cols)) %>%
      mutate(
        note = if_else(
          plot_name %in% duplicate_surveys$plot_name & !grepl("Duplicate form submission detected", issue, ignore.case = TRUE),
          case_when(
            is.na(note) | trimws(note) == "" ~ "Duplicate plot",
            TRUE ~ paste0(note, " | Duplicate plot")
          ),
          note
        )
      ) %>%
      rename(note_justification = note) %>%
      mutate(answer = NA_character_)
      
    addWorksheet(wb, "QA Issues - Plots and Sites")
    writeData(wb, "QA Issues - Plots and Sites", final_qa_log)
    addStyle(wb, "QA Issues - Plots and Sites", style = wrap_style, rows = 1:(nrow(final_qa_log)+1), cols = which(names(final_qa_log) %in% c("issue", "note_justification", "answer")), gridExpand = TRUE)
    setColWidths(wb, "QA Issues - Plots and Sites", cols = which(names(final_qa_log) == "issue"), widths = 60)
    setColWidths(wb, "QA Issues - Plots and Sites", cols = which(names(final_qa_log) == "correct_value"), widths = 35)
    setColWidths(wb, "QA Issues - Plots and Sites", cols = which(names(final_qa_log) == "note_justification"), widths = 40)
    setColWidths(wb, "QA Issues - Plots and Sites", cols = which(names(final_qa_log) == "answer"), widths = 40)
    setColWidths(wb, "QA Issues - Plots and Sites", cols = which(names(final_qa_log) == "plot_name"), widths = 20)
    setColWidths(wb, "QA Issues - Plots and Sites", cols = which(names(final_qa_log) %in% c("site_id", "org_name", "country_name", "timeframe", "uuid")), widths = 5)
  }

  if (!is.null(tree_issues_log)) {
    addWorksheet(wb, "QA Issues - Trees")
    writeData(wb, "QA Issues - Trees", tree_issues_log)
    addStyle(wb, "QA Issues - Trees", style = wrap_style, rows = 1:(nrow(tree_issues_log)+1), cols = which(names(tree_issues_log) %in% c("issue", "note_justification", "answer")), gridExpand = TRUE)
    setColWidths(wb, "QA Issues - Trees", cols = which(names(tree_issues_log) == "issue"), widths = 60)
    setColWidths(wb, "QA Issues - Trees", cols = which(names(tree_issues_log) == "correct_value"), widths = 35)
    setColWidths(wb, "QA Issues - Trees", cols = which(names(tree_issues_log) == "note_justification"), widths = 40)
    setColWidths(wb, "QA Issues - Trees", cols = which(names(tree_issues_log) == "answer"), widths = 40)
    setColWidths(wb, "QA Issues - Trees", cols = which(names(tree_issues_log) == "plot_name"), widths = 20)
    setColWidths(wb, "QA Issues - Trees", cols = which(names(tree_issues_log) == "species"), widths = 20)
    setColWidths(wb, "QA Issues - Trees", cols = which(names(tree_issues_log) %in% c("origin_table", "tree_type", "size_class")), widths = 15)
    setColWidths(wb, "QA Issues - Trees", cols = which(names(tree_issues_log) %in% c("site_id", "org_name", "country_name", "timeframe", "uuid")), widths = 5)
  }

  saveWorkbook(wb, log_path, overwrite = TRUE)
  
  fatal_issues <- c(length(unmapped_species) > 0, nrow(logic_failures) > 0)
  
  if (any(fatal_issues)) {
    stop(sprintf("--> !!! QA FAILED. Unified error log generated at: %s", log_path))
  } else {
    message(sprintf("--> !!! QA WARNING. Warnings log generated at: %s", log_path))
  }
} else {
  message("    [OK] All QA checks passed cleanly!")
}

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# END