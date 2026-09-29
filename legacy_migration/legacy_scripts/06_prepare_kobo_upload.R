# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Description: This script converts the cleaned SQL-ready tables into the 
# nested JSON format required by the KoboToolbox API for the new form schema.
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

library(tidyverse)
library(jsonlite)
library(readxl)
library(here)
source(here("scripts/utils/config.R"))

# 1. Configuration
# Target the most recently generated clean data (e.g. PHI)
# In production, this should be parameterized
input_dir <- file.path(dir_legacy, "legacy_tables", "PHI_y0_20260914") # Example folder
output_dir <- file.path(dir_legacy, "legacy_upload")
if(!dir.exists(output_dir)) dir.create(output_dir)

new_form_path <- here("kobo_management/forms/aGg4YcYHYyqKLiBfWPkjnA_260907.xlsx")

cat("Reading cleaned SQL tables from:", input_dir, "\n")
surveys <- read_csv(file.path(input_dir, "surveys_table.csv"), show_col_types = FALSE)
trees <- read_csv(file.path(input_dir, "tree_counts.csv"), show_col_types = FALSE)

# 2. Map Parent Data (Surveys)
# We map the SQL table columns directly to the new form's exact 'name' attributes
# Note: Kobo API requires the exact keys defined in the XLSForm
mapped_surveys <- surveys %>%
  mutate(
    # The new form uses calculate fields and specific group names. 
    # Example minimal mapping based on the new schema:
    timeframe_update = timeframe, # Or appropriately mapped
    date = as.character(date),
    start_time = as.character(start_time),
    end_time = as.character(end_time),
    # Map the dropdown keys appropriately (assuming plot_key is available or joinable)
    plot_id = plot_name # Needs to match plot_dropdown.csv strictly
  ) %>%
  # Select only the columns that actually exist in the Kobo form
  select(
    plot_id, timeframe_update, date, start_time, end_time, uuid
  )

# 3. Process and Nest Tree Repeating Groups
# The new form splits trees into 5 distinct repeat groups.
# We must split the tree_tree_counts_table based on DBH rules and nest them.

# 3a. >10cm DBH
trees_10cm <- trees %>%
  filter(size_class == ">10cm DBH") %>%
  select(
    uuid,
    tree_species_10cm_dbh = species,
    number_tree_10cm_dbh = tree_count,
    tree_type_10cm_dbh = tree_type
  ) %>%
  nest(trees_10cm_dbh_repeat = -uuid)

# 3b. 1-9.9cm DBH
trees_1_9 <- trees %>%
  filter(size_class == "1-9.9cm DBH") %>%
  select(
    uuid,
    tree_species_1_9_9cm_dbh = species,
    number_tree_1_9_9cm_dbh = tree_count,
    tree_type_1_9_9cm_dbh = tree_type
  ) %>%
  nest(trees_1_9_9cm_dbh_repeat = -uuid)

# 3c. Planted Trees (Regardless of DBH, based on tree_type = planted)
# Assuming 'planted' is a type in your clean data
trees_planted <- trees %>%
  filter(tolower(tree_type) == "planted") %>%
  select(
    uuid,
    tree_species_planted = species,
    number_tree_planted = tree_count
  ) %>%
  nest(trees_planted_repeat = -uuid)

# 4. Join Nested Groups to Parent Surveys
# Join all the nested list-columns back to the main survey data using the UUID
final_payload <- mapped_surveys %>%
  left_join(trees_10cm, by = "uuid") %>%
  left_join(trees_1_9, by = "uuid") %>%
  left_join(trees_planted, by = "uuid") %>%
  # Drop UUID from the main body (it will be passed in the meta tag by build_payload.R)
  select(-uuid)

# 5. Export to JSON
out_file <- file.path(output_dir, "legacy_upload_payload.json")
json_string <- toJSON(final_payload, pretty = TRUE, auto_unbox = TRUE, na = "null")
writeLines(json_string, out_file)

cat("Successfully generated Kobo JSON payload at:", out_file, "\n")
