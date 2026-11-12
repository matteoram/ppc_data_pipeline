# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Matteo Ramina (raminamatteo@me.com)
# Date: 2026-11-13
# Version: 2.0
#
# Description: This script prepares the migrated legacy monitoring data for
# upload to the new KoBoToolbox project ("NEW Tree Monitoring Form" or test
# asset "Test 1"). It:
#   1. Queries cleaned, QA-verified entities from `database/ppc_database.sqlite`
#      (surveys, plots, sites, organizations, countries, tree_counts, species).
#   2. Enriches each survey with non-SQL fields from the raw legacy extractions
#      (`plot_data_2026_11_09.csv`, `Geo_Data_2026_11_09.csv`, and
#      `Photo_Data_2026_11_09.csv`) linked via the legacy `_uuid`:
#        - Group-level notes and 3x3 subplot descriptions
#        - 30x30 corners, 10x10 corners, and 3x3 subplot centroids
#        - Photo and file attachments
#   3. Optionally downloads all referenced legacy photos/files into a local
#      cache (`legacy_migration/legacy_data/media_cache/`) so they can be
#      attached in `07_upload_to_kobo.R`.
#   4. Structures only the analysis-relevant fields (matching the schema of
#      `scripts/02_post_collection/01_extract_data.R`) into an `.rds` and `.json`
#      payload bundle in `legacy_migration/legacy_upload/`.
#
# Inputs:
#   - `scripts/utils/config.R`
#   - `legacy_migration/legacy_scripts/00_legacy_migration_params.R`
#   - `database/ppc_database.sqlite`
#   - `data/raw/2026_11_09/plot_data_2026_11_09.csv`
#   - `data/raw/2026_11_09/Geo_Data_2026_11_09.csv`
#   - `data/raw/2026_11_09/Photo_Data_2026_11_09.csv`
#
# Outputs:
#   - `legacy_migration/legacy_upload/kobo_upload_payloads_<tag>.rds`
#   - `legacy_migration/legacy_upload/kobo_upload_payloads_<tag>.json`
#   - `legacy_migration/legacy_upload/kobo_upload_payloads_latest.rds`
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 0. Load packages and configuration

source(here::here("scripts/utils/config.R"))
source(here::here("legacy_migration/legacy_scripts/00_legacy_migration_params.R"))

library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(jsonlite)
library(httr)
library(here)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 1. Configuration

# Set to TRUE to prepare only `target_org_code` and `target_time` from
# `00_legacy_migration_params.R`. Set to FALSE to prepare ALL migrated
# monitoring surveys currently in `ppc_database.sqlite`.
filter_by_params <- TRUE

# Set to TRUE to download all matched legacy photos/files from KoBo's API v2
# into the local media cache directory before uploading.
download_media <- TRUE

# Folder under `data/raw/` holding the raw legacy CSV extractions
raw_legacy_folder <- "2026_11_09"
dir_raw_legacy    <- file.path(dir_raw_data, raw_legacy_folder)

path_raw_plot  <- file.path(dir_raw_legacy, file_kobo_plot)
path_raw_geo   <- file.path(
  dir_raw_legacy,
  if (exists("file_kobo_geo")) file_kobo_geo else "Geo_Data_2026_11_09.csv"
)
path_raw_photo <- file.path(
  dir_raw_legacy,
  if (exists("file_kobo_photo")) file_kobo_photo else "Photo_Data_2026_11_09.csv"
)

# Output and media cache directories
output_dir      <- file.path(dir_legacy, "legacy_upload")
media_cache_dir <- file.path(dir_legacy_data, "media_cache")

if (!dir.exists(output_dir))      dir.create(output_dir, recursive = TRUE)
if (!dir.exists(media_cache_dir)) dir.create(media_cache_dir, recursive = TRUE)

# Verify API token if media download is enabled
KOBO_API_TOKEN <- Sys.getenv("KOBO_API_TOKEN")
if (download_media && !nzchar(KOBO_API_TOKEN)) {
  stop(
    "KOBO_API_TOKEN not found in environment. ",
    "Please set it in .Renviron or set download_media <- FALSE.",
    call. = FALSE
  )
}

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2. Query cleaned relational data from SQLite

db_path <- file.path(dir_database, "ppc_database.sqlite")
if (!file.exists(db_path)) {
  stop("SQLite database not found at: ", db_path, call. = FALSE)
}

message("--> Connecting to SQLite database: ", db_path)
con <- dbConnect(RSQLite::SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)

# Query all monitoring surveys (excluding 'APP' baseline application entries)
sql_surveys <- "
  SELECT
    s.survey_key,
    s.plot_key,
    p.plot_name,
    p.plot_type,
    p.plot_permanence,
    st.site_key,
    st.site_id,
    st.site_name,
    st.site_type,
    st.site_size_restoration,
    st.site_size_control,
    o.org_key,
    o.org_id,
    o.org_name,
    c.country_id,
    c.country_name,
    s.timeframe,
    s.date,
    s.start_time,
    s.end_time,
    s.stratum,
    s.planting_pattern,
    s.planting_indicator,
    s.anr_indicator,
    s.resampling_30x30,
    s.large_trees_present,
    s.resampling_3x3,
    s.little_trees_present,
    s.tiny_trees_present,
    s.uuid AS legacy_uuid
  FROM surveys s
  JOIN plots p          ON s.plot_key    = p.plot_key
  JOIN sites st         ON p.site_key    = st.site_key
  JOIN organizations o  ON st.org_key    = o.org_key
  JOIN countries c      ON o.country_key = c.country_key
  WHERE s.timeframe != 'APP'
  ORDER BY o.org_id, st.site_id, p.plot_key, s.date
"

surveys_df <- dbGetQuery(con, sql_surveys) %>%
  as_tibble()

if (filter_by_params) {
  message(sprintf(
    "--> Filtering SQLite surveys to org_id = '%s' and timeframe in (%s)...",
    target_org_code, paste(target_time, collapse = ", ")
  ))
  surveys_df <- surveys_df %>%
    filter(
      org_id %in% target_org_code,
      timeframe %in% target_time
    )
}

if (nrow(surveys_df) == 0) {
  stop("No matching surveys found in ppc_database.sqlite for the selected filter.", call. = FALSE)
}

message(sprintf("--> Loaded %d survey(s) from SQLite.", nrow(surveys_df)))

# Query tree_counts joined with species for the selected surveys
sql_trees <- "
  SELECT
    tc.observation_key,
    tc.survey_key,
    tc.species_key,
    sp.scientific_name,
    tc.tree_species_other,
    tc.tree_type,
    tc.size_class,
    tc.tree_count
  FROM tree_counts tc
  JOIN species sp ON tc.species_key = sp.species_key
"

trees_df <- dbGetQuery(con, sql_trees) %>%
  as_tibble() %>%
  filter(survey_key %in% surveys_df$survey_key)

message(sprintf(
  "--> Loaded %d tree count record(s) across %d survey(s).",
  nrow(trees_df), n_distinct(trees_df$survey_key)
))

# Compute `little_trees_census_present` for surveys where `little_trees_present == "no"`
census_surveys <- trees_df %>%
  filter(size_class == "1 - 9.9cm") %>%
  distinct(survey_key) %>%
  mutate(has_1_9_trees = TRUE)

surveys_df <- surveys_df %>%
  left_join(census_surveys, by = "survey_key") %>%
  mutate(
    has_1_9_trees = coalesce(has_1_9_trees, FALSE),
    little_trees_census_present = case_when(
      little_trees_present == "no" & has_1_9_trees  ~ "yes",
      little_trees_present == "no" & !has_1_9_trees ~ "no",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-has_1_9_trees)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 3. Enrich with non-SQL fields from raw legacy CSVs (Notes, Geo, Media)

clean_na_str <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  if_else(is.na(x) | x == "" | x == "NA" | x == "null", NA_character_, x)
}

combine_notes <- function(...) {
  vals <- list(...)
  pmap_chr(vals, function(...) {
    v <- c(...)
    v <- v[!is.na(v) & nzchar(trimws(v))]
    v <- unique(trimws(v))
    if (length(v) == 0) NA_character_ else paste(v, collapse = "; ")
  })
}

# Normalize filenames using KoBo/Django's storage convention (spaces -> '_', strip special chars)
normalize_kobo_filename <- function(x) {
  ifelse(
    is.na(x) | !nzchar(x),
    NA_character_,
    gsub("[^A-Za-z0-9._-]", "", gsub(" ", "_", trimws(basename(x))))
  )
}

# 3.1 Load raw legacy plot_data for Notes, Subplot Descriptions, and Photo filenames
message("--> Loading raw legacy plot data for notes and photo filenames: ", path_raw_plot)
raw_plot_extra <- read_csv(
  path_raw_plot,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  mutate(across(everything(), clean_na_str)) %>%
  filter(!is.na(`_uuid`), `_uuid` %in% surveys_df$legacy_uuid) %>%
  distinct(`_uuid`, .keep_all = TRUE) %>%
  transmute(
    legacy_uuid = `_uuid`,
    raw_other_timeframe = OtherTime,
    notes_trees_10cm_dbh_group = combine_notes(Notes21, Notes5),
    notes_trees_1_9_9cm_dbh_group = combine_notes(Notes17, Notes11, Notes14),
    description_location_plot_3x3 = combine_notes(
      Description_of_subpl_ithin_30m_x_30m_plot1,
      Description_of_subpl_ithin_30m_x_30m_plot2
    ),
    # 30x30 corner photos
    ne_photo_plot_30x30 = NE_Photo,
    se_photo_plot_30x30 = SE_Photo,
    sw_photo_plot_30x30 = SW_Photo,
    nw_photo_plot_30x30 = NW_Photo,
    # 10x10 corner photos
    ne_photo_small_site  = NE_Photo_Small_Site,
    se_photo_smallsites  = SE_Photo_SmallSites,
    sw_photo_smallsites  = SW_Photo_SmallSites,
    nw_photo_smallsites  = NW_Photo_SmallSites,
    # 3x3 subplot photo
    photo_plot_3x3 = coalesce(
      Photos, Additional_Photos_Optional1, Additional_Photos_Optional3, Photos4
    ),
    # File attachment
    file = File
  )

# 3.2 Load raw legacy Geo_Data for corner coordinates and 3x3 centroids
message("--> Loading raw legacy geolocation data: ", path_raw_geo)
raw_geo_extra <- read_csv(
  path_raw_geo,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  mutate(across(everything(), clean_na_str)) %>%
  filter(!is.na(`_uuid`), `_uuid` %in% surveys_df$legacy_uuid) %>%
  distinct(`_uuid`, .keep_all = TRUE) %>%
  transmute(
    legacy_uuid = `_uuid`,
    # 30x30 corner geopoints
    northeast_corner_plot_30x30 = Northeast_Corner_of_30m_x_30m_Plot,
    southeast_corner_plot_30x30 = Southeast_Corner_of_30m_x_30m_Plot,
    southwest_corner_plot_30x30 = Southwest_Corner_of_30m_x_30m_Plot,
    northwest_corner_plot_30x30 = Northwest_Corner_of_30m_x_30m_Plot,
    # 10x10 corner geopoints
    northeast_corner_of_10m_x_10m_plot = Northeast_Corner_of_10m_x_10m_Plot,
    southeast_corner_of_10m_x_10m_plot = Southeast_Corner_of_10m_x_10m_Plot,
    southwest_corner_of_10m_x_10m_plot = Southwest_Corner_of_10m_x_10m_Plot,
    northwest_corner_of_10m_x_10m_plot = Northwest_Corner_of_10m_x_10m_Plot,
    # 3x3 centroid geopoint
    centroid_3x3_plot = coalesce(
      Centroid_of_3m_x_3m_Subplot,
      Centroid_of_3m_x_3m_Subplot1,
      Centroid_of_3m_x_3m_Subplot2
    )
  )

# Merge notes, media filenames, and geolocation into surveys_df
surveys_enriched <- surveys_df %>%
  left_join(raw_plot_extra, by = "legacy_uuid") %>%
  left_join(raw_geo_extra, by = "legacy_uuid") %>%
  mutate(
    # Standardize `plot_id` as the integer/character `plot_key` required by the new form
    plot_id = as.character(plot_key),
    site_id = as.character(site_id),
    other_timeframe = if_else(
      !timeframe %in% c("y0", "y2.5", "y5"),
      coalesce(raw_other_timeframe, timeframe),
      NA_character_
    )
  )

# 3.3 Match and optionally download media attachments from Photo_Data_2026_11_09.csv
media_fields <- c(
  "ne_photo_plot_30x30", "se_photo_plot_30x30",
  "sw_photo_plot_30x30", "nw_photo_plot_30x30",
  "ne_photo_small_site", "se_photo_smallsites",
  "sw_photo_smallsites", "nw_photo_smallsites",
  "photo_plot_3x3", "file"
)

message("--> Loading raw legacy photo metadata: ", path_raw_photo)
raw_photo_lookup <- read_csv(
  path_raw_photo,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  mutate(across(everything(), clean_na_str)) %>%
  filter(
    !is.na(`_uuid`),
    `_uuid` %in% surveys_enriched$legacy_uuid,
    !is.na(filename),
    !is.na(download_url)
  ) %>%
  mutate(
    server_basename = basename(filename),
    norm_basename   = normalize_kobo_filename(server_basename)
  ) %>%
  select(legacy_uuid = `_uuid`, server_basename, norm_basename, download_url) %>%
  distinct(legacy_uuid, norm_basename, .keep_all = TRUE)

# Pivot survey media columns to long format and match against raw_photo_lookup
survey_media_long <- surveys_enriched %>%
  select(survey_key, plot_id, legacy_uuid, all_of(media_fields)) %>%
  pivot_longer(
    cols = all_of(media_fields),
    names_to = "new_field_name",
    values_to = "raw_filename",
    values_drop_na = TRUE
  ) %>%
  mutate(norm_basename = normalize_kobo_filename(raw_filename)) %>%
  left_join(raw_photo_lookup, by = c("legacy_uuid", "norm_basename"))

matched_media   <- survey_media_long %>% filter(!is.na(download_url))
unmatched_media <- survey_media_long %>% filter(is.na(download_url))

message(sprintf(
  "--> Matched %d media attachment(s) across %d survey(s) (%d unmatched/deleted on legacy server).",
  nrow(matched_media), n_distinct(matched_media$survey_key), nrow(unmatched_media)
))

if (nrow(matched_media) > 0) {
  # Construct local cache path per survey_key + field so filenames never collide
  matched_media <- matched_media %>%
    mutate(
      # Prefix with field name if the same filename is used in multiple fields of the same survey
      upload_filename = server_basename,
      local_dir       = file.path(media_cache_dir, legacy_uuid, new_field_name),
      local_path      = file.path(local_dir, upload_filename)
    )

  if (download_media) {
    message(sprintf("--> Verifying / downloading %d media file(s) to cache...", nrow(matched_media)))
    for (i in seq_len(nrow(matched_media))) {
      row_m <- matched_media[i, ]
      if (!dir.exists(row_m$local_dir)) {
        dir.create(row_m$local_dir, recursive = TRUE, showWarnings = FALSE)
      }
      # Skip download if file already exists in cache and is non-empty
      if (file.exists(row_m$local_path) && file.info(row_m$local_path)$size > 0) {
        next
      }
      cat(sprintf(
        "    [%d/%d] Downloading %s (plot_id %s, %s)...\n",
        i, nrow(matched_media), row_m$upload_filename, row_m$plot_id, row_m$new_field_name
      ))
      resp <- tryCatch(
        httr::GET(
          url = row_m$download_url,
          httr::add_headers(Authorization = paste("Token", KOBO_API_TOKEN)),
          httr::write_disk(row_m$local_path, overwrite = TRUE)
        ),
        error = function(e) {
          warning(sprintf("Failed to download %s: %s", row_m$download_url, e$message))
          NULL
        }
      )
      if (is.null(resp) || httr::status_code(resp) != 200) {
        warning(sprintf(
          "HTTP %s when downloading %s for plot_id %s",
          if (is.null(resp)) "ERR" else httr::status_code(resp),
          row_m$upload_filename, row_m$plot_id
        ))
        if (file.exists(row_m$local_path)) file.remove(row_m$local_path)
      }
      Sys.sleep(0.2)
    }
    message("--> Media download check complete.")
  }
}

# Build lookup of verified media per survey_key
media_by_survey <- if (nrow(matched_media) > 0) {
  matched_media %>%
    select(survey_key, new_field_name, upload_filename, local_path) %>%
    split(.$survey_key)
} else {
  list()
}

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 4. Format and nest tree repeat groups

trees_formatted <- trees_df %>%
  mutate(
    species_clean = as.character(scientific_name),
    other_clean   = if_else(
      species_clean == "Other or don't know",
      clean_na_str(tree_species_other),
      NA_character_
    ),
    # New form choice list `tree_type` uses Title Case: Planted, Regenerating, Present, Unknown
    type_clean    = str_to_title(trimws(as.character(tree_type))),
    count_clean   = as.integer(round(as.numeric(tree_count)))
  )

# 4.1 >10cm DBH (`trees_10cm_dbh_repeat`)
trees_10cm_map <- trees_formatted %>%
  filter(size_class == ">10cm") %>%
  transmute(
    survey_key,
    tree_species_10cm_dbh       = species_clean,
    tree_species_10cm_dbh_other = other_clean,
    tree_type_10cm_dbh          = type_clean,
    number_tree_10cm_dbh        = count_clean
  ) %>%
  split(.$survey_key) %>%
  map(~ select(.x, -survey_key))

# 4.2 1 - 9.9cm DBH (`trees_1_9_9cm_dbh_repeat`)
trees_1_9_map <- trees_formatted %>%
  filter(size_class == "1 - 9.9cm") %>%
  transmute(
    survey_key,
    tree_species_1_9_9cm_dbh       = species_clean,
    tree_species_1_9_9cm_dbh_other = other_clean,
    tree_type_1_9_9cm_dbh          = type_clean,
    number_tree_1_9_9cm_dbh        = count_clean
  ) %>%
  split(.$survey_key) %>%
  map(~ select(.x, -survey_key))

# 4.3 >1cm DBH in 3x3 small restoration plots (`trees_more_1cm_dbh_repeat`)
trees_more_1cm_map <- trees_formatted %>%
  filter(size_class == ">1cm") %>%
  transmute(
    survey_key,
    tree_species_more_1cm_dbh       = species_clean,
    tree_species_more_1cm_dbh_other = other_clean,
    tree_type_more_1cm_dbh          = type_clean,
    number_tree_more_1cm_dbh        = count_clean
  ) %>%
  split(.$survey_key) %>%
  map(~ select(.x, -survey_key))

# 4.4 <10cm Planted seedlings (`trees_planted_repeat`)
trees_planted_map <- trees_formatted %>%
  filter(size_class == "<10cm planted") %>%
  transmute(
    survey_key,
    tree_species_planted       = species_clean,
    tree_species_planted_other = other_clean,
    number_tree_planted        = count_clean
  ) %>%
  split(.$survey_key) %>%
  map(~ select(.x, -survey_key))

# 4.5 <1cm DBH in 1x1 subplots (`trees_less_1cm_dbh_repeat`)
trees_less_1cm_map <- trees_formatted %>%
  filter(size_class == "<1cm") %>%
  transmute(
    survey_key,
    tree_species_less_1cm_dbh       = species_clean,
    tree_species_less_1cm_dbh_other = other_clean,
    tree_type_less_1cm_dbh          = type_clean,
    number_tree_less_1cm_dbh        = count_clean
  ) %>%
  split(.$survey_key) %>%
  map(~ select(.x, -survey_key))

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 5. Assemble hierarchical payload list and export

# Helper to recursively drop NULL, NA, and empty lists/dataframes
is_empty_val <- function(x) {
  if (is.null(x)) return(TRUE)
  if (is.data.frame(x)) return(nrow(x) == 0)
  if (is.list(x)) return(length(x) == 0)
  if (length(x) == 1 && (is.na(x) || !nzchar(trimws(as.character(x))))) return(TRUE)
  return(FALSE)
}

remove_empty_nodes <- function(l) {
  if (!is.list(l) || is.data.frame(l)) return(l)
  l <- map(l, function(x) {
    if (is.list(x) && !is.data.frame(x)) remove_empty_nodes(x) else x
  })
  discard(l, is_empty_val)
}

message("--> Assembling structured submission payloads...")
payload_records <- vector("list", nrow(surveys_enriched))

for (i in seq_len(nrow(surveys_enriched))) {
  s <- surveys_enriched[i, ]
  s_key_chr <- as.character(s$survey_key)

  # Retrieve matched media filenames & local paths for this survey
  s_media_df <- media_by_survey[[s_key_chr]]
  get_media_filename <- function(field_nm) {
    if (is.null(s_media_df)) return(NA_character_)
    hit <- s_media_df %>% filter(new_field_name == field_nm)
    if (nrow(hit) == 0) NA_character_ else hit$upload_filename[1]
  }

  media_attachments_list <- if (!is.null(s_media_df) && nrow(s_media_df) > 0) {
    setNames(as.list(s_media_df$local_path), s_media_df$new_field_name)
  } else {
    list()
  }

  # Construct form data matching the exact XML group hierarchy of the New Form
  form_fields <- list(
    date              = s$date,
    start_time        = s$start_time,
    country_id        = s$country_id,
    organization_id   = s$org_id,
    plot_id           = s$plot_id,
    plot_type         = s$plot_type,
    plot_permanence   = s$plot_permanence,
    strata_number     = s$stratum,
    site_id           = s$site_id,
    site_name         = s$site_name,
    site_type         = s$site_type,
    organization_name = s$org_name,
    country_name      = s$country_name,

    timeframe_group = list(
      other_timeframe = s$other_timeframe,
      timeframe       = s$timeframe
    ),

    site_plot_info_group = list(
      site_size_restoration = s$site_size_restoration,
      site_size_control     = s$site_size_control,
      resampling_30x30      = s$resampling_30x30,
      large_trees_present   = s$large_trees_present,
      resampling_3x3        = s$resampling_3x3,
      little_trees_present  = s$little_trees_present,
      planting_pattern      = s$planting_pattern
    ),

    gps_pictures_30x30 = list(
      northeast_corner_plot_30x30 = s$northeast_corner_plot_30x30,
      ne_photo_plot_30x30         = get_media_filename("ne_photo_plot_30x30"),
      southeast_corner_plot_30x30 = s$southeast_corner_plot_30x30,
      se_photo_plot_30x30         = get_media_filename("se_photo_plot_30x30"),
      southwest_corner_plot_30x30 = s$southwest_corner_plot_30x30,
      sw_photo_plot_30x30         = get_media_filename("sw_photo_plot_30x30"),
      northwest_corner_plot_30x30 = s$northwest_corner_plot_30x30,
      nw_photo_plot_30x30         = get_media_filename("nw_photo_plot_30x30")
    ),

    gps_pictures_10x10 = list(
      northeast_corner_of_10m_x_10m_plot = s$northeast_corner_of_10m_x_10m_plot,
      ne_photo_small_site                = get_media_filename("ne_photo_small_site"),
      southeast_corner_of_10m_x_10m_plot = s$southeast_corner_of_10m_x_10m_plot,
      se_photo_smallsites                = get_media_filename("se_photo_smallsites"),
      southwest_corner_of_10m_x_10m_plot = s$southwest_corner_of_10m_x_10m_plot,
      sw_photo_smallsites                = get_media_filename("sw_photo_smallsites"),
      northwest_corner_of_10m_x_10m_plot = s$northwest_corner_of_10m_x_10m_plot,
      nw_photo_smallsites                = get_media_filename("nw_photo_smallsites")
    ),

    gps_pictures_3x3 = list(
      centroid_3x3_plot             = s$centroid_3x3_plot,
      description_location_plot_3x3 = s$description_location_plot_3x3,
      photo_plot_3x3                = get_media_filename("photo_plot_3x3")
    ),

    trees_10cm_dbh_group = list(
      trees_10cm_dbh_repeat      = trees_10cm_map[[s_key_chr]],
      notes_trees_10cm_dbh_group = s$notes_trees_10cm_dbh_group
    ),

    trees_1_9_9cm_dbh_group = list(
      little_trees_census_present   = s$little_trees_census_present,
      trees_1_9_9cm_dbh_repeat      = trees_1_9_map[[s_key_chr]],
      notes_trees_1_9_9cm_dbh_group = s$notes_trees_1_9_9cm_dbh_group
    ),

    trees_more_1cm_dbh_group = list(
      trees_more_1cm_dbh_repeat      = trees_more_1cm_map[[s_key_chr]],
      notes_trees_more_1cm_dbh_group = NA_character_
    ),

    trees_planted_group = list(
      trees_planted_repeat      = trees_planted_map[[s_key_chr]],
      notes_trees_planted_group = NA_character_
    ),

    trees_less_1cm_dbh_group = list(
      tiny_trees_present             = s$tiny_trees_present,
      trees_less_1cm_dbh_repeat      = trees_less_1cm_map[[s_key_chr]],
      notes_trees_less_1cm_dbh_group = NA_character_
    ),

    files = list(
      file = get_media_filename("file")
    ),

    end_time = s$end_time,
    end_note = NA_character_
  )

  payload_records[[i]] <- list(
    meta_info = list(
      survey_key  = s$survey_key,
      plot_id     = s$plot_id,
      plot_name   = s$plot_name,
      site_id     = s$site_id,
      org_id      = s$org_id,
      timeframe   = s$timeframe,
      legacy_uuid = s$legacy_uuid
    ),
    form_data         = remove_empty_nodes(form_fields),
    media_attachments = media_attachments_list
  )
}

# Build file tag based on filter settings
bundle_tag <- if (filter_by_params) {
  paste0(
    paste(target_org_code, collapse = "_"), "_",
    paste(target_time, collapse = "_"), "_",
    format(Sys.Date(), "%Y_%m_%d")
  )
} else {
  paste0("ALL_ORGS_", format(Sys.Date(), "%Y_%m_%d"))
}

rds_out_path    <- file.path(output_dir, paste0("kobo_upload_payloads_", bundle_tag, ".rds"))
rds_latest_path <- file.path(output_dir, "kobo_upload_payloads_latest.rds")
json_out_path   <- file.path(output_dir, paste0("kobo_upload_payloads_", bundle_tag, ".json"))

saveRDS(payload_records, rds_out_path)
saveRDS(payload_records, rds_latest_path)
writeLines(
  jsonlite::toJSON(payload_records, pretty = TRUE, auto_unbox = TRUE, dataframe = "rows"),
  json_out_path
)

message(sprintf("\n[OK] Prepared %d submission payload(s):", length(payload_records)))
message("  • RDS bundle : ", rds_out_path)
message("  • Latest RDS : ", rds_latest_path)
message("  • JSON preview: ", json_out_path)

#endregion
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# END
