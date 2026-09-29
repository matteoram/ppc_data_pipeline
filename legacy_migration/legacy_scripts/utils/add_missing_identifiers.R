#' Impute missing identifiers based on known organizational mappings
#'
#' @param df The data frame to modify
#' @param country_col Name of the country column
#' @param org_col Name of the organization column
#' @return Data frame with imputed missing values

add_missing_identifiers <- function(
  site_df,
  site_size_df,
  kobo_plot_df
  ) {
  
  # 2 Add country column in site data
  if (!("country_name" %in% names(site_df))) {
    
    # Extract pairs from site_size_df
    lookup_site_size <- data.frame(
      org_name = character(),
      country_name = character(),
      stringsAsFactors = FALSE
    )
    if (all(c("org_name", "country_name") %in% names(site_size_df))) {
      lookup_site_size <- data.frame(
        org_name = as.character(site_size_df[["org_name"]]),
        country_name = as.character(site_size_df[["country_name"]]),
        stringsAsFactors = FALSE
      )
    }

    # Extract pairs from kobo_plot_df
    lookup_kobo_plot <- data.frame(
      org_name = character(),
      country_name = character(),
      stringsAsFactors = FALSE
    )
    if (all(c("org_name", "country_name") %in% names(kobo_plot_df))) {
      lookup_kobo_plot <- data.frame(
        org_name = as.character(kobo_plot_df[["org_name"]]),
        country_name = as.character(kobo_plot_df[["country_name"]]),
        stringsAsFactors = FALSE
      )
    }
    
    # Combine the datasets into a single master pool
    combined_pool <- rbind(lookup_site_size, lookup_kobo_plot)

    # Strip rows with missing values or blank characters
    combined_pool <- combined_pool[
      !is.na(combined_pool$org_name) & combined_pool$org_name != "" &
      !is.na(combined_pool$country_name) & combined_pool$country_name != "", 
    ]
    
    # Remove duplicate combinations to create unique keys mapping org to country
    lookup_table <- unique(combined_pool)
    
    # Map the unique keys back to site_df
    if ("org_name" %in% names(site_df)) {
      
      # Apply countries from the lookup table
      if (nrow(lookup_table) > 0) {
        match_indices <- match(as.character(site_df[["org_name"]]), lookup_table$org_name)
        site_df[["country_name"]] <- lookup_table$country_name[match_indices]
      }
      
      # Apply manual fallbacks and overrides
      site_df <- site_df %>%
        mutate(
          country_name = case_when(
            str_detect(org_name, "ACT")  ~ "Scotland",
            org_name == "CI Brazil" ~ "Brazil",
            org_name == "CI Mexico" ~ "Mexico",
            str_detect(org_name, "RIOTERRA") ~ "Brazil",
            org_name == "Global Forest Generation" ~ "High Andes (Ecuador and Peru)",
            org_name == "IUCN Thailand" ~ "Thailand",
            org_name == "Pangea EcoNetAssets" ~ "India",
            org_name == "Conservação Internacional do Brasil" ~ "Brazil",
            TRUE ~ country_name
          )
        )
      }
    }

  # Add organization in site_size_data
  if (!("org_name" %in% names(site_size_df))) {
    site_size_df <- site_size_df %>%

      # Change site_name to character for consistent merging
      mutate(site_name = as.character(site_name)) %>%
      
      left_join(
        site_df %>% select(site_name, org_name) %>% distinct(),
        by = "site_name"
      )
  }
  
  return(list(
    site_df = site_df,
    site_size_df = site_size_df,
    kobo_plot_df = kobo_plot_df
  ))
}