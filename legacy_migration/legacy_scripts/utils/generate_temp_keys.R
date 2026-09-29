#' Generate temporary composite keys for sites and plots
#'
#' @param df The data frame to modify
#' @param country_col Name of the country column
#' @param org_col Name of the organization column
#' @param site_col Name of the site column
#' @param plot_col Name of the plot column (optional)
#' @return Data frame with generated temporary keys

generate_temp_keys <- function(
    df, 
    country_col = "country_name", 
    org_col = "org_name", 
    site_col = "site_name", 
    plot_col = "plot_name",
    time_col = "timeframe"
    ) {
        
    # Coerce to character and trim whitespace to keep the composite keys uniform
    country_clean <- trimws(as.character(df[[country_col]]))
    org_clean     <- trimws(as.character(df[[org_col]]))
    site_clean    <- trimws(as.character(df[[site_col]]))
    
    # Generate the temporary site key using an underscore separator
    df$temp_site_key <- paste(country_clean, org_clean, site_clean, sep = "_")
    
    # Generate the temporary plot key if the plot column is valid and present
    if (!is.null(plot_col) && plot_col %in% names(df)) {
        plot_clean <- trimws(as.character(df[[plot_col]]))
        time_clean <- trimws(gsub("Y0 \\(baseline\\)", "y0", as.character(df[[time_col]])))
        df$temp_plot_key <- paste(
            country_clean, org_clean, site_clean, plot_clean, time_clean,
            sep = "_"
        )
    }

  return(df)
}