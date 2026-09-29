#' View Plot Details by UUID
#'
#' @description This is a utility function to quickly view all the data 
#' associated with a specific survey UUID across the three main tables:
#' plots_table, surveys_table, and tree_counts_table.
#'
#' @param target_uuid The character UUID to search for.
#' @param plots_df The plots table (defaults to plots_table_final)
#' @param surveys_df The surveys table (defaults to surveys_table)
#' @param trees_df The tree counts table (defaults to tree_counts_table)
#'
#' @return NULL. Prints formatted tables to the console.
#' @export

view_plot_by_uuid <- function(target_uuid, 
                              plots_df = NULL, 
                              surveys_df = NULL, 
                              trees_df = NULL) {
  
  # Fallback to global environment variables if not explicitly passed
  if (is.null(plots_df) && exists("plots_table_final", envir = .GlobalEnv)) {
    plots_df <- get("plots_table_final", envir = .GlobalEnv)
  }
  if (is.null(surveys_df) && exists("surveys_table", envir = .GlobalEnv)) {
    surveys_df <- get("surveys_table", envir = .GlobalEnv)
  }
  if (is.null(trees_df) && exists("tree_counts_table", envir = .GlobalEnv)) {
    trees_df <- get("tree_counts_table", envir = .GlobalEnv)
  }

  if (is.null(surveys_df)) stop("surveys_table not found in environment.")

  # Filter survey
  s_data <- surveys_df %>% dplyr::filter(uuid == target_uuid)
  
  if (nrow(s_data) == 0) {
    message("UUID not found in surveys_table.")
    return(invisible(NULL))
  }
  
  target_plot_name <- s_data$plot_name[1]

  cat("\n======================================================\n")
  cat(sprintf("1. PLOT METADATA (Plot: %s)\n", target_plot_name))
  cat("======================================================\n")
  if (!is.null(plots_df)) {
    p_data <- plots_df %>% 
      dplyr::filter(plot_name == target_plot_name) %>%
      dplyr::select(plot_name, plot_type, plot_permanence, site_id)
    print(p_data)
  } else {
    message("plots_table not provided.")
  }

  cat("\n======================================================\n")
  cat("2. SURVEY DETAILS (surveys_table)\n")
  cat("======================================================\n")
  dplyr::glimpse(s_data)

  cat("\n======================================================\n")
  cat("3. TREE COUNTS (tree_counts_table)\n")
  cat("======================================================\n")
  if (!is.null(trees_df)) {
    t_data <- trees_df %>% dplyr::filter(uuid == target_uuid)
    if (nrow(t_data) > 0) {
      print(t_data)
    } else {
      message("No trees recorded for this UUID.")
    }
  } else {
    message("tree_counts_table not provided.")
  }
  cat("======================================================\n\n")
}
