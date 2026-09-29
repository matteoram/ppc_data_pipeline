#' Centralized function to harmonize Country, Organization, and Site identifiers
#' and create a temporary composite key.
#'
#' @param df The data frame to harmonize
#' @param country_col Name of the country column
#' @param org_col Name of the organization column
#' @param site_col Name of the site column
#' @param plot_col Name of the plot column (optional, for temp_key)
#' @return Harmonized data frame with temp_key

harmonize_identifiers <- function(
  df,
  country_map,
  map_orgs,
  map_sites,
  map_plots,
  map_species = NULL,
  map_columns,
  country_col,
  org_col,
  site_col,
  plot_col,
  species_col = NULL) {

  #region 1 Fix organization names
  if (!is.null(org_col) && length(org_col) > 0 && org_col %in% names(df)) {

    # Recast as character to remove labels
    df[[org_col]] <- as.character(df[[org_col]])

    # Split the semicolon-separated potential names into individual elements
    split_names <- strsplit(as.character(map_orgs$potential_names), ";")
    
    # Replicate the correct names to match the length of the split names
    replicated_correct <- rep(map_orgs$correct_name, lengths(split_names))
    
    # Create a lookup table with trimmed keys and values
    org_lookup <- data.frame(
      potential = trimws(unlist(split_names)),
      correct = trimws(replicated_correct),
      stringsAsFactors = FALSE
    )
    
    # Clean the input target column by removing leading and trailing whitespace
    trimmed_target <- trimws(df[[org_col]])
    
    # Map the target elements to the rows of the lookup table
    match_indices <- match(trimmed_target, org_lookup$potential)
    
    # Replace matched items with the correct name or keep original values
    df[[org_col]] <- ifelse(
      !is.na(match_indices), org_lookup$correct[match_indices], df[[org_col]]
    )

    # Fix Reforest'Action in site_data
    if ("project_name" %in% names(df)) {

    # Match the target organization
    is_reforest <- grepl("^Reforest'Action", df[[org_col]], ignore.case = TRUE)
    
    # Standardize project names to lowercase for case-insensitive matching
    project_lower <- tolower(df[["project_name"]])
    
    # Update the organization column step-by-step based on project keywords
    df[[org_col]] <- ifelse(
      is_reforest & grepl("chantilly", project_lower),
      "Reforest Action (France)", df[[org_col]]
    )
    df[[org_col]] <- ifelse(
      is_reforest & grepl("proenca", project_lower),
      "Reforest Action (Portugal)", df[[org_col]]
    )
    df[[org_col]] <- ifelse(
      is_reforest & grepl("palencia", project_lower),
      "Reforest Action (Spain)", df[[org_col]]
    )
    }

    # Fix Brazilian and Mexican organizations in site_data
    if (org_col %in% names(df) && "project_name" %in% names(df)) {
      df[[org_col]] <- ifelse(
        grepl("AMBIO", df$project_name) & df[[org_col]] == "Conservation International - México",
        "AMBIO", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("Cafe Capitán", df$project_name) & df[[org_col]] == "Conservation International - México",
        "Café Capitan", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("CUCOS", df$project_name) & df[[org_col]] == "Conservation International - México",
        "CUCOS", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("MACUILES", df$project_name) & df[[org_col]] == "Conservation International - México",
        "Macuiles", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("UCIRI", df$project_name) & df[[org_col]] == "Conservation International - México",
        "UCIRI", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("WWF", df$project_name) & df[[org_col]] == "Conservation International - México",
        "WWF", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("La Frailescana", df$project_name) & df[[org_col]] == "Conservation International - México",
        "APRN La Frailescana", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("REBISE", df$project_name) & df[[org_col]] == "Conservation International - México",
        "REBISE", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("REBISO", df$project_name) & df[[org_col]] == "Conservation International - México",
        "REBISO", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("REBITRI", df$project_name) & df[[org_col]] == "Conservation International - México",
        "REBITRI", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("REBIVTA", df$project_name) & df[[org_col]] == "Conservation International - México",
        "REBIVTA", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("CEPAN", df$project_name) & df[[org_col]] == "Conservação Internacional do Brasil",
        "CEPAN", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("Ciclos", df$project_name) & df[[org_col]] == "Conservação Internacional do Brasil",
        "CICLOS", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("MDPS", df$project_name) & df[[org_col]] == "Conservação Internacional do Brasil",
        "MDPS", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("MVGI", df$project_name) & df[[org_col]] == "Conservação Internacional do Brasil",
        "MVGI", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("MVGI", df$project_name) & df[[org_col]] == "Conservação Internacional do Brasil",
        "MVGI", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("Peroa", df$project_name) & df[[org_col]] == "Conservação Internacional do Brasil",
        "PEROA", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("Polímata", df$project_name) & df[[org_col]] == "Conservação Internacional do Brasil",
        "POLIMATA", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("Cooperativa Canteiros", df$project_name) & df[[org_col]] == "Conservação Internacional do Brasil",
        "COOPERATIVA CANTEIROS", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("Primaflora", df$project_name) & df[[org_col]] == "Conservação Internacional do Brasil",
        "PRIMAFLORA", df[[org_col]]
      )
      df[[org_col]] <- ifelse(
        grepl("GANB - Bahia (Atlantic Forest)", df$project_name, fixed = TRUE),
        "GANB (2021)", df[[org_col]]
      )
    }

    # Fix FONCET in plot_data, kobo_plot_data, tree_data by using common columns
    if (!is.null(org_col) && !is.null(plot_col) && org_col %in% names(df) && plot_col %in% names(df)) {
      
      df <- df %>%
        mutate(
          !!sym(org_col) := case_when(
            !!sym(org_col) == "FONCET" & !!sym(site_col) %in% c(295, 296, 1313, 1314, 1387, 1388, 1393, 1394, 1395, 1396, 1397, 1398, 1399, 1400, 1401, 1402, 1403, 1404, 1405, 1406, 1407, 1408, 1409, 1410, 2293, 2295, 2296, 2297, 2298, 2299, 2300, 2301, 2302, 2303, 2304, 2305, 2306, 2307, 2312, 2313, 2314, 2315, 2316, 2317, 2318, 2319, 2321, 2322, 2323, 2324, 2326, 2328, 2329, 2330, 2331, 2332, 2333, 2334, 2335, 2336, 2338, 2339, 2340, 2341, 2342, 2343, 2344, 2345, 2346, 2347, 2348, 2349, 2350, 2351, 2352, 2353, 2354, 2355, 2356, 2357, 2358, 2360, 2361, 2362, 2363, 2365, 2366, 2367, 2368, 2369, 2370, 2371, 2372, 2373, 2374, 2375, 2376, 2377, 2378, 2379, 2380, 2381, 2382, 2384, 2385, 2386, 2387, 2388, 2389, 2390, 2391, 2392, 2393, 2394, 2395, 2396, 2397, 2734, 2735, 2736, 2738, 2767, 2772, 2783, 2784, 2843, 3946, 3947, 3948, 3950, 3951, 3952, 3953, 3954, 3955, 3956, 3957, 3958, 3959, 3960, 3961, 3962, 3963, 3964, 3965, 3966, 3967, 3968, 3969, 3970, 3971, 3972, 3973, 3974, 3975, 4113, 4114, 4115, 4116, 4117, 4118, 4119) ~ "APRN La Frailescana",
            !!sym(org_col) == "FONCET" & !!sym(site_col) %in% c(2091, 2092, 2093, 2560, 2608, 2609, 2610, 2612, 2613, 2663, 2664, 2665, 2666, 2667, 2668, 2669, 2670, 2671, 2672, 2673, 2674, 2675, 2676, 2677, 2678, 2679, 2680, 2681, 2682, 2683, 2684, 2685, 2686, 2687, 2688, 2689, 2690, 2691, 2692, 2693, 2694, 2695, 2696, 2697, 2698, 2699, 2700, 2701, 2702, 2703, 2704, 2705, 2706, 2707, 2708, 2709, 2710, 2711, 2712, 2713, 2714, 2715, 2716, 2717, 2718, 2719, 2720, 2721, 2722, 2723, 2724, 2725, 2726, 2727, 2728, 2729, 2730, 2731, 2732, 2733, 2739, 2740, 2741, 2742, 2743, 2744, 2745, 2746, 2747, 2748, 2749, 2750, 2751, 2752, 2753, 2754, 2755, 2756, 2757, 2758, 2759, 2763, 2770, 2771, 2773, 2774, 2775, 2777, 2778, 2779, 2781, 2786, 2787, 2788, 2790, 2791, 2792, 2793, 2794, 2797, 2798, 2801, 2803, 2804, 2808, 2811, 2812, 2813, 2814, 2815, 2816, 2817, 2818, 2819, 3234, 3236, 3529, 3932, 3933, 3934, 3935, 3936, 3937, 3938, 4084, 4085, 4086, 4087) ~ "REBISE",
            !!sym(org_col) == "FONCET" & !!sym(site_col) %in% c(1416, 1617, 1618, 1619, 1620, 1621, 1622, 1623, 1624, 1625, 1626, 1627, 1628, 1629, 1630, 1631, 1632, 1633, 1634, 1636, 1637, 1638, 1639, 1732, 1733, 1734, 1735, 1736, 1737, 1738, 1739, 1740, 1741, 1902, 1903, 1904, 1905, 1906, 1907, 1908, 1909, 2911, 2947, 2948, 2949, 2950, 2951, 2952, 2954, 2955, 2956, 2957, 2958, 2959, 2960, 2961, 2962, 2963, 2964, 2965, 2966, 2967, 2968, 2969, 2970, 2971, 2972, 2973, 2974, 2975, 3001, 3002, 3003, 3004, 3005, 3006, 3007, 3008, 3009, 3010, 3011, 3012, 3013, 3014, 3015, 3016, 3017, 3018, 3020, 3021, 3023, 3024, 3025, 3026, 3027, 3028, 3029, 3030, 3031, 3032, 3033, 3034, 3035, 3036, 3037, 3038, 3039, 3040, 3041, 3042, 3047, 3048, 3049, 3050, 3051, 3052, 3053, 3061, 3062, 3977, 3978, 3979, 3981, 3982, 3983, 3984) ~ "REBISO",
            !!sym(org_col) == "FONCET" & !!sym(site_col) %in% c(1654, 1655, 1674, 1675, 1676, 1677, 1678, 1679, 1680, 1681, 1682, 1683, 1684, 1685, 1686, 1687, 1689, 1690, 1691, 1692, 1693, 1694, 1695, 1696, 1697, 1698, 1700, 1701, 1702, 1703, 1704, 1705, 1706, 1707, 1708, 1709, 1710, 1711, 1712, 1713, 2875, 2876, 2877, 2878, 2880, 2881, 2882, 2883, 2884, 2885, 2886, 2887, 2888, 2889, 2890, 2891, 2892, 2893, 2894, 2895, 2896, 2898, 2899, 2900, 2901, 2903, 2904, 2905, 2914, 2915, 2916, 2917, 2918, 2919, 2920, 2921, 2922, 2923, 2924, 2925, 2926, 2927, 2931, 2932, 3043, 3044, 3045, 3046, 3063, 3065, 3071, 3072, 3073, 3075, 3087, 3089, 3095, 3096, 3097, 3098, 3099, 3100, 3101, 3102, 3103, 3104, 3105, 3106, 3107, 3108, 3109, 3110, 3111, 3112, 3113, 3114, 3115, 3116, 3117, 3118, 3119, 3120, 3121, 3122, 3123, 3124, 3125, 3126, 3127, 3128, 3129, 3131, 3133, 3136, 3137, 3138, 3139, 3140, 3141, 3142, 3143, 3144, 3145, 3146, 3147, 3148, 3149, 3150, 3151, 3152, 3153, 3154, 3155, 3156, 3158, 3159, 3160, 3161, 3162, 3163, 3164, 3165, 3166, 3167, 3168, 3169, 3170, 3171, 3172, 3173, 3174, 3175, 3176, 3177, 3178, 3179, 3180, 3181, 3182, 3183, 3184, 3185, 3186, 3187, 3188, 3189, 3190, 3191, 3192, 3193, 3194, 3195, 3196, 3197, 3198, 3199, 3202, 3203, 3204, 3205, 3206, 3207, 3208, 3210, 3211, 3212, 3213, 3214, 3215, 3216, 3217, 3218, 3219, 3220, 3221, 3222, 3223, 3224, 3232, 3534, 3536, 4060, 4061, 4062, 4063, 4064, 4065, 4066, 4067, 4068, 4069, 4070, 4071, 4072, 4075, 4076, 4077, 4078, 4079, 4080, 4081, 4082, 4083, 4088, 4099, 4100, 4101, 4121, 4147, 4148) ~ "REBITRI",
            !!sym(org_col) == "FONCET" & !!sym(site_col) %in% c(335, 336, 337, 338, 339, 340, 341, 2795, 2796, 2802, 2805, 2806, 2807, 2809, 2810, 3537, 4089, 4090, 4091, 4092, 4093, 4094, 4095, 4096) ~ "REBIVTA",
            TRUE ~ !!sym(org_col)
        )
      )
    }
  }

  #region 2 Fix country names
  if (
    !is.null(country_col) && length(country_col) > 0 && country_col %in% names(df)
  ) {

    # Recast as character to remove labels
    df[[country_col]] <- as.character(df[[country_col]])

    # Standardize UAE, USA, and Andes variants
    df[[country_col]][
      grepl("^United Arab Emirates", trimws(df[[country_col]]))
    ] <- "UAE"
    df[[country_col]][
      grepl("^United States", trimws(df[[country_col]]))
    ] <- "USA"
    df[[country_col]][
      grepl("^Andes", trimws(df[[country_col]]))
    ] <- "High Andes (Ecuador and Peru)"    
    
    # Disaggregate Europe regions using the cleaned organization values
    if (!is.null(org_col) && length(org_col) > 0 && org_col %in% names(df)) {
      is_europe <- grepl("^Europe", df[[country_col]], ignore.case = TRUE)
      
      df[[country_col]] <- ifelse(
        is_europe & df[[org_col]] == "Reforest Action (France)",
        "France", df[[country_col]]
      )
      df[[country_col]] <- ifelse(
        is_europe & df[[org_col]] == "Reforest Action (Portugal)", 
        "Portugal", df[[country_col]]
      )
      df[[country_col]] <- ifelse(
        is_europe & df[[org_col]] == "Reforest Action (Spain)",
        "Spain", df[[country_col]]
      )
    }

    # Replace Peru with High Andes (Ecuador and Peru)
    df[[country_col]] <- ifelse(
      trimws(df[[country_col]]) == "Peru",
      "High Andes (Ecuador and Peru)", df[[country_col]]
    )
  }

  #region 3 Fix site names
  if (
    !is.null(site_col) && length(site_col) > 0 &&
    !is.null(org_col) && length(org_col) > 0 &&
    site_col %in% names(df) &&
    org_col %in% names(df)
  ) {
    
    # Recast as character to remove labels
    df[[site_col]] <- as.character(df[[site_col]])  

    # Unnest the semicolon-separated potential site names from mapping sheet
    split_site_names <- strsplit(as.character(map_sites$potential_names), ";")
    
    # Replicate correct names and org names to match length of split names
    replicated_site_correct <- rep(
      map_sites$correct_name, lengths(split_site_names)
    )
    replicated_site_org <- rep(
      map_sites$org_name, lengths(split_site_names)
    )
    
    # Construct an itemized dictionary containing tracking combinations
    site_lookup <- data.frame(
      potential_site = trimws(unlist(split_site_names)),
      org_name = trimws(replicated_site_org),
      correct_site = trimws(replicated_site_correct),
      stringsAsFactors = FALSE
    )
    
    # Create composite strings for matching to handle identical site entries
    site_lookup$composite_key <- paste(site_lookup$potential_site, site_lookup$org_name, sep = "_")
    
    # Target original columns dynamically using clean string representations
    df_composite_keys <- paste(
      trimws(as.character(df[[site_col]])), 
      trimws(as.character(df[[org_col]])), 
      sep = "_"
    )
    
    # Match the composite keys to the lookup table and replace site names
    match_site_indices <- match(df_composite_keys, site_lookup$composite_key)
    
    # Replace matched site names with correct names or keep original values
    df[[site_col]] <- ifelse(
      !is.na(match_site_indices), 
      site_lookup$correct_site[match_site_indices], 
      df[[site_col]]
    )
  }

  #region 4 Fix plot names
  if (
    !is.null(plot_col) && length(plot_col) > 0 &&
    !is.null(site_col) && length(site_col) > 0 &&
    !is.null(org_col) && length(org_col) > 0 &&
    plot_col %in% names(df) &&
    site_col %in% names(df) &&
    org_col %in% names(df)
  ) {
    
    # Recast as character to remove labels
    df[[plot_col]] <- as.character(df[[plot_col]])  

    # Unnest the semicolon-separated potential plot names from mapping sheet
    split_plot_names <- strsplit(as.character(map_plots$potential_names), ";")
    
    # Replicate correct names, site names, and org names to match length of split names
    replicated_plot_correct <- rep(
      map_plots$correct_name, lengths(split_plot_names)
    )
    replicated_plot_site <- rep(
      map_plots$site_name, lengths(split_plot_names)
    )
    replicated_plot_org <- rep(
      map_plots$org_name, lengths(split_plot_names)
    )
    
    # Construct an itemized dictionary containing tracking combinations
    plot_lookup <- data.frame(
      potential_plot = trimws(unlist(split_plot_names)),
      site_name = trimws(as.character(replicated_plot_site)),
      org_name = trimws(as.character(replicated_plot_org)),
      correct_plot = trimws(as.character(replicated_plot_correct)),
      stringsAsFactors = FALSE
    )
    
    # Create composite strings for matching to handle identical plot entries across sites
    plot_lookup$composite_key <- paste(
      plot_lookup$potential_plot, 
      plot_lookup$site_name, 
      plot_lookup$org_name, 
      sep = "_"
    )
    
    # Target original columns dynamically using clean string representations
    df_composite_keys <- paste(
      trimws(as.character(df[[plot_col]])), 
      trimws(as.character(df[[site_col]])), 
      trimws(as.character(df[[org_col]])), 
      sep = "_"
    )
    
    # Match the composite keys to the lookup table and replace plot names
    match_plot_indices <- match(df_composite_keys, plot_lookup$composite_key)
    
    # Replace matched plot names with correct names or keep original values
    df[[plot_col]] <- ifelse(
      !is.na(match_plot_indices), 
      plot_lookup$correct_plot[match_plot_indices], 
      df[[plot_col]]
    )
  }

  #region 4.5 Fix species names
  if (
    !is.null(species_col) && length(species_col) > 0 &&
    species_col %in% names(df) &&
    !is.null(map_species)
  ) {
    
    # Recast as character to remove labels
    df[[species_col]] <- as.character(df[[species_col]])
    
    # Split the semicolon-separated potential names into individual elements
    split_names <- strsplit(as.character(map_species$potential_names), ";")
    
    # Replicate the correct names to match the length of the split names
    replicated_correct <- rep(map_species$correct_name, lengths(split_names))
    
    # Create a lookup table with trimmed keys and values
    species_lookup <- data.frame(
      potential = trimws(unlist(split_names)),
      correct = trimws(replicated_correct),
      stringsAsFactors = FALSE
    )
    
    # Target original column dynamically
    df_values <- trimws(as.character(df[[species_col]]))
    
    # Match the values to the lookup table
    match_indices <- match(df_values, species_lookup$potential)
    
    # Replace matched names with correct names or keep original values
    df[[species_col]] <- ifelse(
      !is.na(match_indices), 
      species_lookup$correct[match_indices], 
      df[[species_col]]
    )
  }

  #region 5 Standardize identifier names and clean data types
  if (!is.null(country_col) && length(country_col) > 0 && country_col %in% names(df)) {
    names(df)[names(df) == country_col] <- "country_name"
    df[["country_name"]] <- as.character(trimws(df[["country_name"]]))
  }
  if (!is.null(org_col) && length(org_col) > 0 && org_col %in% names(df)) {
    names(df)[names(df) == org_col] <- "org_name"
    df[["org_name"]] <- as.character(trimws(df[["org_name"]]))
  }
  if (!is.null(site_col) && length(site_col) > 0 && site_col %in% names(df)) {
    names(df)[names(df) == site_col] <- "site_name"
    df[["site_name"]] <- as.character(trimws(df[["site_name"]]))
  }
  if (!is.null(plot_col) && length(plot_col) > 0 && plot_col %in% names(df)) {
    names(df)[names(df) == plot_col] <- "plot_name"
    df[["plot_name"]] <- as.character(trimws(df[["plot_name"]]))
  }

  # Return dataframe
  return(df)
}

harmonize_columns <- function(df, map_columns) {

  # Resolve conflict when both uuid and _uuid columns exist simultaneously
  if ("uuid" %in% names(df) && "_uuid" %in% names(df)) {
    names(df)[names(df) == "uuid"] <- "uuid_alt"
  }

  # Resolve conflict when both uuid and _uuid columns exist simultaneously
  if ("Sampling Timeframe" %in% names(df) && "Timeframe" %in% names(df)) {
    names(df)[names(df) == "Timeframe"] <- "Timeframe_alt"
  }  

  # Unnest the semicolon-separated potential site names from mapping sheet
  split_cols <- strsplit(as.character(map_columns$potential_names), ";")

  # Replicate the correct names to match the length of the split names
  replicated_correct_cols <- rep(map_columns$correct_name, lengths(split_cols))

  # Create a lookup table with trimmed keys and values
  col_lookup <- data.frame(
    potential = trimws(unlist(split_cols)),
    correct = trimws(replicated_correct_cols),
    stringsAsFactors = FALSE
  )

  # Map current column names to the lookup table
  current_cols <- names(df)
  match_col_indices <- match(current_cols, col_lookup$potential)

  # Replace matched column names with correct names or keep original names
  names(df) <- ifelse(
    !is.na(match_col_indices),
    col_lookup$correct[match_col_indices],
    current_cols
  )

  # Standardize to lowercase
  target_text_cols <- c(
    "site_type", "site_size_restoration", "site_size_control", "active_status", 
    "plot_type", "plot_permanence", "stratum", "planting_pattern", 
    "resampling_30x30", "large_trees_present", "resampling_3x3", 
    "little_trees_present", "tiny_trees_present", "tree_type"
  )
  for (col in target_text_cols) {
    if (col %in% names(df)) {
      df[[col]] <- tolower(as.character(df[[col]]))
    }
  }

  # Harmonize timeframe
  if ("timeframe" %in% names(df)) {
    df[["timeframe"]] <- trimws(tolower(as.character(df[["timeframe"]])))
    df[["timeframe"]] <- ifelse(df[["timeframe"]] == "y0 (baseline)", "y0", df[["timeframe"]])
    df[["timeframe"]] <- gsub("y 2.5", "y2.5", df[["timeframe"]])
  }

  return(df)
}