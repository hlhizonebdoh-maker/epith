#' Title: Calculate Epidemic Threshold
#'
#' This function calculates the alert and epidemic thresholds based on historical data.
#'
#'
#' @import dplyr
#' @import rio
#' @import skimr
#' @import janitor
#' @import RColorBrewer
#' @import stringr
#' @import MMWRweek
#' @importFrom dplyr mutate across filter
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom tidyr pivot_longer
#' @import tidyverse
#' @import gghighlight
#' @param df A data frame of disease counts.
#' @param latest_year Column name for year.
#' @param latest_week Column name for MMWR week.
#' @param area Area label to add to the title of the plot
#' @param disease Disease label to add to the title of the plot
#' @param onset_column Column name for date of onset (default is "donset")
#' @param alert_sd One (1) standard deviation above the mean (default is 1)
#' @param epidemic_sd One (1.65) standard deviation above the mean (default is 1.65)
#'
#' @return A data frame with epidemic and alert thresholds.
#' @export
epith <- function(data,
                  disease,
                  latest_week,
                  latest_year,
                  area,
                  alert_sd = 1,
                  epidemic_sd = 1.65,
                  onset_column="donset") {

  if (missing(data) || missing(latest_week) || missing(latest_year) ||
      missing(disease) || missing(area) || missing(onset_column)) {
    stop("One of the required parameters is missing.")
  }

  if (!is.data.frame(data)) {
    stop("The 'data' parameter must be a data frame.")
  }

  if (!(onset_column %in% names(data))) {
    stop(paste("Column", onset_column, "not found in data frame."))
  }

  print("Parameters received correctly.")

  # Ensure onset column is Date
  if (!inherits(data[[onset_column]], "Date")) {
    data[[onset_column]] <- as.Date(data[[onset_column]])
  }

  #  ensure the onset_column really is a date string, otherwise it may become NA when coerced.
  if (all(is.na(data[[onset_column]]))) stop("All values in onset_column became NA after conversion. Check date format.")

  # Compute MMWR week
  # data <- data %>%


  data <- data %>%
  filter(!is.na(.data[[onset_column]])) %>%
  mutate(mmwr = MMWRweek(.data[[onset_column]])) %>%
    mutate(
      MMWRyear = mmwr$MMWRyear,
      MMWRweek = mmwr$MMWRweek
    ) %>%
    select(mw = MMWRweek, myear = MMWRyear) %>%
    filter(myear >= (latest_year - 5) & myear <= latest_year)

  # Count how many years are present
  data_years <- data %>% count(myear)

  if (nrow(data_years) < 6) {
    stop("Not enough years in the dataset. You need at least 6 years of data (including the current year).")
  }

  data <- data %>%
    count(myear, mw)

  # Wide transformation
  df_wide <- data %>%
    tidyr::pivot_wider(id_cols = myear, names_from = mw, values_from = n) %>%
    t() %>%
    as.data.frame() %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    slice(-1)

  df_mweeks <- data.frame(rownames(df_wide))
  df_combined <- bind_cols(df_mweeks, df_wide)

  df_mw <- data.frame(mw = 1:53)

  df_combined <- df_combined %>%
    rename(mw = `rownames.df_wide.`) %>%
    mutate(mw = as.numeric(mw)) %>%
    right_join(df_mw, by = "mw") %>%
    mutate(across(everything(), ~ replace_na(.x, 0))) %>%
    mutate(
      avg = rowMeans(across(c(V1, V2, V3, V4, V5)), na.rm = TRUE),
      stdev = apply(across(c(V1, V2, V3, V4, V5)), 1, sd, na.rm = TRUE),
      alert = avg + alert_sd * stdev,
      epidemic = avg + epidemic_sd * stdev
    ) %>%
    arrange(mw)

  # Current year (6th column is current year)
  df_curryear <- df_combined %>%
    filter(mw <= latest_week) %>%
    select(mw, V6)

  df_alert <- df_combined %>% select(mw, alert)
  df_epidemic <- df_combined %>% select(mw, epidemic)

  # Plot
  ggplot() +
    geom_bar(data = df_curryear, aes(x = mw, y = V6), stat = "identity", fill = "#7091B3", alpha = 0.5) +
    geom_line(data = df_alert, aes(x = mw, y = alert), linewidth = 1, alpha = 0.5, color = "blue") +
    geom_line(data = df_epidemic, aes(x = mw, y = epidemic), linewidth = 1.5, alpha = 0.5, color = "red") +
    labs(
      title = paste(disease, "Alert and Epidemic Thresholds in", area),
      subtitle = paste("Latest Week:", latest_week,  "Year:", latest_year),
      x = "Morbidity Week",
      y = "Number of Cases",
      caption = paste0("Bars = ", latest_year, " cases; Blue Line = Alert; Red Line = Epidemic")
    ) +
    theme_minimal() +
    theme(
      plot.caption = element_text(size = 7, face = "italic"),
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    scale_x_continuous(breaks = 1:53)
}

