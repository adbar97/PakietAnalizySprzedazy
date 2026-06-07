#' Wczytanie danych sprzedażowych
#'
#' @description Wczytuje dane w odpowiednim formacie, korzystając z tidyverse.
#' Łączy zbiory train, stores i opcjonalnie holidays.
#' @param train_path Ścieżka do pliku train.csv
#' @param stores_path Ścieżka do pliku stores.csv
#' @param holidays_path Ścieżka do pliku holidays_events.csv
#' @return Dataframe (tibble)
#' @export
load_sales_data <- function(train_path, stores_path, holidays_path) {
  library(dplyr)
  library(readr)

  train <- read_csv(train_path, show_col_types = FALSE)
  stores <- read_csv(stores_path, show_col_types = FALSE)
  holidays <- read_csv(holidays_path, show_col_types = FALSE)

  # Łączenie danych
  df <- train %>%
    left_join(stores, by = "store_nbr") %>%
    left_join(holidays, by = "date", suffix = c("", "_holiday"))

  return(df)
}

#' Walidacja jakości danych sprzedażowych
#'
#' @description Sprawdzenie jakości danych (braki, duplikaty, poprawne daty, zakres wartości, częstotliwość)
#' @param df Dataframe z historią sprzedaży
#' @return Dataframe z listą ostrzeżeń
#' @export
validate_sales_ts <- function(df) {
  warnings <- list()

  if(any(is.na(df$sales))) warnings <- append(warnings, "Wykryto braki danych w kolumnie sales.")
  if(any(duplicated(df))) warnings <- append(warnings, "Wykryto zduplikowane wiersze.")
  if(any(df$sales < 0, na.rm = TRUE)) warnings <- append(warnings, "Wykryto ujemną sprzedaż.")
  if(!inherits(df$date, "Date")) warnings <- append(warnings, "Kolumna date jest w błędnym formacie.")

  # Sprawdzanie niespójnej częstotliwości
  dates_sorted <- sort(unique(df$date))
  date_diffs <- as.numeric(diff(dates_sorted))
  if(length(unique(date_diffs)) > 1) {
    warnings <- append(warnings, "Wykryto luki w datach.")
  }

  print(warnings)
  return(invisible(warnings))
}

#' Czyszczenie i przygotowanie danych
#'
#' @description Czyszczenie danych (obsługa braków, duplikatów, sortowanie, opcja agregacji czasowej).
#' @param df Dataframe
#' @param agg_level Poziom agregacji: "day", "week", "month"
#' @return Wyczyszczony dataframe
#' @export
clean_sales_ts <- function(df, agg_level = "day") {
  library(dplyr)
  library(lubridate)

  df_clean <- df %>%
    distinct() %>%
    filter(sales >= 0) %>%
    mutate(sales = ifelse(is.na(sales), 0, sales),
           date = as.Date(date)) %>%
    arrange(date)

  # Agregacja tygodniowa
  if (agg_level == "week") {
    df_clean <- df_clean %>%
      mutate(date = floor_date(date, "week")) %>%
      group_by(date, store_nbr, family, city, state, type) %>%
      summarise(
        sales = sum(sales, na.rm = TRUE),
        onpromotion = sum(onpromotion, na.rm = TRUE),
        .groups = "drop"
      )
  }
  # Agregacja miesięczna
  else if (agg_level == "month") {
    df_clean <- df_clean %>%
      mutate(date = floor_date(date, "month")) %>%
      group_by(date, store_nbr, family, city, state, type) %>%
      summarise(
        sales = sum(sales, na.rm = TRUE),
        onpromotion = sum(onpromotion, na.rm = TRUE),
        .groups = "drop"
      )
  }

  return(df_clean)
}

#' Obliczanie kluczowych metryk biznesowych
#'
#' @description Oblicza sprzedaż całkowitą, przeciętną, średnią kroczącą, zmienność i udział promocji
#' @param df Wyczyszczony Dataframe
#' @return Dataframe z metrykami
#' @export
compute_sales_metrics <- function(df) {
  library(dplyr)
  library(zoo)

  df_metrics <- df %>%
    group_by(date) %>%
    summarise(
      total_sales = sum(sales, na.rm = TRUE),
      avg_sales = mean(sales, na.rm = TRUE),
      promo_share = sum(onpromotion, na.rm = TRUE) / sum(sales+1, na.rm = TRUE),
      sales_variance = var(sales, na.rm = TRUE)
    ) %>%
    arrange(date) %>%
    mutate(moving_avg_7d = rollmean(total_sales, k = 7, fill = NA, align = "right"))

  # Obliczanie długości pomiędzy szczytami sprzedaży
  df_metrics <- df_metrics %>%
    mutate(
      is_peak = total_sales > lag(total_sales, default = 0) & total_sales > lead(total_sales, default = 0)
    )

  peak_dates <- df_metrics$date[df_metrics$is_peak]
  avg_peak_distance <- mean(as.numeric(diff(peak_dates)), na.rm = TRUE)

  df_metrics$avg_peak_distance_days <- round(avg_peak_distance, 1)

  return(df_metrics)
}

#' Wizualizacja trendów sprzedaży
#'
#' @description Tworzy wizualizacje danych sprzedażowych.
#' @param df_metrics Dataframe z metrykami
#' @return Obiekt ggplot
#' @export
plot_sales_trends <- function(df_metrics) {
  library(ggplot2)
  library(scales)

  ggplot(df_metrics, aes(x = date)) +
    geom_line(aes(y = total_sales, color = "Całkowita sprzedaż"), alpha = 0.5, size = 1) +
    geom_line(aes(y = moving_avg_7d, color = "Średnia krocząca"), size = 1.2) +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
    labs(title = "Długoterminowe Trendy Sprzedażowe", x = "Miesiąc", y = "Sprzedaż Miesięczna", color = "Legenda") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Obrót tekstu
}

#' Funkcja wyższego rzędu (Logika Szeregów Czasowych)
#'
#' @description Łączy obliczanie metryk i wizualizację na wybranych metadanych i w zadanym czasie
#' @param df Dataframe
#' @param p_city Miasto
#' @param p_state Stan
#' @param start_date Data początkowa
#' @param end_date Data końcowa
#' @return Lista zawierająca metryki i wykres
#' @export
sales_ts_logic <- function(df, p_city = NULL, p_state = NULL, start_date = NULL, end_date = NULL) {
  library(dplyr)

  df_filtered <- df
  if(!is.null(p_city)) df_filtered <- df_filtered %>% filter(city == p_city)
  if(!is.null(p_state)) df_filtered <- df_filtered %>% filter(state == p_state)
  if(!is.null(start_date)) df_filtered <- df_filtered %>% filter(date >= as.Date(start_date))
  if(!is.null(end_date)) df_filtered <- df_filtered %>% filter(date <= as.Date(end_date))

  metrics <- compute_sales_metrics(df_filtered)
  plot <- plot_sales_trends(metrics)

  return(list(metrics = metrics, plot = plot))
}

#' Generowanie podsumowania
#'
#' @description Tworzy podsumowanie biznesowe (najlepszy/najgorszy sklep, rosnąca kategoria, itp.)
#' @param df Dataframe
#' @return Lista z informacjami dla menedżera
#' @export
create_management_summary <- function(df) {
  library(dplyr)

  store_summary <- df %>%
    group_by(store_nbr) %>%
    summarise(total_sales = sum(sales, na.rm=TRUE)) %>%
    arrange(desc(total_sales))

  best_store <- head(store_summary, 1)$store_nbr
  worst_store <- tail(store_summary, 1)$store_nbr

  cat_summary <- df %>%
    group_by(family) %>%
    summarise(total_sales = sum(sales, na.rm=TRUE)) %>%
    arrange(desc(total_sales))

  top_category <- head(cat_summary, 1)$family

  list(
    Best_Store = best_store,
    Worst_Store = worst_store,
    Top_Category = top_category
  )
}

#' Tworzenie prognoz z wykorzystaniem ARIMA i Prophet
#'
#' @description Generuje prognozy na podstawie dotychczasowych trendów, używając ARIMA oraz pakietu prophet.
#' @param df Dataframe
#' @param periods Liczba okresów do prognozy
#' @return Lista z modelami
#' @export
create_prognosis <- function(df, periods = 12) {
  library(dplyr)
  library(forecast)
  library(prophet)
  library(lubridate)

  # Przygotowanie danych
  monthly_sales <- df %>%
    group_by(date) %>%
    summarise(y = sum(sales, na.rm = TRUE)) %>%
    arrange(date) %>%
    rename(ds = date)

  # Wyciągnięcie daty startowej
  start_year <- year(min(monthly_sales$ds))
  start_month <- month(min(monthly_sales$ds))

  # ARIMA
  # Ustawienie częstotliwości na 12 miesięcy i zdefiniowanie punktu startowego
  ts_data <- ts(monthly_sales$y, frequency = 12, start = c(start_year, start_month))
  arima_model <- auto.arima(ts_data)
  arima_forecast <- forecast(arima_model, h = periods)

  # PROPHET
  prophet_model <- prophet(monthly_sales, daily.seasonality = FALSE, weekly.seasonality = FALSE, yearly.seasonality = TRUE)
  future <- make_future_dataframe(prophet_model, periods = periods, freq = "month")
  prophet_forecast <- predict(prophet_model, future)

  return(list(
    ARIMA = arima_forecast,
    Prophet = prophet_forecast
  ))
}
