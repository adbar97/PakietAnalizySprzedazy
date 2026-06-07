# Pakiet Analizy Sprzedaży 📊

Celem pakietu jest wsparcie wewnętrznego zespołu analitycznego w firmie handlowej. Narzędzie umożliwia przekształcenie surowych danych sprzedażowych w użyteczne wnioski biznesowe.

Pakiet oferuje kompletny workflow: od wczytania danych, przez ich czyszczenie, aż po zaawansowane prognozowanie z użyciem algorytmów ARIMA i Prophet.

## Instalacja

Pakiet można zainstalować bezpośrednio z GitHuba, korzystając z biblioteki `devtools`:

```r
# install.packages("devtools")
devtools::install_github("adbar97/PakietAnalizySprzedazy")
```

## Przykładowe użycie (Workflow)

Poniżej znajduje się pełny przepływ pracy, pokazujący możliwości analityczne pakietu.

### 1. Wczytanie i walidacja danych

Funkcja load_sales_data() wczytuje dane korzystając z ekosystemu tidyverse. Następnie funkcja validate_sales_ts() weryfikuje jakość danych, sprawdzając m.in. braki, duplikaty, ujemną sprzedaż oraz niespójną częstotliwość dat.  

```r
library(PakietAnalizySprzedazy)

# Wczytanie surowych danych
df_raw <- load_sales_data("train.csv", "stores.csv", "holidays_events.csv")

# Sprawdzenie jakości szeregów czasowych
validate_sales_ts(df_raw)
```

### 2. Czyszczenie i agregacja

Funkcja clean_sales_ts() przygotowuje dane do analizy, obsługując braki i opcjonalnie agregując dane czasowo. Poniżej wykorzystujemy agregację miesięczną w celu wyeliminowania szumu dziennego.

```r
df_clean <- clean_sales_ts(df_raw, agg_level = "month")
```

### 3. Obliczanie metryk i wizualizacja

Funkcja compute_sales_metrics() generuje kluczowe statystyki, takie jak sprzedaż całkowita, udział promocji czy długość pomiędzy szczytami sprzedaży. Wyniki te możemy zwizualizować za pomocą plot_sales_trends()

```r
metrics <- compute_sales_metrics(df_clean)
plot_sales_trends(metrics)
```

### 4. Funkcje zaawansowane i podsumowania biznesowe

Możemy zawęzić analizę do wybranych metadanych (np. konkretnego miasta) wykorzystując funkcję wyższego rzędu sales_ts_logic(). Dla zarządu przewidziano funkcję create_management_summary(), która szybko wskazuje najlepszy sklep i rosnącą kategorię.

```r
# Podsumowanie menedżerskie
summary <- create_management_summary(df_clean)
print(summary)

# Szybka analiza dla miasta Quito
quito_analysis <- sales_ts_logic(df_clean, p_city = "Quito")
print(quito_analysis$plot)
```

### 5. Prognozowanie (ARIMA & Prophet)

Pakiet wspiera predykcję przyszłych trendów na podstawie danych historycznych

```r
# Prognoza na kolejne 12 miesięcy
forecasts <- create_prognosis(df_clean, periods = 12)
plot(forecasts$ARIMA)
```
