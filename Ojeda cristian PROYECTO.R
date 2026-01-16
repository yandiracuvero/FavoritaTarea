library(fpp3)
library(readr)
library(dplyr)
library(tidyr)
library(conflicted)

conflicts_prefer(
  fable::ARIMA,
  fabletools::model,
  fabletools::forecast
)

train <- read_csv(
  "train.csv",
  col_types = cols(
    id = col_integer(),
    date = col_date(),
    store_nbr = col_integer(),
    family = col_factor(),
    sales = col_double(),
    onpromotion = col_integer()
  )
)

A <- "AUTOMOTIVE"
B <- "LAWN AND GARDEN"
productos <- c(A, B)

train_ab <- train %>%
  dplyr::filter(family %in% productos)


top5_A <- train_ab %>%
  dplyr::filter(family == A) %>%
  group_by(store_nbr) %>%
  summarise(sales_A = sum(sales, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(sales_A)) %>%
  slice(1:5)

stores_top5 <- top5_A$store_nbr

ventas_AB_top5 <- train_ab %>%
  dplyr::filter(store_nbr %in% stores_top5) %>%
  group_by(store_nbr, family) %>%
  summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = family, values_from = sales, values_fill = 0) %>%
  mutate(
    sales_AB = .data[[A]] + .data[[B]],
    vende_B = .data[[B]] > 0
  )

if (any(ventas_AB_top5$vende_B)) {
  
  store_best_AB <- ventas_AB_top5 %>%
    dplyr::filter(vende_B) %>%
    arrange(desc(sales_AB)) %>%
    slice(1) %>%
    pull(store_nbr)
  
  store_to_replace <- top5_A %>%
    arrange(sales_A) %>%
    slice(1) %>%
    pull(store_nbr)
  
  if (store_best_AB %in% stores_top5) {
    stores_final <- stores_top5
  } else {
    stores_final <- c(
      setdiff(stores_top5, store_to_replace),
      store_best_AB
    )
  }
} else {
  stores_final <- stores_top5
}

stores_final

ts_hier <- train_ab %>%
  dplyr::filter(store_nbr %in% stores_final) %>%
  as_tsibble(index = date, key = c(store_nbr, family)) %>%
  aggregate_key(
    store_nbr / family,
    value = sum(sales, na.rm = TRUE)
  ) %>%
  fill_gaps(value = 0)



models <- ts_hier %>%
  model(arima = ARIMA(value))

models_rec <- models %>%
  reconcile(
    bottom_up = bottom_up(arima),
    top_down  = top_down(arima, method = "forecast_proportions")
  )

fc <- models_rec %>%
  forecast(h = 15)


ts_hier %>%
  dplyr::filter(is_aggregated(family)) %>%
  autoplot(value) +
  labs(
    title = "Ventas jerárquicas: Total y por tienda",
    y = "Ventas"
  ) +
  facet_wrap(vars(store_nbr), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")




fc %>%
  dplyr::filter(
    is_aggregated(store_nbr),
    is_aggregated(family)
  ) %>%
  autoplot(ts_hier) +
  labs(
    title = "Pronóstico total de ventas (15 días)",
    y = "Ventas"
  )

metricas <- accuracy(fc, ts_hier) %>%
  dplyr::filter(
    is_aggregated(store_nbr),
    is_aggregated(family)
  ) %>%
  dplyr::select(.model, RMSE, MAE, MAPE)

metricas


library(tsibble)

predicciones_totales <- fc %>%
  dplyr::filter(
    is_aggregated(store_nbr),
    is_aggregated(family)
  ) %>%
  as_tibble() %>%
  dplyr::rename(
    date = !!tsibble::index_var(fc)
  ) %>%
  dplyr::select(
    .model,
    date,
    ventas_predichas = .mean
  )

predicciones_totales
