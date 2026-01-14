library(readxl)
library(fpp3)

train <- read_csv("D:/Productos A y B/train.csv")
#train <- read_csv("train.csv")

mis_tiendas_finales <- c(44, 45, 47, 49, 50)

base_jerarquica <- train %>%
  filter(
    date >= as.Date("2014-01-01"),
    store_nbr %in% mis_tiendas_finales,
    family %in% c("SCHOOL AND OFFICE SUPPLIES", "BOOKS")
  ) %>%
  as_tsibble(
    key   = c(family, store_nbr), index = date
  ) %>%
  # Rellenamos huecos temporales con 0 (CRÍTICO)
  fill_gaps(sales = 0) %>%
  # Jerarquía: Total -> Familia -> Tienda
  aggregate_key(
    family * store_nbr, ventas = sum(sales)
  )

#productos por tienda
base_jerarquica %>%
  # Filtramos para ver solo los datos individuales
  filter(
    #date >= as.Date("2014-01-01"),
    !is_aggregated(family), 
    !is_aggregated(store_nbr)) %>%
  
  ggplot(aes(x = date, y = ventas, color = factor(store_nbr))) +
  geom_line(alpha = 0.4) +
  # Separamos por familia (A y B)
  facet_wrap(~family, scales = "free_y", ncol = 1) +
  labs(
    title = "Desglose por Tienda y Producto",
    x = "Fecha",
    y = "Ventas diarias",
    color = "Tienda #"
  ) +
  theme_minimal()

#### Fechas train - test
# Calcular el número de la fila que representa el 80%
n_corte <- round(nrow(base_jerarquica) * 0.8)
# Extraer la fecha de esa fila específica
fecha_limite <- base_jerarquica |>
  slice(n_corte) |>
  pull(date)
print(fecha_limite)

#### train - test

fecha_corte <- as.Date("2016-04-26")
my_dcmp_spec <- decomposition_model(
  STL(sqrt(ventas) ~ season(period = 7) + season(period = 30), robust = TRUE),
  ETS(season_adjust ~ season("N"))
)

fit <- base_jerarquica %>%
  filter(date <= fecha_corte) %>%
  # Volvemos a asegurar que no existan huecos tras el corte
  fill_gaps(ventas = 0) %>%
  model(
    base = my_dcmp_spec
  ) %>%
  reconcile(
    bu  = bottom_up(base),
    ols = min_trace(base, method = "ols")#,
    #mint = min_trace(base, method = "mint_shrink")
  )

fecha_fin_validacion <- as.Date("2017-08-15")

h_validacion <- as.numeric(fecha_fin_validacion - fecha_corte)

fc <- fit %>%
  forecast(h = h_validacion)

fc %>%
  filter(
    is_aggregated(family), is_aggregated(store_nbr)
  ) %>%
  autoplot(
    base_jerarquica %>%
      filter(date >= as.Date("2016-01-01")), level = NULL
  ) +
  labs(
    title = "Validacion (Total ventas A + B)",  x = "Fecha", y = "Ventas"
  )


fc %>%
  filter(is_aggregated(family), is_aggregated(store_nbr)) %>%
  accuracy(
    data = base_jerarquica,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase))

######## pronostico

fit <- base_jerarquica %>%
  #filter(date <= as.Date("2016-09-11")) %>%  # TRAIN
  model(base = my_dcmp_spec  ) %>%
  reconcile(
    #mint = min_trace(base, method = "mint_shrink"),
    bu = bottom_up(base) #,
    #ols = min_trace(base, method = "ols")
  )

   #Pronosticar 15 días
fc <- fit %>% forecast(h = "15 days")

   #Ver el TOTAL
fc %>%
  filter(is_aggregated(family), is_aggregated(store_nbr)) %>%
  autoplot(base_jerarquica %>% filter(date > as.Date("2017-07-01"))  )


