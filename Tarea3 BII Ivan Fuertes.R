library(readxl)
library(dplyr)
library(purrr)
library(tsibble)
library(lubridate)
library(stringr)
library(ggplot2)
library(fpp3)
library(fable)
library(zoo)
library(tidyr)
library(readr)
library(conflicted)

# Usar conflicted para evitar conflictos de funciones
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

setwd("C:/Users/ivanf/Desktop/Octavo/Series Temporales/Trabajo examen")
# Leer archivos
datos <- read_csv("train.csv", 
                  col_types = cols(),      
                  locale = locale(encoding = "UTF-8"))

datos <- datos %>% mutate(date = as.Date(date))

datos

# MIS PRODUCTOS SON CLEANING  ANAD HOME CARE
datos_cleaning <- datos %>% filter(family == 'CLEANING')
datos_homecare <- datos %>% filter(family == 'HOME CARE')

datos_cleaning %>% group_by(store_nbr)

ventas_cleaning_tienda <- datos_cleaning %>%
  group_by(store_nbr) %>%
  summarise(
    ventas_totales_cleaning = sum(sales, na.rm = TRUE)
  ) %>%
  arrange(desc(ventas_totales_cleaning))

top5_cleaning <- ventas_cleaning_tienda %>% slice_head(n = 5)

top5_cleaning

ventas_homecare_tienda <- datos_homecare %>%
  group_by(store_nbr) %>%
  summarise(
    ventas_totales_homecare = sum(sales, na.rm = TRUE)
  ) %>% 
  arrange(desc(ventas_totales_homecare))

  ventas_ambos <- ventas_cleaning_tienda %>%
    inner_join(ventas_homecare_tienda, by = "store_nbr") %>%
    mutate(
      ventas_conjuntas = ventas_totales_cleaning + ventas_totales_homecare
    ) %>%
    arrange(desc(ventas_conjuntas))
  
  top5_con_homecare <- top5_cleaning %>%
    semi_join(ventas_homecare_tienda, by = "store_nbr")
  
  top5_con_homecare
  
  
  mejor_tienda_ambos <- ventas_ambos %>%
    slice_head(n = 1)
  
  mejor_tienda_ambos
  
  tiendas_finales <- top5_cleaning %>%
    slice_head(n = 5) %>%     
    bind_rows(
      top5_con_homecare,# %>% slice_head(n = 1),
      mejor_tienda_ambos
    ) %>%
    distinct(store_nbr, .keep_all = TRUE)
  
tiendas_finales


datos_cleaning_1 <- datos_cleaning %>% filter(store_nbr == 44)

fechas <- datos_cleaning_1 %>%
  distinct(date) %>%
  arrange(date) %>%
  pull(date)

n_dias <- length(fechas)
cut <- floor(0.8 * n_dias)

fecha_corte <- fechas[cut]

tiendas_top <- c(44,45,47,46,48)
familias_ab <- c("CLEANING","HOME CARE")

datos_ab <- datos %>%
  filter(store_nbr %in% tiendas_top, family %in% familias_ab) %>%
  mutate(date = as.Date(date))

map_tiendas <- tibble(
  store_nbr = tiendas_top,
  tienda_id = factor(1:5)
)

datos_ab <- datos_ab %>%
  left_join(map_tiendas, by = "store_nbr")

ts_ab <- datos_ab %>%
  group_by(date, tienda_id, family) %>%
  summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop") %>%
  as_tsibble(index = date, key = c(tienda_id, family)) %>%
  fill_gaps(sales = 0)

gts_ab <- ts_ab %>%
  aggregate_key(
    family * tienda_id,
    sales = sum(sales)
  )

fecha_corte # fecha corte es "2016-09-11"

train <- gts_ab %>% filter_index(. ~ "2016-09-11")
test  <- gts_ab %>% filter_index("2016-09-12" ~ .)

#MODELADO
fit <- train %>%
  model(base = ETS(sales)) %>%
  reconcile(
    bu   = bottom_up(base),
    ols  = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink")
  )
fc_test <- fit %>% 
  forecast(new_data = test)
fc_test %>%
  filter(is_aggregated(tienda_id), is_aggregated(family)) %>%
  autoplot(
    gts_ab %>% filter_index("2016-09-01" ~ .),
    level = NULL
  ) +
  labs(
    title = "Pronóstico TOTAL (CLEANING + HOME CARE)",
    y = "Ventas",
    x = "Fecha"
  )


fc_test %>%
  filter(is_aggregated(tienda_id), !is_aggregated(family)) %>%
  autoplot(
    gts_ab %>% filter_index("2016-09-01" ~ .),
    level = NULL
  ) +
  facet_wrap(vars(family), scales = "free_y") +
  labs(
    title = "Pronóstico por familia (agregado en tiendas)",
    y = "Ventas",
    x = "Fecha"
  )

#grafico por tienda
fc_test %>%
  filter(!is_aggregated(tienda_id), is_aggregated(family)) %>%
  autoplot(
    gts_ab %>% filter_index("2016-09-01" ~ .),
    level = NULL
  ) +
  facet_wrap(vars(tienda_id), scales = "free_y") +
  labs(
    title = "Pronóstico por tienda (CLEANING + HOME CARE)",
    y = "Ventas",
    x = "Fecha"
  )

#AC TOTAL
acc_total <- fc_test %>%
  filter(is_aggregated(tienda_id), is_aggregated(family)) %>%
  accuracy(
    data = gts_ab,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(
    rmse = mean(rmse),
    mase = mean(mase)
  )

acc_total

#AC POR FAMILIA
acc_family <- fc_test %>%
  filter(is_aggregated(tienda_id), !is_aggregated(family)) %>%
  accuracy(
    data = gts_ab,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(
    rmse = mean(rmse),
    mase = mean(mase)
  )
acc_family

#POR TIENDA
acc_tienda <- fc_test %>%
  filter(!is_aggregated(tienda_id), is_aggregated(family)) %>%
  accuracy(
    data = gts_ab,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(
    rmse = mean(rmse),
    mase = mean(mase),
    .groups = "drop"
  )

acc_tienda


#AC TODAS
acc_all <- fc_test %>%
  accuracy(
    data = gts_ab,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(
    rmse = mean(rmse),
    mase = mean(mase)
  )

acc_all


fit_final <- gts_ab %>%
  model(base = ETS(sales)) %>%
  reconcile(
    mint = min_trace(base, method = "mint_shrink")
  )
fc_final <- fit_final %>% forecast(h = "30 days")


ultima_fecha <- max(gts_ab$date)
fecha_inicio_zoom <- as.Date(ultima_fecha - 30)

fc_final %>%
  filter(is_aggregated(tienda_id), is_aggregated(family)) %>%
  autoplot(
    gts_ab %>%
      dplyr::filter(date >= fecha_inicio_zoom),
    level = NULL
  ) +
  labs(
    title = "Pronóstico final TOTAL",
    y = "Ventas",
    x = "Fecha"
  )+
  theme_minimal(base_size = 12)

fc_final %>%
  filter(is_aggregated(tienda_id), !is_aggregated(family)) %>%
  autoplot(
    gts_ab %>%
      dplyr::filter(date >= fecha_inicio_zoom),
    level = NULL
  ) +
  facet_wrap(vars(family), scales = "free_y") +
  labs(
    title = "Pronóstico final por familia",
    y = "Ventas",
    x = "Fecha"
  )+
  theme_minimal(base_size = 12)


fc_final %>%
  filter(!is_aggregated(tienda_id), is_aggregated(family)) %>%
  autoplot(
    gts_ab%>%
      dplyr::filter(date >= fecha_inicio_zoom),
    level = NULL
  ) +
  facet_wrap(vars(tienda_id),scales = "fixed", ncol = 2) +
  labs(
    title = "Pronóstico final por tienda (CLEANING + HOME CARE)",
    y = "Ventas",
    x = "Fecha"
  )+
  theme_minimal(base_size = 12)

