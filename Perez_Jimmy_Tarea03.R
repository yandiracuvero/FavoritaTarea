library(fpp3)
library(readr)

# 1) Data
path <- "C:\\Users\\User\\OneDrive - Fundación Janus\\Documentos\\wrk\\epn\\prueba03ST\\bs\\train.csv"
df <- read_csv(path, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

producto_A <- "PREPARED FOODS"
producto_B <- "FROZEN FOODS"

# 2) Top 5 tiendas con más sales de PREPARED FOODS
top_prepared <- df %>%
  filter(family == producto_A) %>%
  group_by(store_nbr) %>%
  summarise(sales_A = sum(sales, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(sales_A)) %>%
  slice_head(n = 5)

cat("\nTOP 5 stores por sales de", producto_A, ":\n")
print(top_prepared)

top5_stores <- top_prepared$store_nbr

# 3) Ventas de esas mismas 5 tiendas, pero para FROZEN FOODS (incluye 0 si no hay)
sales_frozen_top5 <- df %>%
  filter(family == producto_B, store_nbr %in% top5_stores) %>%
  group_by(store_nbr) %>%
  summarise(sales_B = sum(sales, na.rm = TRUE), .groups = "drop") %>%
  right_join(tibble(store_nbr = top5_stores), by = "store_nbr") %>%
  mutate(sales_B = replace_na(sales_B, 0)) %>%
  arrange(match(store_nbr, top5_stores))

cat("\nSales de esas 5 stores pero para", producto_B, ":\n")
print(sales_frozen_top5)

# 4) Store con mayor venta de A y B a la vez
#    (criterio: maximiza el mínimo entre A y B)
best_both <- df %>%
  filter(family %in% c(producto_A, producto_B)) %>%
  group_by(store_nbr, family) %>%
  summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = family, values_from = sales, values_fill = 0) %>%
  rename(sales_A = !!producto_A, sales_B = !!producto_B) %>%
  mutate(balance_score = pmin(sales_A, sales_B),
         sum_score     = sales_A + sales_B) %>%
  arrange(desc(balance_score), desc(sum_score)) %>%
  slice_head(n=5)

best_store <- best_both$store_nbr

cat("\nStore 'mejor en A y B a la vez' (criterio min(A,B) y desempate por A+B):\n")
print(best_both)

# 5) Reemplazar la 5ta del top anterior por esta store (si no está ya en el top 5)
final_stores <- top5_stores

for (i in 1:length(best_store)){
  if (!(best_store[i] %in% final_stores)) {
    final_stores[5] <- best_store[i]
  }
}

cat("\nStores finales elegidas:\n")
print(final_stores)

# 6) Contruir serie temporal
ts_data <- df %>%
  filter(
    store_nbr %in% final_stores,
    family %in% c(producto_A, producto_B)
  ) %>%
  group_by(date, family, store_nbr) %>%
  summarise(
    sales = sum(sales, na.rm = TRUE),
    onpromotion = sum(onpromotion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  #pivot_wider(names_from = family, values_from = sales, values_fill = 0) %>%
  arrange(date)

ventas_gts <- ventas_tsibble %>%
  fill_gaps(sales=0) %>%
  aggregate_key(
    family / store_nbr,  # nivel familia -> nivel tienda
    sales = sum(sales)   # sumar ventas al agregarlas
  )

ventas_gts %>%
  filter(!is_aggregated(family), is_aggregated(store_nbr)) %>%
  autoplot(sales)

# Ajustar modelos ETS, ARIMA y Naive a cada serie de tiempo en la jerarquía
fit_models <- ventas_gts %>%
  model(
    ETS   = ETS(sales),    # modelo ETS automático
    ARIMA = ARIMA(sales),  # modelo ARIMA automático
    Naive = NAIVE(sales)   # modelo ingenuo (pronóstico igual al último valor)
  )

fit_models %>%
  glance() %>%
  arrange(AICc) %>%
  slice(1:6)

glance(fit_models) %>% 
  arrange(.model, AICc) %>% 
  slice(1:6)

h <- 56  

fc_all <- fit_models %>% forecast(h = h)

# Graficar pronósticos para ventas agregadas por familia
fc_all %>%
  filter(is_aggregated(store_nbr), !is_aggregated(family)) %>%
  autoplot(ventas_gts, level = 95) +
  labs(title = "Pronóstico de ventas por familia (agrupado sobre todas las tiendas)",
       y = "Ventas agregadas") +
  facet_wrap(vars(family), scales = "free_y") +
  guides(colour = guide_legend(title = "Modelo"))


# Ajustar modelo ARIMA final a la jerarquía completa y reconciliar pronósticos (bottom-up)
fit_final <- ventas_gts %>%
  model(ARIMA = ARIMA(sales)) %>%
  reconcile(ARIMA_BU = bottom_up(ARIMA))

# Generar pronóstico reconciliado
fc_final <- fit_final %>% forecast(h = h)


# Graficar pronóstico desagregado por tienda (usando modelo ARIMA reconciliado bottom-up)
fc_final %>%
  filter(.model == "ARIMA_BU", !is_aggregated(store_nbr)) %>%
  autoplot(ventas_gts, level = 95, colour = "red") +
  labs(title = "Pronóstico de ventas por tienda (ARIMA con reconciliación bottom-up)",
       y = "Ventas por tienda", x = "Fecha") +
  facet_wrap(vars(store_nbr), scales = "free_y")
