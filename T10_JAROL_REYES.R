library(fpp3)
library(readxl)

# ============================================================================
# CARGA DE DATOS
# ============================================================================

ventas_raw <- read_csv("C:/Users/Smart/Downloads/train (2).csv") %>%
  mutate(date = as.Date(date))

# ============================================================================
# DEFINICIÓN DE PRODUCTOS DE INTERÉS
# ============================================================================

producto_A <- "BEAUTY"
producto_B <- "PERSONAL CARE"

# ============================================================================
# IDENTIFICACIÓN DE TIENDAS RELEVANTES
# ============================================================================

# Ventas acumuladas de BEAUTY por tienda
ranking_A <- ventas_raw %>%
  filter(family == producto_A) %>%
  group_by(store_nbr) %>%
  summarise(total_A = sum(sales, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_A))

# Top 5 tiendas en BEAUTY
top5_A <- ranking_A %>% slice_head(n = 5)

# Ventas acumuladas de PERSONAL CARE
ranking_B <- ventas_raw %>%
  filter(family == producto_B) %>%
  group_by(store_nbr) %>%
  summarise(total_B = sum(sales, na.rm = TRUE), .groups = "drop")

# Evaluación conjunta en las top 5 de BEAUTY
ranking_mix <- top5_A %>%
  left_join(ranking_B, by = "store_nbr") %>%
  mutate(
    total_B  = replace_na(total_B, 0),
    total_AB = total_A + total_B
  )

# Tienda que vende ambos productos y maximiza ventas conjuntas
store_mix <- ranking_mix %>%
  filter(total_B > 0) %>%
  slice_max(total_AB, n = 1) %>%
  pull(store_nbr)

# Construcción final de tiendas
if (length(store_mix) > 0 &&
    !(store_mix %in% top5_A$store_nbr[1:4])) {
  stores_finales <- c(top5_A$store_nbr[1:4], store_mix)
} else {
  stores_finales <- top5_A$store_nbr
}

print(stores_finales)

# ============================================================================
# CONSTRUCCIÓN DE LA ESTRUCTURA JERÁRQUICA
# ============================================================================

ts_jerarquica <- ventas_raw %>%
  filter(
    date >= as.Date("2014-01-01"),
    store_nbr %in% stores_finales,
    family %in% c(producto_A, producto_B)
  ) %>%
  as_tsibble(
    key   = c(family, store_nbr),
    index = date
  ) %>%
  fill_gaps(sales = 0) %>%
  aggregate_key(
    family * store_nbr,
    ventas = sum(sales)
  )

# ============================================================================
# INSPECCIÓN VISUAL
# ============================================================================

ts_jerarquica %>%
  filter(!is_aggregated(family), !is_aggregated(store_nbr)) %>%
  ggplot(aes(date, ventas, color = factor(store_nbr))) +
  geom_line(alpha = 0.35) +
  facet_wrap(~family, scales = "free_y", ncol = 1) +
  labs(
    title = "Ventas por tienda y categoría",
    x = "Fecha",
    y = "Ventas",
    color = "Tienda"
  ) +
  theme_minimal()

# ============================================================================
# DEFINICIÓN DE TRAIN / VALIDACIÓN (CRITERIO TEMPORAL 80%–20%)
# ============================================================================

fechas <- ts_jerarquica %>%
  distinct(date) %>%
  arrange(date)

fecha_train <- fechas %>%
  slice(round(nrow(fechas) * 0.8)) %>%
  pull(date)

fecha_validacion <- max(fechas$date)

print(fecha_train)
print(fecha_validacion)

# ============================================================================
# ESPECIFICACIÓN DEL MODELO
# ============================================================================

modelo_stl_ets <- decomposition_model(
  STL(
    sqrt(ventas) ~ season(period = 7) + season(period = 30),
    robust = TRUE
  ),
  ETS(season_adjust ~ season("N"))
)

# ============================================================================
# AJUSTE Y RECONCILIACIÓN
# ============================================================================

ajuste <- ts_jerarquica %>%
  filter(date <= fecha_train) %>%
  fill_gaps(ventas = 0) %>%
  model(base = modelo_stl_ets) %>%
  reconcile(
    bu  = bottom_up(base),
    ols = min_trace(base, method = "ols")
  )

# ============================================================================
# VALIDACIÓN FUERA DE MUESTRA
# ============================================================================

h_val <- as.numeric(fecha_validacion - fecha_train)

pred_val <- ajuste %>% forecast(h = h_val)

pred_val %>%
  filter(is_aggregated(family), is_aggregated(store_nbr)) %>%
  autoplot(
    ts_jerarquica %>% filter(date >= fecha_train - years(1)),
    level = NULL
  ) +
  labs(
    title = "Validación del pronóstico agregado (A + B)",
    x = "Fecha",
    y = "Ventas"
  )

pred_val %>%
  filter(is_aggregated(family), is_aggregated(store_nbr)) %>%
  accuracy(
    data = ts_jerarquica,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(
    rmse = mean(rmse),
    mase = mean(mase)
  )

# ============================================================================
# PRONÓSTICO FINAL (15 DÍAS)
# ============================================================================

ajuste_final <- ts_jerarquica %>%
  model(base = modelo_stl_ets) %>%
  reconcile(bu = bottom_up(base))

pronostico_15 <- ajuste_final %>% forecast(h = "15 days")

pronostico_15 %>%
  filter(is_aggregated(family), is_aggregated(store_nbr)) %>%
  autoplot(
    ts_jerarquica %>% filter(date >= fecha_validacion - months(1))
  ) +
  labs(
    title = "Pronóstico final agregado (15 días)",
    x = "Fecha",
    y = "Ventas"
  )

