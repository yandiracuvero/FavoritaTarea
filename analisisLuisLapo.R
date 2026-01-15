# ==============================================================================
# ANÁLISIS Y PREDICCIÓN DE VENTAS: GROCERY I Y GROCERY II
# ==============================================================================
# Este script realiza:
# 1. Identificación de las 5 tiendas que más venden GROCERY I
# 2. Verificación si alguna también vende GROCERY II
# 3. Ajuste de la lista de tiendas si es necesario
# 4. Predicción del total de ventas de ambos productos
# ==============================================================================

# Limpiar entorno
rm(list = ls())
gc()

# ------------------------------------------------------------------------------
# 1. CONFIGURACIÓN Y CARGA DE LIBRERÍAS
# ------------------------------------------------------------------------------

cat(paste0(rep("=", 70), collapse = ""), "\n")
cat("ANÁLISIS DE PRODUCTOS ESPECÍFICOS: GROCERY I Y GROCERY II\n")
cat(paste0(rep("=", 70), collapse = ""), "\n\n")

# Cargar librerías necesarias
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(ggplot2)
  library(gridExtra)
  library(scales)
  library(readr)
})

# Intentar cargar librerías de forecasting
tryCatch({
  library(forecast)
  library(tseries)
  library(zoo)
  cat("✓ Librerías de forecasting cargadas\n")
}, error = function(e) {
  cat("⚠️  Advertencia: Algunas librerías de forecasting no están disponibles\n")
})

# Detectar directorio base del proyecto
detect_base_directory <- function() {
  current_wd <- getwd()
  if (dir.exists(file.path(current_wd, "data"))) {
    return(current_wd)
  }
  
  base_dir <- current_wd
  for (i in 1:5) {
    if (dir.exists(file.path(base_dir, "data"))) {
      return(base_dir)
    }
    base_dir <- dirname(base_dir)
  }
  
  stop("No se pudo detectar el directorio base del proyecto")
}

base_dir <- detect_base_directory()
cat("Directorio base del proyecto:", base_dir, "\n\n")

# Definir rutas
data_path <- file.path(base_dir, "data")
forecast_path <- file.path(data_path, "store-sales-time-series-forecasting")
output_dir <- file.path(base_dir, "analisis_productos_especificos", "output")
figures_dir <- file.path(base_dir, "analisis_productos_especificos", "figures")

# Crear directorios si no existen
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# 2. CARGA DE DATOS
# ------------------------------------------------------------------------------

cat(paste0(rep("=", 70), collapse = ""), "\n")
cat("FASE 1: CARGA DE DATOS\n")
cat(paste0(rep("=", 70), collapse = ""), "\n")

# Cargar datos de entrenamiento
train <- read_csv(
  file.path(forecast_path, "train.csv"),
  col_types = cols(
    id = col_integer(),
    date = col_date(format = ""),
    store_nbr = col_integer(),
    family = col_character(),
    sales = col_double(),
    onpromotion = col_integer()
  )
)

cat("✓ Datos cargados\n")
cat("  - Total de registros:", nrow(train), "\n")
cat("  - Rango de fechas:", min(train$date), "a", max(train$date), "\n")
cat("  - Número de tiendas:", length(unique(train$store_nbr)), "\n")
cat("  - Familias de productos:", length(unique(train$family)), "\n\n")

# ------------------------------------------------------------------------------
# 3. IDENTIFICACIÓN DE TIENDAS TOP PARA GROCERY I
# ------------------------------------------------------------------------------

cat(paste0(rep("=", 70), collapse = ""), "\n")
cat("FASE 2: IDENTIFICACIÓN DE TIENDAS TOP\n")
cat(paste0(rep("=", 70), collapse = ""), "\n")

# Productos objetivo
PRODUCTO_A <- "GROCERY I"
PRODUCTO_B <- "GROCERY II"

cat("\n📦 Productos a analizar:\n")
cat("  - Producto A:", PRODUCTO_A, "\n")
cat("  - Producto B:", PRODUCTO_B, "\n\n")

# Calcular ventas totales por tienda para GROCERY I
ventas_grocery_i <- train %>%
  filter(family == PRODUCTO_A) %>%
  group_by(store_nbr) %>%
  summarise(
    ventas_totales = sum(sales, na.rm = TRUE),
    ventas_promedio = mean(sales, na.rm = TRUE),
    dias_con_ventas = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(ventas_totales))

cat("Top 10 tiendas que más venden GROCERY I:\n")
print(head(ventas_grocery_i, 10))

# Seleccionar las 5 tiendas top
top_5_tiendas <- head(ventas_grocery_i, 5)$store_nbr
cat("\n✓ Top 5 tiendas seleccionadas:", paste(top_5_tiendas, collapse = ", "), "\n\n")

# ------------------------------------------------------------------------------
# 4. VERIFICACIÓN DE VENTAS DE GROCERY II EN LAS TIENDAS TOP
# ------------------------------------------------------------------------------

cat(paste0(rep("=", 70), collapse = ""), "\n")
cat("FASE 3: VERIFICACIÓN DE VENTAS DE GROCERY II\n")
cat(paste0(rep("=", 70), collapse = ""), "\n")

# Verificar si las tiendas top también venden GROCERY II
ventas_grocery_ii <- train %>%
  filter(family == PRODUCTO_B) %>%
  group_by(store_nbr) %>%
  summarise(
    ventas_totales = sum(sales, na.rm = TRUE),
    ventas_promedio = mean(sales, na.rm = TRUE),
    dias_con_ventas = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(ventas_totales))

cat("\nTop 10 tiendas que más venden GROCERY II:\n")
print(head(ventas_grocery_ii, 10))

# Verificar intersección
tiendas_que_venden_ambos <- intersect(top_5_tiendas, ventas_grocery_ii$store_nbr)

cat("\n📊 Análisis de intersección:\n")
cat("  - Tiendas top 5 de GROCERY I:", paste(top_5_tiendas, collapse = ", "), "\n")
cat("  - Tiendas que también venden GROCERY II:", 
    ifelse(length(tiendas_que_venden_ambos) > 0, 
           paste(tiendas_que_venden_ambos, collapse = ", "), 
           "NINGUNA"), "\n")

# Calcular ventas combinadas (A + B) para todas las tiendas
ventas_combinadas <- train %>%
  filter(family %in% c(PRODUCTO_A, PRODUCTO_B)) %>%
  group_by(store_nbr, family) %>%
  summarise(ventas_totales = sum(sales, na.rm = TRUE), .groups = "drop") %>%
  group_by(store_nbr) %>%
  summarise(
    ventas_grocery_i = sum(ventas_totales[family == PRODUCTO_A], na.rm = TRUE),
    ventas_grocery_ii = sum(ventas_totales[family == PRODUCTO_B], na.rm = TRUE),
    ventas_combinadas = sum(ventas_totales, na.rm = TRUE),
    vende_ambos = n_distinct(family) == 2,
    .groups = "drop"
  ) %>%
  filter(vende_ambos == TRUE) %>%
  arrange(desc(ventas_combinadas))

cat("\n📈 Tiendas que venden AMBOS productos (ordenadas por ventas combinadas):\n")
if (nrow(ventas_combinadas) > 0) {
  print(head(ventas_combinadas, 10))
  
  # Seleccionar la mejor tienda que vende ambos
  mejor_tienda_ambos <- ventas_combinadas$store_nbr[1]
  cat("\n✓ Mejor tienda que vende ambos productos:", mejor_tienda_ambos, "\n")
  cat("  - Ventas GROCERY I:", ventas_combinadas$ventas_grocery_i[1], "\n")
  cat("  - Ventas GROCERY II:", ventas_combinadas$ventas_grocery_ii[1], "\n")
  cat("  - Ventas combinadas:", ventas_combinadas$ventas_combinadas[1], "\n")
} else {
  cat("⚠️  No hay tiendas que vendan ambos productos\n")
  mejor_tienda_ambos <- NULL
}

# Ajustar lista de tiendas si es necesario
tiendas_finales <- top_5_tiendas

if (!is.null(mejor_tienda_ambos) && !mejor_tienda_ambos %in% top_5_tiendas) {
  cat("\n🔄 Ajustando lista: Reemplazando tienda #5 (", top_5_tiendas[5], 
      ") por tienda #", mejor_tienda_ambos, " (mejor en ambos productos)\n")
  tiendas_finales[5] <- mejor_tienda_ambos
} else if (!is.null(mejor_tienda_ambos) && mejor_tienda_ambos %in% top_5_tiendas) {
  cat("\n✓ La mejor tienda que vende ambos productos ya está en el top 5\n")
} else {
  cat("\n⚠️  No se encontró tienda que venda ambos productos. Usando top 5 original.\n")
}

cat("\n✓ Tiendas finales seleccionadas:", paste(tiendas_finales, collapse = ", "), "\n\n")

# Guardar información de tiendas seleccionadas
tiendas_info <- data.frame(
  tienda = tiendas_finales,
  rank = 1:5,
  ventas_grocery_i = sapply(tiendas_finales, function(s) {
    ventas_grocery_i$ventas_totales[ventas_grocery_i$store_nbr == s]
  }),
  vende_grocery_ii = sapply(tiendas_finales, function(s) {
    s %in% ventas_grocery_ii$store_nbr
  })
)

write_csv(tiendas_info, file.path(output_dir, "tiendas_seleccionadas.csv"))
cat("✓ Información de tiendas guardada en:", file.path(output_dir, "tiendas_seleccionadas.csv"), "\n\n")

# ------------------------------------------------------------------------------
# 5. PREPARACIÓN DE DATOS PARA PREDICCIÓN
# ------------------------------------------------------------------------------

cat(paste0(rep("=", 70), collapse = ""), "\n")
cat("FASE 4: PREPARACIÓN DE DATOS PARA PREDICCIÓN\n")
cat(paste0(rep("=", 70), collapse = ""), "\n")

# Filtrar datos de las tiendas seleccionadas y productos objetivo
datos_analisis <- train %>%
  filter(
    store_nbr %in% tiendas_finales,
    family %in% c(PRODUCTO_A, PRODUCTO_B)
  ) %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    day_of_week = wday(date, label = TRUE),
    is_weekend = day_of_week %in% c("Sat", "Sun"),
    onpromotion = as.logical(onpromotion)
  )

# Agregar ventas totales por día (sumando todas las tiendas)
ventas_diarias <- datos_analisis %>%
  group_by(date, family) %>%
  summarise(
    ventas_totales = sum(sales, na.rm = TRUE),
    total_promociones = sum(onpromotion, na.rm = TRUE),
    num_tiendas = n_distinct(store_nbr),
    .groups = "drop"
  ) %>%
  arrange(date, family)

cat("\n✓ Datos preparados para análisis\n")
cat("  - Total de días:", length(unique(ventas_diarias$date)), "\n")
cat("  - Rango de fechas:", min(ventas_diarias$date), "a", max(ventas_diarias$date), "\n")
cat("  - Registros por producto:\n")
print(ventas_diarias %>% group_by(family) %>% summarise(n = n(), .groups = "drop"))

# Resumen estadístico
cat("\n📊 Resumen estadístico de ventas diarias:\n")
resumen_ventas <- ventas_diarias %>%
  group_by(family) %>%
  summarise(
    ventas_totales = sum(ventas_totales),
    ventas_promedio = mean(ventas_totales),
    ventas_mediana = median(ventas_totales),
    ventas_sd = sd(ventas_totales),
    ventas_min = min(ventas_totales),
    ventas_max = max(ventas_totales),
    .groups = "drop"
  )
print(resumen_ventas)

write_csv(resumen_ventas, file.path(output_dir, "resumen_ventas.csv"))
write_csv(ventas_diarias, file.path(output_dir, "ventas_diarias.csv"))

cat("\n✓ Datos guardados en:", output_dir, "\n\n")

# ------------------------------------------------------------------------------
# 6. ANÁLISIS EXPLORATORIO Y VISUALIZACIONES
# ------------------------------------------------------------------------------

cat(paste0(rep("=", 70), collapse = ""), "\n")
cat("FASE 5: ANÁLISIS EXPLORATORIO\n")
cat(paste0(rep("=", 70), collapse = ""), "\n")

# Gráfico 1: Serie temporal de ventas (por producto)
for (prod in c(PRODUCTO_A, PRODUCTO_B)) {
  datos_prod <- ventas_diarias %>% filter(family == prod)
  
  p1 <- datos_prod %>%
    ggplot(aes(x = date, y = ventas_totales)) +
    geom_line(color = ifelse(prod == PRODUCTO_A, "#1f77b4", "#ff7f0e"), alpha = 0.7) +
    geom_smooth(method = "loess", se = TRUE, span = 0.1, color = "red") +
    labs(
      title = paste("Serie Temporal de Ventas:", prod),
      subtitle = paste("Tiendas:", paste(tiendas_finales, collapse = ", ")),
      x = "Fecha",
      y = "Ventas Totales"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  nombre_archivo <- paste0("01_serie_temporal_ventas_", 
                           tolower(gsub(" ", "_", prod)), ".png")
  ggsave(file.path(figures_dir, nombre_archivo), 
         p1, width = 12, height = 6, dpi = 300)
  cat("✓ Gráfico guardado:", nombre_archivo, "\n")
}

# Gráfico 2: Distribución de ventas (por producto)
for (prod in c(PRODUCTO_A, PRODUCTO_B)) {
  datos_prod <- ventas_diarias %>% filter(family == prod)
  
  p2 <- datos_prod %>%
    ggplot(aes(x = ventas_totales)) +
    geom_histogram(fill = ifelse(prod == PRODUCTO_A, "#1f77b4", "#ff7f0e"), 
                   alpha = 0.7, bins = 50) +
    labs(
      title = paste("Distribución de Ventas Diarias:", prod),
      x = "Ventas Totales",
      y = "Frecuencia"
    ) +
    theme_minimal()
  
  nombre_archivo <- paste0("02_distribucion_ventas_", 
                           tolower(gsub(" ", "_", prod)), ".png")
  ggsave(file.path(figures_dir, nombre_archivo), 
         p2, width = 12, height = 6, dpi = 300)
  cat("✓ Gráfico guardado:", nombre_archivo, "\n")
}

# Gráfico 3: Ventas por día de la semana (por producto)
for (prod in c(PRODUCTO_A, PRODUCTO_B)) {
  datos_prod <- ventas_diarias %>%
    filter(family == prod) %>%
    mutate(day_of_week = wday(date, label = TRUE, week_start = 1)) %>%
    group_by(day_of_week) %>%
    summarise(ventas_promedio = mean(ventas_totales), .groups = "drop")
  
  p3 <- datos_prod %>%
    ggplot(aes(x = day_of_week, y = ventas_promedio)) +
    geom_col(fill = ifelse(prod == PRODUCTO_A, "#1f77b4", "#ff7f0e"), alpha = 0.7) +
    labs(
      title = paste("Ventas Promedio por Día de la Semana:", prod),
      x = "Día de la Semana",
      y = "Ventas Promedio"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  nombre_archivo <- paste0("03_ventas_por_dia_semana_", 
                           tolower(gsub(" ", "_", prod)), ".png")
  ggsave(file.path(figures_dir, nombre_archivo), 
         p3, width = 10, height = 6, dpi = 300)
  cat("✓ Gráfico guardado:", nombre_archivo, "\n")
}

# Gráfico 4: Ventas por mes (por producto)
for (prod in c(PRODUCTO_A, PRODUCTO_B)) {
  datos_prod <- ventas_diarias %>%
    filter(family == prod) %>%
    mutate(month = month(date, label = TRUE)) %>%
    group_by(month) %>%
    summarise(ventas_promedio = mean(ventas_totales), .groups = "drop")
  
  p4 <- datos_prod %>%
    ggplot(aes(x = month, y = ventas_promedio)) +
    geom_col(fill = ifelse(prod == PRODUCTO_A, "#1f77b4", "#ff7f0e"), alpha = 0.7) +
    labs(
      title = paste("Ventas Promedio por Mes:", prod),
      x = "Mes",
      y = "Ventas Promedio"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  nombre_archivo <- paste0("04_ventas_por_mes_", 
                           tolower(gsub(" ", "_", prod)), ".png")
  ggsave(file.path(figures_dir, nombre_archivo), 
         p4, width = 12, height = 6, dpi = 300)
  cat("✓ Gráfico guardado:", nombre_archivo, "\n")
}

cat("✓ Gráficos guardados en:", figures_dir, "\n\n")

# ------------------------------------------------------------------------------
# 6.1. DESCOMPOSICIÓN STL (Seasonal and Trend decomposition using Loess)
# ------------------------------------------------------------------------------

cat(paste0(rep("=", 70), collapse = ""), "\n")
cat("FASE 5.1: DESCOMPOSICIÓN STL\n")
cat(paste0(rep("=", 70), collapse = ""), "\n")

# Función para realizar descomposición STL y generar gráficos
descomposicion_stl <- function(datos, producto) {
  
  cat("\n📊 Descomposición STL para:", producto, "\n")
  
  # Filtrar datos del producto
  datos_producto <- datos %>%
    filter(family == producto) %>%
    arrange(date)
  
  # Crear serie temporal con frecuencia semanal (7 días)
  ts_data <- ts(
    datos_producto$ventas_totales,
    start = c(year(min(datos_producto$date)), yday(min(datos_producto$date))),
    frequency = 7  # Frecuencia semanal para capturar patrones semanales
  )
  
  # Realizar descomposición STL
  tryCatch({
    stl_result <- stl(ts_data, s.window = "periodic", robust = TRUE)
    
    # Extraer componentes
    # Los datos originales son la suma de tendencia + estacional + residual
    datos_originales <- as.numeric(stl_result$time.series[, "trend"] + 
                                   stl_result$time.series[, "seasonal"] + 
                                   stl_result$time.series[, "remainder"])
    
    componentes <- data.frame(
      fecha = datos_producto$date,
      datos = datos_originales,
      tendencia = as.numeric(stl_result$time.series[, "trend"]),
      estacional = as.numeric(stl_result$time.series[, "seasonal"]),
      residual = as.numeric(stl_result$time.series[, "remainder"])
    )
    
    # Crear gráfico de descomposición usando ggplot2
    # Preparar datos en formato largo para facet_wrap
    componentes_long <- componentes %>%
      pivot_longer(cols = c(datos, tendencia, estacional, residual),
                   names_to = "componente",
                   values_to = "valor") %>%
      mutate(componente = factor(componente, 
                                levels = c("datos", "tendencia", "estacional", "residual"),
                                labels = c("Datos", "Tendencia", "Estacional", "Residual")))
    
    p_stl <- componentes_long %>%
      ggplot(aes(x = fecha, y = valor)) +
      geom_line(color = ifelse(producto == PRODUCTO_A, "#1f77b4", "#ff7f0e"), 
                alpha = 0.7) +
      facet_wrap(~componente, ncol = 1, scales = "free_y", 
                 strip.position = "left") +
      labs(
        title = paste("Descomposición STL:", producto),
        subtitle = "Componentes: Datos, Tendencia, Estacional y Residual",
        x = "Fecha",
        y = "Valor"
      ) +
      theme_minimal() +
      theme(
        strip.background = element_blank(),
        strip.placement = "outside",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10)
      )
    
    nombre_archivo <- paste0("05_stl_", tolower(gsub(" ", "_", producto)), ".png")
    ggsave(file.path(figures_dir, nombre_archivo), 
           p_stl, width = 14, height = 10, dpi = 300)
    
    cat("  ✓ Descomposición STL completada\n")
    cat("  ✓ Gráfico guardado:", nombre_archivo, "\n")
    
    return(stl_result)
    
  }, error = function(e) {
    cat("  ✗ Error en descomposición STL:", e$message, "\n")
    return(NULL)
  })
}

# Realizar descomposición STL para ambos productos
stl_grocery_i <- descomposicion_stl(ventas_diarias, PRODUCTO_A)
stl_grocery_ii <- descomposicion_stl(ventas_diarias, PRODUCTO_B)

# ------------------------------------------------------------------------------
# 7. MODELADO Y PREDICCIÓN
# ------------------------------------------------------------------------------

cat(paste0(rep("=", 70), collapse = ""), "\n")
cat("FASE 6: MODELADO Y PREDICCIÓN\n")
cat(paste0(rep("=", 70), collapse = ""), "\n")

# Función para crear serie temporal y hacer predicción
forecast_producto <- function(datos, producto, horizonte = 30) {
  
  cat("\n📦 Modelando:", producto, "\n")
  
  # Filtrar datos del producto
  datos_producto <- datos %>%
    filter(family == producto) %>%
    arrange(date)
  
  # Crear serie temporal
  ts_data <- ts(
    datos_producto$ventas_totales,
    start = c(year(min(datos_producto$date)), yday(min(datos_producto$date))),
    frequency = 365.25
  )
  
  cat("  - Longitud de serie:", length(ts_data), "\n")
  cat("  - Rango:", min(datos_producto$date), "a", max(datos_producto$date), "\n")
  
  # Dividir en entrenamiento y validación (últimos 30 días para validación)
  if (length(ts_data) > horizonte) {
    # Usar índices para dividir la serie
    n_train <- length(ts_data) - horizonte
    train_ts <- ts(ts_data[1:n_train], 
                   start = start(ts_data), 
                   frequency = frequency(ts_data))
    test_ts <- ts(ts_data[(n_train + 1):length(ts_data)], 
                  start = time(ts_data)[n_train + 1], 
                  frequency = frequency(ts_data))
  } else {
    train_ts <- ts_data
    test_ts <- NULL
  }
  
  resultados <- list()
  
  # Modelo 1: Naive
  tryCatch({
    modelo_naive <- naive(train_ts, h = horizonte)
    if (!is.null(test_ts)) {
      accuracy_naive <- accuracy(modelo_naive$mean, test_ts)
    } else {
      accuracy_naive <- NULL
    }
    resultados$naive <- list(
      modelo = modelo_naive,
      accuracy = accuracy_naive,
      forecast = modelo_naive$mean
    )
    cat("  ✓ Naive model completado\n")
  }, error = function(e) {
    cat("  ✗ Error en Naive:", e$message, "\n")
  })
  
  # Modelo 2: Seasonal Naive
  tryCatch({
    modelo_snaive <- snaive(train_ts, h = horizonte)
    if (!is.null(test_ts)) {
      accuracy_snaive <- accuracy(modelo_snaive$mean, test_ts)
    } else {
      accuracy_snaive <- NULL
    }
    resultados$snaive <- list(
      modelo = modelo_snaive,
      accuracy = accuracy_snaive,
      forecast = modelo_snaive$mean
    )
    cat("  ✓ Seasonal Naive model completado\n")
  }, error = function(e) {
    cat("  ✗ Error en Seasonal Naive:", e$message, "\n")
  })
  
  # Modelo 3: ETS (Exponential Smoothing)
  tryCatch({
    modelo_ets <- ets(train_ts)
    forecast_ets <- forecast(modelo_ets, h = horizonte)
    if (!is.null(test_ts)) {
      accuracy_ets <- accuracy(forecast_ets$mean, test_ts)
    } else {
      accuracy_ets <- NULL
    }
    resultados$ets <- list(
      modelo = modelo_ets,
      accuracy = accuracy_ets,
      forecast = forecast_ets$mean,
      forecast_obj = forecast_ets
    )
    cat("  ✓ ETS model completado\n")
  }, error = function(e) {
    cat("  ✗ Error en ETS:", e$message, "\n")
  })
  
  # Modelo 4: ARIMA
  tryCatch({
    modelo_arima <- auto.arima(train_ts, stepwise = TRUE, approximation = TRUE)
    forecast_arima <- forecast(modelo_arima, h = horizonte)
    if (!is.null(test_ts)) {
      accuracy_arima <- accuracy(forecast_arima$mean, test_ts)
    } else {
      accuracy_arima <- NULL
    }
    resultados$arima <- list(
      modelo = modelo_arima,
      accuracy = accuracy_arima,
      forecast = forecast_arima$mean,
      forecast_obj = forecast_arima
    )
    cat("  ✓ ARIMA model completado\n")
  }, error = function(e) {
    cat("  ✗ Error en ARIMA:", e$message, "\n")
  })
  
  # Guardar información sobre el último punto de entrenamiento
  ultima_fecha_entrenamiento <- if (length(ts_data) > horizonte) {
    datos_producto$date[n_train]
  } else {
    max(datos_producto$date)
  }
  
  return(list(
    producto = producto,
    datos = datos_producto,
    ts_data = ts_data,
    resultados = resultados,
    ultima_fecha_entrenamiento = ultima_fecha_entrenamiento,
    n_train = if (length(ts_data) > horizonte) n_train else length(ts_data)
  ))
}

# Aplicar modelos a ambos productos
resultados_grocery_i <- forecast_producto(ventas_diarias, PRODUCTO_A, horizonte = 30)
resultados_grocery_ii <- forecast_producto(ventas_diarias, PRODUCTO_B, horizonte = 30)

# ------------------------------------------------------------------------------
# 8. COMPARACIÓN DE MODELOS Y SELECCIÓN DEL MEJOR
# ------------------------------------------------------------------------------

cat("\n", paste0(rep("=", 70), collapse = ""), "\n")
cat("FASE 7: COMPARACIÓN DE MODELOS\n")
cat(paste0(rep("=", 70), collapse = ""), "\n")

# Función para comparar modelos
comparar_modelos <- function(resultados, producto) {
  
  cat("\n📊 Comparación de modelos para", producto, ":\n")
  
  metricas <- data.frame()
  
  for (nombre_modelo in names(resultados$resultados)) {
    modelo_info <- resultados$resultados[[nombre_modelo]]
    
    if (!is.null(modelo_info$accuracy)) {
      metricas <- rbind(metricas, data.frame(
        modelo = nombre_modelo,
        RMSE = modelo_info$accuracy[1, "RMSE"],
        MAE = modelo_info$accuracy[1, "MAE"],
        MAPE = modelo_info$accuracy[1, "MAPE"]
      ))
    }
  }
  
  if (nrow(metricas) > 0) {
    metricas <- metricas %>% arrange(RMSE)
    print(metricas)
    
    mejor_modelo <- metricas$modelo[1]
    cat("\n✓ Mejor modelo (menor RMSE):", mejor_modelo, "\n")
    
    return(list(metricas = metricas, mejor_modelo = mejor_modelo))
  } else {
    cat("⚠️  No hay métricas disponibles (sin datos de validación)\n")
    return(list(metricas = NULL, mejor_modelo = "ets"))  # Default
  }
}

comparacion_i <- comparar_modelos(resultados_grocery_i, PRODUCTO_A)
comparacion_ii <- comparar_modelos(resultados_grocery_ii, PRODUCTO_B)

# Guardar métricas
if (!is.null(comparacion_i$metricas)) {
  write_csv(comparacion_i$metricas, 
            file.path(output_dir, "metricas_modelos_grocery_i.csv"))
}
if (!is.null(comparacion_ii$metricas)) {
  write_csv(comparacion_ii$metricas, 
            file.path(output_dir, "metricas_modelos_grocery_ii.csv"))
}

# ------------------------------------------------------------------------------
# 9. GENERACIÓN DE PRONÓSTICOS FINALES
# ------------------------------------------------------------------------------

cat("\n", paste0(rep("=", 70), collapse = ""), "\n")
cat("FASE 8: PRONÓSTICOS FINALES\n")
cat(paste0(rep("=", 70), collapse = ""), "\n")

# Obtener pronósticos del mejor modelo para cada producto
obtener_pronostico <- function(resultados, mejor_modelo) {
  if (mejor_modelo %in% names(resultados$resultados)) {
    modelo_info <- resultados$resultados[[mejor_modelo]]
    
    if (!is.null(modelo_info$forecast_obj)) {
      return(modelo_info$forecast_obj)
    } else if (!is.null(modelo_info$forecast)) {
      # Crear objeto forecast básico
      return(list(
        mean = modelo_info$forecast,
        lower = modelo_info$forecast * 0.9,
        upper = modelo_info$forecast * 1.1
      ))
    }
  }
  return(NULL)
}

pronostico_i <- obtener_pronostico(resultados_grocery_i, comparacion_i$mejor_modelo)
pronostico_ii <- obtener_pronostico(resultados_grocery_ii, comparacion_ii$mejor_modelo)

# Obtener fechas del último punto de entrenamiento para cada producto
ultima_fecha_i <- resultados_grocery_i$ultima_fecha_entrenamiento
ultima_fecha_ii <- resultados_grocery_ii$ultima_fecha_entrenamiento

# Crear fechas futuras desde el último punto de entrenamiento
fechas_futuras_i <- seq(ultima_fecha_i + 1, by = "day", length.out = 30)
fechas_futuras_ii <- seq(ultima_fecha_ii + 1, by = "day", length.out = 30)

# Preparar pronósticos para guardar
if (!is.null(pronostico_i)) {
  pronosticos_i <- data.frame(
    fecha = fechas_futuras_i,
    producto = PRODUCTO_A,
    pronostico = as.numeric(pronostico_i$mean),
    lower_80 = if (!is.null(pronostico_i$lower)) as.numeric(pronostico_i$lower[, 1]) else NA,
    upper_80 = if (!is.null(pronostico_i$upper)) as.numeric(pronostico_i$upper[, 1]) else NA,
    lower_95 = if (!is.null(pronostico_i$lower) && ncol(pronostico_i$lower) > 1) 
      as.numeric(pronostico_i$lower[, 2]) else NA,
    upper_95 = if (!is.null(pronostico_i$upper) && ncol(pronostico_i$upper) > 1) 
      as.numeric(pronostico_i$upper[, 2]) else NA
  )
} else {
  pronosticos_i <- NULL
}

if (!is.null(pronostico_ii)) {
  pronosticos_ii <- data.frame(
    fecha = fechas_futuras_ii,
    producto = PRODUCTO_B,
    pronostico = as.numeric(pronostico_ii$mean),
    lower_80 = if (!is.null(pronostico_ii$lower)) as.numeric(pronostico_ii$lower[, 1]) else NA,
    upper_80 = if (!is.null(pronostico_ii$upper)) as.numeric(pronostico_ii$upper[, 1]) else NA,
    lower_95 = if (!is.null(pronostico_ii$lower) && ncol(pronostico_ii$lower) > 1) 
      as.numeric(pronostico_ii$lower[, 2]) else NA,
    upper_95 = if (!is.null(pronostico_ii$upper) && ncol(pronostico_ii$upper) > 1) 
      as.numeric(pronostico_ii$upper[, 2]) else NA
  )
} else {
  pronosticos_ii <- NULL
}

# Combinar pronósticos
if (!is.null(pronosticos_i) && !is.null(pronosticos_ii)) {
  pronosticos_finales <- rbind(pronosticos_i, pronosticos_ii)
  
  # Calcular totales (suma de ambos productos)
  pronosticos_totales <- pronosticos_finales %>%
    group_by(fecha) %>%
    summarise(
      pronostico_total = sum(pronostico, na.rm = TRUE),
      lower_80_total = sum(lower_80, na.rm = TRUE),
      upper_80_total = sum(upper_80, na.rm = TRUE),
      lower_95_total = sum(lower_95, na.rm = TRUE),
      upper_95_total = sum(upper_95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(producto = "TOTAL (A + B)")
  
  # Guardar pronósticos
  write_csv(pronosticos_finales, 
            file.path(output_dir, "pronosticos_por_producto.csv"))
  write_csv(pronosticos_totales, 
            file.path(output_dir, "pronosticos_totales.csv"))
  
  cat("\n✓ Pronósticos guardados:\n")
  cat("  - Por producto:", file.path(output_dir, "pronosticos_por_producto.csv"), "\n")
  cat("  - Totales:", file.path(output_dir, "pronosticos_totales.csv"), "\n")
  
  # Resumen de pronósticos
  cat("\n📊 Resumen de pronósticos (próximos 30 días):\n")
  cat("\nPor producto:\n")
  print(pronosticos_finales %>% 
        group_by(producto) %>% 
        summarise(
          pronostico_promedio = mean(pronostico),
          pronostico_total = sum(pronostico),
          .groups = "drop"
        ))
  
  cat("\nTotal combinado (A + B):\n")
  print(pronosticos_totales %>% 
        summarise(
          pronostico_promedio = mean(pronostico_total),
          pronostico_total = sum(pronostico_total)
        ))
  
} else {
  cat("⚠️  No se pudieron generar todos los pronósticos\n")
}

# ------------------------------------------------------------------------------
# 10. VISUALIZACIÓN DE PRONÓSTICOS
# ------------------------------------------------------------------------------

cat("\n", paste0(rep("=", 70), collapse = ""), "\n")
cat("FASE 9: VISUALIZACIÓN DE PRONÓSTICOS\n")
cat(paste0(rep("=", 70), collapse = ""), "\n")

# Gráfico de pronósticos para GROCERY I
if (!is.null(pronostico_i)) {
  p5 <- autoplot(pronostico_i) +
    labs(
      title = paste("Pronóstico de Ventas:", PRODUCTO_A),
      subtitle = paste("Modelo:", comparacion_i$mejor_modelo, "| Tiendas:", 
                       paste(tiendas_finales, collapse = ", ")),
      x = "Fecha",
      y = "Ventas Totales"
    ) +
    theme_minimal()
  
  ggsave(file.path(figures_dir, "05_pronostico_grocery_i.png"), 
         p5, width = 12, height = 6, dpi = 300)
  cat("✓ Gráfico de pronóstico GROCERY I guardado\n")
}

# Gráfico de pronósticos para GROCERY II
if (!is.null(pronostico_ii)) {
  p6 <- autoplot(pronostico_ii) +
    labs(
      title = paste("Pronóstico de Ventas:", PRODUCTO_B),
      subtitle = paste("Modelo:", comparacion_ii$mejor_modelo, "| Tiendas:", 
                       paste(tiendas_finales, collapse = ", ")),
      x = "Fecha",
      y = "Ventas Totales"
    ) +
    theme_minimal()
  
  ggsave(file.path(figures_dir, "06_pronostico_grocery_ii.png"), 
         p6, width = 12, height = 6, dpi = 300)
  cat("✓ Gráfico de pronóstico GROCERY II guardado\n")
}

# Gráfico combinado de pronósticos (por producto)
if (!is.null(pronosticos_finales)) {
  # Preparar datos históricos para el gráfico
  datos_historicos <- ventas_diarias %>%
    filter(date >= max(date) - 180)  # Últimos 6 meses
  
  # Generar gráfico por producto
  for (prod in c(PRODUCTO_A, PRODUCTO_B)) {
    datos_historicos_prod <- datos_historicos %>% filter(family == prod)
    pronosticos_prod <- pronosticos_finales %>% filter(producto == prod)
    
    # Obtener la fecha del último punto de entrenamiento
    ultima_fecha_entrenamiento <- if (prod == PRODUCTO_A) {
      ultima_fecha_i
    } else {
      ultima_fecha_ii
    }
    
    # Obtener el último valor observado para conectar con el pronóstico
    ultimo_valor <- datos_historicos_prod %>%
      filter(date == ultima_fecha_entrenamiento) %>%
      pull(ventas_totales)
    
    if (nrow(pronosticos_prod) > 0 && nrow(datos_historicos_prod) > 0) {
      # Crear punto de conexión (último valor histórico)
      punto_conexion <- data.frame(
        fecha = ultima_fecha_entrenamiento,
        ventas = ultimo_valor
      )
      
      # Primer punto del pronóstico para conectar
      primer_punto_pronostico <- data.frame(
        fecha = pronosticos_prod$fecha[1],
        ventas = pronosticos_prod$pronostico[1]
      )
      
      p7 <- ggplot() +
        # Datos históricos (solo hasta el último punto de entrenamiento)
        geom_line(data = datos_historicos_prod %>% filter(date <= ultima_fecha_entrenamiento), 
                  aes(x = date, y = ventas_totales),
                  color = ifelse(prod == PRODUCTO_A, "#1f77b4", "#ff7f0e"),
                  linetype = "solid", size = 1) +
        # Datos de validación (si existen, en gris claro)
        geom_line(data = datos_historicos_prod %>% filter(date > ultima_fecha_entrenamiento), 
                  aes(x = date, y = ventas_totales),
                  color = "gray70", linetype = "solid", size = 0.8, alpha = 0.5) +
        # Línea de conexión entre último valor y primer pronóstico
        geom_segment(aes(x = ultima_fecha_entrenamiento, 
                        xend = pronosticos_prod$fecha[1],
                        y = ultimo_valor, 
                        yend = pronosticos_prod$pronostico[1]),
                    color = ifelse(prod == PRODUCTO_A, "#1f77b4", "#ff7f0e"),
                    linetype = "dashed", size = 1.2, alpha = 0.7) +
        # Pronósticos
        geom_line(data = pronosticos_prod, 
                  aes(x = fecha, y = pronostico),
                  color = ifelse(prod == PRODUCTO_A, "#1f77b4", "#ff7f0e"),
                  linetype = "dashed", size = 1.2) +
        geom_ribbon(data = pronosticos_prod, 
                    aes(x = fecha, ymin = lower_80, ymax = upper_80),
                    fill = ifelse(prod == PRODUCTO_A, "#1f77b4", "#ff7f0e"),
                    alpha = 0.2) +
        # Línea vertical en el punto de división
        geom_vline(xintercept = as.numeric(ultima_fecha_entrenamiento), 
                   linetype = "dotted", color = "gray50", size = 0.5) +
        labs(
          title = paste("Pronóstico de Ventas:", prod),
          subtitle = paste("Tiendas:", paste(tiendas_finales, collapse = ", ")),
          x = "Fecha",
          y = "Ventas Totales",
          caption = "Línea sólida: histórico | Línea punteada: pronóstico | Línea gris: validación | Sombra: intervalo 80%"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      
      nombre_archivo <- paste0("07_pronostico_combinado_", 
                               tolower(gsub(" ", "_", prod)), ".png")
      ggsave(file.path(figures_dir, nombre_archivo), 
             p7, width = 14, height = 7, dpi = 300)
      cat("✓ Gráfico guardado:", nombre_archivo, "\n")
    }
  }
}

# Gráfico de pronóstico total
if (!is.null(pronosticos_totales)) {
  # Calcular totales históricos
  datos_totales_historicos <- ventas_diarias %>%
    filter(date >= max(date) - 180) %>%
    group_by(date) %>%
    summarise(ventas_totales = sum(ventas_totales), .groups = "drop")
  
  p8 <- ggplot() +
    geom_line(data = datos_totales_historicos, 
              aes(x = date, y = ventas_totales), 
              color = "black", linetype = "solid", size = 1) +
    geom_line(data = pronosticos_totales, 
              aes(x = fecha, y = pronostico_total), 
              color = "red", linetype = "dashed", size = 1.2) +
    geom_ribbon(data = pronosticos_totales, 
                aes(x = fecha, ymin = lower_80_total, ymax = upper_80_total),
                fill = "red", alpha = 0.2) +
    labs(
      title = "Pronóstico Total de Ventas (GROCERY I + GROCERY II)",
      subtitle = paste("Tiendas:", paste(tiendas_finales, collapse = ", ")),
      x = "Fecha",
      y = "Ventas Totales",
      caption = "Línea negra: histórico | Línea roja: pronóstico | Sombra: intervalo 80%"
    ) +
    theme_minimal()
  
  ggsave(file.path(figures_dir, "08_pronostico_total.png"), 
         p8, width = 14, height = 7, dpi = 300)
  cat("✓ Gráfico de pronóstico total guardado\n")
}

cat("\n✓ Todos los gráficos guardados en:", figures_dir, "\n\n")

# ------------------------------------------------------------------------------
# 11. RESUMEN FINAL
# ------------------------------------------------------------------------------

cat(paste0(rep("=", 70), collapse = ""), "\n")
cat("RESUMEN FINAL\n")
cat(paste0(rep("=", 70), collapse = ""), "\n\n")

cat("✅ ANÁLISIS COMPLETADO\n\n")

cat("📦 Productos analizados:\n")
cat("  - Producto A:", PRODUCTO_A, "\n")
cat("  - Producto B:", PRODUCTO_B, "\n\n")

cat("🏪 Tiendas seleccionadas:\n")
for (i in 1:length(tiendas_finales)) {
  cat(sprintf("  %d. Tienda #%d\n", i, tiendas_finales[i]))
}
cat("\n")

cat("📊 Modelos utilizados:\n")
cat("  - GROCERY I: Mejor modelo =", comparacion_i$mejor_modelo, "\n")
cat("  - GROCERY II: Mejor modelo =", comparacion_ii$mejor_modelo, "\n\n")

cat("📁 Archivos generados:\n")
cat("  - Output:", output_dir, "\n")
cat("  - Figuras:", figures_dir, "\n\n")

cat("🎯 Próximos pasos sugeridos:\n")
cat("  1. Revisar los pronósticos en: pronosticos_totales.csv\n")
cat("  2. Analizar las métricas de los modelos\n")
cat("  3. Validar los pronósticos con datos futuros cuando estén disponibles\n")
cat("  4. Considerar ajustes estacionales si es necesario\n\n")

cat(paste0(rep("=", 70), collapse = ""), "\n")
cat("FIN DEL ANÁLISIS\n")
cat(paste0(rep("=", 70), collapse = ""), "\n")
