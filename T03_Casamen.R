# -- Librerias
library(readr)
library(dplyr)
library(tidyr)
library(fpp3)

# -- Cargar datos
train <- read_csv("train.csv")
str(train)


# -- Analisis de los productos A y B

# Producto A: BEVERAGES
producto_A <- train %>% filter(family == "BEVERAGES") %>% group_by(store_nbr) %>% 
  summarise(ventas_T = sum(sales), .groups="drop") %>%
  arrange(desc(ventas_T))

unique(producto_A$store_nbr)
#Las tiendas que venden mas el producto A son: 44,45,3,47,49


# -- Producto B: LIQUOR,WINE,BEER
producto_B <- train %>% 
  filter(family == "LIQUOR,WINE,BEER") %>%
  group_by(store_nbr) %>%
  summarise(ventas_T = sum(sales), .groups="drop") %>%
  arrange(desc(ventas_T))

#Todos las tiendas del producto A venden el producto B

# --Tiendas que vendan  mas productos A y B a la vez
productos_AB <- train %>% filter(family %in% c("BEVERAGES","LIQUOR,WINE,BEER")) %>%
  group_by(store_nbr,family) %>% summarise(total_sales = sum(sales), .groups="drop") %>%
  pivot_wider(names_from = family, values_from = total_sales,values_fill = 0)%>%
  arrange(desc(BEVERAGES+`LIQUOR,WINE,BEER`))

unique(productos_AB$store_nbr)
#Las tiendas que venden mas productos A y B a la vez son las tiendas 44 3 45 47 49 46

# -- Fechas, Productos y Tiendas con las cuales se va a realizar el análisis
productos <- c("BEVERAGES", "LIQUOR,WINE,BEER")
tiendas   <- c(44, 45, 3, 47, 49)

base_bottom <- train %>%filter(family %in% productos,store_nbr %in% tiendas) %>%
               group_by(date, family, store_nbr) %>%
               summarise(ventas = sum(sales), .groups = "drop")


# -- Transformamos la base a tsibble 
serie_base <- base_bottom %>%as_tsibble(index = date, 
                                        key= c(family, store_nbr)) %>%
              fill_gaps(ventas = 0)

# -- Jerarquía
# Nivel 0: Total (A + B, todas las tiendas)
# Nivel 1: Producto (BEVERAGES, LIQUOR,WINE,BEER)
# Nivel 2: Tienda dentro de producto (5 × 2 = 10 series)

serie_jerarquica <- serie_base %>%aggregate_key(family * store_nbr,ventas = sum(ventas))%>%
                    fill_gaps(ventas = 0)

# -- Datos de entrenamiento y Prueba
# -- Fecha de separación para tomar 80% de entrenamiento y 20% de prueba 
fecha_separacion_tyt <- serie_jerarquica %>%as_tibble() %>%distinct(date) %>%
                        arrange(date) %>%slice(round(n() * 0.8)) %>% pull(date)


entrenamiento <- serie_jerarquica %>%filter(date <= fecha_separacion_tyt)
prueba <- serie_jerarquica %>%filter(date > fecha_separacion_tyt)


# -- Ajuste del Modelo

modelo_ets <- entrenamiento %>% model(ets=ETS(ventas))
modelo_arima <- entrenamiento %>% model(arima=ARIMA(ventas))

ets_bu <- modelo_ets %>% reconcile(bu = bottom_up(ets))
arima_bu <- modelo_arima %>%reconcile(bu = bottom_up(arima))


# -- Pronósticos
fc_ets <- ets_bu %>%forecast(h = "30 days")
fc_ets_productos <- fc_ets %>%filter(
  !is_aggregated(family),is_aggregated(store_nbr))

autoplot(fc_ets_productos,level = NULL) +facet_wrap(~family, scales = "free_y") +
  labs(title = "Pronóstico Bottom-Up por Producto (ETS)",x = "Fecha",y = "Ventas")

fc_arima <- arima_bu %>%forecast(h = "30 days")
fc_arima_productos <- fc_arima %>%filter(
  !is_aggregated(family),is_aggregated(store_nbr))
autoplot(fc_arima_productos,level = NULL) +facet_wrap(~family, scales = "free_y") +
  labs(title = "Pronóstico Bottom-Up por Producto (ARIMA)",x = "Fecha",y = "Ventas")





