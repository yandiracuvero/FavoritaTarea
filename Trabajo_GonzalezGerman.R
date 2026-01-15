library(dplyr)
library(fpp3)
library(lubridate)
library(tsibble)
library(tidyverse)
library(ggplot2)
library(here)
library(tidyr)
library(tseries)
library(stringr) 
#library(progressr)
#handlers(global = TRUE)
#handlers("progress")


#------------------------ Bases de datos ----------------------------------------
#Train, datos productos SEAFOOD y MEATS
here<-here("C:/Users/germa/Desktop/Universidad/Semestre 8/Series temporales/Tarea 14-1-2026")

prod_A <- "SEAFOOD" 
prod_B <- "MEATS"

train<-read.csv(paste0(here,"/train.csv"))%>%
  mutate(date = as.Date(date)) %>%
  filter(family %in% c(prod_A,prod_B))


#-------------------------- Exploracion --------------------------------------


#Revisamos las ventas de los productos
resumen_productos <- train %>%
  group_by(family) %>%
  summarise(
    Total_Ventas = sum(sales, na.rm = TRUE),
    Promedio_Diario = mean(sales, na.rm = TRUE),
    Max_Venta = max(sales, na.rm = TRUE),
    Min_Venta = min(sales[sales > 0], na.rm = TRUE)
  )

print(resumen_productos)


#Como se comportan las ventas
train %>%
  group_by(date, family) %>%
  summarise(sales = sum(sales), .groups = "drop") %>%
  ggplot(aes(x = date, y = sales, color = family)) +
  geom_line() +
  facet_wrap(~family, scales = "free_y", ncol = 1) +
  labs(title = "Ventas totales de SEAFOOD y MEATS ",
       y = "Ventas Totales", x = "Fecha") +
  theme_minimal()


#Tiendas que venden mas Seafood
tiendas_venta_A <- train %>%
  filter(family == prod_A) %>% 
  group_by(store_nbr) %>%
  summarise(ventas_totales_A = sum(sales, na.rm = TRUE)) %>%
  arrange(desc(ventas_totales_A)) %>%
  slice(1:5)

#De las 5 mejores tiendas vemos cuales venden Meats
tiendas_venta_B<-train%>%
  filter(store_nbr %in% tiendas_venta_A$store_nbr)%>% 
  filter(family == prod_B) %>%
  group_by(store_nbr) %>%
  summarise(ventas_totales_B = sum(sales, na.rm = TRUE))

#Las tiendas tambien venden Meats 
print(tiendas_venta_B)


#
tienda_lider_combinada <- train %>%
  filter(family %in% c("SEAFOOD", "MEATS")) %>%
  group_by(store_nbr) %>%
  summarise(ventas_totales_AB = sum(sales, na.rm = TRUE)) %>%
  arrange(desc(ventas_totales_AB)) %>%
  slice(1:5)

tienda_reemplazo <- tienda_lider_combinada %>%
  filter(!store_nbr %in% tiendas_venta_A$store_nbr) %>%
  slice(1)

tiendas_venta_A[5, "store_nbr"] <- tienda_reemplazo$store_nbr
tiendas_venta_A[5, "ventas_totales_A"] <- tienda_reemplazo$ventas_totales_AB


tiendas_venta_A

#-------------------------- MODELIZACION --------------------------------------
#Modelizando las transacciones totales
datos_1 <- train %>%
  filter(family %in% c("SEAFOOD", "MEATS")) %>%
  group_by(date) %>% 
  summarise(ventas = sum(sales, na.rm = TRUE)) %>%
  as_tsibble(index = date)%>%
  fill_gaps() %>%
  mutate(ventas = replace_na(ventas, 0))

datos_1 %>%
  autoplot() +
  labs(title = "Ventas totales de productos SEAFOOD y MEATS",
       y = "Ventas") +
  theme_minimal()

#Box cox
lambda_optimos <- datos_1 %>%
  features(ventas+1, features = guerrero)

print(lambda_optimos)

lambda <- lambda_optimos %>% pull(lambda_guerrero)

#Estacionalidades
pruebas_estacionariedad <- datos_1 %>%
  features(box_cox(ventas+1,lambda), list(
    KPSS = unitroot_kpss,
    N_Diffs = unitroot_nsdiffs,
    Diffs=unitroot_ndiffs,
    ADF_pvalue = function(x){tseries::adf.test(na.omit(x))$p.value},
    PP_pvalue = function(x){tseries::pp.test(na.omit(x))$p.value}
  ))

print(pruebas_estacionariedad) #d=0 , D=0

n_ceros <- sum(datos_1$ventas == 0, na.rm = TRUE)
n_nas <- sum(is.na(datos_1$ventas))


datos_1%>% 
  model(
    Descomposicion = STL(box_cox(ventas+1, lambda))
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Descomposición STL")


datos_1%>%gg_season() 

#ACF
datos_1 %>%
  ACF(box_cox(ventas,lambda), lag_max = 50) %>%
  autoplot() +
  labs(title = "(ACF) de Ventas totales")

## q=0 , Q=0

#PACF
datos_1 %>%
  PACF(box_cox(ventas,lambda), lag_max = 50) %>%
  autoplot() +
  labs(title = "(ACF) de Ventas totales en Logaritmos")

#p=0 P=0...

fecha_corte_1<-datos_1$date[floor(length(datos_1$date)*0.8)]

fit_completo_1 <- datos_1 %>% filter(date<=fecha_corte_1) %>%
model(
  SNAIVE=SNAIVE(box_cox(ventas+1,lambda)),
  Mean=MEAN(box_cox(ventas+1,lambda)),
  Arima_CTE = ARIMA(box_cox(ventas+1,lambda)~ 1 + pdq(0,0,0) + PDQ(0,0,0)), #Nada que hacer si es una cte :)
  ETS = ETS(box_cox(ventas+1,lambda))
) 

resultados_ljung_box <- fit_completo_1 %>%
  augment() %>%
  features(.innov, ljung_box, lag = 14, dof = 0) %>%
  filter(lb_pvalue >= 0.05) %>%
  arrange(lb_pvalue)# Ordenar de peor a "menos peor"

print(resultados_ljung_box)

fit_completo_1%>% select(SNAIVE)%>%gg_tsresiduals()

test_set <- datos_1 %>% 
  filter(date > fecha_corte_1)

fc_test <- fit_completo_1 %>%
  forecast(new_data = test_set)

fc_test_corregido <- fc_test %>%
  mutate(.mean = median(ventas))

print(head(fc_test_corregido))

##Accuracy
df_fc <- fc_test_corregido %>% as_tibble() %>% select(date, .model, .mean)
df_real <- datos_1 %>% as_tibble() %>% select(date, ventas_real = ventas)

comparativa <- df_fc %>%
  inner_join(df_real, by = "date")

metricas_manuales <- comparativa %>%
    group_by(.model) %>%
    summarise(
      N = n(),
      # Error = Real - Pronóstico (Mediana)
      MSE = mean((ventas_real - .mean)^2, na.rm = TRUE),
      RMSE = sqrt(MSE),
      MAE = mean(abs(ventas_real - .mean), na.rm = TRUE)
    ) %>%
    arrange(RMSE)
  
print(metricas_manuales)

modelo_final<-"Snaive"

fit_f<-datos_1 %>%
  model(
    Snaive = SNAIVE(box_cox(ventas+1,lambda))
  ) 

fc_f<-fit_f%>% forecast(h=30)

df_pronostico <- fc_f %>%
  mutate(.mean = median(ventas)) %>%
  as_tibble() %>%
  select(date, .mean)

datos_1 %>%
  filter(date > "2017-04-01") %>% 
  ggplot(aes(x = date, y = ventas)) +
  geom_line(aes(color = "Historia"), size = 1) +
  geom_line(data = df_pronostico, 
            aes(y = .mean, color = "Pronostico"), 
            linetype = "dashed", size = 1, alpha = 0.8) +
  scale_color_manual(values = c("Historia" = "black", "Pronostico" = "blue")) +
  labs(title = "Pronóstico SNaive vs Realidad",
       subtitle = "Visualización manual de la Mediana",
       y = "Ventas", x = "Fecha", color = "Leyenda") +
  theme_minimal() +
  theme(legend.position = "bottom")

## Usando bottom-up y top-down.
datos <- train %>% 
  filter(family %in% c("SEAFOOD", "MEATS")) %>%
  as_tsibble(index = date, key = c(store_nbr, family)) %>%
  fill_gaps() %>%
  mutate(sales = replace_na(sales, 0)) %>%
  aggregate_key(store_nbr / family, sales = sum(sales))


fit_completo <- datos %>%
    model(
      base = SNAIVE(sales) 
    ) %>%
    reconcile(
      bu = bottom_up(base), 
      td = top_down(base, method = "forecast_proportions") 
    )

fc_completo <- fit_completo %>% 
  forecast(h=30)

fc_completo %>%
  filter(.model == "bu") %>%
  filter(is_aggregated(store_nbr) & is_aggregated(family)) %>%
  autoplot(datos%>%filter(date > "2017-04-01")) + 
  labs(title = "Pronóstico bottom up",
       y = "Ventas Totales",
       x = "Fecha") +
  theme_minimal()

