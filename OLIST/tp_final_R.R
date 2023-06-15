library(tidyverse)
library(modelr)

orders <- read_csv("C:\\Users\\arsan\\Downloads\\archive (5)\\olist_orders_dataset.csv")
order_items <- read_csv("C:\\Users\\arsan\\Downloads\\archive (5)\\olist_order_items_dataset.csv")
products <- read_csv("C:\\Users\\arsan\\Downloads\\archive (5)\\olist_products_dataset.csv")
sellers <- read_csv("C:\\Users\\arsan\\Downloads\\archive (5)\\olist_sellers_dataset.csv")
items <- read_csv("C:\\Users\\arsan\\Downloads\\archive (5)\\olist_order_items_dataset.csv")
payment <- read_csv("C:\\Users\\arsan\\Downloads\\archive (5)\\olist_order_payments_dataset.csv")
customers <- read_csv("C:\\Users\\arsan\\Downloads\\archive (5)\\olist_customers_dataset.csv")
geolocalation <- read_csv("C:\\Users\\arsan\\Downloads\\archive (5)\\olist_geolocation_dataset.csv")

# ¿¿ Qué podemos hacer ?? Podríamos predecir el costo de envio



# Utilizamos el dataframe de geolocalización para agregar latitud y longitud

# Aproximamos la latitud y longitud dentro de una misma ciudad por el valor de la mediana 

geolocalation <- geolocalation %>% 
  group_by(geolocation_city) %>% 
  summarise(latitude = median(geolocation_lat),
            longitude = median(geolocation_lng))

# Realizamos left_join para agregar valores de LAT y LONG en tablas customers y sellers

customers <- left_join(customers, geolocalation, by = c('customer_city' = 'geolocation_city'))

sellers <- left_join(sellers, geolocalation, by = c('seller_city' = 'geolocation_city'))

# Chequeamos que tan bien se realizó el merge

sum(is.na(customers$latitude)) # Obtuvimos 66 join vacíos

sum(is.na(sellers$latitude)) # Obtuvimos 68 join vacíos

# Fabricamos tablas puente para conectar la tabla customer con la tabla seller

puente_customer_order_item <- data.frame(customer_id = customers$customer_id,
                                    order_id = orders$order_id)

customers <- left_join(customers, puente_customer_order_item, by ='customer_id')

geoloc_customer <- data.frame(order_id = customers$order_id ,
                              customer_latitude = customers$latitude,
                              customer_longitude = customers$longitude)

geoloc_seller <- data.frame(seller_id = sellers$seller_id,
                            seller_latitude = sellers$latitude,
                            seller_longitude = sellers$longitude)

order_items <- order_items %>% 
  left_join(geoloc_customer, by = 'order_id') %>% 
  left_join(geoloc_seller, by = 'seller_id')

# CÁLCULO DE LA DISTANCIA  ------------------------------------------------


# Función para calcular la distancia entre dos puntos en kilómetros utilizando la fórmula del haversine
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Radio de la Tierra en kilómetros
  earth_radius <- 6371
  
  # Convertir las coordenadas a radianes
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Diferencia entre las latitudes y longitudes
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  # Calcular el haversine
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  
  # Calcular la distancia
  distance <- earth_radius * c
  
  return(distance)
}

# Agregamos una columna con la distancia para en la tabla order_items

order_items <-order_items %>% 
  mutate(distance = haversine_distance(customer_latitude,
                                       customer_longitude,
                                       seller_latitude,
                                       seller_longitude))

median(order_items$distance, na.rm = T) # La mediana de la distancia es de 460 km



# EDA: Exploratory Data Analysis ------------------------------------------



ggplot(data = order_items)+
  geom_point(aes(x = distance, y = freight_value),
             alpha = 0.1))

# continuar con el EDA, agregar variables de interes a la tabla order_items
