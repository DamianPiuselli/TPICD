library(tidyverse)
library(modelr)

### Carga de los datos

setwd("D:/documents/UNSAM/INTRO A DATOS/tpfinal") # Cambiar al directorio en el que tengan el script/datos

orders <- read_csv("olist_orders_dataset.csv")
order_items <- read_csv("olist_order_items_dataset.csv")
products <- read_csv("olist_products_dataset.csv")
sellers <- read_csv("olist_sellers_dataset.csv")
items <- read_csv("olist_order_items_dataset.csv")
payment <- read_csv("olist_order_payments_dataset.csv")
customers <- read_csv("olist_customers_dataset.csv")
geolocalation <- read_csv("olist_geolocation_dataset.csv")


### Modelos para prediccion del costo de envio ~ distancia, volumen del paquete, tipo de item?.

#Geolocation tiene multiples valores por zipcode para cada ciudad. Reemplazo las coordenadas con la mediana p/ cada zipcode

geolocalation <- geolocalation %>% 
  group_by(geolocation_zip_code_prefix) %>% 
  summarise(geolocation_lat = median(geolocation_lat),
         geolocation_lng = median(geolocation_lng))

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

# order_items tiene los datos de cada item enviado, incl. costo de envio, precio del item, id del cliente, id del vendedor
# con la id del vendedor/cliente se puede sacar el codigo ZIP de sellers/customers
# con los zip codes y geolocalation se pueden sacar las coordenadas y en funcion de ellas calcular la distancia del envio
# para cada item.

envios <- order_items %>% 
  left_join(orders, by="order_id") %>% 
  left_join(customers, by="customer_id") %>% 
  left_join(sellers, by="seller_id") %>%
  left_join(products, by="product_id") %>% 
  mutate(product_volume = product_length_cm*product_height_cm*product_width_cm) %>% 
  left_join(geolocalation, by=c("customer_zip_code_prefix" = "geolocation_zip_code_prefix")) %>% 
  rename(customer_lat = geolocation_lat) %>% 
  rename(customer_lng = geolocation_lng) %>% 
  left_join(geolocalation, by=c("seller_zip_code_prefix" = "geolocation_zip_code_prefix")) %>% 
  rename(seller_lat = geolocation_lat) %>% 
  rename(seller_lng = geolocation_lng) %>% 
  mutate(distancia_envio = haversine_distance(customer_lat, customer_lng, seller_lat, seller_lng)) %>% 
  filter(freight_value > 0) %>% #filtro envios bonificados
  select(price, freight_value, distancia_envio, customer_state, seller_state, product_category_name, product_volume, product_weight_g)
  
### EDA


median(envios$distancia_envio, na.rm=T) # Mediana de la distancia de envio 431 Km! 
max(envios$distancia_envio, na.rm=T) # Max 8677 Km. Aproximadamente de punta a punta del pais? > ver con los states.
# Tal vez seria interesante ver la combinaciones de envios mas recurrentes y graficarlas en un mapa de brazil.

envios %>% 
  ggplot(aes(y=distancia_envio))+
  geom_boxplot()

envios %>% 
  ggplot(aes(x=distancia_envio, y= freight_value))+
  geom_point(alpha=.1)

envios %>% 
  ggplot(aes(product_volume, y= freight_value))+
  geom_point(alpha=.1)

envios %>% 
  ggplot(aes(product_weight_g, y= freight_value))+
  geom_point(alpha=.1)


### Modelo lineal

mod1 = lm(data=envios, freight_value ~ distancia_envio + product_volume + product_weight_g)
summary(mod1) #R^2 0.555, todas las variables altamente significativas.

envios %>% add_predictions(model=mod1) %>% 
  ggplot(aes(y=pred, x=freight_value))+
  geom_point()
