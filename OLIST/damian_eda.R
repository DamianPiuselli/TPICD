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

### Retrasos del envio respecto de la fecha estimada

retrasos <- orders %>% 
  mutate(retraso_envio = date(order_delivered_customer_date)-date(order_estimated_delivery_date),
         tiempo_arribo = date(order_estimated_delivery_date)-date(order_purchase_timestamp)) %>%
  select(order_purchase_timestamp,order_estimated_delivery_date, order_delivered_customer_date, retraso_envio,
         tiempo_arribo)

retrasos %>% 
  ggplot(aes(x=retraso_envio))+
  geom_density()

retrasos %>% 
  ggplot(aes(x=retraso_envio))+
  geom_histogram(binwidth = 5)

### Retrasos del envio respecto de la fecha estimada

retrasos %>% 
  ggplot(aes(x=tiempo_arribo))+
  geom_density()

retrasos %>% 
  ggplot(aes(y=tiempo_arribo))+
  geom_boxplot(binwidth = 5)

# -------------------------------------------------------------------------

orders %>% 
  mutate(mes = cut(order_purchase_timestamp ,"month")) %>% 
  group_by(mes) %>% 
  summarise(numero_compras=n()) %>% 
  ggplot(aes(y=numero_compras, x=mes))+
  geom_point()+
  coord_flip()

orders %>% 
  mutate(mes = cut(order_purchase_timestamp ,"month")) %>% 
  group_by(mes) %>% 
  summarise(numero_compras=n()) %>% 
  ggplot(aes(y=numero_compras, x=mes))+
  geom_point()

# Tipo de plataforma de pago

payment %>% 
  ggplot(aes(x=payment_type)) + 
  geom_bar()

payment %>% 
  filter(payment_type=="credit_card") %>%  
  ggplot(aes(x=payment_installments)) + 
  geom_bar()

payment %>% 
  filter(payment_type=="credit_card") %>%
  filter(payment_installments < 11, payment_installments > 0) %>% 
  ggplot(aes(x = as.factor(payment_installments), y=payment_value)) + 
  ylim(c(0, 500))+
  geom_boxplot()

payment %>% 
  filter(payment_type=="credit_card") %>%
  group_by(payment_installments) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

####




