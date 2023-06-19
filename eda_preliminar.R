library(tidyverse)
library(modelr)
library(ggcorrplot)
library(ggridges)

data <- read_csv("Life-Expectancy-Data-Updated.csv")

glimpse(data) #Categoricas -> Economy_status_Developed, Economy_status_Developing, Region, Country, Year*


### Matriz de correlacion.

#set_plot_dimensions(16,10) #Para setear cuando exportemos graficos a powerpoint.

corr <- round(cor(subset(data, select =-c(Economy_status_Developed, Economy_status_Developing, Region, Country, Year))), 3) 
ggcorrplot(corr,type = "upper", lab = TRUE, outline.color = "black", lab_size = 4, legend.title = "Correlation",
           colors = c("blue", "white", "#0093d5"))+
  ggtitle("Correlation Matrix")+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       




### Distribución de la expectativa de vida

data %>% 
  ggplot(aes(x=Life_expectancy))+
  geom_histogram(aes(y= ..density..), color = "black", fill = "#0093d5")+
  labs(x = 'Expectativa de vida [Años]', y = 'Densidad', 
       title = 'Distribución de la expectativa de vida',
       subtitle = 'Expresado en densidad')+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       

       #Por ahora colores std, despues elegimos paleta de colore

## comparando desarrollado vs sub

data %>% 
  ggplot(aes(x=Life_expectancy, fill=as.factor(Economy_status_Developed)))+
  geom_density(alpha=0.6)+
  labs(x = 'Expectativa de vida [Años]', y = 'Densidad',
       title = 'Distribución de la expectativa de vida',
       subtitle = 'Se discrimina por paises desarrollados y no')+
  guides(fill = guide_legend(title = "Desarrollado"))+
  scale_fill_hue(labels = c('si','no'))+
  scale_fill_manual( values = c("#0093d5","blue"))+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       


data %>%   
  ggplot(aes(y=Life_expectancy, fill=as.factor(Economy_status_Developed)))+
  geom_boxplot()+
  labs(y = 'Expectativa de vida [Años]',
       title = 'Distribución de la expectativa de vida',
       subtitle = 'Se discrimina por paises desarrollados y no')+
  guides(fill = guide_legend(title = "Desarrollado"))+
  scale_fill_hue(labels = c('si','no'))+
  scale_fill_manual( values = c("#0093d5","blue"))+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       

  
  #Creo que queda mejor un boxplot.


## Comparando por Region

data %>% 
  ggplot(aes(x=Life_expectancy, y=as.factor(Region)))+
  geom_density_ridges2(rel_min_height = 0.001, alpha=0.7, color = "black", fill= "#0093d5")+
  labs(x = 'Expectativa de vida [Años]', y = 'Región',
  title = 'Distribución de la expectativa de vida',
  subtitle = 'Se discrimina por región')+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       

  
#Las distribuciones Bimodales son interesantes, indicaria heterogeneidad entre las distintas regiones
#America del norte, america del sur distribuciones mas unimodales. Oceania/Africa mas bimodales.



# Factores economicos -----------------------------------------------------


## GDP per capita

data %>% 
  ggplot(aes(y = Life_expectancy, x = log_GDP, color = as.factor(Economy_status_Developed))) +
  geom_point(alpha = 0.3) +
  labs(x = 'log(GDP) [U$D]', y = 'Expectativa de vida [Años]',
       title = 'Relación entre el GDP y la expectativa de vida',
       subtitle = 'Se discrimina por países desarrollados y no') +
  guides(color = guide_legend(title = "Desarrollado")) +
  scale_color_hue(labels = c('si', 'no'))+
  scale_color_manual(values = c('#0093d5', 'blue'))+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       

  
#Paises con pocos recursos per capita experimentan un gran cambio en la expectativa de vida al aumentar recursos econom.
#Distribucion logaritmica. A medida que aumenta GDP la pendiente se plancha, como cabria esperar (si no en los paises ricos serian inmortales)
#No se si graficarlo o no con gdp en escala log. Los dos graficos tienen su gracia.
#Esto indica también que para el modelo linear sea prudente usar log(gdp) y no gdp como variable.

## Estado de desarrollo del país (Economy_status_developed)

data %>% 
  ggplot(aes(x = Life_expectancy))+
  geom_histogram(color = "black", fill= "#0093d5")+
  facet_grid(~ Economy_status_Developed, labeller = labeller(Economy_status_Developed = c('0' = "No desarrollado",'1' = "Desarrollado")))+
  labs(x = 'Expectativa de vida [Años]', y = 'Cantidad',
       title = 'Distribución de la expectativa de vida',
       subtitle = 'Se discrimina por paises desarrollados y no')+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       

  

# Vemos claramente una concentración de esperanzas de vida cercana a los 80 años para países desarrollados, mientras que para los que no
# lo son la esperanza de vida está más dispersadan (cambiar 0 y 1 por 'en desarrollo' y 'desarrollado')


# Top 5 países con mayor esperanza de vida: ** menor si sacamos el desc de la func arrange ;)

data %>% 
  group_by(Country) %>%
  summarise(promedio = mean(Life_expectancy)) %>% 
  arrange(desc(promedio)) %>% 
  head(5) %>% 
  ggplot()+
  geom_col(aes(x = Country, y = promedio, fill = Country))+
  coord_flip()


# Al ser casi iguales el gráfico no sirve, pero se pueden utilizar los valores como KPI's en las diapos 

# Analicemos como influye la escolarización en la perspectiva de vida: 

ggplot(data = data)+
  geom_point(aes(x = Schooling, y = Life_expectancy, color = factor(Economy_status_Developed)))+
  labs(x = 'Escolarización [Años]', y = 'Expectativa de vida [Años]',
       title = 'Relación entre los años de escolarización y la exp. de vida',
       subtitle = 'Se discrimina por paises desarrollados y no')+
  guides(color = guide_legend(title = "Desarrollado")) +
  scale_color_hue(labels = c('si', 'no'))+
  scale_color_manual(values = c('#0093d5', 'blue'))+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       


# Vemos una tendencia positiva entre variables, y ademas como era esperable que los paises desarrollados con aquellos con 
# una cantidad de años de escolarizacion mayor 



# Factores de salud -------------------------------------------------------



## Consumo de alcohol: 

ggplot(data = data)+
  geom_point(aes(x = Alcohol_consumption, y = Life_expectancy), color = '#0093d5')+
  labs(x = 'Consumo de alcohol [L/año]', y = 'Expectativa de vida [Años]',
       title = 'Relación entre el consumo de alcohol y la exp. de vida')+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       


# Parece ser que cuanto más alcohol se consume aumenta la expectativa de vida. Utilicemos la variable (Economy_status_developed):

ggplot(data = data)+
  geom_point(aes(x = Alcohol_consumption, y = Life_expectancy, color = factor(Economy_status_Developed)))+
  labs(x = 'Consumo de alcohol [L/año]', y = 'Expectativa de vida [Años]',
       title = 'Relación entre el consumo de alcohol y la exp. de vida',
       subtitle = 'Se discrimina por paises desarrollados y no')+
  guides(color = guide_legend(title = "Desarrollado")) +
  scale_color_hue(labels = c('si', 'no'))+
  scale_color_manual(values = c('#0093d5', 'blue'))+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       



# Ahora vemos claramente que los países desarrollados son los que consumen más alcohol. Sería interesante analizar por región: 

ggplot(data = data)+
  geom_histogram(aes(x = Alcohol_consumption),fill = '#0093d5')+
  facet_grid(~ Region)+
  labs(x = 'Consumo de alcohol [L/Año]', y = 'Cantidad',
  title = 'Distribución del consumo de alcohol por región')


# No suma mucho pero vemos que en Europa es donde se consume más alcohol

# Veamos como las muertes menores a 5 años influye en la expectativa de vida: 

ggplot(data = data)+
  geom_point(aes(x = Under_five_deaths, y = Life_expectancy))

# Vemos una tendencia fuertemente negatíva como era esperable

# Agrupemos en categorias de salud según el BMI:
# PD: saque la info de una pagina de internet

data <- data %>% 
  mutate(estado_nutricional = case_when(
    BMI<18.5 ~ 'bajo_peso',
    BMI >= 18.8 & BMI < 24.9 ~ 'normal',
    BMI >=24.9 & BMI < 29.9 ~ 'sobrepeso',
    BMI >=29.9 ~ 'obesidad'
  ))


# Veamos como se distribuye la expectativa de vida para estas categorias: 

ggplot(data = data)+
  geom_boxplot(aes(x = estado_nutricional , y = Life_expectancy, fill = estado_nutricional))+
  labs(x = 'Estado Nutricional', y = 'Expectativa de vida',
       title = 'Distribución de la expectativa de vida',
       subtitle = 'Se discrimina por BMI')+
  guides(fill = guide_legend(title = "Estado Nutricional"))+
  scale_fill_manual( values = c("lightblue","#0093d5","blue"))+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       


# No es lo que esperariamos chan chan chaaaan

#Damian# El problema con el BMI es que no es un indicador perfecto de salud. Por ejemplo podes tener una dieta de baja calidad, alimentacion infrecuente, o
#baja de peso debido a enfermedades y todas esas podrian ocasionar un BMI que es indistinguible de alguien sano y peso normal.
#Seguramente por eso esa distribucion es mas ancha. 
#Otro variable que confunde es que generalmente para poder tener problemas de obesidad/sobrepeso necesitas que en esa sociedad haya un cierto nivel
# de recursos economicos, es parecido al alcohol.


## Trabajemos con las restantes variables de salud: 

# Veamos como influye la inmunización contra la hepatitis B: 

ggplot(data = data)+
  geom_point(aes(x = Hepatitis_B, y = Life_expectancy), color = '#0093d5')+
  labs(x = 'Inmunización Hepatitis B [%]', y = 'Expectativa de vida [Años]',
       title = 'Relación entre la inmun. contra Hepatitis B y exp. de vida',
       subtitle = 'Inmunización en menores de 1 año')+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       


# Vemos una tendencia positiva entre las variables como era esperable

# Veamos como influyen los casos de sarampion detectados cada 1000 habitantes: 

ggplot(data = data)+
  geom_point(aes(x = Measles , y = Life_expectancy), color = '#0093d5')+
  labs(x = 'Sarampión [Casos/1000 hab]', y = 'Expectativa de vida [Años]',
       title = 'Relación entre casos de Sarampión y exp. de vida',
       subtitle = 'Casos de Sarampión cada 1000 habitantes')+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       


# Parece haber una correlación positiva (NO ESPERABLE)

# Veamos como influyen las muertes por HIV/AIDS en niños menores de 4 años por cada 1000 nacimientos: 

ggplot(data = data)+
  geom_point(aes(x = Incidents_HIV, y = Life_expectancy), color = '#0093d5')+
  labs(x = 'Muertes VIH/SIDA', y = 'Expectativa de vida',
       title = 'Relación entre muertes por VIH/SIDA y expectativa de vida',
       subtitle = 'Muertes por VIH/SIDA para menores de 1 año cada 1000 hab.')+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       

  

# Aquí si vemos una tendencia negativa esperable

# Podemos analizar si los años de escolarización influyen en las muertes por HIV/AIDS: 

ggplot(data = data)+
  geom_point(aes(x = Schooling, y = Incidents_HIV), color = '#0093d5')+
  labs(x = 'Escolarización [Años]', y = 'Muertes VIH/SIDA',
       title = 'Relación entre escolarización y muertes por VIH/SIDA',
       subtitle = 'Muertes por VIH/SIDA para menores de 1 año cada 1000 hab.')+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       


# Hay una extraña cantidad de muertes por HIV/AIDS para paises donde la escolarización conlleva entre 3 y 10 años

# Podemos filtrar el DF y ver quienes son esos países: 

max(data$Incidents_HIV)
min(data$Incidents_HIV)

data %>% 
  filter(Schooling>3 & Schooling<10 & Incidents_HIV>5) %>%
  select(Country, Incidents_HIV, Schooling) %>% 
  group_by(Country) %>% 
  summarise(Incidents_HIV = mean(Incidents_HIV),
            Schooling = mean(Schooling)) %>% 
  arrange(desc(Incidents_HIV))+
  

# Parece ser que hay 8 países que generaban la perturbación en el gráfico
# Para ellos los años de escolarización son bajos y los casos de HIV/AIDS son altos, posiblemente debido a la educación sexual
# si la hay 


# Analizamos la expectativa de vida en relación a la inmunización contra el virus del polio

ggplot(data = data)+
  geom_point(aes(x = Polio , y = Life_expectancy), color = '#0093d5')+
  labs(x = 'Inmunización Polio [%]', y = 'Expectativa de vida [Años]',
       title = 'Relación entre la inmun. Polio y la expec. de vida',
       subtitle = 'Inmunización en menores de 1 año')+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       


# Hay una clara correlación positiva

# Analizamos la expectativa de vida en relación a la inmunizacion contra el virus de la diphteria

ggplot(data = data)+
  geom_point(aes(x = Diphtheria, y = Life_expectancy), color = '#0093d5')+
  labs(x = 'Inmunización Diphtheria [%]', y = 'Expectativa de vida [Años]',
       title = 'Relación entre inmun. Diphtheria y expect. de vida',
       subtitle = 'Inmunización en menores de 1 año')+
  theme_bw() + 
  theme(panel.border = element_rect(fill = "transparent",
                                    color = 4,          
                                    linewidth = 2))       

# Hay una clara correlación positiva entre ambas variables 


#### PARA CONTINUAR CON EL EDA: Agregar lineas de marco, cambiar color fondo (o no...), ver si se nos ocurre algo más
