#global escalas

#librerias ----
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(plotly)
library(GGally)
library(openxlsx)
library(ggExtra)
library(scales)
library(ggridges)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
library(DT)
library(reactable)
library(reactablefmtr)
library(sparkline)
library(shinyWidgets)

# datos ----
                                # ACA SE ESCRIBE EL PATH a la base de datos
datos_actual <- read_excel("C:/Users/Ignac/Desktop/PEV Ternium/app_control_limites/base_de_datos_simulados.xlsx")

# preprocesamiento ----
# cambio el nombre a Puesto y PC (ambos tenian el mismo nombre), tambien le cambio el nombre a las variables de interes
datos_actual <- rename(datos_actual, ID = `ID FRANQUICIA`,
                       posicion = `posición`, Q = `CUANTIL DEL NIVEL`,
                       zona = ZONA,
                       nivel = nivel, antig_nivel = `ANTIGÜEDAD DENTRO DEL NIVEL`,
                       rend = `FACTOR RENDIMIENTO`, desemp = `FACTOR DESEMPEÑO`, 
                       facturacion_mensual = `FACTURACIÓN MENSUAL`, facturacion_anual = `FACTURACIÓN ANUAL`,
                       aum_fact = `AUMENTO DE FACTURACIÓN ANUAL`)


# selecciono las variables de interes
data_facturacion <- select(datos_actual, ID, zona, rend, desemp, posicion, nivel, Q, antig_nivel, facturacion_mensual, facturacion_anual, aum_fact)

# remuevo NAs en total_anual_std
data_facturacion <- drop_na(data_facturacion, facturacion_anual)

# solo me quedo con las categoria mayor o iguales a 45
data_facturacion <- filter(data_facturacion, nivel >= 1, nivel <= 14)

# preprocesamiento segunda parte ----
data_facturacion <- mutate(data_facturacion, grupo = ifelse(nivel <= 7, 1,
                                                            ifelse(nivel <= 10, 2, 
                                                                   ifelse(nivel <= 14, 3, 4))))

# paso categoricas a factor (puede ser conveniente o no para algunas cardinales/ordinales como nivel o posicion)
data_facturacion$zona <- as.factor(data_facturacion$zona)
data_facturacion$rend <- as.factor(data_facturacion$rend)
data_facturacion$desemp <- as.factor(data_facturacion$desemp)
data_facturacion$Q <- as.factor(data_facturacion$Q)
data_facturacion$grupo <- as.factor(data_facturacion$grupo)

# para el nivel
nivel_vector <- as.factor(sort(unique(data_facturacion$nivel)))
data_facturacion$nivel <- as.factor(data_facturacion$nivel)

# Exploracion de datos (fue un analisis aparte al desarrollo de la app) ----
# creo nuevas variables del logaritmo natural de los sueldos
data_facturacion <- mutate(data_facturacion,
                           ln_facturacion_mensual = log(facturacion_mensual),
                           ln_facturacion_anual = log(facturacion_anual), 
                           ln_antig_nivel = log(antig_nivel)) #analizo aplicarle el ln a antig_cat


#tipo de boxplot
boxp_sel <- c("Boxplot comparando límites teóricos con estadisticas", 
              "Boxplot mostrando estadisticas y outliers",
              "Boxplot mostrando puntos y tam de muestra")#, "Boxplot mostrando personas por categoria según ancho de cajas")


#zonas de la base de datos
zonas_vector <- c("A", "B", "C", "D", "E")

#opciones globales datatables
options(DT.options = list(language = list(search = 'Buscar:')))

#niveles analizadas en la app
nivel_inicial <- 1
nivel_final <- 14


