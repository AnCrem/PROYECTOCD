# Cargar librerías necesarias
library(tidyverse)
library(readr)

# Establecer el directorio de trabajo
setwd("C:/Users/Iuliu/Documents/PROYECTOCD")

# Definir la ruta del archivo CSV utilizando una ruta relativa
archivo <- "1_Datos/1_Base de datos original ProyectoOCD.csv"

# Leer el archivo CSV sin convertir cadenas de caracteres a factores
datos <- read.csv(archivo, stringsAsFactors = FALSE)

# Leer el archivo CSV sin convertir cadenas de caracteres a factores
datos <- read.csv('1_Base de datos original ProyectoOCD.csv', stringsAsFactors = FALSE)

# Mostrar las primeras filas de la base de datos
print(head(datos))

# Verificar los países y sus códigos en la variable 'cntry'
paises <- unique(datos$cntry)
print(paises)

#### LIMPIEZA DE LOS DATOS ####

# Resumen de la base de datos
print(summary(datos))

# Verificar valores faltantes para cada variable
valores_faltantes <- sapply(datos, function(x) sum(is.na(x)))
print("Valores faltantes por variable:")
print(valores_faltantes)

# Ejemplo de manejo de valores faltantes:
# Reemplazar valores de 999 en 'agea' con NA
datos <- datos %>% mutate(agea = ifelse(agea == 999, NA, agea))

# Reemplazar valores de 77, 88, 99 en 'ccrdprs' y 'wrclmch' con NA y convertir a factor ordenado
datos <- datos %>% mutate(
  ccrdprs = ifelse(ccrdprs %in% c(77, 88, 99), NA, ccrdprs),
  wrclmch = ifelse(wrclmch %in% c(77, 88, 99), NA, wrclmch),
  ccrdprs = factor(ccrdprs, ordered = TRUE, levels = 0:10, labels = c("Not at all", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A lot")),
  wrclmch = factor(wrclmch, ordered = TRUE, levels = 1:5, labels = c("Not worried", "Not very worried", "Somewhat worried", "Very worried", "Extremely worried"))
)

# Reemplazar valores de 77, 88, 99 en 'ccnthum' con NA y convertir a factor ordenado, incluyendo el valor 55
datos <- datos %>% mutate(
  ccnthum = ifelse(ccnthum %in% c(77, 88, 99), NA, ccnthum),
  ccnthum = factor(ccnthum, levels = c(1:5, 55), labels = c("Natural processes", "Mainly natural", "Equal natural and human", "Mainly human", "Entirely human", "I don't think climate change is happening"))
)

# Seleccionar variables de interés
selected_vars <- datos %>% select(cntry, ccnthum, ccrdprs, wrclmch, gndr, agea, eisced, impricha, ipeqopta, ipmodsta, impfuna, impenva)

# Convertir variables categóricas a factores con etiquetas en inglés
selected_vars <- selected_vars %>% mutate(
  gndr = factor(gndr, levels = c(1, 2, 9), labels = c("Male", "Female", "No answer")),
  eisced = factor(eisced, levels = c(0:7, 55, 77, 88, 99), labels = c("Less than lower sec", "Lower secondary", "Lower upper sec", "Upper upper sec", "Advanced voc", "Lower tertiary", "Higher tertiary", "Other", "Not harmonised", "Refusal", "Don't know", "No answer")),
  impricha = factor(impricha, levels = 1:6, labels = c("Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me")),
  ipeqopta = factor(ipeqopta, levels = 1:6, labels = c("Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me")),
  ipmodsta = factor(ipmodsta, levels = 1:6, labels = c("Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me")),
  impfuna = factor(impfuna, levels = 1:6, labels = c("Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me")),
  impenva = factor(impenva, levels = 1:6, labels = c("Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not at all like me"))
)

# Guardar el dataset depurado en un archivo CSV en la carpeta 'datos' del proyecto
output_path <- "1_Datos/3_Datos_depurados.csv"
write_csv(selected_vars, output_path)

# Confirmar la ubicación donde se guarda el archivo
print(paste("El dataset depurado se ha guardado en:", output_path))

