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
  ccrdprs = factor(ccrdprs, ordered = TRUE, levels = 0:10, labels = c("Nada en absoluto", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Mucho")),
  wrclmch = factor(wrclmch, ordered = TRUE, levels = 1:5, labels = c("Nada preocupado", "No muy preocupado", "Algo preocupado", "Muy preocupado", "Extremadamente preocupado"))
)

# Reemplazar valores de 77, 88, 99 en 'ccnthum' con NA y convertir a factor ordenado, incluyendo el valor 55
datos <- datos %>% mutate(
  ccnthum = ifelse(ccnthum %in% c(77, 88, 99), NA, ccnthum),
  ccnthum = factor(ccnthum, levels = c(1:5, 55), labels = c("Procesos naturales", "Principalmente natural", "Igualmente natural y humano", "Principalmente humano", "Completamente humano", "No creo que el cambio climático esté ocurriendo"))
)

# Seleccionar variables de interés
selected_vars <- datos %>% select(cntry, ccnthum, ccrdprs, wrclmch, gndr, agea, eisced, impricha, ipeqopta, ipmodsta, impfuna, impenva)

# Convertir variables categóricas a factores con etiquetas en español
selected_vars <- selected_vars %>% mutate(
  gndr = factor(gndr, levels = c(1, 2, 9), labels = c("Hombre", "Mujer", "Sin respuesta")),
  eisced = factor(eisced, levels = c(0:7, 55, 77, 88, 99), labels = c("Menos que secundaria baja", "Secundaria baja", "Secundaria alta baja", "Secundaria alta alta", "Vocacional avanzada", "Terciaria baja", "Terciaria alta", "Otro", "No armonizado", "Rechazo", "No sabe", "Sin respuesta")),
  impricha = factor(impricha, levels = 1:6, labels = c("Muy parecido a mí", "Parecido a mí", "Algo parecido a mí", "Un poco parecido a mí", "No parecido a mí", "Nada parecido a mí")),
  ipeqopta = factor(ipeqopta, levels = 1:6, labels = c("Muy parecido a mí", "Parecido a mí", "Algo parecido a mí", "Un poco parecido a mí", "No parecido a mí", "Nada parecido a mí")),
  ipmodsta = factor(ipmodsta, levels = 1:6, labels = c("Muy parecido a mí", "Parecido a mí", "Algo parecido a mí", "Un poco parecido a mí", "No parecido a mí", "Nada parecido a mí")),
  impfuna = factor(impfuna, levels = 1:6, labels = c("Muy parecido a mí", "Parecido a mí", "Algo parecido a mí", "Un poco parecido a mí", "No parecido a mí", "Nada parecido a mí")),
  impenva = factor(impenva, levels = 1:6, labels = c("Muy parecido a mí", "Parecido a mí", "Algo parecido a mí", "Un poco parecido a mí", "No parecido a mí", "Nada parecido a mí"))
)

######SEGMENTACION DE VARIABLES#####################################################################################################################

# Paso 1: Segmentación de la edad
selected_vars <- selected_vars %>%
  mutate(
    age_group = case_when(
      agea >= 15 & agea <= 24 ~ "Juventud",
      agea >= 25 & agea <= 29 ~ "Juventud Adulta",
      agea >= 30 & agea <= 50 ~ "Adultez",
      agea >= 51 & agea <= 64 ~ "Madurez",
      agea >= 65 & agea <= 75 ~ "3ª Edad",
      agea > 75 ~ "4ª Edad",
      TRUE ~ NA_character_
    )
  )

# Paso 2: Clasificación de países según regiones de Europa (ajustado a países disponibles)
selected_vars <- selected_vars %>%
  mutate(
    region = case_when(
      cntry %in% c("GB", "IE", "NL") ~ "Nord Oeste",
      cntry %in% c("LT") ~ "Nord Este",
      cntry %in% c("FI", "NO") ~ "Nord Europa",
      cntry %in% c("AT", "CH", "DE") ~ "Centro Europa",
      cntry %in% c("HR", "HU", "SI", "SK") ~ "Sud Este",
      TRUE ~ "Other"
    )
  )

# Paso 3: Clasificación de niveles de estudios en bajo, medio y alto
selected_vars <- selected_vars %>%
  mutate(
    education_level = case_when(
      eisced %in% c("Less than lower sec", "Lower secondary") ~ "Bajo",
      eisced %in% c("Lower upper sec", "Upper upper sec") ~ "Medio",
      eisced %in% c("Advanced voc", "Lower tertiary", "Higher tertiary") ~ "Alto",
      TRUE ~ "Other"
    )
  )


# Segmentar la variable 'ccrdprs' en tres categorías: Bajo, Medio y Alto
filtered_vars <- filtered_vars %>%
  mutate(ccrdprs_segmented = case_when(
    as.numeric(ccrdprs) >= 0 & as.numeric(ccrdprs) <= 3 ~ "Bajo",
    as.numeric(ccrdprs) >= 4 & as.numeric(ccrdprs) <= 7 ~ "Medio",
    as.numeric(ccrdprs) >= 8 & as.numeric(ccrdprs) <= 10 ~ "Alto",
    TRUE ~ NA_character_
  ))


# Guardar el dataset depurado en un archivo CSV en la carpeta 'datos' del proyecto
output_path <- "1_Datos/3_Datos_depurados.csv"
write_csv(selected_vars, output_path)

# Confirmar la ubicación donde se guarda el archivo
print(paste("El dataset depurado se ha guardado en:", output_path))

