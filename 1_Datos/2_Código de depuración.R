####CÓDIGO DE DEPURACIÓN ###############################################################################################################################

# Cargar librerías necesarias
library(tidyverse)
library(readr)

# Establecer el directorio de trabajo
setwd("C:/Users/Iuliu/Documents/PROYECTOCD")

# Definir la ruta del archivo CSV utilizando una ruta relativa
archivo <- "1_Datos/1_Base de datos original ProyectoOCD.csv"

# Leer el archivo CSV sin convertir cadenas de caracteres a factores
datos <- read.csv(archivo, stringsAsFactors = FALSE)

# Mostrar las primeras filas de la base de datos
print(head(datos))


#### LIMPIEZA DE LOS DATOS #############################################################################################################################

# Resumen de la base de datos
print(summary(datos))

# Verificar valores faltantes para cada variable
valores_faltantes <- sapply(datos, function(x) sum(is.na(x)))
print("Valores faltantes por variable:")
print(valores_faltantes)


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
  ccnthum = factor(ccnthum, levels = c(1:5, 55), labels = c("Procesos naturales", "Principalmente natural", "Igualmente natural y humano", "Principalmente humano", "Completamente humano", "No creo que el cambio climatico este ocurriendo"))
)

# Seleccionar variables de interés
selected_vars <- datos %>% select(cntry, ccnthum, ccrdprs, wrclmch, gndr, agea, eisced, impricha, ipeqopta, ipmodsta, impfuna, impenva)

# Convertir variables categóricas a factores con etiquetas en español, sin tildes
selected_vars <- selected_vars %>% mutate(
  gndr = factor(gndr, levels = c(1, 2), labels = c("Hombre", "Mujer")),  # Eliminar "Sin respuesta"
  eisced = factor(eisced, levels = c(0:7, 55, 77, 88, 99), labels = c("Menos que secundaria baja", "Secundaria baja", "Secundaria alta baja", "Secundaria alta alta", "Vocacional avanzada", "Terciaria baja", "Terciaria alta", "Otro", "No armonizado", "Rechazo", "No sabe", "Sin respuesta")),
  impricha = factor(impricha, levels = 1:6, labels = c("Muy parecido a mi", "Parecido a mi", "Algo parecido a mi", "Un poco parecido a mi", "No parecido a mi", "Nada parecido a mi")),
  ipeqopta = factor(ipeqopta, levels = 1:6, labels = c("Muy parecido a mi", "Parecido a mi", "Algo parecido a mi", "Un poco parecido a mi", "No parecido a mi", "Nada parecido a mi")),
  ipmodsta = factor(ipmodsta, levels = 1:6, labels = c("Muy parecido a mi", "Parecido a mi", "Algo parecido a mi", "Un poco parecido a mi", "No parecido a mi", "Nada parecido a mi")),
  impfuna = factor(impfuna, levels = 1:6, labels = c("Muy parecido a mi", "Parecido a mi", "Algo parecido a mi", "Un poco parecido a mi", "No parecido a mi", "Nada parecido a mi")),
  impenva = factor(impenva, levels = 1:6, labels = c("Muy parecido a mi", "Parecido a mi", "Algo parecido a mi", "Un poco parecido a mi", "No parecido a mi", "Nada parecido a mi"))
)

### SEGMENTACION DE LAS VARIABLES ###

# Paso 1: Segmentación de la edad
selected_vars <- selected_vars %>%
  mutate(
    age_group = case_when(
      agea >= 15 & agea <= 24 ~ "Juventud",
      agea >= 25 & agea <= 29 ~ "Juventud Adulta",
      agea >= 30 & agea <= 50 ~ "Adultez",
      agea >= 51 & agea <= 64 ~ "Madurez",
      agea >= 65 & agea <= 75 ~ "3a Edad",
      agea > 75 ~ "4a Edad",
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
      TRUE ~ NA_character_
    )
  )

# Paso 3: Clasificación de niveles de estudios en bajo, medio y alto, eliminando categorías no deseadas
selected_vars <- selected_vars %>%
  mutate(
    education_level = case_when(
      eisced %in% c("Menos que secundaria baja", "Secundaria baja") ~ "Bajo",
      eisced %in% c("Secundaria alta baja", "Secundaria alta alta") ~ "Medio",
      eisced %in% c("Vocacional avanzada", "Terciaria baja", "Terciaria alta") ~ "Alto",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!eisced %in% c("Otro", "No armonizado", "Rechazo", "No sabe", "Sin respuesta"))  # Eliminar registros con estas categorías

# Paso 4: Segmentación de la variable 'wrclmch' en tres categorías: Bajo, Medio y Alto
selected_vars <- selected_vars %>%
  mutate(wrclmch_segmented = case_when(
    wrclmch %in% c("Nada preocupado", "No muy preocupado") ~ "Bajo",
    wrclmch == "Algo preocupado" ~ "Medio",
    wrclmch %in% c("Muy preocupado", "Extremadamente preocupado") ~ "Alto",
    TRUE ~ NA_character_
  ))

# Paso 5: Segmentación de la variable 'ccrdprs' (nivel de responsabilidad) en tres categorías: Bajo, Medio y Alto
selected_vars <- selected_vars %>%
  mutate(ccrdprs_segmented = case_when(
    ccrdprs %in% c("Nada en absoluto", "1", "2", "3") ~ "Bajo",
    ccrdprs %in% c("4", "5", "6", "7") ~ "Medio",
    ccrdprs %in% c("8", "9", "Mucho") ~ "Alto",
    TRUE ~ NA_character_
  ))

# Guardar el dataset depurado en un archivo CSV en la carpeta 'datos' del proyecto
output_path <- "1_Datos/3_Datos_depurados.csv"
write_csv(selected_vars, output_path)

# Confirmar la ubicación donde se guarda el archivo
print(paste("El dataset depurado se ha guardado en:", output_path))
