# ============================================================
# PROYECTO GAPMINDER
# Evolución de la esperanza de vida y desarrollo económico
# ============================================================

# ------------------------------------------------------------
# 1. CARGA DE PAQUETES
# ------------------------------------------------------------

library(gapminder)
library(tidyverse)


# ------------------------------------------------------------
# 2. IMPORTACIÓN DE LOS DATOS
# ------------------------------------------------------------

# Cargamos la base Gapminder
datos <- gapminder


# ------------------------------------------------------------
# 3. EXPLORACIÓN INICIAL
# ------------------------------------------------------------

# Dimensiones de la base
dim(datos)

# Estructura de los datos
str(datos)

# Primeras observaciones
head(datos)


# ------------------------------------------------------------
# 4. DEPURACIÓN
# ------------------------------------------------------------

# Comprobar valores perdidos
colSums(is.na(datos))

# Comprobar duplicados
sum(duplicated(datos))

# Número de países
length(unique(datos$country))

# Años disponibles
sort(unique(datos$year))

# Continentes disponibles
unique(datos$continent)


# ------------------------------------------------------------
# 5. CREACIÓN DE NUEVAS VARIABLES
# ------------------------------------------------------------

# Agrupamos los años por décadas
datos <- datos %>%
  mutate(
    decade = floor(year / 10) * 10
  )

# Clasificamos los países según su PIB per cápita
datos <- datos %>%
  mutate(
    nivel_economico = case_when(
      gdpPercap < 5000 ~ "Bajo",
      gdpPercap < 15000 ~ "Medio",
      TRUE ~ "Alto"
    )
  )


# ------------------------------------------------------------
# 6. COMPROBACIÓN
# ------------------------------------------------------------

head(datos)

table(datos$nivel_economico)


# ------------------------------------------------------------
# 7. GUARDAR DATOS DEPURADOS
# ------------------------------------------------------------

write_csv(
  datos,
  "data/gapminder_limpio.csv"
)