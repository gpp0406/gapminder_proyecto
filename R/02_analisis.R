# ============================================================
# PROYECTO GAPMINDER
# Análisis de los datos
# ============================================================

# Cargar paquetes
library(tidyverse)


# ------------------------------------------------------------
# 1. CARGAR LOS DATOS DEPURADOS
# ------------------------------------------------------------

datos <- read_csv("data/gapminder_limpio.csv")


# ------------------------------------------------------------
# 2. EVOLUCIÓN DE LA ESPERANZA DE VIDA
# ------------------------------------------------------------

evolucion_vida <- datos %>%
  group_by(year) %>%
  summarise(
    esperanza_vida_media = mean(lifeExp)
  )

evolucion_vida


# ------------------------------------------------------------
# 3. ESPERANZA DE VIDA POR CONTINENTE
# ------------------------------------------------------------

vida_continente <- datos %>%
  group_by(continent) %>%
  summarise(
    esperanza_vida_media = mean(lifeExp)
  )

vida_continente


# ------------------------------------------------------------
# 4. CORRELACIÓN ENTRE PIB Y ESPERANZA DE VIDA
# ------------------------------------------------------------

correlacion <- cor(
  datos$gdpPercap,
  datos$lifeExp
)

correlacion


# ------------------------------------------------------------
# 5. GRÁFICO 1: EVOLUCIÓN TEMPORAL
# ------------------------------------------------------------

grafico_vida <- ggplot(
  evolucion_vida,
  aes(x = year, y = esperanza_vida_media)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolución de la esperanza de vida mundial",
    x = "Año",
    y = "Esperanza de vida media (años)"
  ) +
  theme_minimal()

grafico_vida


# ------------------------------------------------------------
# 6. GRÁFICO 2: COMPARACIÓN ENTRE CONTINENTES
# ------------------------------------------------------------

grafico_continentes <- ggplot(
  vida_continente,
  aes(x = continent, y = esperanza_vida_media)
) +
  geom_col() +
  labs(
    title = "Esperanza de vida media por continente",
    x = "Continente",
    y = "Esperanza de vida media (años)"
  ) +
  theme_minimal()

grafico_continentes


# ------------------------------------------------------------
# 7. GRÁFICO 3: PIB Y ESPERANZA DE VIDA
# ------------------------------------------------------------

grafico_pib <- ggplot(
  datos,
  aes(x = gdpPercap, y = lifeExp)
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  labs(
    title = "Relación entre PIB per cápita y esperanza de vida",
    x = "PIB per cápita (escala logarítmica)",
    y = "Esperanza de vida (años)"
  ) +
  theme_minimal()

grafico_pib
