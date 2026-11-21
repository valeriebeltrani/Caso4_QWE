# Caso Predicting Customer Churn at QWE Inc.
# Exploración y preparación de datos

# Librerías
# install.packages(c("readxl", "dplyr", "ggplot2", "tidyr"))
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Importar datos
excel_sheets("DATA.xlsx")
datos_raw <- read_excel("DATA.xlsx", sheet = "Case Data")

# Estructura de los datos
str(datos_raw)
summary(datos_raw)
head(datos_raw)
dim(datos_raw)

datos <- datos_raw
names(datos) <- c("id", "edad_meses", "churn", "chi_mes0", "chi_cambio",
                  "casos_mes0", "casos_cambio", "prioridad_mes0",
                  "prioridad_cambio", "logins_cambio", "blogs_cambio",
                  "vistas_cambio", "dias_ultimo_login_cambio")
str(datos)

# Valores faltantes y datos inconsistentes
colSums(is.na(datos))
sum(is.na(datos))
sum(duplicated(datos$id))
table(datos$churn)

datos %>%
  summarise(
    edad_negativa      = sum(edad_meses < 0),
    chi_negativo       = sum(chi_mes0 < 0),
    casos_negativos    = sum(casos_mes0 < 0),
    prioridad_negativa = sum(prioridad_mes0 < 0),
    prioridad_mayor_4  = sum(prioridad_mes0 > 4)
  )

datos %>%
  summarise(
    chi_nov_negativo    = sum(chi_mes0 - chi_cambio < 0),
    casos_nov_negativos = sum(casos_mes0 - casos_cambio < 0),
    prioridad_nov_fuera = sum((prioridad_mes0 - prioridad_cambio) < 0 |
                              (prioridad_mes0 - prioridad_cambio) > 4)
  )

inconsistentes <- datos %>% filter(casos_mes0 > 0 & prioridad_mes0 == 0)
nrow(inconsistentes)

summary(datos$vistas_cambio)
boxplot(datos$vistas_cambio, main = "Cambio en vistas de blog", horizontal = TRUE)
sum(abs(datos$vistas_cambio) > 10000)

sum(datos$chi_mes0 == 0)

# Limpieza y preparación
datos_limpios <- datos %>%
  mutate(
    churn_factor = factor(churn, levels = c(0, 1), labels = c("No", "Sí")),
    grupo_edad = cut(edad_meses,
                     breaks = c(-Inf, 5, 14, Inf),
                     labels = c("Menos de 6 meses", "6 a 14 meses", "Más de 14 meses")),
    tiene_casos_soporte = ifelse(casos_mes0 > 0, 1, 0),
    chi_cero            = ifelse(chi_mes0 == 0, 1, 0),
    chi_bajo_cambio     = ifelse(chi_cambio < 0, 1, 0),
    alerta_prioridad    = ifelse(casos_mes0 > 0 & prioridad_mes0 == 0, 1, 0),
    atipico_vistas      = ifelse(abs(vistas_cambio) > 10000, 1, 0)
  )

str(datos_limpios)
summary(datos_limpios)

write.csv(datos_limpios, "datos_churn_limpios.csv", row.names = FALSE)

# Análisis exploratorio
datos_limpios %>%
  count(churn_factor) %>%
  mutate(porcentaje = round(100 * n / sum(n), 1))

ggplot(datos_limpios, aes(x = churn_factor, fill = churn_factor)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4) +
  scale_fill_manual(values = c("No" = "#4C78A8", "Sí" = "#E45756")) +
  labs(title = "Clientes que abandonaron QWE", x = "Churn", y = "Clientes") +
  theme_minimal() +
  theme(legend.position = "none")

tabla_edad <- datos_limpios %>%
  group_by(grupo_edad) %>%
  summarise(clientes = n(),
            se_fueron = sum(churn),
            tasa_churn_pct = round(100 * mean(churn), 1))
tabla_edad

ggplot(tabla_edad, aes(x = grupo_edad, y = tasa_churn_pct)) +
  geom_col(fill = "#E45756") +
  geom_text(aes(label = paste0(tasa_churn_pct, "%")), vjust = -0.4) +
  labs(title = "Tasa de churn por antigüedad", x = "Antigüedad", y = "Tasa de churn (%)") +
  theme_minimal()

ggplot(datos_limpios, aes(x = edad_meses)) +
  geom_histogram(binwidth = 2, fill = "#4C78A8", color = "white") +
  labs(title = "Antigüedad de los clientes", x = "Meses", y = "Clientes") +
  theme_minimal()

comparacion <- datos_limpios %>%
  group_by(churn_factor) %>%
  summarise(across(c(edad_meses, chi_mes0, chi_cambio, casos_mes0, prioridad_mes0,
                     logins_cambio, blogs_cambio, vistas_cambio,
                     dias_ultimo_login_cambio), mean)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
comparacion

ggplot(datos_limpios, aes(x = churn_factor, y = chi_mes0, fill = churn_factor)) +
  geom_boxplot() +
  scale_fill_manual(values = c("No" = "#4C78A8", "Sí" = "#E45756")) +
  labs(title = "CHI en diciembre", x = "Churn", y = "CHI") +
  theme_minimal() +
  theme(legend.position = "none")

datos_limpios %>%
  group_by(chi_bajo_cambio) %>%
  summarise(clientes = n(), tasa_churn_pct = round(100 * mean(churn), 1))

datos_limpios %>%
  group_by(tiene_casos_soporte) %>%
  summarise(clientes = n(), tasa_churn_pct = round(100 * mean(churn), 1))

ggplot(datos_limpios, aes(x = churn_factor, y = dias_ultimo_login_cambio, fill = churn_factor)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_cartesian(ylim = c(-30, 40)) +
  scale_fill_manual(values = c("No" = "#4C78A8", "Sí" = "#E45756")) +
  labs(title = "Cambio en días desde el último login", x = "Churn", y = "Días") +
  theme_minimal() +
  theme(legend.position = "none")

variables_num <- datos_limpios %>%
  select(edad_meses, chi_mes0, chi_cambio, casos_mes0, casos_cambio,
         prioridad_mes0, prioridad_cambio, logins_cambio, blogs_cambio,
         vistas_cambio, dias_ultimo_login_cambio, churn)

correlaciones <- cor(variables_num)[, "churn"]
round(sort(correlaciones[names(correlaciones) != "churn"]), 3)
