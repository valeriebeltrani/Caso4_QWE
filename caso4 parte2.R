# PARTE 2
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)


if (!exists("datos_limpios")) {
  datos_limpios <- read_csv("datos_churn_limpios.csv", show_col_types = FALSE)
  datos_limpios$churn_factor <- factor(datos_limpios$churn, levels = c(0, 1),
                                       labels = c("No", "Sí"))
  datos_limpios$grupo_edad <- factor(datos_limpios$grupo_edad,
                                     levels = c("Menos de 6 meses", "6 a 14 meses",
                                                "Más de 14 meses"))
}


colores <- c("No" = "#4C78A8", "Sí" = "#E45756")


# Guardar gráficas en carpeta
dir.create("gráficas", showWarnings = FALSE)
guardar <- function(nombre, ancho = 8, alto = 5) {
  ggsave(filename = file.path("gráficas", paste0(nombre, ".png")),
         plot = last_plot(), width = ancho, height = alto, dpi = 300, bg = "white")
}


# Función auxiliar: tasa de churn por grupo
tasa_por <- function(df, var) {
  df %>%
    group_by({{ var }}) %>%
    summarise(clientes = n(),
              se_fueron = sum(churn),
              tasa_churn_pct = round(100 * mean(churn), 1),
              .groups = "drop")
}


# 1. VARIABLE OBJETIVO: CHURN
tabla_churn <- datos_limpios %>%
  count(churn_factor) %>%
  mutate(porcentaje = round(100 * n / sum(n), 1))
tabla_churn


tasa_global <- mean(datos_limpios$churn)
cat("Tasa global de churn:", round(100 * tasa_global, 2), "%\n")


ggplot(tabla_churn, aes(x = churn_factor, y = n, fill = churn_factor)) +
  geom_col() +
  geom_text(aes(label = paste0(n, " (", porcentaje, "%)")), vjust = -0.4) +
  scale_fill_manual(values = colores) +
  labs(title = "Distribución de la variable objetivo: Churn",
       x = "¿Abandonó en los 2 meses siguientes?", y = "Clientes") +
  theme_minimal() + theme(legend.position = "none")
guardar("01_distribucion_churn")




# 2. ANTIGÜEDAD Y ABANDONO
# 2.1 Por los grupos que sugiere el VP (<6, 6-14, >14 meses)
tabla_edad <- tasa_por(datos_limpios, grupo_edad)
tabla_edad


ggplot(tabla_edad, aes(x = grupo_edad, y = tasa_churn_pct)) +
  geom_col(fill = "#E45756") +
  geom_hline(yintercept = 100 * tasa_global, linetype = "dashed") +
  geom_text(aes(label = paste0(tasa_churn_pct, "%")), vjust = -0.4) +
  labs(title = "Tasa de churn por grupo de antigüedad",
       subtitle = "Línea punteada = tasa global",
       x = "Antigüedad", y = "Tasa de churn (%)") +
  theme_minimal()
guardar("02_churn_por_grupo_edad")


# 2.2 Con más detalle (rangos más finos): los contratos vencen en los meses 6 y 12
datos_limpios <- datos_limpios %>%
  mutate(edad_detalle = cut(edad_meses,
                            breaks = c(-Inf, 5, 8, 11, 14, 20, 30, Inf),
                            labels = c("0-5", "6-8", "9-11", "12-14",
                                       "15-20", "21-30", "31+")))
tabla_edad_det <- tasa_por(datos_limpios, edad_detalle)
tabla_edad_det


ggplot(tabla_edad_det, aes(x = edad_detalle, y = tasa_churn_pct)) +
  geom_col(fill = "#E45756") +
  geom_text(aes(label = paste0(tasa_churn_pct, "%")), vjust = -0.4) +
  labs(title = "Tasa de churn por rango de antigüedad (meses)",
       x = "Antigüedad (meses)", y = "Tasa de churn (%)") +
  theme_minimal()
guardar("03_churn_por_rango_edad")


# 2.3 Distribución de la antigüedad según churn
ggplot(datos_limpios, aes(x = edad_meses, fill = churn_factor)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = colores, name = "Churn") +
  labs(title = "Antigüedad de los clientes según churn", x = "Meses", y = "Densidad") +
  theme_minimal()
guardar("04_densidad_antiguedad")


# 2.4 Prueba estadística: ¿la antigüedad se asocia con el abandono?
chisq.test(table(datos_limpios$grupo_edad, datos_limpios$churn))
wilcox.test(edad_meses ~ churn, data = datos_limpios)




# 3. CHI (Customer Happiness Index) Y ABANDONO
# 3.1 Nivel de CHI en diciembre (mes 0)
datos_limpios <- datos_limpios %>%
  mutate(chi_rango = cut(chi_mes0,
                         breaks = c(-Inf, 0, 50, 100, 150, Inf),
                         labels = c("CHI = 0", "1-50", "51-100", "101-150", "150+")))
tabla_chi <- tasa_por(datos_limpios, chi_rango)
tabla_chi


ggplot(tabla_chi, aes(x = chi_rango, y = tasa_churn_pct)) +
  geom_col(fill = "#E45756") +
  geom_text(aes(label = paste0(tasa_churn_pct, "%")), vjust = -0.4) +
  labs(title = "Tasa de churn según nivel de CHI (diciembre)",
       x = "CHI", y = "Tasa de churn (%)") +
  theme_minimal()
guardar("05_churn_por_nivel_chi")


ggplot(datos_limpios, aes(x = churn_factor, y = chi_mes0, fill = churn_factor)) +
  geom_boxplot() +
  scale_fill_manual(values = colores) +
  labs(title = "CHI en diciembre según churn", x = "Churn", y = "CHI") +
  theme_minimal() + theme(legend.position = "none")
guardar("06_boxplot_chi")


# 3.2 Cambio en el CHI (noviembre -> diciembre)
datos_limpios <- datos_limpios %>%
  mutate(chi_tendencia = case_when(chi_cambio < 0 ~ "Bajó",
                                   chi_cambio == 0 ~ "Sin cambio",
                                   TRUE ~ "Subió"),
         chi_tendencia = factor(chi_tendencia, levels = c("Bajó", "Sin cambio", "Subió")))
tabla_chi_cambio <- tasa_por(datos_limpios, chi_tendencia)
tabla_chi_cambio


ggplot(tabla_chi_cambio, aes(x = chi_tendencia, y = tasa_churn_pct)) +
  geom_col(fill = "#E45756") +
  geom_text(aes(label = paste0(tasa_churn_pct, "%")), vjust = -0.4) +
  labs(title = "Tasa de churn según la tendencia del CHI",
       x = "Cambio del CHI vs. mes anterior", y = "Tasa de churn (%)") +
  theme_minimal()
guardar("07_churn_tendencia_chi")


ggplot(datos_limpios, aes(x = churn_factor, y = chi_cambio, fill = churn_factor)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_cartesian(ylim = c(-60, 60)) +
  scale_fill_manual(values = colores) +
  labs(title = "Cambio del CHI según churn", x = "Churn", y = "Cambio en CHI") +
  theme_minimal() + theme(legend.position = "none")
guardar("08_boxplot_cambio_chi")


# 3.3 Pruebas estadísticas
wilcox.test(chi_mes0 ~ churn, data = datos_limpios)
wilcox.test(chi_cambio ~ churn, data = datos_limpios)




# 4. SOPORTE: SOLICITUDES (CASOS) Y PRIORIDAD
# 4.1 ¿Tener casos de soporte?
tabla_casos <- datos_limpios %>%
  mutate(tiene_casos = ifelse(casos_mes0 > 0, "Con casos", "Sin casos")) %>%
  tasa_por(tiene_casos)
tabla_casos


# 4.2 Número de casos (agrupado)
datos_limpios <- datos_limpios %>%
  mutate(casos_rango = cut(casos_mes0, breaks = c(-Inf, 0, 1, 2, Inf),
                           labels = c("0", "1", "2", "3 o más")))
tabla_casos_rango <- tasa_por(datos_limpios, casos_rango)
tabla_casos_rango


ggplot(tabla_casos_rango, aes(x = casos_rango, y = tasa_churn_pct)) +
  geom_col(fill = "#E45756") +
  geom_text(aes(label = paste0(tasa_churn_pct, "%")), vjust = -0.4) +
  labs(title = "Tasa de churn según número de casos de soporte",
       x = "Casos de soporte (diciembre)", y = "Tasa de churn (%)") +
  theme_minimal()
guardar("09_churn_numero_casos")


# 4.3 Prioridad (solo clientes con casos; prioridad 0 = no tuvo casos)
con_casos <- datos_limpios %>% filter(casos_mes0 > 0)
con_casos <- con_casos %>%
  mutate(prioridad_rango = cut(prioridad_mes0, breaks = c(-Inf, 2.5, 3, Inf),
                               labels = c("Baja (<=2.5)", "Media (2.5-3]", "Alta (>3)")))
tabla_prioridad <- tasa_por(con_casos, prioridad_rango)
tabla_prioridad


ggplot(con_casos, aes(x = churn_factor, y = prioridad_mes0, fill = churn_factor)) +
  geom_boxplot() +
  scale_fill_manual(values = colores) +
  labs(title = "Prioridad promedio de los casos (clientes con casos)",
       x = "Churn", y = "Prioridad") +
  theme_minimal() + theme(legend.position = "none")
guardar("10_boxplot_prioridad")


wilcox.test(casos_mes0 ~ churn, data = datos_limpios)
wilcox.test(prioridad_mes0 ~ churn, data = con_casos)




# 5. VARIABLES DE USO: LOGINS, BLOGS, VIEWS, DÍAS DESDE ÚLTIMO LOGIN
# Nota: vistas_cambio tiene valores extremos (|x| > 10,000); por eso se usan
# medianas y se limita el eje en los gráficos.


# 5.1 Estadísticas descriptivas por grupo
uso_resumen <- datos_limpios %>%
  group_by(churn_factor) %>%
  summarise(across(c(logins_cambio, blogs_cambio, vistas_cambio, dias_ultimo_login_cambio),
                   list(media = mean, mediana = median, sd = sd),
                   .names = "{.col}_{.fn}"),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
t(uso_resumen)


# 5.2 Tasa de churn según la dirección del cambio de cada variable de uso
direccion <- function(x) factor(case_when(x < 0 ~ "Bajó", x == 0 ~ "Sin cambio", TRUE ~ "Subió"),
                                levels = c("Bajó", "Sin cambio", "Subió"))


uso_direccion <- datos_limpios %>%
  transmute(churn,
            Logins = direccion(logins_cambio),
            Blogs = direccion(blogs_cambio),
            Vistas = direccion(vistas_cambio),
            `Días desde último login` = direccion(dias_ultimo_login_cambio)) %>%
  pivot_longer(-churn, names_to = "variable", values_to = "direccion") %>%
  group_by(variable, direccion) %>%
  summarise(clientes = n(), tasa_churn_pct = round(100 * mean(churn), 1), .groups = "drop")
uso_direccion


ggplot(uso_direccion, aes(x = direccion, y = tasa_churn_pct, fill = direccion)) +
  geom_col() +
  geom_text(aes(label = paste0(tasa_churn_pct, "%")), vjust = -0.4, size = 3) +
  geom_hline(yintercept = 100 * tasa_global, linetype = "dashed") +
  facet_wrap(~ variable) +
  scale_fill_manual(values = c("Bajó" = "#E45756", "Sin cambio" = "grey70", "Subió" = "#4C78A8")) +
  labs(title = "Tasa de churn según la dirección del cambio en el uso",
       subtitle = "Línea punteada = tasa global", x = NULL, y = "Tasa de churn (%)") +
  theme_minimal() + theme(legend.position = "none")
guardar("11_churn_direccion_uso")


# 5.3 Boxplots comparativos (ejes limitados por los extremos)
ggplot(datos_limpios, aes(x = churn_factor, y = logins_cambio, fill = churn_factor)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_cartesian(ylim = c(-50, 60)) +
  scale_fill_manual(values = colores) +
  labs(title = "Cambio en logins", x = "Churn", y = "Logins") +
  theme_minimal() + theme(legend.position = "none")
guardar("12_boxplot_logins")


ggplot(datos_limpios, aes(x = churn_factor, y = vistas_cambio, fill = churn_factor)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_cartesian(ylim = c(-500, 500)) +
  scale_fill_manual(values = colores) +
  labs(title = "Cambio en vistas de blogs (eje limitado)", x = "Churn", y = "Vistas") +
  theme_minimal() + theme(legend.position = "none")
guardar("13_boxplot_vistas")


ggplot(datos_limpios, aes(x = churn_factor, y = dias_ultimo_login_cambio, fill = churn_factor)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_cartesian(ylim = c(-30, 40)) +
  scale_fill_manual(values = colores) +
  labs(title = "Cambio en días desde el último login", x = "Churn", y = "Días") +
  theme_minimal() + theme(legend.position = "none")
guardar("14_boxplot_dias_login")


# 5.4 Pruebas no paramétricas (variables muy sesgadas)
wilcox.test(logins_cambio ~ churn, data = datos_limpios)
wilcox.test(blogs_cambio ~ churn, data = datos_limpios)
wilcox.test(vistas_cambio ~ churn, data = datos_limpios)
wilcox.test(dias_ultimo_login_cambio ~ churn, data = datos_limpios)




# 6. CORRELACIONES Y COMPARATIVO GENERAL
variables_num <- datos_limpios %>%
  select(edad_meses, chi_mes0, chi_cambio, casos_mes0, casos_cambio,
         prioridad_mes0, prioridad_cambio, logins_cambio, blogs_cambio,
         vistas_cambio, dias_ultimo_login_cambio, churn)


# 6.1 Correlación de cada variable con churn (Spearman, robusta a extremos)
cor_churn <- data.frame(
  variable = setdiff(names(variables_num), "churn"),
  pearson  = sapply(setdiff(names(variables_num), "churn"),
                    function(v) cor(variables_num[[v]], variables_num$churn)),
  spearman = sapply(setdiff(names(variables_num), "churn"),
                    function(v) cor(variables_num[[v]], variables_num$churn, method = "spearman"))
) %>%
  mutate(across(c(pearson, spearman), ~ round(.x, 3))) %>%
  arrange(desc(abs(spearman)))
cor_churn


ggplot(cor_churn, aes(x = reorder(variable, spearman), y = spearman, fill = spearman > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#E45756", "FALSE" = "#4C78A8"),
                    labels = c("TRUE" = "Aumenta churn", "FALSE" = "Reduce churn"),
                    name = NULL) +
  labs(title = "Correlación (Spearman) de cada variable con churn",
       x = NULL, y = "Correlación") +
  theme_minimal()
guardar("15_correlacion_con_churn")


# 6.2 Matriz de correlación entre todas las variables (heatmap)
matriz_cor <- cor(variables_num, method = "spearman")
cor_larga <- as.data.frame(as.table(matriz_cor))
names(cor_larga) <- c("var1", "var2", "correlacion")


ggplot(cor_larga, aes(x = var1, y = var2, fill = correlacion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlacion, 2)), size = 2.5) +
  scale_fill_gradient2(low = "#4C78A8", mid = "white", high = "#E45756", limits = c(-1, 1)) +
  labs(title = "Matriz de correlación (Spearman)", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
guardar("16_matriz_correlacion")


# 6.3 Tabla comparativa: promedios y medianas por grupo
comparacion_medianas <- datos_limpios %>%
  group_by(churn_factor) %>%
  summarise(across(c(edad_meses, chi_mes0, chi_cambio, casos_mes0, prioridad_mes0,
                     logins_cambio, blogs_cambio, vistas_cambio, dias_ultimo_login_cambio),
                   median), .groups = "drop")
comparacion_medianas
