# Predicting Customer Churn at QWE Inc.
## Parte 1 – Exploración y preparación de datos

**Archivos**
- `DATA.xlsx`: base original del caso (hoja *Case Data*).
- `caso_churn_parte1.R`: código de importación, revisión, limpieza y análisis exploratorio.
- `datos_churn_limpios.csv`: base de datos limpia (se genera al correr el script).

## Revisión de la base

La base tiene **6.347 clientes y 13 variables**, todas numéricas, con datos de diciembre de 2011 y su cambio frente a noviembre. La variable a predecir es *churn* (1 = el cliente se fue en los dos meses siguientes).

| Revisión | Resultado |
|---|---|
| Valores faltantes | 0 |
| Clientes (id) duplicados | 0 |
| Valores negativos imposibles (antigüedad, CHI, casos, prioridad) | 0 |
| Valores de noviembre imposibles (diciembre − cambio < 0) | 0 |
| Clientes con casos de soporte pero prioridad promedio 0 | 32 |
| Cambios extremos en vistas de blog (más de ±10.000) | 22 |
| Clientes con Índice de Felicidad del Cliente (CHI) igual a 0 | 1.193 |

## Limpieza y preparación

- Se renombraron las variables con nombres cortos en español, sin espacios.
- No se eliminó ningún cliente: no hay faltantes ni duplicados, y como el churn es poco frecuente, cada observación es valiosa.
- Los casos inconsistentes y atípicos se **marcaron** con variables indicadoras (`alerta_prioridad`, `atipico_vistas`, `chi_cero`) en lugar de borrarlos.
- Se creó `churn_factor` (Sí / No) y `grupo_edad` con los rangos que propone Richard Wall en el caso: menos de 6 meses, 6 a 14 meses y más de 14 meses.
- Se agregaron `tiene_casos_soporte` y `chi_bajo_cambio` (el CHI bajó de noviembre a diciembre).

## Descripción general de los clientes

| Grupo | Clientes | Tasa de churn |
|---|---|---|
| Total | 6.347 | 5,1 % (323 se fueron) |
| Menos de 6 meses | 1.669 | 1,6 % |
| 6 a 14 meses | 2.284 | 7,2 % |
| Más de 14 meses | 2.394 | 5,5 % |
| CHI bajó en el mes | 2.306 | 6,1 % |
| CHI no bajó | 4.041 | 4,5 % |
| Con casos de soporte | 1.791 | 3,2 % |
| Sin casos de soporte | 4.556 | 5,8 % |

| Promedio | Se quedaron | Se fueron |
|---|---|---|
| Antigüedad (meses) | 13,8 | 15,4 |
| CHI en diciembre | 88,6 | 63,3 |
| Cambio en CHI | +5,5 | −3,7 |
| Casos de soporte | 0,72 | 0,37 |
| Cambio en inicios de sesión | +16,1 | +8,1 |
| Cambio en días desde el último inicio de sesión | +1,5 | +6,5 |

**Hallazgos principales**

1. **El churn es un evento raro (5,1 %).** Las clases están muy desbalanceadas, lo que hay que tener en cuenta al construir el modelo.
2. **Los clientes de 6 a 14 meses son los más riesgosos (7,2 %),** tal como intuía Richard Wall. Los nuevos (menos de 6 meses) casi no se van.
3. **La felicidad del cliente importa:** quienes se fueron tenían un CHI mucho menor (63 frente a 89) y, en promedio, su CHI estaba cayendo.
4. **El uso del servicio baja antes de irse:** los que abandonaron aumentaron menos sus inicios de sesión y pasaron más días sin entrar.
5. **Contrario a lo esperado, tener casos de soporte no se asocia con más churn:** los clientes con casos se van menos (3,2 % frente a 5,8 %), posiblemente porque son clientes más activos.
6. Ninguna variable por sí sola tiene una correlación fuerte con churn (todas por debajo de 0,1 en valor absoluto), así que se necesitará un modelo que combine varias variables.
