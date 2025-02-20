# Datos panel: Ejercicio de Milena

# 0. Preliminares ------------------------------------------------------------

# Limipieza del entorno de trabajo 
rm(list = ls())

# Paquetes de proposito general 
library(tidyverse)
library(readxl)
library(writexl)

# Paquetes para manejar datos panel
library(plm)

# Paquetes clásicos para hacer estimaciones con errores robustos en R
library(lmtest)
library(sandwich)

# Paquete novedoso con estimadores robustos en R 
library(estimatr)

# Paquetes para exportar tablas de regresión a LaTeX
library(texreg)
library(stargazer)
library(modelsummary)

# Rutas de trabajo
output_path = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/output"
matriculados_path = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/bases_regresiones/matriculados"
puntaje_admision_path = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/puntaje_admision"

# 0. Base de datos matriculados y puntaje admisión adicionales -------------------------

setwd(matriculados_path)

matricula_2023_1 = read_xlsx("Matriculados_fce_2023-1S.xlsx") %>% 
  select(CORREO, PAPA_PERIODO, PUNTAJE_ADMISION) %>% 
  rename(PAPA_PERIODO_2023_1 = PAPA_PERIODO,
         PUNTAJE_ADMISION_2023_1 = PUNTAJE_ADMISION)
  
matricula_2023_2 = read_xlsx("Matriculados_fce_2023-2S.xlsx") %>% 
  select(CORREO, PAPA_PERIODO, PUNTAJE_ADMISION) %>% 
  rename(PAPA_PERIODO_2023_2 = PAPA_PERIODO,
         PUNTAJE_ADMISION_2023_2 = PUNTAJE_ADMISION)

setwd(puntaje_admision_path)

puntaje_admision = read_xlsx("estudiantes_sin_puntaje_admsion.xlsx") %>% 
  mutate(puntaje_admision_adicionales = as.numeric(puntaje_admision_adicionales)) %>% 
  select(CORREO, puntaje_admision_adicionales)

# 1. Procesamiento base de datos generada por Milena -------------------------

setwd(output_path)

# Base de datos generada por Milena + encuesta de percepción 
base_principal = read_xlsx("base_milena_con_encuesta_percepcion.xlsx") 

# Base_principal Empalme con base matrícula 2023-1 y 2023-2

# Base 2023-1

base_principal = base_principal %>% 
  left_join(matricula_2023_1, by = c("CORREO"))

# Base 2023-2

base_principal = base_principal %>% 
  left_join(matricula_2023_2, by = c("CORREO"))

# Base base_principal

base_principal = base_principal %>% 
  mutate(PAPA_PERIODO_FINAL = as.numeric(ifelse(!is.na(PAPA_PERIODO_2023_2), PAPA_PERIODO_2023_2,
                                     ifelse(!is.na(PAPA_PERIODO_2023_1), PAPA_PERIODO_2023_1,
                                            PAPA_PERIODO))),
         PUNTAJE_ADMISION_FINAL = as.numeric(ifelse(!is.na(PUNTAJE_ADMISION_2023_2), PUNTAJE_ADMISION_2023_2,
                                                ifelse(!is.na(PUNTAJE_ADMISION_2023_1), PUNTAJE_ADMISION_2023_1,
                                                       PUNTAJE_ADMISION))))

# Recategoriación variables 
base_principal = base_principal %>% 
  mutate(Periodo = as.factor(Periodo),
         PERIODO = as.factor(PERIODO), 
         DOCUMENTO = as.factor(DOCUMENTO),
         SUBACCESO = as.factor(SUBACCESO),
         SEXO = as.factor(SEXO),
         MUNICIPIO_PROCEDENCIA = as.factor(MUNICIPIO_PROCEDENCIA),
         ESTRATO = as.factor(ESTRATO),
         MATRICULAS = as.factor(MATRICULAS),
         TIPCOLEGIO = as.factor(TIPCOLEGIO),
         MUNICIPIO_COLEGIO = as.factor(MUNICIPIO_COLEGIO),
         Periodo_agregado = as.factor(Periodo_agregado),
         Estrato_recategorizado = fct_relevel(as_factor(Estrato_recategorizado), "bajo", "medio", "alto"),
         Matriculas_recategorizado = as.factor(Matriculas_recategorizado),
         Tipo_colegio_recategorizado = fct_collapse(as_factor(Tipo_colegio_recategorizado), "Público" = c("Público", "No reporta")),
         prueba = as_factor(prueba),
         PROGRAMA_CURRICULAR = fct_relevel(as_factor(PROGRAMA_CURRICULAR), "ECONOMÍA"))

# Recategorización adicional variables 
base_principal = base_principal %>% 
  mutate(municipio_bachillerato_categorica =  as_factor(municipio_bachillerato_categorica), 
         lectura_entretenimiento = as_factor(lectura_entretenimiento),
         lectura_academica = as_factor(lectura_academica),
         estrato_socioeconomico_ep = as_factor(estrato_socioeconomico_ep),
         personas_hogar = as_factor(personas_hogar),
         lectura_academica = fct_relevel(lectura_academica, 
                                         "No dedico tiempo a navegar en internet con fines académicos",
                                         "15 minutos",
                                         "30 minutos",
                                         "1 hora",
                                         "2 horas"),
         lectura_entretenimiento = fct_relevel(lectura_entretenimiento, 
                                         "No dedico tiempo a leer por entretenimiento",
                                         "15 minutos",
                                         "30 minutos",
                                         "1 hora",
                                         "2 horas"),
         personas_hogar = fct_relevel(personas_hogar,
                                      "1",
                                      "2",
                                      "3",
                                      "4",
                                      "5 o más"))

# base_principal = base_principal %>% 
#   mutate(prueba = fct_relevel(prueba, "mod_din"))

# Recodifico los labels de las variables categóricas de "lectura_academica" y "lectura_entretenimiento"

## Como variables factor ordenada

# base_principal$lectura_academica <- factor(fct_recode(base_principal$lectura_academica,
#                                                `1` = "No dedico tiempo a navegar en internet con fines académicos",
#                                                `2` = "15 minutos",
#                                                `3` = "30 minutos",
#                                                `4` = "1 hora",
#                                                `5` = "2 horas",
#                                                `6` = "Más de 2 horas"),
#                                            levels = c("1", "2", "3", "4", "5", "6"),
#                                            ordered = TRUE)
# 
# 
# base_principal$lectura_entretenimiento = factor(fct_recode(base_principal$lectura_entretenimiento,
#                                                     `1` = "No dedico tiempo a leer por entretenimiento",
#                                                     `2` = "15 minutos",
#                                                     `3` = "30 minutos",
#                                                     `4` = "1 hora",
#                                                     `5` = "2 horas",
#                                                     `6` = "Más de 2 horas"),
#                                                     levels = c("1", "2", "3", "4", "5", "6"),
#                                                     ordered = TRUE)

## Como variable numérica de 1 a 6

base_principal$lectura_academica <- as.numeric(fct_recode(base_principal$lectura_academica,
                                                      `1` = "No dedico tiempo a navegar en internet con fines académicos",
                                                      `2` = "15 minutos",
                                                      `3` = "30 minutos",
                                                      `4` = "1 hora",
                                                      `5` = "2 horas",
                                                      `6` = "Más de 2 horas"))


base_principal$lectura_entretenimiento = as.numeric(fct_recode(base_principal$lectura_entretenimiento,
                                                           `1` = "No dedico tiempo a leer por entretenimiento",
                                                           `2` = "15 minutos",
                                                           `3` = "30 minutos",
                                                           `4` = "1 hora",
                                                           `5` = "2 horas",
                                                           `6` = "Más de 2 horas"))

## Como variable numérica en términos de tiempos 

# base_principal <- base_principal %>%
#   mutate(lectura_academica = recode(lectura_academica,
#                                     `1` = 0,
#                                     `2` = 15,
#                                     `3` = 30,
#                                     `4` = 60,
#                                     `5` = 120,
#                                     `6` = 160),
#          lectura_entretenimiento = recode(lectura_entretenimiento,
#                                     `1` = 0,
#                                     `2` = 15,
#                                     `3` = 30,
#                                     `4` = 60,
#                                     `5` = 120,
#                                     `6` = 160))

# Recodifico los labels de las variable categórica "Estrato_recategorizado"

## Como variables factor ordenada

# base_principal$Estrato_recategorizado <- factor(fct_recode(base_principal$Estrato_recategorizado,
#                                                `1` = "bajo",
#                                                `2` = "medio",
#                                                `3` = "alto"),
#                                            levels = c("1", "2", "3"),
#                                            ordered = TRUE)

## Como variable numérica de 1 a 6

base_principal$Estrato_recategorizado <- as.numeric(fct_recode(base_principal$Estrato_recategorizado,
                                                          `1` = "bajo",
                                                          `2` = "medio",
                                                          `3` = "alto"))

# Eliminar estudiantes con PAPA menor a 3

# Estudiantes sin puntaje de admision base de matriculados
# base_principal_sin_puntaje_admision = base_principal %>% 
#   filter(is.na(PUNTAJE_ADMISION_FINAL)) %>% 
#   select(CORREO, NOMBRES, APELLIDO1, APELLIDO2, DOCUMENTO, CONVOCATORIA, APERTURA)
# 
# write_xlsx(base_principal_sin_puntaje_admision, "estudiantes_sin_puntaje_admsion.xlsx")

# Empalme con base de datos de los estudiantes con puntaje de admisión faltante
base_principal = base_principal %>% 
  left_join(puntaje_admision, by = c("CORREO"))

# Generación de la base con todos los puntajes de admisión
base_principal = base_principal %>% 
  mutate(PUNTAJE_ADMISION_FINAL_FINAL = as.numeric(ifelse(!is.na(PUNTAJE_ADMISION_FINAL), PUNTAJE_ADMISION_FINAL, puntaje_admision_adicionales)))


# base_principal = base_principal %>% 
#   filter(PAPA_PERIODO_FINAL > 3)


# 2. Procesamiento bases de datos --------------------------------------------

# Identificación de estudiantes duplicados: Hicierón dos pruebas diferentes
duplicados = base_principal %>%
  group_by(CORREO) %>%
  filter(n() > 1) %>%
  ungroup() 

# Procesamiento bases de datos principal 
base_limpia = base_principal %>% 
  select(-starts_with("P. ")) %>%
  group_by(CORREO) %>%
  mutate(CORREO = ifelse(row_number() > 1, paste0(CORREO, "2"), CORREO)) %>%
  ungroup()

# Transformación de la base de datos a formato de datos panel 
base_limpia_panel = base_limpia %>% 
  pivot_longer(cols = starts_with("Calificación/10.00_prueba"),
               names_to = "periodos_pruebas",
               values_to = "calificacion_pruebas") %>% 
  select(CORREO, periodos_pruebas, calificacion_pruebas, everything()) %>% 
  mutate(periodos_pruebas = str_replace(periodos_pruebas, "Calificación/10.00_prueba", ""))

# Creción de las Dummy
base_limpia_panel = base_limpia_panel %>% 
  mutate(AV = as.factor(ifelse(Periodo_agregado == "virtual", 1, 0)),
         AP = as.factor(ifelse(Periodo_agregado == "presencial", 1, 0)),
         PL = as.factor(ifelse(Periodo_agregado == "placebos", 1, 0)),
         EX2 = as.factor(ifelse(periodos_pruebas == 2, 1, 0)))

# Agregar variables adicionales relacionadas con los tests

## Genera las variables de duración del test
base_limpia_panel = base_limpia_panel %>% 
  mutate(Tiempo_requerido_prueba = ifelse(periodos_pruebas == 1, `Tiempo requerido_prueba1`, `Tiempo requerido_prueba2`),
         minutos_prueba = as.numeric(str_extract(`Tiempo_requerido_prueba`, "\\d+")),
         segundos_prueba = as.numeric(str_extract(str_remove(`Tiempo_requerido_prueba`, "\\d+ minutos "), "\\d+")),
         tiempo_total_prueba = minutos_prueba * 60 + segundos_prueba)

## Genera las variables de percepcion de dificultad de los tests
base_limpia_panel = base_limpia_panel %>% 
  mutate(percepcion_dificultad_prueba = ifelse(periodos_pruebas == 1, dificultad_prueba1, dificultad_prueba2))

# 3. Scatterplots ------------------------------------------------------------

# Función para generar los Scatterplots
scatterplot <- function(base, var_x, var_y, titulo, periodo_agregado) {
  
  # Coeficiente de correlación
  coef_correlacion <- cor(base %>% pull({{ var_x }}), base %>% pull({{ var_y }}), use = "complete.obs")
  
  # Gráfico de dispersión entre las dos variables de interés 
  grafica <- base %>%
    ggplot(aes(x = {{ var_x }}, y = {{ var_y }}, color = {{periodo_agregado}})) +
    geom_point() +
    labs(title = titulo,
         x = deparse(substitute(var_x)),
         y = deparse(substitute(var_y)),
         caption = paste("Coeficiente de correlación:", round(coef_correlacion, 2))) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal()
  
  return(grafica)
}

# 3.1 Scatterplots con placebos ----

## Scatter plot PBM vs resultados de pruebas 
scatterplot(base_limpia_panel, PBM, calificacion_pruebas, "PBM vs Calificación pruebas con placebos", periodo_agregado = Periodo_agregado)

## Scatter plot PAPA periodo vs resultados de pruebas
scatterplot(base_limpia_panel, PAPA_PERIODO_FINAL, calificacion_pruebas, "Promedio académico vs Calificación pruebas con placebos", periodo_agregado = Periodo_agregado)

## Scatter plot Minutos vs resultados de pruebas 
scatterplot(base_limpia_panel, minutos_prueba, calificacion_pruebas, "Minutos prueba vs Calificación pruebas con placebos", periodo_agregado = Periodo_agregado)

## Scatter plot Percepción dificultad prueba vs resultados de pruebas 
scatterplot(base_limpia_panel, percepcion_dificultad_prueba, calificacion_pruebas, "Percepción dificultad prueba vs Calificación pruebas con placebos", periodo_agregado = Periodo_agregado)

## Scatter plot Puntaje de admisión vs resultados de pruebas 
scatterplot(base_limpia_panel, PUNTAJE_ADMISION_FINAL_FINAL, calificacion_pruebas, "Puntaje de admisión vs Calificación pruebas con placebos", periodo_agregado = Periodo_agregado)

# 3.2 Scatterplots sin placebos ----

# Base de datos sin placebos
base_scatter = base_limpia_panel %>% 
  filter(Periodo_agregado != "placebos")

## Scatter plot PBM vs resultados de pruebas 
scatterplot(base_scatter, PBM, calificacion_pruebas, "PBM vs Calificación pruebas sin placebos", periodo_agregado = Periodo_agregado)

## Scatter plot PAPA periodo vs resultados de pruebas
scatterplot(base_scatter, PAPA_PERIODO, calificacion_pruebas, "Promedio académico vs Calificación pruebas sin placebos", periodo_agregado = Periodo_agregado)

## Scatter plot Minutos vs resultados de pruebas
scatterplot(base_scatter, minutos_prueba, calificacion_pruebas, "Minutos prueba vs Calificación pruebas sin placebos", periodo_agregado = Periodo_agregado)

## Scatter plot Percepción dificultad prueba vs resultados de pruebas 
scatterplot(base_scatter, percepcion_dificultad_prueba, calificacion_pruebas, "Percepción dificultad prueba vs Calificación pruebas con placebos", periodo_agregado = Periodo_agregado)

## Scatter plot Puntaje de admisión vs resultados de pruebas 
scatterplot(base_scatter, PUNTAJE_ADMISION_FINAL_FINAL, calificacion_pruebas, "Puntaje de admisión vs Calificación pruebas con placebos", periodo_agregado = Periodo_agregado)


# 4. Base de datos tipo panel --------------------------------------------------

# Panel completo (incluyendo placebos) ----

# Construcción del panel
panel = pdata.frame(base_limpia_panel, index = c("CORREO", "periodos_pruebas"))

# Para conocer las dimensiones del panel
pdim(panel)

# 5. Regresiones -------------------------------------------------------------

# 5.0 Fórmulas ----

# Nota: Posibles regresores 

### PBM: caracterización socioeconómica del estudiante
### PAPA_PERIODO: Promedio Académico Acumulado
### prueba: tipo de prueba
### minutos_prueba: tiempo de realización de la prueba
### lectura_academica: Tiempo dedicado a realizar lectura académica
### lectura_entretenimiento: Tiempo dedicado a realizar lectura por entretenimiento
### PROGRAMA_CURRICULAR: Programa curricular donde se encuentra inscrito el estudiante 
### EDAD: Edad del estudiante
### Tipo_colegio_recategorizado: Tipo de colegio donde estudió el estudiante (público o privado)
### Estrato_recategorizado: Estrato socioeconómico del estudiante
### percepcion_dificultad_prueba: Percepción de los estudiantes sobre la dificultad de los tests
### municipio_bachillerato_categorica: Si el estudiantes estudió en Bogota o afuera
### personas_hogar: Número de personas que habitan en el hogar con el estudiante


# Fórmulas

# 5.1 Fórmulas con interacciones ----

### Fórmulas principales 
formula_sin_reg = calificacion_pruebas ~ EX2 + AV + AP + AV * EX2 + AP * EX2

# Fórmula con PBM

formula_con_reg = calificacion_pruebas ~ EX2 + AV + AP + AV * EX2 + AP * EX2 + PUNTAJE_ADMISION_FINAL_FINAL + prueba
formula_con_reg = calificacion_pruebas ~ EX2 + AV + AP + AV * EX2 + AP * EX2 + PBM + minutos_prueba + prueba
formula_con_reg = calificacion_pruebas ~ EX2 + AV + AP + AV * EX2 + AP * EX2 + prueba + PBM + PROGRAMA_CURRICULAR + lectura_entretenimiento + minutos_prueba
# formula_con_reg = calificacion_pruebas ~ EX2 + AV + AP + AV * EX2 + AP * EX2 + prueba + PBM + PROGRAMA_CURRICULAR + minutos_prueba + lectura_entretenimiento
# formula_con_reg = calificacion_pruebas ~ EX2 + AV + AP + AV * EX2 + AP * EX2 + PBM + PAPA_PERIODO + prueba + PROGRAMA_CURRICULAR + minutos_prueba + lectura_entretenimiento

# Fórmula con Estrato recategorizado 
# formula_con_reg = calificacion_pruebas ~ EX2 + AV + AP + AV * EX2 + AP * EX2 + Estrato_recategorizado + PAPA_PERIODO + prueba + PROGRAMA_CURRICULAR + minutos_prueba + lectura_entretenimiento

### Fórmulas alternativas interesantes 
# formula_con_reg = calificacion_pruebas ~ EX2 + AV + AP + AV * EX2 + AP * EX2 + PBM + PAPA_PERIODO + prueba + PROGRAMA_CURRICULAR + minutos_prueba + Estrato_recategorizado + personas_hogar

### Fórmulas alternativas

# formula_con_reg = calificacion_pruebas ~ EX2 + AV + AP + AV * EX2 + AP * EX2 + PBM + PAPA_PERIODO
# formula_con_reg = calificacion_pruebas ~ EX2 + AV + AP + AV * EX2 + AP * EX2 + PBM + PAPA_PERIODO + percepcion_dificultad_prueba 
# formula_con_reg = calificacion_pruebas ~ EX2 + AV + AP + AV * EX2 + AP * EX2 + PBM + PAPA_PERIODO + prueba + PROGRAMA_CURRICULAR
# formula_con_reg = calificacion_pruebas ~ EX2 + AV + AP + AV * EX2 + AP * EX2 + PBM + PAPA_PERIODO + percepcion_dificultad_prueba + prueba + minutos_prueba  + Tipo_colegio_recategorizado + personas_hogar + lectura_academica

### Fórmulas sin interacciones

# formula_sin_reg = calificacion_pruebas ~ EX2 + AV + AP
# formula_con_reg = calificacion_pruebas ~ EX2 + AV + AP + PBM + PAPA_PERIODO


# 5.1 Pooled OLS ----

# 5.1.1 lm ----

# Estimación con errores estándar convencionales 

# Modelo sin regresoras
pooled_ols_sin_reg_lm = lm(formula_sin_reg, 
                         data = base_limpia_panel); summary(pooled_ols_sin_reg_lm)

# Modelo con regresoras
pooled_ols_con_reg_lm = lm(formula_con_reg, 
                           data = base_limpia_panel); summary(pooled_ols_con_reg_lm)

# Estimaciones con errores robustos HC3

## Modelo sin regresoras
pooled_ols_sin_reg_lm_lmtest = coeftest(pooled_ols_sin_reg_lm, 
                                  vcov = vcovHC(pooled_ols_sin_reg_lm, 
                                                type = "HC3")); print(pooled_ols_sin_reg_lm_lmtest)

## Modelo con regresoras
pooled_ols_con_reg_lm_lmtest = coeftest(pooled_ols_con_reg_lm,
                                  vcov = vcovHC(pooled_ols_con_reg_lm, 
                                                type = "HC3")); print(pooled_ols_con_reg_lm_lmtest)

# Estimaciones con errores robustos tipo cluster, donde se clusteriza por individuo (CORREO)

## Modelo sin regresoras
pooled_ols_sin_reg_lm_lmtest = coeftest(pooled_ols_sin_reg_lm, 
                                        vcov = vcovCL(pooled_ols_sin_reg_lm, 
                                                      type = "HC0",
                                                      method = "arellano",
                                                      cluster = ~ CORREO)); print(pooled_ols_sin_reg_lm_lmtest)

## Modelo con regresoras
pooled_ols_con_reg_lm_lmtest = coeftest(pooled_ols_con_reg_lm,
                                        vcov = vcovHC(pooled_ols_con_reg_lm, 
                                                      type = "HC0",
                                                      method = "arellano",
                                                      cluster = ~ CORREO)); print(pooled_ols_con_reg_lm_lmtest)

# 5.1.2 plm (pooled) ----

# Estimación con errores estándar convencionales 

### Modelo sin regresoras
pooled_ols_sin_reg_plm = plm(formula_sin_reg, 
                         data = panel, 
                         model = "pooling"); summary(pooled_ols_sin_reg_plm)

### Modelo con regresoras
pooled_ols_con_reg_plm = plm(formula_con_reg, 
                         data = panel, 
                         model = "pooling"); summary(pooled_ols_con_reg_plm)

# Estimaciones con errores robustos HC3

### Modelo sin regresoras
pooled_ols_sin_reg_plm_lmtest = coeftest(pooled_ols_sin_reg_plm, 
                                         vcov = function(x) vcovHC(x, method = "white1", type = "HC3")); print(pooled_ols_sin_reg_plm_lmtest)

### Modelo con regresoras
pooled_ols_con_reg_plm_lmtest = coeftest(pooled_ols_con_reg_plm, 
                                         vcov = function(x) vcovHC(x, method = "white1", type = "HC3")); print(pooled_ols_con_reg_plm_lmtest)


# Estimaciones con errores robustos tipo cluster, donde se clusteriza por individuo (CORREO)

### Modelo sin regresoras
pooled_ols_sin_reg_plm_lmtest = coeftest(pooled_ols_sin_reg_plm, 
                                         vcov = vcovHC(pooled_ols_sin_reg_plm)); print(pooled_ols_sin_reg_plm_lmtest)

### Modelo con regresoras
pooled_ols_con_reg_plm_lmtest = coeftest(pooled_ols_con_reg_plm, 
                                         vcov = vcovHC(pooled_ols_con_reg_plm)); print(pooled_ols_con_reg_plm_lmtest)

# Estimaciones con errores robustos tipo DoubleCluster (Donde se clusteriza por individuo (CORREO) y por tiempo (periodos_pruebas))

### Modelo sin regresoras
pooled_ols_sin_reg_plm_lmtest = coeftest(pooled_ols_sin_reg_plm, 
                                         vcov = vcovDC(pooled_ols_sin_reg_plm)); print(pooled_ols_sin_reg_plm_lmtest)

### Modelo con regresoras
pooled_ols_con_reg_plm_lmtest = coeftest(pooled_ols_con_reg_plm, 
                                         vcov = vcovDC(pooled_ols_con_reg_plm)); print(pooled_ols_con_reg_plm_lmtest)

# Estimaciones con errores robustos tipo Panel Newey West (Donde se estima errores tipo HAC Newey West adaptados para datos panel)

### Modelo sin regresoras
pooled_ols_sin_reg_plm_lmtest = coeftest(pooled_ols_sin_reg_plm, 
                                         vcov = vcovNW(pooled_ols_sin_reg_plm)); print(pooled_ols_sin_reg_plm_lmtest)

### Modelo con regresoras
pooled_ols_con_reg_plm_lmtest = coeftest(pooled_ols_con_reg_plm, 
                                         vcov = vcovNW(pooled_ols_con_reg_plm)); print(pooled_ols_con_reg_plm_lmtest)

# 5.1.3 estimatr ----

# Estimaciones con errores robustos HC3

### Modelo sin regresoras
pooled_ols_sin_reg_estimatr = lm_robust(formula_sin_reg, 
          data = base_limpia_panel, se_type = "HC3"); summary(pooled_ols_sin_reg_estimatr)

### Modelo con regresoras
pooled_ols_con_reg_estimatr = lm_robust(formula_con_reg, 
                 data = base_limpia_panel, se_type = "HC3"); summary(pooled_ols_con_reg_estimatr)

# Estimaciones con errores robustos tipo cluster, donde se clusteriza por individuo (CORREO)

### Modelo sin regresoras
pooled_ols_sin_reg_estimatr = lm_robust(formula_sin_reg, 
                                        data = base_limpia_panel, 
                                        se_type = "CR0", 
                                        clusters = CORREO); summary(pooled_ols_sin_reg_estimatr)

### Modelo con regresoras
pooled_ols_con_reg_estimatr = lm_robust(formula_con_reg, 
                                        data = base_limpia_panel, 
                                        se_type = "CR0", 
                                        clusters = CORREO); summary(pooled_ols_con_reg_estimatr)

# Estimaciones con errores robustos tipo cluster, donde se clusteriza por grupo agregado (Periodo_agregado)

### Modelo sin regresoras
pooled_ols_sin_reg_estimatr = lm_robust(formula_sin_reg, 
                                        data = base_limpia_panel, 
                                        se_type = "CR0", 
                                        clusters = Periodo_agregado); summary(pooled_ols_sin_reg_estimatr)

### Modelo con regresoras
pooled_ols_con_reg_estimatr = lm_robust(formula_con_reg, 
                                        data = base_limpia_panel, 
                                        se_type = "CR0", 
                                        clusters = Periodo_agregado); summary(pooled_ols_con_reg_estimatr)

# Estimaciones con errores robustos tipo cluster, donde se clusteriza por grupo desagregado (Periodo)

### Modelo sin regresoras
pooled_ols_sin_reg_estimatr = lm_robust(formula_sin_reg, 
                                        data = base_limpia_panel, 
                                        se_type = "CR0", 
                                        clusters = Periodo); summary(pooled_ols_sin_reg_estimatr)

### Modelo con regresoras
pooled_ols_con_reg_estimatr = lm_robust(formula_con_reg, 
                                        data = base_limpia_panel, 
                                        se_type = "CR0", 
                                        clusters = Periodo); summary(pooled_ols_con_reg_estimatr)

# 5.1.4 plm (random effects) ----

# Estimación con errores estándar convencionales 

### Modelo sin regresoras
random_ols_sin_reg_plm = plm(formula_sin_reg, 
                             data = panel, 
                             model = "random"); summary(random_ols_sin_reg_plm)

### Modelo con regresoras
random_ols_con_reg_plm = plm(formula_con_reg, 
                             data = panel, 
                             model = "random"); summary(random_ols_con_reg_plm)

# Estimaciones con errores robustos HC3

### Modelo sin regresoras
random_ols_sin_reg_plm_lmtest = coeftest(random_ols_sin_reg_plm, 
                                         vcov = function(x) vcovHC(x, method = "white1", type = "HC3")); print(random_ols_sin_reg_plm_lmtest)

### Modelo con regresoras
random_ols_con_reg_plm_lmtest = coeftest(random_ols_con_reg_plm, 
                                         vcov = function(x) vcovHC(x, method = "white1", type = "HC3")); print(random_ols_con_reg_plm_lmtest)


# Estimaciones con errores robustos tipo cluster, donde se clusteriza por individuo (CORREO)

### Modelo sin regresoras
random_ols_sin_reg_plm_lmtest = coeftest(random_ols_sin_reg_plm, 
                                         vcov = vcovHC(random_ols_sin_reg_plm)); print(random_ols_sin_reg_plm_lmtest)

### Modelo con regresoras
random_ols_con_reg_plm_lmtest = coeftest(random_ols_con_reg_plm, 
                                         vcov = vcovHC(random_ols_con_reg_plm)); print(random_ols_con_reg_plm_lmtest)

# Estimaciones con errores robustos tipo DoubleCluster (Donde se clusteriza por individuo (CORREO) y por tiempo (periodos_pruebas))

### Modelo sin regresoras
random_ols_sin_reg_plm_lmtest = coeftest(random_ols_sin_reg_plm, 
                                         vcov = vcovDC(random_ols_sin_reg_plm)); print(random_ols_sin_reg_plm_lmtest)

### Modelo con regresoras
random_ols_con_reg_plm_lmtest = coeftest(random_ols_con_reg_plm, 
                                         vcov = vcovDC(random_ols_con_reg_plm)); print(random_ols_con_reg_plm_lmtest)

# Estimaciones con errores robustos tipo Panel Newey West (Donde se estima errores tipo HAC Newey West adaptados para datos panel)

### Modelo sin regresoras
random_ols_sin_reg_plm_lmtest = coeftest(random_ols_sin_reg_plm, 
                                         vcov = vcovNW(random_ols_sin_reg_plm)); print(random_ols_sin_reg_plm_lmtest)

### Modelo con regresoras
random_ols_con_reg_plm_lmtest = coeftest(random_ols_con_reg_plm, 
                                         vcov = vcovNW(random_ols_con_reg_plm)); print(random_ols_con_reg_plm_lmtest)

# 6. Adicional ----

# Test entre efectos aleatorios y pooled 
plmtest(pooled_ols_con_reg_plm, type = "bp", effect = "individual")  #Ho:Mejor Pooled porque var(ai)=0

# 7. Otro códidgo ----

# texreg(list(hola, hola2), include.ci = FALSE)


#modelsummary(list(hola, hola2), output = "latex")


# # Tipo 2 (Presencial grupo de referencia)
# 
# # Modelo sin regresoras
# pooled_ols_sin_reg = plm(calificacion_pruebas ~ EX2 + AV + PL + AV * EX2 + PL * EX2, 
#                          data = base_limpia_panel, 
#                          model = "pooling"); summary(pooled_ols_sin_reg)
# 
# # Modelo con regresoras
# pooled_ols_con_reg = plm(calificacion_pruebas ~ EX2 + AV + PL + AV * EX2 + PL * EX2 + PBM + PAPA_PERIODO, 
#                          data = base_limpia_panel, 
#                          model = "pooling"); summary(pooled_ols_con_reg)
# 
# 
# # Efectos fijos ----
# 
# # Tipo 1 (Placebos grupo de referencia)
# 
# # Modelo sin regresoras 
# fe_sin_reg = plm(calificacion_pruebas ~ EX2 + AV + AV * EX2 + AP * EX2 + PBM + PAPA_PERIODO,
#                  data = base_limpia_panel,
#                  model = "within"); summary(fe_sin_reg)
# 
# # Modelo con regresoras
# fe_con_reg = plm(calificacion_pruebas ~ EX2 + AV + AV * EX2 + AP * EX2 + PBM * EX2 + PAPA_PERIODO * EX2,
#                  data = base_limpia_panel,
#                  model = "within"); summary(fe_con_reg)
# 
# # Primeras diferencias ----
# 
# # Tipo 1 (Placebos grupo de referencia)
# 
# # Modelo sin regresoras 
# fd_sin_reg = plm(calificacion_pruebas ~ EX2 + AV + AV * EX2 + AP * EX2 + PBM + PAPA_PERIODO,
#                  data = base_limpia_panel,
#                  model = "fd"); summary(fd_sin_reg)
# 
# # Modelo con regresoras
# fd_con_reg = plm(calificacion_pruebas ~ EX2 + AV + AV * EX2 + AP * EX2 + PBM * EX2 + PAPA_PERIODO * EX2,
#                  data = base_limpia_panel,
#                  model = "fd"); summary(fd_con_reg)
# 
# # Efectos aleatorios ----
# 
# # Tipo 1 (Placebos grupo de referencia)
# 
# 
# 

# Panel

# theta = 0.6
# 
# panel_random = panel %>% 
#   mutate(PBM = PBM - theta * PBM,
#          PAPA_PERIODO = PAPA_PERIODO - theta * PAPA_PERIODO,
#          calificacion_pruebas = calificacion_pruebas - theta * calificacion_pruebas)
# 
# panel_time_demeaned = panel %>%
#   group_by(CORREO) %>%
#   mutate(calificacion_pruebas = calificacion_pruebas - theta * mean(calificacion_pruebas),
#          PBM = PBM - theta * mean(PBM),
#          PAPA_PERIODO = PAPA_PERIODO - theta * mean(PAPA_PERIODO))
# 
# formula_only = calificacion_pruebas ~ PBM + PAPA_PERIODO
# 
# # summary(plm(formula_only, data = panel_random, model = "pooling"))
# 
# summary(plm(formula_only, data = panel_time_demeaned, model = "pooling"))
# 
# summary(plm(formula_only, data = panel, model = "pooling"))
# 
# summary(plm(formula_only, data = panel, model = "random", random.method = "walhus"))






