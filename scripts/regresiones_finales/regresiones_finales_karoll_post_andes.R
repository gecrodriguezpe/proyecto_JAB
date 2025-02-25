# Datos panel: Ejercicio Karoll

# 0. Preliminares ------------------------------------------------------------

# Limipieza del entorno de trabajo 
rm(list = ls())

# Paquetes de proposito general 
library(tidyverse)
library(readxl)
library(writexl)

# Paquetes para el manejo de rutas y directorios
library(here)
library(fs)
library(glue)

# Paquetes para estimación de modelos de datos panel
library(plm)
library(fixest)

# Paquetes clásicos para hacer estimaciones con errores robustos en R
library(lmtest)
library(sandwich)

# Paquete novedoso con estimadores robustos en R 
library(estimatr)

# Paquetes para exportar tablas de regresión a LaTeX
library(texreg)
library(stargazer)
library(modelsummary)
library(kableExtra)

# Inferencia estadística
library(car)

# Resumen de resultados de regresión
library(broom)


# Root directory 
root_directory = dir_ls(glue("{here()}"))

# Rutas bases de datos (Archivos .xlsx)
bases_matriculados = dir_ls(glue("{here()}/bases_de_datos/bases_regresiones/matriculados/"), glob = "*.xlsx")
bases_puntaje_admision = dir_ls(glue("{here()}/bases_de_datos/puntaje_admision/"), glob = "*.xlsx")
bases_output = dir_ls(glue("{here()}/bases_de_datos/output/"), glob = "*.xlsx")

# resultados directory
resultados_directory = root_directory[grepl("resultados", root_directory)]


# 0. Base de datos matriculados y puntaje admisión adicionales -------------------------

matricula_2023_1 = read_xlsx(bases_matriculados[7]) %>% 
  select(CORREO, PAPA_PERIODO, PUNTAJE_ADMISION) %>% 
  rename(PAPA_PERIODO_2023_1 = PAPA_PERIODO,
         PUNTAJE_ADMISION_2023_1 = PUNTAJE_ADMISION)

matricula_2023_2 = read_xlsx(bases_matriculados[8]) %>% 
  select(CORREO, PAPA_PERIODO, PUNTAJE_ADMISION) %>% 
  rename(PAPA_PERIODO_2023_2 = PAPA_PERIODO,
         PUNTAJE_ADMISION_2023_2 = PUNTAJE_ADMISION)

puntaje_admision = read_xlsx(bases_puntaje_admision[1]) %>% 
  mutate(puntaje_admision_adicionales = as.numeric(puntaje_admision_adicionales)) %>% 
  select(CORREO, puntaje_admision_adicionales)

# 1. Procesamiento base de datos generada por Milena -------------------------

# Base de datos generada por Milena + encuesta de percepción 

base_virtuales = read_xlsx(bases_output[2], sheet = "Virtuales")

base_presenciales = read_xlsx(bases_output[2], sheet = "Presenciales")

base_placebos = read_xlsx(bases_output[2], sheet = "Placebos")

base_principal = bind_rows(base_virtuales, base_presenciales, base_placebos)

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
         PROGRAMA_CURRICULAR = fct_relevel(as_factor(PROGRAMA_CURRICULAR), "ECONOMÍA"),
         tipo_admision = as_factor(ifelse(SUBACCESO == "REGULAR DE PREGRADO", 1, 0)))

## Como variable numérica de 1 a 6

base_principal$Estrato_recategorizado <- as.numeric(fct_recode(base_principal$Estrato_recategorizado,
                                                               `1` = "bajo",
                                                               `2` = "medio",
                                                               `3` = "alto"))

# Eliminar estudiantes con PAPA menor a 3

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
  ungroup() %>% 
  mutate(Modalidad = replace_na(Modalidad, "placebos"))

## Generar las variables de efectos fijos para la estimación de efectos fijos 
base_limpia = base_limpia %>% 
  mutate(fecha_pruebas = as.factor(str_trim(str_remove(`Comenzado el_prueba1`, " \\d{2}:\\d{2}"))),
         semestre_ingreso = as.factor(CONVOCATORIA)) %>% 
  mutate(Matriculas_numeric= as.numeric(MATRICULAS))

# Transformación de la base de datos a formato de datos panel 
base_limpia_panel = base_limpia %>% 
  pivot_longer(cols = starts_with("Calificación/10.00_prueba"),
               names_to = "periodos_pruebas",
               values_to = "calificacion_pruebas") %>% 
  select(CORREO, periodos_pruebas, calificacion_pruebas, everything()) %>% 
  mutate(periodos_pruebas = str_replace(periodos_pruebas, "Calificación/10.00_prueba", ""))

# Creción de las Dummy
# base_limpia_panel = base_limpia_panel %>%
#   mutate(AV = as.factor(ifelse(Periodo_agregado == "virtual", 1, 0)),
#          AP = as.factor(ifelse(Periodo_agregado == "presencial", 1, 0)),
#          PL = as.factor(ifelse(Periodo_agregado == "placebos", 1, 0)),
#          EX2 = as.factor(ifelse(periodos_pruebas == 2, 1, 0)))

base_limpia_panel = base_limpia_panel %>%
  mutate(AV = ifelse(Periodo_agregado == "virtual", 1, 0),
         AP = ifelse(Periodo_agregado == "presencial", 1, 0),
         PL = ifelse(Periodo_agregado == "placebos", 1, 0),
         EX2 = ifelse(periodos_pruebas == 2, 1, 0))

# Agregar variables adicionales relacionadas con los tests

## Genera las variables de duración del test
base_limpia_panel = base_limpia_panel %>% 
  mutate(Tiempo_requerido_prueba = ifelse(periodos_pruebas == 1, `Tiempo requerido_prueba1`, `Tiempo requerido_prueba2`),
         minutos_prueba = as.numeric(str_extract(`Tiempo_requerido_prueba`, "\\d+")),
         segundos_prueba = as.numeric(str_extract(str_remove(`Tiempo_requerido_prueba`, "\\d+ minutos "), "\\d+")),
         tiempo_total_prueba = minutos_prueba * 60 + segundos_prueba)


# 3. Análisis incondicional -----------------------------------------------

# 3.1 Triple diferencia ----

## Seleccion de grupos de interés 

### Grupo virtual
virtuales = base_limpia %>% 
  filter(Periodo_agregado == "virtual")

### Grupo presencial
presenciales = base_limpia %>% 
  filter(Periodo_agregado == "presencial") 

### Grupo placebo
placebo = base_limpia %>% 
  filter(Periodo_agregado == "placebos")

## Medias

### Medias Grupo "Virtual" 
virtual_prueba1 = mean(virtuales$`Calificación/10.00_prueba1`); virtual_prueba1 # Prueba 1
virtual_prueba2 = mean(virtuales$`Calificación/10.00_prueba2`); virtual_prueba2 # Prueba 2

### Medias Grupo "presencial" 
presencial_prueba1 = mean(presenciales$`Calificación/10.00_prueba1`); presencial_prueba1 # Prueba 1
presencial_prueba2 =  mean(presenciales$`Calificación/10.00_prueba2`); presencial_prueba2 # Prueba 2

### Medias Grupo "placebo" 
placebo_prueba1 = mean(placebo$`Calificación/10.00_prueba1`); placebo_prueba1 # Prueba 1
placebo_prueba2 =  mean(placebo$`Calificación/10.00_prueba2`); placebo_prueba2 # Prueba 2

## Diferencia de medias 

###
diferencia_virtual = virtual_prueba2 - virtual_prueba1

### 
diferencia_presencial = presencial_prueba2 - presencial_prueba1

### 
diferencia_placebos = placebo_prueba2 - placebo_prueba1

## Triple diferencia 
triple_diferencia = (diferencia_virtual - diferencia_placebos) - (diferencia_presencial - diferencia_placebos); triple_diferencia

# 3.2 Análisis incondicional de varianzas ponderadas (pooled sd) individuales ----

# Nota: Se realiza el cálculo de varianzas ponderadas individuales 

# Clasificación de estudiantes por modalidad de instrucción 
placebos = base_limpia %>% filter(Modalidad == "placebos")
presencial = base_limpia %>% filter(Modalidad == "presencial")
virtual = base_limpia %>% filter(Modalidad == "virtual")

# Función para calcular las varianzas ponderadas 
pooled_sd = function(df, group_col, value_col) {
  
  group_stats = df %>%
    group_by({{group_col}}) %>%
    summarise(
      sd_k = sd({{value_col}}, na.rm = TRUE),
      n_k = n(),
      .groups = "drop"
    )
  
  numerator <- sum((group_stats$n_k - 1) * (group_stats$sd_k)^2, na.rm = TRUE)
  denominator <- sum(group_stats$n_k - 1, na.rm = TRUE)
  
  sqrt(numerator / denominator)
}


# Desviación estándar ponderada del "grupo virtual"
pooled_sd(virtual, prueba, `Calificación/10.00_prueba1`)
pooled_sd(virtual, prueba, `Calificación/10.00_prueba2`)

# Desviación estándar ponderada del "grupo presencial"
pooled_sd(presencial, prueba, `Calificación/10.00_prueba1`)
pooled_sd(presencial, prueba, `Calificación/10.00_prueba2`)

# Desviación estándar ponderada del "grupo placebol"
pooled_sd(placebos, prueba, `Calificación/10.00_prueba1`)
pooled_sd(placebos, prueba, `Calificación/10.00_prueba2`)

# 4. Regresiones --------------------------------------------------

# 4.1 Fixed effects (fixest) models ----

# Regresion 1
fe1 = feols(calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) | fecha_pruebas + semestre_ingreso, 
                  data = base_limpia_panel, 
                  panel.id = c("CORREO", "periodos_pruebas"),
                  cluster = ~prueba); summary(fe1)

# Regresion 2
fe2 = feols(calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) +  minutos_prueba | fecha_pruebas + semestre_ingreso, 
      data = base_limpia_panel, 
      panel.id = c("CORREO", "periodos_pruebas"),
      cluster = ~prueba); summary(fe2)

# Regresion 3
fe3 = feols(calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) +  minutos_prueba + PBM + PUNTAJE_ADMISION_FINAL_FINAL | fecha_pruebas + semestre_ingreso, 
      data = base_limpia_panel, 
      panel.id = c("CORREO", "periodos_pruebas"),
      cluster = ~prueba); summary(fe3)

# Regresion 4
fe4 = feols(calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) +  minutos_prueba + PBM + PUNTAJE_ADMISION_FINAL_FINAL + SEXO | fecha_pruebas + semestre_ingreso, 
            data = base_limpia_panel, 
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(fe4)

# Regresion 5
fe5 = feols(calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) +  minutos_prueba + PBM + PUNTAJE_ADMISION_FINAL_FINAL + SEXO + PAPA_PERIODO_FINAL | fecha_pruebas + semestre_ingreso, 
            data = base_limpia_panel, 
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(fe5)

# Regresion 6
fe6 = feols(calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) +  minutos_prueba + PBM + PUNTAJE_ADMISION_FINAL_FINAL + SEXO + PAPA_PERIODO_FINAL + Matriculas_recategorizado | fecha_pruebas + semestre_ingreso, 
            data = base_limpia_panel, 
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(fe6)

# Regresion 7
fe7 = feols(calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) +  PAPA_PERIODO_FINAL | fecha_pruebas + semestre_ingreso, 
            data = base_limpia_panel, 
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(fe7)

# Regresion otra
# fe4_tipo1 =feols(calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) +  minutos_prueba + PBM + PUNTAJE_ADMISION_FINAL_FINAL + prueba | fecha_pruebas + semestre_ingreso, 
#       data = base_limpia_panel, 
#       panel.id = c("CORREO", "periodos_pruebas")); summary(fe4_tipo1)

# 4.2 Exportación de la Tabla de regresiones ----

## Usando "texreg"
texreg(
  list(fe1, fe2, fe3, fe4, fe5, fe6, fe7),
  file = file.path(resultados_directory, "regresiones_efectos_fijos.tex"),
  caption = "Efecto del cambio en la modalidad de enseñanza-aprendizaje: resultados de estimación",
  label = "tab:regresiones_efectos_fijos",
  digits = 3,  
  custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"), 
  custom.coef.map = list("EX2" =  "Prueba 2 (T)", 
                      "AV" = "Virtual (V)", 
                      "PL" = "Placebo (P)", 
                      "I(EX2 * AV)" = "Prueba 2 (T) * Virtual (V)", 
                      "I(EX2 * PL)" = "Prueba 2 (T) * Placebo (P)",
                      "minutos_prueba" = "Minutos prueba", 
                      "PBM" = "PBM", 
                      "PUNTAJE_ADMISION_FINAL_FINAL" = "Puntaje de admisión",
                      "SEXOM" = "Sexo", 
                      "PAPA_PERIODO_FINAL" = "Promedio académico",
                      "Matriculas_recategorizadoNumero de matriculas (4, 5, 6)" = "Cuarta, Quinta y Sexta matrícula",
                      "Matriculas_recategorizadoNumero de matriculas (7, 8, 9, 10)" = "Septima, Octaba, Novena y Decima matrícula"),
  stars = c(0.01, 0.05, 0.1),
  include.adjrs = FALSE,  
  include.aic = FALSE,  
  include.bic = FALSE,  
  use.packages = FALSE  
)

## Usando "modelsummary"

# modelsummary(
#   list("Modelo1" = fe1_tipo1, 
#        "Modelo2" = fe2_tipo1,
#        "Modelo3" = fe3_tipo1,
#        "Modelo4" = fe1_tipo2,
#        "Modelo5" = fe1_tipo2,
#        "Modelo6" = fe1_tipo2),
#   output = "latex",
#   stars = TRUE, 
#   title = "Efecto del cambio en la modalidad de ensenanza-aprendizaje: resultados de estimacion"
# )

# 5. Pruebas de hipótesis lineales ----

# Pruebas de hipótesis

hyp_EX2_AV_EX2 = "I(EX2 * AV) + EX2 = 0"

# 5.1 Pruebas de hipótesis lineales (adecuación del placebo) para los modelos de efectos fijos ----

## Regresión fixed effects 1
linearHypothesis(fe1, hyp_EX2_AV_EX2)

## Regresión fixed effects 2
linearHypothesis(fe2, hyp_EX2_AV_EX2)

## Regresión fixed effects 3
linearHypothesis(fe3, hyp_EX2_AV_EX2)

## Regresión fixed effects 4
linearHypothesis(fe4, hyp_EX2_AV_EX2)

## Regresión fixed effects 5
linearHypothesis(fe5, hyp_EX2_AV_EX2)

## Regresión fixed effects 6
linearHypothesis(fe6, hyp_EX2_AV_EX2)

## Regresión fixed effects 7
linearHypothesis(fe7, hyp_EX2_AV_EX2)

