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

# 
source(glue("{here()}/scripts/regresiones_finales/randomization_test.R"))





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
  mutate(Matriculas_numeric= as.numeric(MATRICULAS)) %>% 
  mutate(mes_nacimiento = str_extract(FECHA_NACIMIENTO, "-\\d{2}-") %>%  str_replace_all("-", "") %>%  as.numeric())  %>% 
  mutate(semestre_nacimiento = ifelse(mes_nacimiento %in% 7:12, 1, 0))

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
         INST = ifelse(Periodo_agregado == "placebos", 0, 1),
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

# 3.3 Regresiones para el cálculo de los errores estándar usando la metodología de Chetty (tread_prod = Virtuales (AV)) ----

# Nota: Se calculan los errores estándar usando la metodología de Chetty. "Equivalencia" de tratamientos:
## treat_prod = Virtuales (AV)
## treat_store = Placebos (PL)
## treat_time = Dummy para la prueba 2/test 2 (EX2)

# Variables de interacción:
##  i1 = I(AV * EX2) (treat_products * treat_time)
##  i2 = I(AV * PL) (treat_products * treat_store)
##  i3 = I(PL * EX2) (treat_store * treat_time)
##  TREATMENT= I(AV * PL * EX2)  (treat_products * treat_store * treat_time)

# Presenciales

# 3.3.1 Instruidos

# Pre
inst1 = feols(calificacion_pruebas ~ AV,
            data = base_limpia_panel %>%  filter(EX2 == 0) %>% filter(PL == 0),
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(inst1)

# Post
inst2 = feols(calificacion_pruebas ~ AV,
              data = base_limpia_panel %>%  filter(EX2 == 1) %>% filter(PL == 0),
              panel.id = c("CORREO", "periodos_pruebas"),
              cluster = ~prueba); summary(inst2)


# Diferencia de tiempo para presenciales
ins_diff_time_pres = feols(calificacion_pruebas ~ EX2,
              data = base_limpia_panel %>%  filter(AV == 0) %>% filter(PL == 0),
              panel.id = c("CORREO", "periodos_pruebas"),
              cluster = ~prueba); summary(ins_diff_time_pres)

# Diferencia de tiempo para virtuales
ins_diff_time_virt = feols(calificacion_pruebas ~ I(AV * EX2) + AV ,
                       data = base_limpia_panel  %>% filter(PL == 0),
                       panel.id = c("CORREO", "periodos_pruebas"),
                       cluster = ~prueba); summary(ins_diff_time_virt)

# 3.3.2 Placebos

# Pre
placebos1 = feols(calificacion_pruebas ~ AV,
              data = base_limpia_panel %>%  filter(EX2 == 0) %>% filter(PL == 1),
              panel.id = c("CORREO", "periodos_pruebas"),
              cluster = ~prueba); summary(placebos1)

# Post
placebos2 = feols(calificacion_pruebas ~ AV,
              data = base_limpia_panel %>%  filter(EX2 == 1) %>% filter(PL == 1),
              panel.id = c("CORREO", "periodos_pruebas"),
              cluster = ~prueba); summary(placebos2)


# Diferencia de tiempo para presenciales
placebos_diff_time_pres = feols(calificacion_pruebas ~ EX2,
                            data = base_limpia_panel %>%  filter(AV == 0) %>% filter(PL == 1),
                            panel.id = c("CORREO", "periodos_pruebas"),
                            cluster = ~prueba); summary(placebos_diff_time_pres)

# Diferencia de tiempo para virtuales
placebos_diff_time_virt = feols(calificacion_pruebas ~ I(AV * EX2) + AV ,
                            data = base_limpia_panel %>% filter(PL == 1),
                            panel.id = c("CORREO", "periodos_pruebas"),
                            cluster = ~prueba); summary(placebos_diff_time_virt)

# Virtuales

virt_inst1 = feols(calificacion_pruebas ~ 1 ,
                                data = base_limpia_panel %>% filter(AV == 1) %>% filter(PL == 0) %>%  filter(EX2 == 0),
                                panel.id = c("CORREO", "periodos_pruebas"),
                                cluster = ~prueba); summary(virt_inst1)


virt_inst2 = feols(calificacion_pruebas ~ 1 ,
                                data = base_limpia_panel %>% filter(AV == 1) %>% filter(PL == 0) %>%  filter(EX2 == 1),
                                panel.id = c("CORREO", "periodos_pruebas"),
                                cluster = ~prueba); summary(virt_inst2)


# virt_placebo1 = feols(calificacion_pruebas ~ 1 ,
#                                 data = base_limpia_panel %>% filter(AV == 1) %>% filter(PL == 1) %>%  filter(EX2 == 0),
#                                 panel.id = c("CORREO", "periodos_pruebas"),
#                                 cluster = ~prueba); summary(virt_placebo1)


# virt_placebo2 = feols(calificacion_pruebas ~ 1 ,
#                                 data = base_limpia_panel %>% filter(AV == 1) %>% filter(PL == 1) %>%  filter(EX2 == 1),
#                                 panel.id = c("CORREO", "periodos_pruebas"),
#                                 cluster = ~prueba); summary(virt_placebo2)

# Segundas diferencias
segunda_diferencia1 = feols(calificacion_pruebas ~ I(AV * EX2) + AV + EX2,
                      data = base_limpia_panel %>%  filter(PL == 0),
                      panel.id = c("CORREO", "periodos_pruebas"),
                      cluster = ~prueba); summary(segunda_diferencia1)

segunda_diferencia2 = feols(calificacion_pruebas ~ I(AV * EX2) + AV + EX2,
                            data = base_limpia_panel %>%  filter(PL == 1),
                            panel.id = c("CORREO", "periodos_pruebas"),
                            cluster = ~prueba); summary(segunda_diferencia2)


# Triple diferencia
triple_diferencia = feols(calificacion_pruebas ~ I(AV * PL * EX2) + AV + PL + EX2 + I(AV * EX2) + I(AV * PL) + I(PL * EX2) ,
                            data = base_limpia_panel,
                            panel.id = c("CORREO", "periodos_pruebas"),
                            cluster = ~prueba); summary(triple_diferencia)

# 3.4 Regresiones para el cálculo de los errores estándar usando la metodología de Chetty (tread_prod = Placebos (PL)) ----

# # Nota: Se calculan los errores estándar usando la metodología de Chetty. "Equivalencia" de tratamientos:
# ## treat_prod = Placebos (PL)
# ## treat_store = Virtuales (AV)
# ## treat_time = Dummy para la prueba 2/test 2 (EX2)
# 
# # Variables de interacción:
# ##  i1 = I(PL * EX2) (treat_products * treat_time)
# ##  i2 = I(PL * AV) (treat_products * treat_store)
# ##  i3 = I(AV * EX2) (treat_store * treat_time)
# ##  TREATMENT= I(PL * AV * EX2) (treat_products * treat_store * treat_time)
# 
# # Instruidos
# 
# # 3.3.1 Virtuales (AV = 1, panel de arriba)
# 
# # Pre
# virt1 = feols(calificacion_pruebas ~ INST,
#               data = base_limpia_panel %>%  filter(EX2 == 0) %>% filter(AV == 1),
#               panel.id = c("CORREO", "periodos_pruebas"),
#               cluster = ~prueba); summary(virt1)
# 
# # Post
# virt2 = feols(calificacion_pruebas ~ INST,
#               data = base_limpia_panel %>%  filter(EX2 == 1) %>% filter(AV == 1),
#               panel.id = c("CORREO", "periodos_pruebas"),
#               cluster = ~prueba); summary(virt2)
# 
# 
# # Diferencia de tiempo para instruidos
# virt_diff_time_inst = feols(calificacion_pruebas ~ EX2,
#                            data = base_limpia_panel %>%  filter(INST == 0) %>% filter(AV == 1),
#                            panel.id = c("CORREO", "periodos_pruebas"),
#                            cluster = ~prueba); summary(virt_diff_time_inst)
# 
# # Diferencia de tiempo para placebos
# virt_diff_time_placebo = feols(calificacion_pruebas ~ I(INST * EX2) + INST ,
#                            data = base_limpia_panel  %>% filter(AV == 1),
#                            panel.id = c("CORREO", "periodos_pruebas"),
#                            cluster = ~prueba); summary(virt_diff_time_placebo)
# 
# # 3.3.2 Presenciales (AV = 0, panel de abajo)
# 
# # Pre
# pres1 = feols(calificacion_pruebas ~ INST,
#                   data = base_limpia_panel %>%  filter(EX2 == 0) %>% filter(AV == 0),
#                   panel.id = c("CORREO", "periodos_pruebas"),
#                   cluster = ~prueba); summary(pres1)
# 
# # Post
# pres2 = feols(calificacion_pruebas ~ INST,
#                   data = base_limpia_panel %>%  filter(EX2 == 1) %>% filter(AV == 0),
#                   panel.id = c("CORREO", "periodos_pruebas"),
#                   cluster = ~prueba); summary(pres2)
# 
# 
# # Diferencia de tiempo para instruidos
# pres_diff_time_inst = feols(calificacion_pruebas ~ EX2,
#                                 data = base_limpia_panel %>%  filter(INST == 0) %>% filter(AV == 0),
#                                 panel.id = c("CORREO", "periodos_pruebas"),
#                                 cluster = ~prueba); summary(pres_diff_time_inst)
# 
# # Diferencia de tiempo para placebos
# pres_diff_time_placebo = feols(calificacion_pruebas ~ I(INST * EX2) + INST ,
#                                 data = base_limpia_panel %>% filter(AV == 0),
#                                 panel.id = c("CORREO", "periodos_pruebas"),
#                                 cluster = ~prueba); summary(pres_diff_time_placebo)
# 
# # Placebos (Columna 2, artículo de Chetty)
# 
# # Nota: No se pueden generar!
# 
# # placebos_virt1 = feols(calificacion_pruebas ~ 1 ,
# #                    data = base_limpia_panel %>% filter(PL == 1) %>% filter(AV == 1) %>%  filter(EX2 == 0),
# #                    panel.id = c("CORREO", "periodos_pruebas"),
# #                    cluster = ~prueba); summary(placebos_virt1)
# 
# 
# # placebos_virt2 = feols(calificacion_pruebas ~ 1 ,
# #                    data = base_limpia_panel %>% filter(PL == 1) %>% filter(AV == 1) %>%  filter(EX2 == 1),
# #                    panel.id = c("CORREO", "periodos_pruebas"),
# #                    cluster = ~prueba); summary(placebos_virt2)
# 
# 
# placebos_pres1 = feols(calificacion_pruebas ~ 1 ,
#                       data = base_limpia_panel %>% filter(INST == 1) %>% filter(AV == 0) %>%  filter(EX2 == 0),
#                       panel.id = c("CORREO", "periodos_pruebas"),
#                       cluster = ~prueba); summary(placebos_pres1)
# 
# 
# placebos_pres2 = feols(calificacion_pruebas ~ 1 ,
#                       data = base_limpia_panel %>% filter(INST == 1) %>% filter(AV == 0) %>%  filter(EX2 == 1),
#                       panel.id = c("CORREO", "periodos_pruebas"),
#                       cluster = ~prueba); summary(placebos_pres2)
# 
# # Segundas diferencias
# 
# # (AV = 1)
# segunda_diferencia1 = feols(calificacion_pruebas ~ I(INST * EX2) + INST + EX2,
#                             data = base_limpia_panel %>%  filter(AV == 1),
#                             panel.id = c("CORREO", "periodos_pruebas"),
#                             cluster = ~prueba); summary(segunda_diferencia1)
# 
# # (AV = 0)
# segunda_diferencia2 = feols(calificacion_pruebas ~ I(INST * EX2) + INST + EX2,
#                             data = base_limpia_panel %>%  filter(AV == 0),
#                             panel.id = c("CORREO", "periodos_pruebas"),
#                             cluster = ~prueba); summary(segunda_diferencia2)
# 
# 
# # Triple diferencia
# triple_diferencia = feols(calificacion_pruebas ~ I(INST * AV * EX2) + INST + EX2 + AV + I(INST * EX2) + I(INST * AV) + I(AV * EX2) ,
#                           data = base_limpia_panel,
#                           panel.id = c("CORREO", "periodos_pruebas"),
#                           cluster = ~prueba); summary(triple_diferencia)

# 4. Regresiones --------------------------------------------------

# 4.0 Fixed effects (fixest) formulas ----

fe1_formula = calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) | fecha_pruebas + semestre_ingreso 
fe2_formula = calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) +  minutos_prueba | fecha_pruebas + semestre_ingreso 
fe3_formula = calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) +  minutos_prueba + PBM + PUNTAJE_ADMISION_FINAL_FINAL | fecha_pruebas + semestre_ingreso 
fe4_formula = calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) +  minutos_prueba + PBM + PUNTAJE_ADMISION_FINAL_FINAL + SEXO | fecha_pruebas + semestre_ingreso
fe5_formula = calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) +  minutos_prueba + PBM + PUNTAJE_ADMISION_FINAL_FINAL + SEXO + PAPA_PERIODO_FINAL | fecha_pruebas + semestre_ingreso 
fe6_formula = calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) +  minutos_prueba + PBM + PUNTAJE_ADMISION_FINAL_FINAL + SEXO + PAPA_PERIODO_FINAL + Matriculas_recategorizado | fecha_pruebas + semestre_ingreso
fe7_formula = calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * AV) + I(EX2 * PL) +  PAPA_PERIODO_FINAL | fecha_pruebas + semestre_ingreso

# 4.1 Fixed effects (fixest) models ----

# Regresion 1
fe1 = feols(fe1_formula, 
            data = base_limpia_panel, 
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(fe1)

# Regresion 2
fe2 = feols(fe2_formula, 
      data = base_limpia_panel, 
      panel.id = c("CORREO", "periodos_pruebas"),
      cluster = ~prueba); summary(fe2)

# Regresion 3
fe3 = feols(fe3_formula, 
      data = base_limpia_panel, 
      panel.id = c("CORREO", "periodos_pruebas"),
      cluster = ~prueba); summary(fe3)

# Regresion 4
fe4 = feols(fe4_formula, 
            data = base_limpia_panel, 
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(fe4)

# Regresion 5
fe5 = feols(fe5_formula, 
            data = base_limpia_panel, 
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(fe5)

# Regresion 6
fe6 = feols(fe6_formula, 
            data = base_limpia_panel, 
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(fe6)

# Regresion 7
fe7 = feols(fe7_formula, 
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

# 6. Ejercicios de robustez ----

# 6.1 Estimaciones por variable instrumental

# Resumen estimaciones IV: 
## Variable endógena: Virtualidad (AV): 
## Instrumentos:  
### Semestre de nacimiento 
### Número de matrículas

# 6.1 Variable Instrumental ----

# 6.1.0 Estimaciones IV (formulas) ----

iv_reg_semestre_1_formula = calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * PL) | fecha_pruebas + semestre_ingreso | I(EX2 * AV) ~ I(EX2 * mes_nacimiento)
iv_reg_semestre_2_formula = calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * PL) +  minutos_prueba | fecha_pruebas + semestre_ingreso | I(EX2 * AV) ~ I(EX2 * mes_nacimiento)
iv_reg_semestre_3_formula = calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * PL) +  minutos_prueba + PBM + PUNTAJE_ADMISION_FINAL_FINAL | fecha_pruebas + semestre_ingreso | I(EX2 * AV) ~ I(EX2 * mes_nacimiento) 
iv_reg_semestre_4_formula = calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * PL) +  minutos_prueba + PBM + PUNTAJE_ADMISION_FINAL_FINAL + SEXO | fecha_pruebas + semestre_ingreso | I(EX2 * AV) ~ I(EX2 * mes_nacimiento)
iv_reg_semestre_5_formula = calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * PL) +  minutos_prueba + PBM + PUNTAJE_ADMISION_FINAL_FINAL + SEXO + PAPA_PERIODO_FINAL | fecha_pruebas + semestre_ingreso | I(EX2 * AV) ~ I(EX2 * mes_nacimiento)
iv_reg_semestre_6_formula = calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * PL) +  minutos_prueba + PBM + PUNTAJE_ADMISION_FINAL_FINAL + SEXO + PAPA_PERIODO_FINAL + Matriculas_recategorizado | fecha_pruebas + semestre_ingreso | I(EX2 * AV) ~ I(EX2 * mes_nacimiento)
iv_reg_semestre_7_formula = calificacion_pruebas ~ EX2 + AV + PL + I(EX2 * PL) +  PAPA_PERIODO_FINAL | fecha_pruebas + semestre_ingreso | I(EX2 * AV) ~ I(EX2 * mes_nacimiento)

# 6.1.1 Estimaciones IV (Instrumento: semestre) ----

# Regresion variable instrumental "semestre" 1
iv_reg_semestre_1 = feols(iv_reg_semestre_1_formula, 
            data = base_limpia_panel, 
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(iv_reg_semestre_1)

# Regresion variable instrumental "semestre" 2
iv_reg_semestre_2 = feols(iv_reg_semestre_2_formula, 
            data = base_limpia_panel, 
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(iv_reg_semestre_2)

# Regresion variable instrumental "semestre" 3
iv_reg_semestre_3 = feols(iv_reg_semestre_3_formula, 
            data = base_limpia_panel, 
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(iv_reg_semestre_3)

# Regresion variable instrumental "semestre" 4
iv_reg_semestre_4 = feols(iv_reg_semestre_4_formula, 
            data = base_limpia_panel, 
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(iv_reg_semestre_4)

# Regresion variable instrumental "semestre" 5
iv_reg_semestre_5 = feols(iv_reg_semestre_5_formula, 
            data = base_limpia_panel,  
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(iv_reg_semestre_5)

# Regresion variable instrumental "semestre" 6
iv_reg_semestre_6 = feols(iv_reg_semestre_6_formula, 
            data = base_limpia_panel, 
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(iv_reg_semestre_6)

# Regresion variable instrumental "semestre" 7
iv_reg_semestre_7 = feols(iv_reg_semestre_7_formula, 
            data = base_limpia_panel, 
            panel.id = c("CORREO", "periodos_pruebas"),
            cluster = ~prueba); summary(iv_reg_semestre_7)

# Exportación resultados: Variable Instrumental

## Usando "texreg"
texreg(
  list(iv_reg_semestre_1, iv_reg_semestre_2, iv_reg_semestre_3, iv_reg_semestre_4, iv_reg_semestre_5, iv_reg_semestre_6, iv_reg_semestre_7),
  file = file.path(resultados_directory, "regresiones_variable_instrumental_semestre_nacimiento.tex"),
  caption = "Efecto del cambio en la modalidad de enseñanza-aprendizaje: resultados de estimación (Variable instrumental 'Semestre Nacimiento')",
  label = "tab:regresiones_efectos_fijos_IV",
  digits = 3,
  custom.model.names = c("(IV 1)", "(IV 2)", "(IV 3)", "(IV 4)", "(IV 5)", "(IV 6)", "(IV 7)"),
  custom.coef.map = list("EX2" =  "Prueba 2 (T)",
                         "AV" = "Virtual (V)",
                         "PL" = "Placebo (P)",
                         "fit_I(EX2 * AV)" = "(Prueba 2 (T) * Virtual (V)) estimada primera etapa",
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

# 6.2 Randomización ----

# Test de randomización para la regresion 1 de efectos fijos
test_de_randomizacion(df_original = base_limpia, 
                      formula_estimacion = fe1_formula,
                      coef_interes = "I(EX2 * AV)",
                      estimacion_original = fe1,
                      sample_size = 500)

# Test de randomización para la regresion 2 de efectos fijos
suppressMessages(test_de_randomizacion(df_original = base_limpia, 
                      formula_estimacion = fe6_formula,
                      coef_interes = "I(EX2 * AV)",
                      estimacion_original = fe6,
                      sample_size = 500))

# Test de randomización para la regresion 3 de efectos fijos
test_de_randomizacion(df_original = base_limpia, 
                      formula_estimacion = fe7_formula,
                      coef_interes = "I(EX2 * AV)",
                      estimacion_original = fe7,
                      sample_size = 500)

# 6.3 Permutación ----

