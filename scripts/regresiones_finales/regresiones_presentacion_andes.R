# Datos panel: Ejercicio de Milena

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

# Inferencia estadística
library(car)

# Resumen de resultados de regresión
library(broom)

# Rutas bases de datos
bases_matriculados = dir_ls(glue("{here()}/bases_de_datos/bases_regresiones/matriculados/"), glob = "*.xlsx")
bases_puntaje_admision = dir_ls(glue("{here()}/bases_de_datos/puntaje_admision/"), glob = "*.xlsx")
bases_output = dir_ls(glue("{here()}/bases_de_datos/output/"), glob = "*.xlsx")

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

base_milena = bases_output[2]

# Base de datos generada por Milena + encuesta de percepción 

base_virtuales = read_xlsx(base_milena, sheet = "Virtuales")

base_presenciales = read_xlsx(base_milena, sheet = "Presenciales")

base_placebos = read_xlsx(base_milena, sheet = "Placebos")

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

# 3. Triple diferencia -------------------------------------------------------

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
  
# 4. Base de datos tipo panel --------------------------------------------------

# Panel completo (incluyendo placebos) ----

# Construcción del panel
panel = pdata.frame(base_limpia_panel, index = c("CORREO", "periodos_pruebas"))

# Para conocer las dimensiones del panel
pdim(panel)

# 5. Regresiones -------------------------------------------------------------

# 5.0 Fórmulas ----

formula_reg1 = calificacion_pruebas ~ EX2 + AV + PL + EX2 * AV + EX2 * PL
formula_reg2 = calificacion_pruebas ~ EX2 + AV + PL + EX2 * AV + EX2 * PL + prueba
formula_reg3 = calificacion_pruebas ~ EX2 + AV + PL + EX2 * AV + EX2 * PL + prueba + PBM  
formula_reg4 = calificacion_pruebas ~ EX2 + AV + PL + EX2 * AV + EX2 * PL + prueba + PBM  + minutos_prueba + PUNTAJE_ADMISION_FINAL_FINAL


# 5.1 plm (pooled OLS) ----

# Modelos de regresión 

## Regresión 1
pooled_ols_reg1 = plm(formula_reg1, 
                      data = panel, 
                      model = "pooling"); summary(pooled_ols_reg1)

## Regresión 2
pooled_ols_reg2 = plm(formula_reg2, 
                      data = panel, 
                      model = "pooling"); summary(pooled_ols_reg2)

## Regresión 3
pooled_ols_reg3 = plm(formula_reg3, 
                      data = panel, 
                      model = "pooling"); summary(pooled_ols_reg3)

## Regresión 4
pooled_ols_reg4 = plm(formula_reg4, 
                      data = panel, 
                      model = "pooling")


# Estimaciones con errores robustos tipo Panel Newey West (Donde se estima errores tipo HAC Newey West adaptados para datos panel)

## Modelo con regresoras

## Regresión pooled 1
pooled_ols_reg1_lmtest = coeftest(pooled_ols_reg1, 
                                  vcov = vcovNW(pooled_ols_reg1)); print(pooled_ols_reg1_lmtest)

## Regresión pooled 2
pooled_ols_reg2_lmtest = coeftest(pooled_ols_reg2, 
                                  vcov = vcovNW(pooled_ols_reg2)); print(pooled_ols_reg2_lmtest)

## Regresión pooled 3
pooled_ols_reg3_lmtest = coeftest(pooled_ols_reg3, 
                                  vcov = vcovNW(pooled_ols_reg3)); print(pooled_ols_reg3_lmtest)

## Regresión pooled 4
pooled_ols_reg4_lmtest = coeftest(pooled_ols_reg4, 
                                  vcov = vcovNW(pooled_ols_reg4)); print(pooled_ols_reg4_lmtest)

# Lista de modelos de regresión pooled
pooleed_ols_reg_list = list(pooled_ols_reg1_lmtest, pooled_ols_reg2_lmtest, 
                            pooled_ols_reg3_lmtest, pooled_ols_reg4_lmtest)

# Tabla de estimación usando stargazer
stargazer(pooleed_ols_reg_list, type="latex",           
          column.labels=c("Especificación 1","Especificación 2","Especificación 3", "Especificación 4"),
          keep.stat=c("n","rsq"), style = "AER")

# 5.2 plm (random effects) ----

# Modelos de regresión 

## Regresión random 1
random_effects_reg1 = plm(formula_reg1, 
                          data = panel, 
                          model = "random")

## Regresión random 2
random_effects_reg2 = plm(formula_reg2, 
                          data = panel, 
                          model = "random")

## Regresión random 3
random_effects_reg3 = plm(formula_reg3, 
                          data = panel, 
                          model = "random")

## Regresión random 4
random_effects_reg4 = plm(formula_reg4, 
                          data = panel, 
                          model = "random")


# Estimaciones con errores robustos tipo cluster, donde se clusteriza por individuo (CORREO)

## Regresión REs 1
random_effects_reg1_lmtest = coeftest(random_effects_reg1, 
                                      vcov = vcovHC(random_effects_reg1)); print(random_effects_reg1_lmtest)

## Regresión REs 2
random_effects_reg2_lmtest = coeftest(random_effects_reg2, 
                                      vcov = vcovHC(random_effects_reg2)); print(random_effects_reg2_lmtest)

## Regresión REs 3
random_effects_reg3_lmtest = coeftest(random_effects_reg3, 
                                      vcov = vcovHC(random_effects_reg3)); print(random_effects_reg3_lmtest)

## Regresión REs 4
random_effects_reg4_lmtest = coeftest(random_effects_reg4, 
                                      vcov = vcovHC(random_effects_reg4)); print(random_effects_reg4_lmtest)

# Lista de modelos de regresión Random Effects
random_effects_reg_list = list(random_effects_reg1_lmtest, random_effects_reg2_lmtest, 
                               random_effects_reg3_lmtest, random_effects_reg4_lmtest)

# Tabla de estimación usando stargazer
stargazer(random_effects_reg_list, type="latex",           
          column.labels=c("Especificación 1","Especificación 2","Especificación 3", "Especificación 4"),
          keep.stat=c("n","rsq"), style = "AER")  

# 6. Pruebas de hipótesis lineales ----

# Pruebas de hipótesis

hyp_EX21_PL1_EX21 = "EX21:PL1 + EX21 = 0"

# 6.1 Pruebas de hipótesis lineales para el modelo "pooled ols" ----

# hyp_AV1

## Regresión pooled 1
linearHypothesis(pooled_ols_reg1, hyp_EX21_PL1_EX21, vcov. = vcovNW(pooled_ols_reg1))

## Regresión pooled 2
linearHypothesis(pooled_ols_reg2, hyp_EX21_PL1_EX21, vcov. = vcovNW(pooled_ols_reg2))

## Regresión pooled 3
linearHypothesis(pooled_ols_reg3, hyp_EX21_PL1_EX21, vcov. = vcovNW(pooled_ols_reg3))

## Regresión pooled 4
linearHypothesis(pooled_ols_reg4, hyp_EX21_PL1_EX21, vcov. = vcovNW(pooled_ols_reg4))
