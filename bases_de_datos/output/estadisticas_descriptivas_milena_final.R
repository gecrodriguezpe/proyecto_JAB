# Estadisticas Descriptivas 

# Preliminares ----

# Librerias 
library(tidyverse)
library(readxl)
library(glue)
library(stargazer)
library(writexl)
library(rlang)

# Limpieza del entorno de trabaja 
rm(list = ls())

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

# Base de datos generada por Milena 
base_virtuales = read_xlsx("base_principal_procesada-Milena1.xlsx", sheet = "Virtuales")

base_presenciales = read_xlsx("base_principal_procesada-Milena1.xlsx", sheet = "Presenciales")

base_placebos = read_xlsx("base_principal_procesada-Milena1.xlsx", sheet = "Placebos")

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
         Estrato_recategorizado = as.factor(Estrato_recategorizado),
         Matriculas_recategorizado = as.factor(Matriculas_recategorizado),
         Tipo_colegio_recategorizado = as.factor(Tipo_colegio_recategorizado),
         minutos_prueba1 = as.numeric(str_extract(`Tiempo requerido_prueba1`, "\\d+")),
         segundos_prueba1 = as.numeric(str_extract(str_remove(`Tiempo requerido_prueba1`, "\\d+ minutos "), "\\d+")),
         tiempo_total_prueba1 = minutos_prueba1 * 60 + segundos_prueba1,
         minutos_prueba2 = as.numeric(str_extract(`Tiempo requerido_prueba2`, "\\d+")),
         segundos_prueba2 = as.numeric(str_extract(str_remove(`Tiempo requerido_prueba2`, "\\d+ minutos "), "\\d+")),
         tiempo_total_prueba2 = minutos_prueba2 * 60 + segundos_prueba2)

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

# 2.0 Funciones ---- 

# Transformacion variables base de datos
transformacion_bases = function(base){
  
  nueva_base = base %>% 
    mutate(PERIODO = as.factor(PERIODO), 
           SEXO = as.factor(SEXO),
           EDAD = as.numeric(EDAD),
           MUNICIPIO_PROCEDENCIA = as.factor(MUNICIPIO_PROCEDENCIA),
           ESTRATO = as.factor(ESTRATO),
           MATRICULAS = as.factor(MATRICULAS),
           TIPCOLEGIO = as.factor(TIPCOLEGIO), 
           PBM = as.numeric(PBM),
           MUNICIPIO_COLEGIO = as.factor(MUNICIPIO_COLEGIO),
           PROM_ACADEMICO_ACTUAL = as.numeric(PROM_ACADEMICO_ACTUAL), 
           PAPA_PERIODO = as.numeric(PAPA_PERIODO), 
           DOCUMENTO = as.factor(DOCUMENTO),
           SUBACCESO = as.factor(SUBACCESO),
           Periodo = as.factor(Periodo),
           Periodo_agregado = as.factor(Periodo_agregado), 
           Estrato_recategorizado = estrato_nuevas_categorias(ESTRATO),
           Matriculas_recategorizado = matriculas_nuevas_categorias(MATRICULAS),
           Tipo_colegio_recategorizado = tipo_colegio_nuevas_categorias(TIPCOLEGIO)) 
  #select(NOMBRES, APELLIDO1, APELLIDO2, DOCUMENTO, CORREO, PERIODO, SEXO, EDAD, MUNICIPIO_PROCEDENCIA, 
  # ESTRATO, MATRICULAS, TIPCOLEGIO, PBM, MUNICIPIO_COLEGIO, PROM_ACADEMICO_ACTUAL, PAPA_PERIODO,
  # SUBACCESO, TIPO_NIVEL)
  
  return(nueva_base)
  
}

# Funciones que permiten clasificar mejor las variables categóricas

### Estrato socioneconomico re categorizado
# estrato_nuevas_categorias <- function(x) {
#   case_when(
#     x == 1 ~ "bajo",
#     x %in% c(2, 3) ~ "bajo-medio",
#     x %in% c(4, 5, 6) ~ "medio-alto",
#     x %in% c("No Estratificado", "No Informa") ~ "No informado",
#     TRUE ~ as.character(x)
#   )
# }

estrato_nuevas_categorias <- function(x) {
  case_when(
    x %in% c(1, 2) ~ "bajo",
    x %in% c(3, 4) ~ "medio",
    x %in% c(5, 6) ~ "alto",
    x %in% c("No Estratificado", "No Informa") ~ "No informado",
    TRUE ~ as.character(x)
  )
}

### Número de matrículas re categorizado
matriculas_nuevas_categorias <- function(x) {
  case_when(
    x %in% c(1, 2, 3) ~ "Numero de matriculas (1, 2, 3)",
    x %in% c(4, 5, 6) ~ "Numero de matriculas (4, 5, 6)",
    x %in% c(7, 8, 9, 10) ~ "Numero de matriculas (7, 8, 9, 10)",
    x %in% c(11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26) ~ "Numero de matriculas (Más de 10)",
    TRUE ~ as.character(x)
  )
}

### Tipo de colegio
tipo_colegio_nuevas_categorias <- function(x) {
  case_when(
    x == "OFI" ~ "Público",
    x == "PRV" ~ "Privado",
    x %in% c("NOC", "OTR") ~ "No reporta",
    TRUE ~ as.character(x)
  )
}

# Estadísticas descriptivas ----

# Funciones para el cálculo de estadísticas descriptivas ----

# Variables categóricas
summary_variables_categoricas = function(base, nombre_var_periodo, var_categorica, var_periodo){
  
  # Categorias
  periodos = levels(base_principal[[nombre_var_periodo]])
  print(periodos)
  
  
  # Itero a través de cada una de las variables categóricas de interés 
  for (periodo in periodos){
    
    # Filtro la base: Ver la estadística descriptiva para cada base
    base_filtrada = base_principal %>% 
      filter({{var_periodo}} == periodo) %>% 
      group_by({{var_categorica}}) %>% 
      summarize(numero_observaciones = n(), 
                porcentaje = (n() / nrow(.)) * 100)
    
    
    print(glue("\n\n\n{nombre_var_periodo}: {periodo}"))
    print(base_filtrada)
    
    
  }
  
}

# Variables numéricas
summary_variables_numericas = function(base, group_var, value_var){
  
  resultado = base %>%
    group_by({{group_var}}) %>% 
    summarise(
      media = mean({{value_var}}, na.rm = TRUE),
      cuartil1 = quantile({{value_var}}, 0.25, na.rm = TRUE),
      mediana = quantile({{value_var}}, 0.5, na.rm = TRUE),
      cuartil2 = quantile({{value_var}}, 0.75, na.rm = TRUE)
    )
  
  return(resultado)  
  
}

# Grupos desagregado ----

# Variables categóricas

## Estrato socioeconómico
summary_variables_categoricas(base = base_principal,
                              nombre_var_periodo = "Periodo", 
                              var_categorica = ESTRATO, 
                              var_periodo = Periodo)


## Estrato socioeconómico recategorizado
summary_variables_categoricas(base = base_principal,
                              nombre_var_periodo = "Periodo", 
                              var_categorica = Estrato_recategorizado, 
                              var_periodo = Periodo)

## Matriculas recategorizado
summary_variables_categoricas(base = base_principal,
                              nombre_var_periodo = "Periodo", 
                              var_categorica = Matriculas_recategorizado, 
                              var_periodo = Periodo)

## Tipo colegio
summary_variables_categoricas(base = base_principal,
                              nombre_var_periodo = "Periodo", 
                              var_categorica = Tipo_colegio_recategorizado, 
                              var_periodo = Periodo)

# Variables numéricas

## PBM
summary_variables_numericas(base = base_principal, 
                            group_var = Periodo, 
                            value_var = PBM)

## Promedio académico (PAPA)
summary_variables_numericas(base = base_principal, 
                            group_var = Periodo, 
                            value_var = PROM_ACADEMICO_ACTUAL)

## Edad
summary_variables_numericas(base = base_principal, 
                            group_var = Periodo, 
                            value_var = EDAD)


# Grupos Agregados ----

# Variables categóricas

## Estrato socioeconómico
summary_variables_categoricas(base = base_principal,
                              nombre_var_periodo = "Periodo_agregado", 
                              var_categorica = ESTRATO, 
                              var_periodo = Periodo_agregado)

## Estrato socioeconómico recategorizado
summary_variables_categoricas(base = base_principal,
                              nombre_var_periodo = "Periodo_agregado", 
                              var_categorica = Estrato_recategorizado, 
                              var_periodo = Periodo_agregado)

## Matriculas recategorizado
summary_variables_categoricas(base = base_principal,
                              nombre_var_periodo = "Periodo_agregado", 
                              var_categorica = Matriculas_recategorizado, 
                              var_periodo = Periodo_agregado)

## Tipo colegio
summary_variables_categoricas(base = base_principal,
                              nombre_var_periodo = "Periodo_agregado", 
                              var_categorica = Tipo_colegio_recategorizado, 
                              var_periodo = Periodo_agregado)

# Variables numéricas

## PBM
summary_variables_numericas(base = base_principal, 
                            group_var = Periodo_agregado, 
                            value_var = PBM)

## Promedio académico (PAPA)
summary_variables_numericas(base = base_principal, 
                            group_var = Periodo_agregado, 
                            value_var = PROM_ACADEMICO_ACTUAL)

## Edad
summary_variables_numericas(base = base_principal, 
                            group_var = Periodo_agregado, 
                            value_var = EDAD)

## Puntaje de admisión 
summary_variables_numericas(base = base_principal, 
                            group_var = Periodo_agregado, 
                            value_var = PUNTAJE_ADMISION_FINAL_FINAL)

# Diferencias de medias (estadístico-t) ---------------------------------------------------

# # 2021-2S
# todas_2021_2s = todas %>% 
#   filter(Periodo == "2021-2S")
# 
# todas_2021_2s_notas1 = todas_2021_2s$`Calificación/10.00_prueba1`
# todas_2021_2s_notas2 = todas_2021_2s$`Calificación/10.00_prueba2`
# 
# # 2022-1S, 2022-2S and 2023-1S
# todas_C = todas %>% 
#   filter(Periodo %in% c("2022-1S", "2022-2S", "2023-1S"))
# 
# todas_C_notas1 = todas_2022_1s$`Calificación/10.00_prueba1`
# todas_C_notas2 = todas_2022_1s$`Calificación/10.00_prueba2`
# 
# # t-test para diferencia de medias
# t.test(todas_2021_2s_notas1, todas_C_notas1)
# t.test(todas_2021_2s_notas2, todas_C_notas2)
# 
# # 3. Revisión con placebos ----
# 
# # Placebos
# todas_placebo = todas %>% 
#   filter(Periodo == "placebos")
# 
# todas_placebo_notas1 = todas_placebo$`Calificación/10.00_prueba1`
# todas_placebo_notas2 = todas_placebo$`Calificación/10.00_prueba2`
# 
# # t-test para diferencia de medias virtual vs Placebo
# t.test(todas_2021_2s_notas1, todas_placebo_notas1)
# t.test(todas_2021_2s_notas2, todas_placebo_notas2)
# 
# # t-test para diferencia de medias presencial 2022-1S vs Placebo
# t.test(todas_2022_1s_notas1, todas_placebo_notas1)
# t.test(todas_2022_1s_notas2, todas_placebo_notas2)
# 
# # t-test para diferencia de medias presencial (control) vs Placebo
# t.test(todas_C_notas1, todas_placebo_notas1)
# t.test(todas_C_notas2, todas_placebo_notas2)
# 
# 
# # Regresiones ----
# 
# base_regresiones = base_principal %>% 
#   filter(Periodo_agregado != "placebos") %>% 
#   rename(calificacion_prueba1 = `Calificación/10.00_prueba1`,
#          calificacion_prueba2 = `Calificación/10.00_prueba2`); glimpse(base_regresiones)
# 
# ## Parte1
# 
# modelo1_prueba1 = lm(calificacion_prueba1 ~ Periodo_agregado, data = base_regresiones); summary(modelo1_prueba1)
# modelo2_prueba1 = lm(calificacion_prueba1 ~ Periodo_agregado + PBM + PAPA_PERIODO , data = base_regresiones); summary(modelo2_prueba1)
# 
# ## Parte2
# 
# modelo1_prueba2 = lm(calificacion_prueba2 ~ Periodo_agregado, data = base_regresiones); summary(modelo1_prueba2)
# modelo2_prueba2 = lm(calificacion_prueba2 ~ Periodo_agregado + PBM + PAPA_PERIODO , data = base_regresiones); summary(modelo2_prueba2)
# 
# # Tablas de regresión ----
# 
# # Resultados
# stargazer(modelo2_prueba2, title = "Resultados de regresion Prueba 2", 
#           type = "latex", 
#           style = "AER",
#           column.labels = c("MCO"),
#           keep.stat=c("n","rsq"))


# Scatterplots ------------------------------------------------------------

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

# Scatterplots con placebos ----

# Scaterplots prueba 1

## Scatter plot de prueba1 vs prueba2
scatterplot(base_principal, `Calificación/10.00_prueba1`, `Calificación/10.00_prueba2`, "Calificaciones prueba1 vs prueba2 con placebos", periodo_agregado = Periodo_agregado)

## Scatter plot de PBM vs prueba1 
scatterplot(base_principal, PBM, `Calificación/10.00_prueba1`, "PBM vs Calificación prueba1 con placebos", periodo_agregado = Periodo_agregado)

## Scatter plot de PAPA periodo vs prueba1 
scatterplot(base_principal, PROM_ACADEMICO_ACTUAL, `Calificación/10.00_prueba1`, "Promedio académico vs Calificación prueba1 con placebos", periodo_agregado = Periodo_agregado)

## Scatter plot Minutos vs resultados de pruebas 
scatterplot(base_principal, minutos_prueba1, `Calificación/10.00_prueba1`, "Minutos prueba vs Calificación pruebas con placebos", periodo_agregado = Periodo_agregado)

# Scaterplots prueba 2

## Scatter plot de PBM vs prueba2 
scatterplot(base_principal, PBM, `Calificación/10.00_prueba2`, "PBM vs Calificación prueba2 con placebos", periodo_agregado = Periodo_agregado)

## Scatter plot de PAPA periodo vs prueba2  
scatterplot(base_principal, PROM_ACADEMICO_ACTUAL, `Calificación/10.00_prueba2`, "Promedio académico vs Calificación prueba2 con placebos", periodo_agregado = Periodo_agregado)

## Scatter plot Minutos vs resultados de pruebas 
scatterplot(base_principal, minutos_prueba2, `Calificación/10.00_prueba2`, "Minutos prueba vs Calificación pruebas con placebos", periodo_agregado = Periodo_agregado)


# Scatterplots sin placebos ----

# Scatterplots prueba 1

# Base de datos sin placebos
base_scatter = base_principal %>% 
  filter(Periodo_agregado != "placebos") 

## Scatter plot de prueba1 vs prueba2
scatterplot(base_scatter, `Calificación/10.00_prueba1`, `Calificación/10.00_prueba2`, "Calificaciones prueba1 vs prueba2 sin placebos", periodo_agregado = Periodo_agregado)

## Scatter plot de PBM vs prueba1 
scatterplot(base_scatter, PBM, `Calificación/10.00_prueba1`, "PBM vs Calificación prueba1 sin placebos", periodo_agregado = Periodo_agregado)

## Scatter plot de PAPA periodo vs prueba1
scatterplot(base_scatter, PROM_ACADEMICO_ACTUAL, `Calificación/10.00_prueba1`, "Promedio académico vs Calificación prueba1 sin placebos", periodo_agregado = Periodo_agregado)

## Scatter plot Minutos vs resultados de pruebas 
scatterplot(base_scatter, minutos_prueba1, `Calificación/10.00_prueba1`, "Minutos prueba vs Calificación pruebas sin placebos", periodo_agregado = Periodo_agregado)

# Scatterplots prueba 2

## Scatter plot de PBM va prueba2 
scatterplot(base_scatter, PBM, `Calificación/10.00_prueba2`, "PBM vs Calificación prueba2 sin placebos", periodo_agregado = Periodo_agregado)

## Scatter plot de PAPA periodo vs prueba2 
scatterplot(base_scatter, PROM_ACADEMICO_ACTUAL, `Calificación/10.00_prueba2`, "Promedio académico vs Calificación prueba2 sin placebos", periodo_agregado = Periodo_agregado)

## Scatter plot Minutos vs resultados de pruebas 
scatterplot(base_scatter, minutos_prueba2, `Calificación/10.00_prueba2`, "Minutos prueba vs Calificación pruebas sin placebos", periodo_agregado = Periodo_agregado)
