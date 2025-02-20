# Estadisticas Descriptivas 

# Preliminares ----

# Librerias 
library(tidyverse)
library(readxl)
library(glue)
library(stargazer)
library(writexl)

# Limpieza del entorno de trabaja 
rm(list = ls())

# Rutas de trabajo 
matriculados_path = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/bases_regresiones/matriculados"
output_path = "C:/Users/germa/Desktop/UNAL/Proyecto JAB/Experimentos/2023_Full/tests_asignaturas/bases_de_datos/output"

# Importación bases de datos ----

# Importacion bases de matriculas 

setwd(matriculados_path)

# Matriculas
matricula_2020_1 = read_xlsx("Matriculados_fce_2020-1S.xlsx")
matricula_2020_2 = read_xlsx("Matriculados_fce_2020-2S.xlsx")
matricula_2021_1 = read_xlsx("Matriculados_fce_2021-1S.xlsx")
matricula_2021_2 = read_xlsx("Matriculados_fce_2021-2S.xlsx")
matricula_2022_1 = read_xlsx("Matriculados_fce_2022-1S.xlsx")
matricula_2022_2 = read_xlsx("Matriculados_fce_2022-2S.xlsx")
matricula_2023_1 = read_xlsx("Matriculados_fce_2023-1S.xlsx")
matricula_2023_2 = read_xlsx("Matriculados_fce_2023-2S.xlsx")

# Resultado de las pruebas 

setwd(output_path)

resultados_pruebas = read_xlsx("resultados_pruebas_agregado.xlsx")

# Sin econometría 2
# resultados_pruebas = resultados_pruebas %>% 
#   filter(asignatura != "econometria2")

# Funciones ---- 

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
           SUBACCESO = as.factor(SUBACCESO)) 
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


# Procesamiento de las bases de datos ----

## Matriculas 

# Transformacion bases de datos 
matricula_2020_1 = transformacion_bases(matricula_2020_1)
matricula_2020_2 = transformacion_bases(matricula_2020_2)
matricula_2021_1 = transformacion_bases(matricula_2021_1)
matricula_2021_2 = transformacion_bases(matricula_2021_2)
matricula_2022_1 = transformacion_bases(matricula_2022_1)
matricula_2022_2 = transformacion_bases(matricula_2022_2)
matricula_2023_1 = transformacion_bases(matricula_2023_1)
matricula_2023_2 = transformacion_bases(matricula_2023_2)


# Todos los matriculados 
matriculados = bind_rows(matricula_2020_1, matricula_2020_2, matricula_2021_1, matricula_2021_2, 
                         matricula_2022_1, matricula_2022_2, matricula_2023_1, matricula_2023_2) %>% 
  distinct(CORREO, .keep_all = TRUE)

# Construccion de la base principal para el analisis ----

# Base que incluye la información de los estudiantes que presentaron las pruebas
base_principal = inner_join(resultados_pruebas, matriculados, by = c("CORREO"))

# Agregación de los periodos 
base_principal = base_principal %>%
  mutate(Periodo_agregado = case_when(
    Periodo == "2021-2S" ~ "virtual",
    Periodo %in% c("2022-1S", "2022-2S", "2023-1S") ~ "presencial",
    TRUE ~ Periodo
  ))

base_principal = transformacion_bases(base_principal) 

# Procesamiento adicional base de datos
base_principal = base_principal %>% 
  mutate(Periodo = as.factor(Periodo),
         Periodo_agregado = as.factor(Periodo_agregado), 
         Estrato_recategorizado = estrato_nuevas_categorias(ESTRATO),
         Matriculas_recategorizado = matriculas_nuevas_categorias(MATRICULAS),
         Tipo_colegio_recategorizado = tipo_colegio_nuevas_categorias(TIPCOLEGIO)) 

# Exportación de la base principal procesada
setwd(output_path)

# Exportación como archivo ".xlsx"
write_xlsx(base_principal, "base_principal_procesada.xlsx")

# Exportación como archivo ".rds"
saveRDS(base_principal, file = "base_principal_procesada.rds")


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


# Regresiones ----

base_regresiones = base_principal %>% 
  filter(Periodo_agregado != "placebos") %>% 
  rename(calificacion_prueba1 = `Calificación/10.00_prueba1`,
         calificacion_prueba2 = `Calificación/10.00_prueba2`); glimpse(base_regresiones)

## Parte1

modelo1_prueba1 = lm(calificacion_prueba1 ~ Periodo_agregado, data = base_regresiones); summary(modelo1_prueba1)
modelo2_prueba1 = lm(calificacion_prueba1 ~ Periodo_agregado + PBM + PAPA_PERIODO , data = base_regresiones); summary(modelo2_prueba1)

## Parte2

modelo1_prueba2 = lm(calificacion_prueba2 ~ Periodo_agregado, data = base_regresiones); summary(modelo1_prueba2)
modelo2_prueba2 = lm(calificacion_prueba2 ~ Periodo_agregado + PBM + PAPA_PERIODO , data = base_regresiones); summary(modelo2_prueba2)

# Tablas de regresión ----

# Resultados
stargazer(modelo2_prueba2, title = "Resultados de regresion Prueba 2", 
          type = "latex", 
          style = "AER",
          column.labels = c("MCO"),
          keep.stat=c("n","rsq"))

